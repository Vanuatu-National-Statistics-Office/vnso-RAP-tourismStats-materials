## Set up ##

# Clear the environment
rm(list = ls())


#install.packages("dplyr") # Data manipulation
#install.packages("tidyverse") #Data science package
#install.packages("tidyr") #Data science packviewvage
#install.packages("janitor") #Data science packviewvage
#install.packages ("openxlsx") #reading excel files
#install.packages ("csvread") #reading csv files

# Packages
library(tidyverse) # standardised general packages for working with data and plotting
library(janitor) # cleaning data
library (dplyr) # data manipulation


# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Note the open data path
openDataFolder <- file.path(repository, "data", "open")

# Note the output folder path
outputFolder <- file.path(repository, "outputs")


# Read in the processed TOURISM data from secure folder of the repository 
tourismStatsFile <- file.path(secureDataFolder, "SEC_RAW_ASY_RawDataAndReferenceTables_31-12-19.csv")
tourismStats <- read_csv(tourismStatsFile, na = c("","NA","NULL","null"))


# Read in visitor classification tables
countryCodesByVisitors <- file.path(openDataFolder, "CountryCodes_VisByNat.csv")
countryVisitors <- read_csv(countryCodesByVisitors, na = c("","NA","NULL","null"))

# Read in resident classification tables
countryCodesByResidents <- file.path(openDataFolder, "CountryCodes_ResbyNat.csv")
countryResidents <- read_csv(countryCodesByResidents, na = c("","NA","NULL","null"))


# Read in the purpose of visit classifications into data frame
travelPurposeByCodes <-file.path(openDataFolder, "PurposeOfVisitCodes.csv")
travelPurpose <- read_csv(travelPurposeByCodes, na = c("", "NA","NULL","null"))

# Read in outer islands visit data from secure folder

outerIslandsFile <- file.path(secureDataFolder, "12.QRYISLANDVISITS-EXPORT.csv")
outerIslands <- read.csv(outerIslandsFile, header = TRUE, na.strings = c("","NA","NULL","null"))

## Parameter ##

# Define length of stay as 120 days 
length_of_stay_threshold <- 120


## Cleaning tourism stats data ##


# Using janitor to clean column names
tourismStats <- clean_names(tourismStats)

outerIslands <- clean_names(outerIslands)

# Substitute values for returning residents in empty spaces 
tourismStats$travel_purpose[which(is.na(tourismStats$travel_purpose))] <- "6. Returning Residents"


# Count the missing values
numberMissing <- apply(tourismStats, MARGIN=2,
                       FUN=function(columnValues){
                         return(sum(is.na(columnValues)))
                       })

#' Flag columns with missing values
#'
#' @param numberMissing a labelled vector reporting number of missing values for each column
#' @param threshold above this number of missing values warning is given. Defaults to 0.
flagMissing <- function(numberMissing, threshold = 0){
  
  # Get column names from labelled vector
  columnNames <- names(numberMissing)
  
  # Examine number of missing values for each column
  for(columnName in columnNames){
    
    # Throw warning when number of missing values exceed threshold
    if(numberMissing[columnName] > threshold){
      warning(numberMissing[columnName], " missing values found in ", columnName, " column.")
    }
  }
}
flagMissing(numberMissing)


#Removing all columns with empty data - THIS MIGHT ONLY APPLY TO THE VERSION OF THE SPREADSHEET I HAVE
tourismStats <- 
  tourismStats %>%
  select(!names(numberMissing[numberMissing == nrow(tourismStats)])) 


#Cleaning data - changing to dates
tourismStats$flight_date <- as.Date(tourismStats$flight_date, format = "%d/%m/%Y")
tourismStats$birthdate <- as.Date(tourismStats$birthdate, format = "%d/%m/%Y")
tourismStats$intended_dep_date <- as.Date(tourismStats$intended_dep_date, format = "%d/%m/%Y")
tourismStats$expiry_date <- as.Date(tourismStats$expiry_date, format = "%d/%m/%Y")


## Extract the Year, Month, and Day as separate columns ##

# Create new columns for dates
tourismStats$Year <- format(tourismStats$flight_date, format= "%Y")
tourismStats$Month <- format(tourismStats$flight_date, format= "%B")
tourismStats$Day <- format(tourismStats$flight_date, format= "%d")


# Removing duplicate rows
tourismClean <- tourismStats %>% distinct()

# code to inspect duplicated rows removed
duplicatedRows <- duplicated(tourismStats) | duplicated(tourismStats, fromLast = TRUE)
duplicated <- tourismStats[duplicatedRows, ]
#View(duplicated)


## Merging with classification tables ## 

# Using janitor to clean column names
countryVisitors <- clean_names(countryVisitors)
countryVisitors <- countryVisitors %>% rename("visitors_by_region" = group)

# Using janitor to clean column names
countryResidents <- clean_names(countryResidents)

#Merging classifications
countryClassifications <- merge(countryVisitors, countryResidents, by= c("country_or_area", "country_of_residence"))

# Merging visitors classification with tourism data
tourismMerged <- merge(tourismClean, countryClassifications, by="country_of_residence", all.x = TRUE)


## Merging travel purpose ##

# Using Janitor to clean column names
travelPurpose <- clean_names(travelPurpose)

# Merging travel classifications and cleaning data from mixture of caps/lower case
tourismMerged <- 
  tourismMerged %>% 
  left_join(travelPurpose, by = c('travel_purpose')) %>%
  mutate(travel_purpose = str_to_sentence(travel_purpose),
         travel_purpose_classifications = str_to_sentence(travel_purpose_classifications)) 


## Processing tourism stats data ## 

# Adding age and clearing out likely inaccurate birthdates. Anything <0.1 month replaced with NA

tourismProcess <-    
  tourismMerged %>%
  mutate(age = round(as.numeric(difftime(flight_date, birthdate, unit = 'weeks')/52.25), 1), # Note using weeks as units to calculate age
         age_clean = replace(age, age < 0.1, NA),
         age_clean = round(age_clean,0))



# Cleaning length of stay data by replacing inaccuracies with NA JC: added assumptions
# Assumptions:
# - Ignoring residents
# - Ignoring negative length of stays - should these be flagged?
# - Ignoring zero length of stays unless stop overs
# - Removing length of stays over above length_of_stay_threshold unless trip is for education or business
tourismProcess <-
  tourismProcess %>% 
  mutate(length_of_stay = round(as.numeric(intended_dep_date - flight_date, unit = 'days')), #define length of stay
         length_of_stay_clean = replace(length_of_stay, visitor_resident == 'Resident', NA), #removing residents as assume not relevant to them 
         length_of_stay_clean = replace(length_of_stay_clean, length_of_stay <0, NA), #removing stays <0 day
         length_of_stay_clean = replace(length_of_stay_clean, 
                                        length_of_stay == 0 & #removing length of stays that are 0 days
                                          travel_purpose_classifications != 'Stop over', NA),  # unless stop overs
         length_of_stay_clean = replace(length_of_stay_clean, 
                                        length_of_stay > length_of_stay_threshold & #removing length of stays >120
                                          travel_purpose_classifications != c('Education', 'Business/official'), NA) #unless education & business
  ) 


# Rename column for Country codes

tourismProcess <- tourismProcess %>% rename("iso_a3" = country_of_residence)

# Cleaning data to not be all caps
tourismProcess <-
  tourismProcess %>%
  mutate(residents_by_region = str_to_title(residents_by_region),
         visitors_by_region = str_to_title(visitors_by_region))


# Saving output file JC: changed to output folder - ignored by GitHub
outputDataFile <- file.path(outputFolder, paste("OUT_PROC_Clean_Example.csv"))
write.csv(tourismProcess, outputDataFile)
