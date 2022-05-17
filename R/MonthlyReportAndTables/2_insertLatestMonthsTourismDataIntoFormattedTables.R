## Set up ##

# Clear the environment
rm(list = ls())

# Packages
library(tidyverse) # standardised general packages for working with data and plotting
library(janitor) # cleaning data


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

## Parameter ##

# Define length of stay as 120 days 
length_of_stay_threshold <- 120


## Cleaning tourism stats data ##


# Using janitor to clean column names
tourismStats <- clean_names(tourismStats)

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


# Removing duplicate rows
tourismClean <- tourismStats %>% distinct()

# code to inspect duplicated rows removed
duplicatedRows <- duplicated(tourismStats) | duplicated(tourismStats, fromLast = TRUE)
duplicated <- tourismStats[duplicatedRows, ]
#View(duplicated)




## Merging with classification tables ## 

# Using janitor to clean column names
countryVisitors <- clean_names(countryVisitors)
countryVisitors <- countryVisitors %>% rename("visiors_by_region" = group)

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

# Cleaning data to not be all caps
tourismProcess <-
  tourismProcess %>%
  mutate(residents_by_region = str_to_title(residents_by_region),
         visiors_by_region = str_to_title(visiors_by_region))


# Saving output file JC: changed to output folder - ignored by GitHub
outputDataFile <- file.path(outputFolder, paste("OUT_PROC_Clean_Example.csv"))
write.csv(tourismProcess, outputDataFile)


#### Preparation ####

# Read in the processed TOURISM data from secure folder of the repository 
tourismProcessedFile <- file.path(secureDataFolder, "OUT_PROC_ASY_ProcessedRawData_31.12.19.csv")
tourismProcessed <- read.csv(tourismProcessedFile, header = TRUE, na.strings = c("","NA","NULL","null"))

#### Table 1: Summary of overseas Migration ####

Tab1_VILA <- tourismProcessed %>%
  group_by(PORT,ARR.DEPART,VisitorResident) %>%
  filter(PORT %in% c("VAIRP","VAIR","SAIRP","SAIR")) %>%
  count()

#### Table 2: Purpose of Visit ####

Tab2_POV <- tourismProcessed %>%
  group_by(TravelPurposeClassifications) %>%
  filter(VisitorResident %in% c("Visitor")) %>%
  filter (ARR.DEPART %in% c("ARRIVAL")) %>%
  count()

#### Table 3: Country of Usual Residence ####

Tab3_VistorsCUR <- tourismProcessed %>%
  filter(PORT %in% c("SAIR","SAIRP","VAIR","VAIRP"))%>%
  filter(VisitorResident %in%("Visitor"))%>%
  filter(ARR.DEPART %in%("ARRIVAL"))%>%
  group_by(GROUP) %>%
  count()

#### Table 4: Residents arrival by Nationality ####

Tab4_ResByNat<- tourismProcessed %>%
  group_by(RESIDENTS.BY.REGION) %>%
  filter(VisitorResident == "Resident", ARR.DEPART == 'ARRIVAL',PORT %in% c("VAIRP","VAIR","SAIRP","SAIR")) %>%
  count()

#### Table 5: Average length of stay ####

# Change length of stay format from integer to character
str(tourismProcessed$LENGTH.OF.STAY)
tourismProcessed$LENGTH.OF.STAY <- as.character(tourismStats$LENGTH.OF.STAY)
str(tourismProcessed$VisitorResident)

# Filter for visiting tourists 
visitors_data <- tourismProcessed %>%
  filter(VisitorResident == "Visitor")

# Identify when Visitors length of stay over 120 days and correct
LOS_threshold <- 120
visitors_data <- visitors_data %>%
  mutate(visit_too_long = LENGTH.OF.STAY > LOS_threshold,
         CORRECTED.LENGTH.OF.STAY = case_when(
           LENGTH.OF.STAY > LOS_threshold ~ 120,
           TRUE ~ LENGTH.OF.STAY
         ))

visitors_data <- visitors_data %>%
  mutate(Towns = case_when(
    grepl(pattern = "VAI", visitors_data$PORT) ~ "PORT VILA",
    grepl(pattern = "SAI", visitors_data$PORT) ~ "LUGANVILLE"
  )) 


AVG_LOS_VUV <- visitors_data %>%
  filter(ARR.DEPART == "ARRIVAL") %>%
  filter(CORRECTED.LENGTH.OF.STAY != "NA") %>%
  filter(Towns %in% c("PORT VILA", "LUGANVILLE")) %>%
  group_by(Towns) %>%
  summarise(AVG_LengthOfStay = round(mean(CORRECTED.LENGTH.OF.STAY), digits = 0)) 

Vanuatu_LOS <- mean(AVG_LOS_VUV$AVG_LengthOfStay)

Towns <- "VANUATU"
AVG_LengthOfStay <- 12
Vanuatu <- data.frame(Towns,AVG_LengthOfStay)

Tab5_AVGLOS <- rbind(AVG_LOS_VUV,Vanuatu)


#### Table 6: Average age ####

str(TourismFINAL$AGE)
str(TourismFINAL$VisitorResident)

visitor_data_age <- tourismProcessed %>%
  filter(VisitorResident == "Visitor")


visitor_data_age <- visitor_data_age %>%
  mutate(cities = case_when(
    grepl(pattern = "VAI", visitor_data_age$PORT) ~ "PORT VILA",
    grepl(pattern = "SAI", visitor_data_age$PORT) ~ "LUGANVILLE"
  )) 


AVG_AGE_VUV <- visitor_data_age %>%
  filter(ARR.DEPART == "ARRIVAL") %>%
  filter(AGE != "NA") %>%
  filter(cities %in% c("PORT VILA", "LUGANVILLE")) %>%
  group_by(cities) %>%
  summarise(AVG_AGE = round(mean(AGE), digits = 0)) 

Vanuatu_AGE <- mean(AVG_AGE_VUV$AVG_AGE)

cities <- "VANUATU"
AVG_AGE <- 38.5
Vanuatu <- data.frame(cities, AVG_AGE)

Tab6_AVGAGE <- rbind(AVG_AGE_VUV,Vanuatu)

#### Table 7: Visitors to outer islands (working on data) ####

# create subset from Main dataset #

DepartureSubset <-tourismProcessed[which(tourismProcessed$ARR.DEPART=="DEPARTURE"),]

# Merge Outer Island into Departure Subset #
FINALOuterIsland <- merge(MergeOuterIslandCodes,DepartureSubset, by="PASSPORT", all.x = TRUE )

Tab7_VisitorOuterIsland <- FINALOuterIsland %>%
  group_by(PORT, MainIsland) %>%
  filter(PORT %in% c("VAIRP","VAIR","SAIRP","SAIR")) %>%
  count()

#### Table 8: Visitors usual residence arrivals by Purpose of Visit ####

#grouping the purpose of visits
POV_Tab8 <- tourismProcessed %>%
  mutate( POV_Groups = case_when(
    grepl(pattern = "Bus", tourismProcessed$TravelPurposeClassifications) ~ "Business",
    grepl(pattern = "Conf", tourismProcessed$TravelPurposeClassifications) ~ "Business",
    grepl(pattern = "Edu", tourismProcessed$TravelPurposeClassifications) ~ "All Other",
    grepl(pattern = "Oth", tourismProcessed$TravelPurposeClassifications) ~ "All Other",
    grepl(pattern = "Spo", tourismProcessed$TravelPurposeClassifications) ~ "All Other",
    grepl(pattern = "Sto", tourismProcessed$TravelPurposeClassifications) ~ "All Other",
    grepl(pattern = "Hol", tourismProcessed$TravelPurposeClassifications) ~ "Holiday",
    grepl(pattern = "Vis", tourismProcessed$TravelPurposeClassifications) ~ "Visitings Friends and relatives"
  )) 

#creating the data frame comparing visitors country of usual residence by purpose of visit
UsualResByPOV <- POV_Tab8 %>%
  group_by(GROUP, POV_Groups) %>%
  filter(VisitorResident %in% c("Visitor")) %>%
  filter (ARR.DEPART %in% c("ARRIVAL")) %>%
  count()


#manipulating the above to a wider format
UsualResByPOV2 <- UsualResByPOV %>%
  pivot_wider (names_from = POV_Groups, values_from = n)

### Add in total row and column ###
#adding a total column

UsualResByPOV2 <- UsualResByPOV2 %>%
  adorn_totals("col")

#adding a total row
UsualResByPOV2 <- UsualResByPOV2 %>%
  adorn_totals("row")

##Percentage of Total Columns

#Calculate Percentage of total
n_cols <- ncol(UsualResByPOV2)
columns_of_interest <- 2:(n_cols - 1) #ignores the first and last columns
UsualResByPOV_proportion <- UsualResByPOV2[, columns_of_interest]/UsualResByPOV2$Total
UsualResByPOV_percentage <- UsualResByPOV_proportion * 100

#Note percentage columns in their name
colnames(UsualResByPOV_percentage) <- paste(colnames(UsualResByPOV_percentage),"(% share)")

#Add percentage columns into main table
UsualResByPOV_percentage <- cbind(UsualResByPOV2, UsualResByPOV_percentage)

##Combining Value and Percentage into one single column

#Initialise a dataframe to store the combined values and percentages
Tab8_UsualResByPOV <- data.frame("GROUP" = UsualResByPOV2$GROUP)

#Note columns of interest
columns_of_interest <- colnames(UsualResByPOV2)[
  c(-1, -ncol(UsualResByPOV2))
]

#Add in each column with combined value and percentages
for(travel_type_column in columns_of_interest){
  
  #create column name with percentage
  travel_type_column_as_percentage <- paste(travel_type_column, "(% share)")
  
  #get the counts for current travel type
  #Note use of "drop" - this extracts the values as a vector instead of a data.frame
  
  travel_type_counts <- UsualResByPOV2 [, travel_type_column, drop = TRUE]
  
  #get the counts as percentages
  #Note use of "drop" - this extracts the values as a vector instead of a data.frame
  
  travel_type_percentages <- UsualResByPOV_percentage [,
                                                       travel_type_column_as_percentage, drop = TRUE]
  
  #Round as percentages
  travel_type_percentages <- round(travel_type_percentages, digits = 0)
  
  #create column with combined value and percentage
  Tab8_UsualResByPOV[, travel_type_column_as_percentage] <- paste(
    travel_type_counts, "(", travel_type_percentages, "%)", sep = ""
  )
}

#Add a total column
Tab8_UsualResByPOV$Total <- UsualResByPOV2$Total

#Remove the percentages from final row

number_rows <- nrow(Tab8_UsualResByPOV)
Tab8_UsualResByPOV[number_rows,] <- UsualResByPOV2[number_rows, ]
Tab8_UsualResByPOV <- Tab8_UsualResByPOV[ , c(1,5, 4, 3, 2, 6)]



#### END ####