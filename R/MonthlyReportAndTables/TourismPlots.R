## Set up ##

# Clear the environment
rm(list = ls())

#install.packages("tmap")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("sf")


library(tidyverse) # for processing data
library(tmap) # for building map
library(ggplot2) # for building plots
library(cowplot) # for combining plots into a grid 
library(sf) # for working with spatial data

# setting font for plots
windowsFonts(Times = windowsFont("Times New Roman"))

## Data import ##

# set working directory
repository <- getwd()

# define output data folder path
outputsFolder <- file.path(repository, "outputs")

# Read in the processed tourism data from secure folder of the repository 
tourismCleanFile <- file.path(outputsFolder, "OUT_PROC_Clean_Example.csv")
tourismCleaned <- read_csv(tourismCleanFile,  na= c("","NA","NULL","null"))

# importing data for country polygons
data("World")


## Building map dataframe #

# dataframe for arranging data for plotting citizenship vs sex
country_arrivals_df <-
  tourismCleaned %>%
  group_by(iso_a3, country_or_area, gender) %>%
  filter(arr_depart == 'ARRIVAL', # arrivals only
         country_or_area != 'Vanuatu') %>% # removing Vanuatu 
  count() %>% # count all rows
  mutate(n = as.numeric(n)) %>%
  spread(gender, n) %>%
  mutate(`F` = replace_na(`F`, 0),
         `M` = replace_na(`M`, 0),
         total = `F` + `M`) %>%
  ungroup() %>%
  arrange(desc(total))


# Get the polygons for the country id's that match tourism stats
world_df <- World %>% select(iso_a3, name)

world_moll <- st_transform(world_df, crs = "+proj=moll")



# merging world polygons and country_arrivals dataframe
arrivals_merged_df <- merge(world_moll, country_arrivals_df,  
                            by.x = "iso_a3", # column in world_df to merge on
                            by.y = "iso_a3") # column in country_arrivals_df to merge on




# building map to show visits by country for total (male + female)
m1 <-
  tm_shape(world_moll) + # this is a hack to not deal with missing countries
  tm_borders() + # not ideal should really fix data
  tm_fill(col = 'white') +
  tm_shape(arrivals_merged_df) +
  tm_polygons(
    "total",
    palette = 'RdPu',
    title = "",
    breaks = c(0, 500, 1000, 1500, 2000, 3000, 7000) # change how data is broken up
  ) +
  tm_style("natural") + # makes globe blue
  tm_layout(title = 'Visitors by usual country of residence',
            title.size = 1.3,
            title.fontface = 'bold',
            title.position = c('left', 'top'), 
            fontfamily = 'serif', # font - for arial etc use 'sans' or courier would be 'monospace'
            
            frame = FALSE,
            inner.margins = c(0.1,0.05,0.1,0.05), # earth margins
            outer.margins = c(0,0,0,0), # outside frame
            
            legend.bg.color = 'transparent',
            legend.frame = FALSE) 


# dataframe to breakdown top ten countries by sex
country_arrivals_sex_df <-
  country_arrivals_df %>%
  slice(1:10) %>% # selecting top ten
  mutate(`F` = -`F`) %>% # making negative for plotting
  gather(`F`, `M`, key= 'sex', value= 'visits') # gathering into one column for plotting


# Sex by country plot
p1 <- 
  ggplot() +
  geom_bar(data = country_arrivals_sex_df,
           aes(
             x = visits,
             y = reorder(country_or_area,-total), # ordering by total value
             fill = sex
           ),
           stat = 'identity') +
  scale_fill_manual(values = c('orchid4', 'darkslategrey')) +
  scale_x_continuous(
    limits = c(-3500, 3500),
    labels = c('3,500', '2,500', '0', '2,500', '3,500') # changing labels to hide that some data made negative for plot
  ) +
  labs(x = 'number of visits',
       y = NULL,
       fill = NULL,
       title = 'The number of visits broken down by sex for the top ten countries.') +
  theme_classic() 




## Age and sex breakdown ##

# for bin - connected to tags i.e. if you change a value change same in tags below
breaks <- c(0, 5, 10, 20, 40, 60, 80, 100)

# for bin - connected to breaks update if change breaks
tags <- c("0-5","5-10", "10-20", "20-40", "40-60", "60-80", "80-100")


# dataframe for grouping by sex and binning data based on age
age_df <- 
  tourismCleaned %>%
  filter(arr_depart == 'ARRIVAL', # arrivals only
         country_or_area != 'Vanuatu') %>%
  group_by(gender) %>%
  filter(!is.na(age_clean)) %>%
  mutate(bin = cut(age_clean, 
                   breaks = breaks, 
                   include.lowest = TRUE, 
                   right = FALSE, 
                   labels = tags)) %>%
  ungroup() %>%
  group_by(gender, bin) %>% 
  count() %>%
  mutate(n = as.numeric(n))


# dataframe for binned age data split by sex ready for plotting
age_binned_df <- 
  age_df %>%
  spread(gender, n) %>%
  mutate(`F` = -`F`) %>% #making negative to put on same plot
  gather(`F`, `M`, key= 'sex', value= 'visits')



# binned age data plot split by sex
p2 <- 
  ggplot() +
  geom_bar(data = age_binned_df,
           aes(x = visits,
               y = bin,
               fill = sex),
           stat = 'identity') +
  scale_fill_manual(values = c('orchid4', 'darkslategrey')) +
  scale_x_continuous(
    limits = c(-2500, 2500),
    labels = c('2,500', '2,000', '1,000', '0', '1,000', '2,000', '2,500')
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  labs(
    x = 'number of visits',
    y = NULL,
    fil = NULL,
    title = 'Age of visitors broken down by sex'
  ) +
  theme_classic() 


# top ten countries based on citizenship of visitors

# dataframe to select top ten countries
top_ten_countries_df <-
  tourismCleaned %>%
  filter(arr_depart == 'ARRIVAL', # arrivals only
         country_or_area != 'Vanuatu') %>%
  group_by(citizenship) %>%
  filter(!is.na(age_clean)) %>%
  count() %>%
  mutate(n = as.numeric(n)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:10) %>% 
  select(citizenship)

#TODO take dataframe above and make into a vector
top_ten_countries <- c("AUS", "FRA", "NZL", "CHN", "GBR", "USA", "FJI", "JPN", "SLB", "DEU")

# dataframe for grouping and binning by citizenship
citizenship_df <-
  tourismCleaned %>%
  filter(
    citizenship %in% top_ten_countries,
    arr_depart == 'ARRIVAL',
    country_or_area != 'Vanuatu'
  ) %>%
  group_by(citizenship) %>%
  filter(!is.na(age_clean)) %>%
  mutate(bin = cut(
    age_clean,
    breaks = breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = tags
  )) %>%
  ungroup() %>%
  group_by(citizenship, bin) %>%
  count() %>%
  mutate(n = as.numeric(n))


# calculating total to make %
total_df <- tourismCleaned %>%
  filter(
    citizenship %in% top_ten_countries,
    arr_depart == 'ARRIVAL',
    country_or_area != 'Vanuatu'
  ) %>%
  group_by(citizenship) %>%
  filter(!is.na(age_clean)) %>%
  count() %>%
  mutate(n = as.numeric(n)) %>%
  rename("total" = n)


# joining total to make %
citizenship_pct_df <-
  citizenship_df %>%
  left_join(total_df,
            by = 'citizenship') %>%
  mutate(pct = n / total * 100) %>%
  ungroup() %>%
  group_by(citizenship)


# labels for x axis
x_labs <- c('0 %', '25 %', '50 %', '75 %', '100 %')  


# plot to build age per top ten citizenship country
p3 <- 
  ggplot() +
  geom_bar(
    data = citizenship_pct_df,
    aes(x = pct,
        y = citizenship,
        fill = bin),
    stat = 'identity',
    position = position_fill(reverse = TRUE)
  ) +
  scale_fill_brewer(palette = 'Dark2') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     labels = x_labs) +
  guides(shape = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  labs(x = NULL,
       y = NULL,
       fill = 'age\ngroup',
       title = 'Distribution of visitors by age group for \ntop ten country of citizenship') +
  theme_classic() 


## Combining plots together ##

map1 <- tmap_grob(m1) # needed to include map in grid

# theme for plot
### I would normally put this right at the start under functions HOWEVER this theme is just for the arranging of the plots 
### as the text size is smaller than if saving as individual .png or if want to knit in markdown
### so for that reason it's here as intention is a secondary plot_theme would be used!

plot_theme  <- theme(legend.position = 'bottom',
                     plot.title = element_text(size = 12, face = 'bold'),
                     legend.text = element_text(size = 8),
                     legend.title = element_text(size = 10),
                     text = element_text(size = 8, family = 'Times'))

p3 <- p3 + plot_theme # readying plot for joining into grid

p2 <- p2 + plot_theme # readying plot for joining into grid

bottomrow <- plot_grid(p2, p3) #joining bottom plots

# building grid - this needs some adjustment as too much whitespace at the minute
plot_grid(map1, bottomrow, nrow = 2, rel_heights = c(3.5,2.5))
