library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# Read the dataset
read_csv("2015.csv") %>%
  rename_all(~ str_replace_all(., " ", "")) %>%
  mutate(Year = parse_number('2015')) -> hap_2015

read_csv("2016.csv") %>%
  rename_all(~ str_replace_all(., " ", "")) %>%
  mutate(Year = parse_number('2016')) -> hap_2016

read_csv("2017.csv") %>%
  mutate(Year = parse_number('2017')) -> hap_2017

read_csv("2018.csv") %>%
  rename(Country = 'Country or region') %>%
  na_if("N/A") %>%
  rename_all(~ str_replace_all(., " ", "")) %>%
  mutate(Year = parse_number('2018')) -> hap_2018

parse_number(hap_2018$Perceptionsofcorruption) -> 
  hap_2018$Perceptionsofcorruption

read_csv("2019.csv") %>%
  rename(Country = 'Country or region') %>%
  rename_all(~ str_replace_all(., " ", "")) %>%
  mutate(Year = parse_number('2019')) -> hap_2019

# Add Region to the dataframe and reorder column
hap_2019 %>%
  arrange(Country) %>%
  .[c(10, 1:9)] %>%
  full_join(., Region, by = "Country") %>%
  .[c(1, 3, 11, 2, 4:10)] -> hap_2019

hap_2018 %>%
  arrange(Country) %>%
  .[c(10, 1:9)] %>%
  full_join(., Region, by = "Country")  %>%
  .[c(1, 3, 11, 2, 4:10)] -> hap_2018

hap_2017 %>%
  arrange(Country) %>%
  .[c(13, 1:12)] %>%
  full_join(., Region, by = "Country") %>%
  .[c(1, 2, 14, 3:13)] -> hap_2017

hap_2016 %>%
  .[c(14, 1:13)] -> hap_2016

hap_2015 %>%
  .[c(13, 1:12)] -> hap_2015

# Taking care of NA values
  # and rename some country name
hap_2017 %>%
  drop_na(Happiness.Rank) %>%
  mutate(Region = case_when(
    Country == "Central African Republic" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Hong Kong S.A.R., China" & is.na(Region) ~ "Eastern Asia",
    Country == "Lesotho" & is.na(Region) ~ "Sub-Saharan Africa", 
    Country == "Mozambique" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Taiwan Province of China" & is.na(Region) ~ "Eastern Asia",
    TRUE ~ Region
  )) -> hap_2017

hap_2018 %>%
  drop_na(Overallrank) %>%
  mutate(Region = case_when(
    Country == "Central African Republic" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Lesotho" & is.na(Region) ~ "Sub-Saharan Africa", 
    Country == "Mozambique" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Northern Cyprus" & is.na(Region) ~ "Western Europe",
    Country == "Trinidad & Tobago" & is.na(Region) ~ "Latin America and Caribbean",
    TRUE ~ Region
  )) -> hap_2018

hap_2019 %>%
  drop_na(Overallrank) %>%
  mutate(Region = case_when(
    Country == "Central African Republic" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Gambia" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Lesotho" & is.na(Region) ~ "Sub-Saharan Africa", 
    Country == "Mozambique" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "North Macedonia" & is.na(Region) ~ "Central and Eastern Europe",
    Country == "Northern Cyprus" & is.na(Region) ~ "Western Europe",
    Country == "Swaziland" & is.na(Region) ~ "Sub-Saharan Africa",
    Country == "Trinidad & Tobago" & is.na(Region) ~ "Latin America and Caribbean",
    TRUE ~ Region
  )) -> hap_2019

# Dropping columns
hap_2017 %>%
  select(-c(Whisker.high, Whisker.low, Dystopia.Residual)) -> hap_2017

hap_2016 %>%
  select(-c(LowerConfidenceInterval, UpperConfidenceInterval, DystopiaResidual)) -> hap_2016

hap_2015 %>%
  select(-c(StandardError, DystopiaResidual)) -> hap_2015

# Renaming columns
  # and swapping column on hap_2015 and hap_2016
hap_2015 %>%
  .[c(1:9, 11, 10)] -> hap_2015

hap_2016 %>%
  .[c(1:9, 11, 10)] -> hap_2016

hap_2015 %>%
  rename(SocialSupport = "Family") %>%
  rename(PerceptionsofCorruption = "Trust(GovernmentCorruption)")-> hap_2015

hap_2016 %>%
  rename_all(., ~ colnames(hap_2015)) -> hap_2016

hap_2017 %>%
  rename_all(., ~ colnames(hap_2015)) -> hap_2017

hap_2018 %>%
  rename_all(., ~ colnames(hap_2015)) -> hap_2018

hap_2019 %>%
  rename_all(., ~ colnames(hap_2015)) -> hap_2019

# Join the dataframe
bind_rows(hap_2015, hap_2016, hap_2017, hap_2018, hap_2019) -> happiness