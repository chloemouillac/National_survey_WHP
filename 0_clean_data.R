# This script is to clean the answer data from the survey

#### Load packages ####
library(dplyr)
library(stringr)

#### Import data ####
raw_data <- read.csv(here::here("results-survey676868.csv"))
names(raw_data)


# Edit column names :
names(raw_data) <-  names(raw_data) %>%
  sapply(function(x) 
    gsub(pattern = "\\.", replacement = " ", x=x)) %>% # remove dots and replace them with spaces
  str_trim(side="both") %>% # trim spaces at the beginning and end of each string
  gsub(pattern = " ", replacement = "_") # replace remaining spaces with underscores

# Trim the entire data
raw_data <- raw_data %>%
  lapply(function(x) str_trim(string=x, side="both")) %>% 
  as.data.frame()

# Transform empty answers into NAs :
raw_data[raw_data == "N/A" | raw_data == ""] <- NA


# Filter data :
names(raw_data)

# raw_data <- raw_data %>%
#   filter(!is.na(G1Q00004) & # remove people who didn't answer the question "Pour le département dont vous connaissez le mieux la flore, combien d'espèces pensez-vous être capable d'identifier sans aide (sans guide, ni flore, ni application téléphone) ?"
#            (!is.na(G2Q00002_SQ001) | # remove people who give ANY species names"
#               !is.na(G2Q00003_SQ001) | 
#               !is.na(G2Q00004_SQ001) |
#               !is.na(G2Q00005_SQ001) |
#               !is.na(G3Q00001) |
#               !is.na(G4Q00001))) 


raw_data <- raw_data %>%
  filter(
    if_all(all_of(select(., matches("^G1Q00004.*")) %>% names()), ~ !is.na(.)) &
      if_any(all_of(select(., matches("^G2Q0000[2-5]_SQ001.*|^G3Q00001.*|^G4Q00001.*")) %>% names()), ~ !is.na(.))
  )


#### Export ####
write.csv(raw_data,
          here::here("results-survey676868_prefilter.csv"),
          row.names=F)
