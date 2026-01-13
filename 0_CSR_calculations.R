# This script is to format the data downloaded from the TRY Plant Trait Database in order to calculate the CSR for each species, using the StrateFy method developed by Pierce et al. in 2016.
# Here, a mean value for each trait is calculated for each species, removing any outliers.


# Import packages :
library(dplyr)
library(reshape2)
library(rio)
library(stringr)


#### Import data ####
# Import the list species :
species_list <- read.csv("raw_data/CSR/CSR_corresp.csv") %>%
  dplyr::select(ESPECE_nom, CD_REF, AccSpeciesID, Leaf) %>%
  na.omit()


# Import the TRY trait data and filter it :
TRY_data <- readr::read_delim("raw_data/CSR/TRY/36998_01112024024943/36998.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  subset(!is.na(TraitID),
         select=c("AccSpeciesID", "AccSpeciesName", "TraitID", "StdValue", "UnitName")) %>%
  mutate(AccSpeciesID = as.character(AccSpeciesID))

TRY_data <- TRY_data[TRY_data$AccSpeciesID %in% species_list$AccSpeciesID,] # subset the TRY data to vascular flora
TRY_data <- left_join(TRY_data, species_list) %>% #to get CD_REF and family
  na.omit()

# Import DivGrass trait data and filter it :
divgrass <- import("raw_data/DivGrass/20200602_Divgrass.rds") %>%
  subset(!is.na(TAXREF_SHORT))

species_list$SPECIES_NAME_upper <- toupper(species_list$ESPECE_nom) # format names to match

divgrass_sub <- divgrass[divgrass$TAXREF_SHORT %in% species_list$SPECIES_NAME_upper,] %>% # keep only species from the list
  dplyr::select(c(TAXREF_SHORT, LA, SLA, LDMC)) # keep only selected traits for CSR calculation

divgrass_sub <- right_join(species_list, divgrass_sub, join_by(SPECIES_NAME_upper==TAXREF_SHORT)) %>%
  subset(select=-SPECIES_NAME_upper) %>%
  unique() # remove any duplicates

DivGrass_data <- divgrass_sub
DivGrass_data$LDMC <- DivGrass_data$LDMC /1000 # original unit was wrong



#### Join TRY and DivGrass data ####
DivGrass_data <- melt(DivGrass_data, id=c("ESPECE_nom","CD_REF", "AccSpeciesID", "Leaf"), na.rm=T)
names(DivGrass_data) <- c("ESPECE_nom", "CD_REF", "AccSpeciesID",  "Leaf", "TraitName", "StdValue")

DivGrass_data$TraitID[DivGrass_data$TraitName=="LDMC"] <- 47
DivGrass_data$TraitID[DivGrass_data$TraitName=="LA"] <- 3112
DivGrass_data$TraitID[DivGrass_data$TraitName=="SLA"] <- 3117

DivGrass_data$StdValue <- as.numeric(DivGrass_data$StdValue)

rawdata <- full_join(TRY_data, DivGrass_data) %>%
  dplyr::select(c("ESPECE_nom", "CD_REF", "Leaf", "TraitID", "StdValue"))


#### Remove outliers ####

# Create an empty dataframe
rawdata_no_outlier <- data.frame(matrix(ncol=6, nrow=0))
colnames(rawdata_no_outlier) <- c("LB_NOM", "CD_REF", "FAMILLE", "TraitID", "StdValue", "UnitName")

# Calculate interquartile range and remove outliers
for (i in unique(rawdata$CD_REF)) {
  for (j in unique(rawdata$TraitID)) {
    data <- rawdata[rawdata$CD_REF==i
                    & rawdata$TraitID==j,]
    
    if (length(data$StdValue)>2) { # remove outliers ONLY when 3 values or more are available
      quartiles <- quantile(data$StdValue, 
                            probs=c(.25, .75), na.rm = T)
      IQR <- IQR(data$StdValue, na.rm=T)
      
      Lower <- quartiles[1] - 1.5*IQR
      Upper <- quartiles[2] + 1.5*IQR 
      
      data_no_outlier <- subset(data, data$StdValue > Lower & data$StdValue < Upper)
      
    } else {
      data_no_outlier <- data
    }
    
    rawdata_no_outlier <- rbind(rawdata_no_outlier, data_no_outlier)
  }}


# write.csv(rawdata_no_outlier, "raw_data/raw_CSR_trait_data_all_France.csv", row.names = F)


#### Mean data for each trait and species ####

## LDMC : Leaf Dry Mass per Leaf Fresh Mass
data_LDMC <- subset(rawdata_no_outlier, TraitID==47, 
                    select=-TraitID) %>%
  group_by(CD_REF, ESPECE_nom) %>%
  summarise(mean_LDMC=mean(StdValue))



## SLA : Specific Leaf Area
# 3115 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded.
# 3116 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included.
# 3117 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or exclu.
# 3085 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) of leaf lamina.
# 3086 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded.

# Mean 3115, 3116 and 3117 (petiole probably not a big difference)
data_SLA <- subset(rawdata_no_outlier, TraitID %in% c(3115, 3116, 3117), 
                   select=-TraitID) %>%
  group_by(CD_REF, ESPECE_nom) %>%
  summarise(mean_SLA=mean(StdValue))



## LA : Leaf Area
# 3108 Leaf area (in case of compound leaves: leaf, petiole excluded)
# 3110 Leaf area (in case of compound leaves: leaf, petiole included)
# 3112 Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)

# 3109 Leaf area (in case of compound leaves: leaflet, petiole excluded)
# 3111 Leaf area (in case of compound leaves: leaflet, petiole included)
# 3113 Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)

# 3114 Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or e.

# Mean the trait data, different traits depending on if the leaf is compound or not

data_LA_not_compound <- subset(rawdata_no_outlier,
                               Leaf=="simple" &
                                 TraitID %in% c(3108, 3110, 3112, 3109, 3111, 3113, 3114),
                               select=-TraitID) %>%
  group_by(CD_REF, ESPECE_nom) %>%
  summarise(mean_LA=mean(StdValue)) 

data_LA_compound <- subset(rawdata_no_outlier,
                           Leaf=="compound" &
                             TraitID %in% c(3108, 3110, 3112), 
                           select=-TraitID) %>%
  group_by(CD_REF, ESPECE_nom) %>%
  summarise(mean_LA=mean(StdValue))

data_LA <- rbind(data_LA_not_compound, data_LA_compound)



#### Bind data for LA, LDMC and SLA into a final tab ####
full_data <- full_join(data_LA, data_LDMC) %>%
  full_join(data_SLA) %>%
  full_join(species_list) %>% # full_join adds empty lines for missing species
  dplyr::select(-SPECIES_NAME_upper) %>%
  unique() 


full_data$mean_LDMC <- full_data$mean_LDMC *100 # necessary for calculations




#### Export data ####

# Export the data for the total French flora :
full_data_no_NA <- na.omit(full_data) %>% # first remove nas from species data
  dplyr::select(ESPECE_nom, CD_REF, mean_LA, mean_LDMC, mean_SLA)

if (!dir.exists("processed_data")) {
  dir.create("processed_data")
}
write.csv(full_data_no_NA, "processed_data/CSR_trait_data.csv", row.names = F)



# this data must then be copy-pasted in the columns of the StrateFy.ods file, in order to get the CSR calculations
# Note : the Stratefy file uses commas as decimal separators

