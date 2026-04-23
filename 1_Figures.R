#### Load packages ####
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(tidyr)
library(stringr)
library(sf)
library(cowplot)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(rstatix)
library(ade4)
library(inflection)
library(ggrepel)
library(lme4)
library(tidytext)
library(forcats)
library(MASS)
library(glmnet)
library(tibble)
library(cluster)
library(readr)
library(caret)
library(pROC)
library(dplyr)
library(purrr)
library(boot)
library(plotly)


#### Import data ####
# raw_data <- read.csv("raw_data/results-survey676868_prefilter.csv")
raw_data <- read.csv("raw_data/results-survey676868_translated.csv")

massifs <- read.csv("raw_data/massifs_cueillette.csv")

massifs$massif <- factor(
  massifs$massif,
  levels = c(
    "Alpes", "Pyrénées", "Jura-Alpes Nord", "Massif Central",
    "Méditerranée", "Massif Corse",
    "Bassin Parisien Nord", "Bassin Parisien Sud", "Nord-Est", "Massif Armoricain", "Sud-Ouest"
  ),
  labels = c(
    "Alps", "Pyrenees", "Jura-Northern Alps", "Central Massif",
    "Mediterranean", "Corsica",
    "Northern Paris Basin", "Southern Paris Basin", "North-East", "Armorican Massif", "South-West"
  )
)

massifs_simple <- read.csv("raw_data/massifs_cueillette_simple.csv")

departements <- read_sf("raw_data/shape/departements_detail_paris.shp") %>%
  mutate(
    dpt_simple = dpt,
    region_simple = region,
    # Replace "-" and "'" with space
    dpt_simple = str_replace_all(dpt_simple, "[-']", " "),
    region_simple = str_replace_all(region_simple, "[-']", " "),
    # Remove accents
    dpt_simple = iconv(dpt_simple, to = "ASCII//TRANSLIT"),
    region_simple = iconv(region_simple, to = "ASCII//TRANSLIT")
  ) %>%
  left_join(massifs, by="dpt")


taxref <-  read.delim("raw_data/TAXREF_v17_2024/TAXREFv17.txt") %>%
  subset((GROUP1_INPN=="Trachéophytes") &
           (REGNE=="Plantae") &
           (CD_REF==CD_NOM) &
           (RANG=="ES"),
         select=c(CD_REF, CD_NOM, LB_NOM, GROUP1_INPN, REGNE)) %>%
  unique()

all_rarity <-  read.csv("raw_data/OpenObs+GBIF_RARITY_20km.csv") %>%
  dplyr::select(CD_REF, dpt_area, sp_area, dpt_name, sp_relative_area) %>%
  mutate(dpt_simple =  str_replace_all(dpt_name, "[-']", " "),
         dpt_simple = iconv(dpt_simple, to = "ASCII//TRANSLIT"))

all_rarity <- all_rarity %>% left_join(all_rarity %>% 
                                         group_by(CD_REF) %>% 
                                         # add a column with total species area in France
                                         summarise(sp_area_FR=sum(sp_area)))
  

raunkieaer <- read.csv("processed_data/type_bio.csv") %>%
  dplyr::select(CD_REF, choix_type_bio) %>%
  filter(!is.na(choix_type_bio))

csr <- read.csv("processed_data/CSR_clean.csv") %>%
  dplyr::select(CD_REF, C, S, R, strategy_class) %>%
  mutate(csr_simple = str_split_i(strategy_class, "/", 1)) %>%
  dplyr::select(-strategy_class)

vascular <- read.csv("raw_data/list_vascular_v17.csv") %>%
  dplyr::select(CD_REF, FR) %>%
  mutate(
    native = ifelse(FR %in% c("I", "M", "J"), "non-native", "native")
  ) %>%
  dplyr::select(-FR) 

# P - Widespread native/undetermined: Indigenous or of uncertain origin, widely present
# S - Subendemic: Mostly native to France with limited distribution elsewhere
# C - Cryptogenic: Origin unclear or unknown
# D - Doubtful Presence: Presence in France uncertain or unconfirmed
# I - Introduced: Non-native, not necessarily invasive
# M - Historically introduced: Long-established non-native species
# J - Introduced & invasive: Non-native species with invasive behavior 


####_________####
#### APPENDIX B - Temporal dynamic of answers ####
raw_data$datestamp__Date_of_last_action <- as.POSIXct(
  raw_data$datestamp__Date_of_last_action,
  format = "%Y-%m-%d %H:%M:%S",   # adjust if needed
  tz = "Europe/Paris"
)

# Make sure months are displayed in English
Sys.setlocale("LC_TIME", "C")  # 'C' forces English month names

# Prepare daily cumulative data
df_daily <- raw_data %>%
  mutate(day = as.Date(datestamp__Date_of_last_action)) %>%
  count(day) %>%
  arrange(day) %>%
  mutate(cum_n = cumsum(n))

# Axis limits
min_day <- min(df_daily$day, na.rm = TRUE)
max_day <- as.Date("2025-04-30")  # adjust as needed to show relevant dates

# Relaunch dates
relaunch1 <- as.Date("2024-11-15")
relaunch2 <- as.Date("2025-02-07")

# Plot
temp_dynamic <- ggplot(df_daily, aes(x = day, y = cum_n)) +
  geom_step(linewidth = 0.8) +
  
  # Relaunch vertical lines
  geom_vline(xintercept = relaunch1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = relaunch2, linetype = "dashed", color = "red") +
  
  # Relaunch labels above curve
  annotate("text", x = relaunch1, y = 800,
           label = "Relaunch 1", angle = 90, vjust = -0.5, size = 3, colour = "red") +
  annotate("text", x = relaunch2, y = 800,
           label = "Relaunch 2", angle = 90, vjust = -0.5, size = 3, colour = "red") +
  
  scale_x_date(
    limits = c(min_day, max_day),
    date_breaks = "1 month",
    date_labels = "%b %Y",  # %b gives abbreviated month in English
    expand = c(0, 0)) +
  
  labs(
    x = "",
    y = "Cumulative number of survey starts") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))
temp_dynamic

# plot_zoom_png?width=702&height=478
png("plots/AppendixB_temporal_dynamic_survey.png", 
    width = 2106,
    height = 1434,
    res = 300)
temp_dynamic
dev.off()


####_________####
#### APPENDIX D - Harvesting areas ####
departements_simpl_PARIS <- departements %>%
  # Create a grouping variable: "IDF" for Ile-de-France, and the original "code" for others
  mutate(group = if_else(region == "Ile-de-France", "IDF", code)) %>%
  # Group the data by the new "group" variable
  group_by(group) %>%
  # Summarise the data by fusing geometries and assigning new values for Ile-de-France
  summarise(
    # Fused geometry for "IDF" and original geometry for others
    geometry = st_union(geometry),
    
    # Assign new values for the grouped rows
    # The 'first' function is used to pick the first value in each group,
    # which is the original department code for all non-IDF regions
    code = first(if_else(group == "IDF", "IDF", code)),
    dpt = first(if_else(group == "IDF", "Ile-de-France", dpt)),
    dpt_simple = first(if_else(group == "IDF", "Ile de France", dpt_simple)),
    region_simple = first(if_else(group == "IDF", "Ile de France", region_simple)),
    massif = first(massif)
  ) %>%
  # Remove the "group" variable created for the aggregation
  dplyr::select(-group)


map_data <- departements_simpl_PARIS %>%
  group_by(massif) %>%
  summarise() %>%
  ungroup()

centroids <- map_data %>%
  mutate(geometry = st_centroid(geometry))

# Boundaries of harvesting areas
harvesting_area_boundaries <- departements_simpl_PARIS %>%
  group_by(massif) %>%
  summarize(geometry = st_union(geometry))

# Define color palettes for harvesting areas
colors_montagne <- c("#4d004b", "#810f7c", "#88419d", "#8c6bb1")   # Mountain areas
colors_med_corse <- c("#FD8D3C", "#fdd0a2")                         # Mediterranean & Corsica
colors_plaine   <- c("#023858", "#045a8d", "#0570b0", "#3690c0", "#74a9cf") # Lowland areas


harvesting_area_levels <- c(
  "Alps", "Pyrenees", "Jura-Northern Alps", "Central Massif",
  "Mediterranean", "Corsica",
  "Northern Paris Basin", "Southern Paris Basin", "North-East", "Armorican Massif", "South-West"
)
harvesting_area_letters <- setNames(LETTERS[1:length(harvesting_area_levels)], harvesting_area_levels)

massif_colors <- c(colors_montagne, colors_med_corse, colors_plaine)
names(massif_colors) <- harvesting_area_levels

plot_harvest_areas <- ggplot() +
  geom_sf(data = departements_simpl_PARIS, aes(fill = massif), colour = "white", alpha = 0.8) +
  geom_sf(data = harvesting_area_boundaries, fill = NA, colour = "black", size = 1) +
  geom_label(data = centroids, aes(label = harvesting_area_letters, geometry = geometry),
             stat = "sf_coordinates", size = 4, fontface = "bold", color = "black",
             fill = "white", label.size = 0.3) +
  scale_fill_manual(
    name = "Harvesting areas",
    values = massif_colors,
    breaks = harvesting_area_levels,
    labels = paste0(harvesting_area_letters, ". ", harvesting_area_levels)
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )
plot_harvest_areas


# plot_zoom_png?width=865&height=615
png("plots/AppendixD_harvest_areas.png", 
    width = 2595,     # pixels
    height = 1845,   # pixels
    res = 300)        # resolution in dpi
plot_harvest_areas
dev.off()



####_________####
#### Prepare main data frame ####

# A single, comprehensive data processing pipeline to avoid redundancy

process_data_helper_1 <- function(data, pattern_one, pattern_two, value_col_name) {
  data %>%
    dplyr::select(id = answer_ID, 
                  matches(paste0("^G\\d+Q0*", c(pattern_one, pattern_two), "_", collapse = "|"))) %>%
    mutate(across(-id, as.character)) %>%
    pivot_longer(
      cols = -id,
      names_to = "column_name",
      values_to = "value",
      values_drop_na = TRUE) %>%
    
    filter(value == "Yes") %>%
    
    mutate(
      group = str_extract(column_name, "(?<=^G)\\d+"),
      qcode = as.numeric(str_extract(column_name, "(?<=Q0{3,4})\\d+")),
      type = case_when(
        group %in% c("3", "4") & qcode == pattern_one ~ value_col_name,
        !(group %in% c("3", "4")) & qcode == pattern_two ~ value_col_name)) %>%
    
    filter(type == value_col_name) %>%
    
    mutate(
      !!sym(value_col_name) := str_to_sentence(str_replace_all(str_remove(column_name, ".*____"), "_", " "))
    ) %>%
    dplyr::select(id, group, !!sym(value_col_name))
}

process_data_helper_2 <- function(data, pattern_one, pattern_two, value_col_name) {
  data %>%
    dplyr::select(id = answer_ID, 
                  matches(paste0("^G\\d+Q0*", c(pattern_one, pattern_two), "_", collapse = "|"))) %>%
    mutate(across(-id, as.character)) %>%
    pivot_longer(
      cols = -id,
      names_to = "column_name",
      values_to = "value",
      values_drop_na = TRUE) %>%
    
    filter(!is.na(value)) %>%
    
    mutate(
      group = str_extract(column_name, "(?<=^G)\\d+"),
      qcode = as.numeric(str_extract(column_name, "(?<=Q0{3,4})\\d+")),
      question = sub(
        "^G\\d+Q\\d+(_SQ\\d+|__other)?_*", # Regex to match and remove the code and optional '_other'
        "",                     # Replace with an empty string
        column_name), 
      
      type = case_when(
        group %in% c("3", "4") & qcode == pattern_one ~ value_col_name,
        !(group %in% c("3", "4")) & qcode == pattern_two ~ value_col_name),
      
      type = ifelse(!is.na(type) & grepl("Other", column_name), paste0(type, "_other"), type),
      
      type = ifelse(!is.na(type) & grepl("SQ0", column_name), 
                    paste0(type, "_", sub(".*____(.*?)", "\\1", question)),
                    type),
      
      # Step to reduce multiple underscores to a single underscore
      type = str_replace_all(type, "_{2,}", "_")) %>%
    
    filter(!is.na(type)) %>%
    
    pivot_wider(names_from = type,
                values_from = value) %>%
    
    dplyr::select(id, group, matches(value_col_name)) %>%
    
    # Step to remove columns that are entirely NA
    dplyr::select(where(~ any(!is.na(.)))) %>%
    
    # Combine multiple rows with the same identifier into a single row
    group_by(id) %>%
    summarise(across(everything(), ~first(na.omit(.))))
  
}

process_all_species_data <- function(data) {
  # Columns related to species, departments, and harvester type
  species_cols <- data %>%
    dplyr::select(answer_ID,
                  matches("^G(?:3|4|6|8|10|12|14|16|18|20)Q000(?:0)?(?:1|2|4|5|6|7|8|9|10|11|12|13|14|18|19|20|21|22|23|24)_"),
                  # careful of the number of zeroes after Q
                  starts_with("GsuppQsupp_")) # GsuppQsupp__Connaissez_vous_une_espece_a_la_cueillette_unsustainable
  
  df_long <- data %>%
    # Select all necessary columns at once
    dplyr::select(id = answer_ID,
                  known_unsustainable = starts_with("GsuppQsupp_"),
                  colnames(species_cols)) %>%
    
    # Pivot to long format for easier manipulation
    pivot_longer(
      cols = -c(id, known_unsustainable),
      names_to = c("group", "qcode"),
      names_pattern = "^G(\\d+)Q0{3,4}(\\d+)_", # careful of the number of zeroes after Q
      values_to = "value",
      values_drop_na = TRUE,
      values_transform = list(value = as.character) # because of the numeric (date columns)
    ) %>%
    
    # Classify values by type (species, presence, etendue_cueill, durability)
    mutate(
      type = case_when(
        qcode == "1" ~ "name",
        group %in% c("3", "4") & qcode == "2" ~ "dpt",
        !(group %in% c("3", "4")) & qcode == "4" ~ "dpt",
        group %in% c("3", "4") & qcode == "4" ~ "presence",
        !(group %in% c("3", "4")) & qcode == "6" ~ "presence",
        group %in% c("3", "4") & qcode == "5" ~ "harv_spread",
        !(group %in% c("3", "4")) & qcode == "7" ~ "harv_spread",
        group %in% c("3", "4") & qcode == "10" ~ "start_obs",
        !(group %in% c("3", "4")) & qcode == "12" ~ "start_obs",
        group %in% c("3", "4") & qcode == "11" ~ "state_resource",
        !(group %in% c("3", "4")) & qcode == "13" ~ "state_resource",
        group %in% c("3", "4") & qcode == "12" ~ "harv_variation",
        !(group %in% c("3", "4")) & qcode == "14" ~ "harv_variation",
        group %in% c("3", "4") & qcode == "13" ~ "harv_intensity",
        !(group %in% c("3", "4")) & qcode == "15" ~ "harv_intensity",
        group %in% c("3", "4") & qcode == "18" ~ "market_trend",
        !(group %in% c("3", "4")) & qcode == "20" ~ "market_trend",
        group %in% c("3", "4") & qcode == "19" ~ "market_trend_risk",
        !(group %in% c("3", "4")) & qcode == "21" ~ "market_trend_risk",
        group %in% c("3", "4") & qcode == "20" ~ "species_regulation",
        !(group %in% c("3", "4")) & qcode == "22" ~ "species_regulation",
        group %in% c("3", "4") & qcode == "21" ~ "regulation_adapted",
        !(group %in% c("3", "4")) & qcode == "23" ~ "regulation_adapted")
    ) %>%
    
    # # Determine durability status based on group
    dplyr::group_by(id, group) %>%
    dplyr::mutate(
      rep_durable = dplyr::first(value[qcode == "2"]),
      sustainability = dplyr::case_when(
        group == "3" ~ "Sustainable",
        group == "4" ~ "Unsustainable",
        rep_durable == "Yes" ~ "Sustainable",
        TRUE ~ "Unsustainable"
      )
    ) %>%
    dplyr::select(-rep_durable) %>%
    dplyr::ungroup() %>%
    
    # Filter out irrelevant rows and select key columns
    filter(!is.na(type)) %>%
    dplyr::select(-qcode) %>%
    
    # Pivot wider to get species, presence, and etendue_cueill in columns
    pivot_wider(
      names_from = type,
      values_from = value,
      values_fn = list
    ) %>%
    
    # Clean and process species and type_cueilleur
    separate_rows(name, sep = ",\\s*") %>%
    mutate(
      name = str_trim(name),
      dpt = str_trim(str_remove(dpt, "\\s*\\([0-9AB]+\\)"))
    ) %>%
    
    unnest(everything()) %>%
    
    # Filter out empty species
    filter(name != "") %>%
    
    # Keep only one record per ID, species, and group to avoid double-counting
    unique() %>%
    
    # Group Ile de France departments
    mutate(
      dpt = ifelse(
        dpt %in% c("Essonne", "Hauts-de-Seine", "Paris", "Seine-Saint-Denis", "Seine-et-Marne", "Val-d'Oise", "Val-de-Marne", "Yvelines"),
        "Ile-de-France",
        dpt
      ),
      dpt = str_replace_all(dpt, "[-']", " ")
    )
  
  
  
  # Use the helper function 1 to process collected parts, uses...
  df_harv_parts <- process_data_helper_1(species_cols, "6", "8", "harv_parts")
  df_uses <- process_data_helper_1(species_cols, "7", "9", "uses")
  df_risk <- process_data_helper_1(species_cols, "17", "19", "risk")
  
  
  # Use the helper function 2 to process type of harvesting practise, company type...
  df_harv_type <- process_data_helper_2(species_cols, "8", "10", "harv_type")
  df_company_type <- process_data_helper_2(species_cols, "9", "11", "company_type")
  df_unadapted_regulation_cause <- process_data_helper_2(species_cols, "22", "24", "unadapted_regulation_cause")
  
  
  # Join the two data frames
  df_final <- left_join(df_long, df_harv_parts, by = c("id", "group")) %>%
    left_join(df_uses, by = c("id", "group")) %>%
    left_join(df_risk, by = c("id", "group")) %>%
    left_join(df_harv_type, by = c("id", "group")) %>%
    left_join(df_company_type, by = c("id", "group")) %>%
    left_join(df_unadapted_regulation_cause, by = c("id", "group")) %>%
    filter(name != "")
  
  
  # Some participants have wrongly informed that they didn't know of any species harvested unsustainably, so we'll correct this:
  for (i in df_final$id) {
    if ("Unsustainable" %in% df_final$sustainability[df_final$id==i]) {
      df_final$known_unsustainable[df_final$id==i] = "Yes"
    }
  }
  
  return(df_final)
}

df_all_species_data <-  process_all_species_data(raw_data)




process_all_profile_data <- function(data) {
  # Columns related to species, departments, and harvester type
  species_cols <- data %>%
    dplyr::select(answer_ID,
                  matches("G21")) # careful of the number of zeroes after Q
  
  df_long <- data %>%
    # Select all necessary columns at once
    dplyr::select(id = answer_ID,
                  colnames(species_cols)) %>%
    
    # Pivot to long format for easier manipulation
    pivot_longer(
      cols = -c(id),
      names_to = c("group", "qcode"),
      names_pattern = "^G(\\d+)Q0{3,4}(\\d+)_", # careful of the number of zeroes after Q
      values_to = "value",
      values_drop_na = TRUE,
      values_transform = list(value = as.character) # because of the numeric (date columns)
    ) %>%
    mutate(
      type = case_when(
        qcode == "1" ~ "gender",
        qcode == "2" ~ "age",
        qcode == "4" ~ "evt_studies",
        qcode == "6" ~ "harvester",
        qcode == "7" ~ "harvesting_start",
        qcode == "8" ~ "nb_harv_species",
        qcode == "9" ~ "harvester_status",
        qcode == "10" ~ "harvester_status_company",
        qcode == "11" ~ "harv_group",
        qcode == "12" ~ "name_harv_group",
        qcode == "14" ~ "name_affiliated_organisation",
        qcode == "15" ~ "job_in_affiliated_organisation",
        qcode == "16" ~ "size_affiliated_organisation",
        qcode == "17" ~ "nb_species_affiliated_organisation")
    ) %>%
    
    filter(!is.na(type)) %>%
    dplyr::select(-qcode) %>%
    
    # Pivot wider to get species, presence, and etendue_cueill in columns
    pivot_wider(
      names_from = type,
      values_from = value,
      values_fn = list
    ) %>%
    
    unnest(everything()) %>%
    
    # Keep only one record per ID, species, and group to avoid double-counting
    unique()
  
  
  # Use the helper function 2 to process education and CSP...
  df_education_level <- process_data_helper_2(species_cols, "3", "3", "education_level")
  df_socio_pro_category <- process_data_helper_2(species_cols, "5", "5", "socio_pro_category")
  df_affiliated_organisation_type <- process_data_helper_2(species_cols, "13", "13", "affiliated_organisation_type")
  
  # Join the two data frames
  df_final <- left_join(df_long, df_education_level, by = c("id", "group")) %>%
    left_join(df_socio_pro_category, by = c("id", "group")) %>%
    left_join(df_affiliated_organisation_type, by = c("id", "group")) %>%
    
    dplyr::select(-group) 
  
  return(df_final)
}

df_all_profile_data <-  process_all_profile_data(raw_data)


##### Join species and profile data ####
# Add prefixes to columns (except id)
df_profile_renamed <- df_all_profile_data %>%
  rename_with(~ paste0("PROFILE_", .), -id)

df_species_renamed <- df_all_species_data %>%
  rename_with(~ paste0("SPECIES_", .), -id)

# Full join
all_data <- df_profile_renamed %>%
  full_join(df_species_renamed, by = "id") %>%
  
  # Simplify categories
  mutate(
    SPECIES_uses = case_when(
      str_detect(SPECIES_uses, "Food") ~ "Food",
      str_detect(SPECIES_uses, "Cosmetics")  ~ "Cosmetics",
      str_detect(SPECIES_uses, "Craft")   ~ "Craft",
      str_detect(SPECIES_uses, "Ornamental")  ~ "Ornamental",
      str_detect(SPECIES_uses, "Medical")     ~ "Medical",
      TRUE ~ SPECIES_uses
    ),
    SPECIES_harv_parts = case_when(
      str_detect(SPECIES_harv_parts, "Aerial")    ~ "Aerial part",
      str_detect(SPECIES_harv_parts, "Underground") ~ "Underground part",
      TRUE ~ SPECIES_harv_parts
    ),
    SPECIES_market_trend= dplyr::recode(SPECIES_market_trend, "Yes" = "Trend", "No" = "No trend"),
    SPECIES_market_trend_risk = dplyr::recode(SPECIES_market_trend_risk, "Yes" = "Trend with risk", "No" = "Trend without risk"),
    PROFILE_harvester_status = case_when(
      str_detect(PROFILE_harvester_status, "leisure")  ~ "Recreational",
      str_detect(PROFILE_harvester_status, "income") ~ "Professional",
      TRUE ~ PROFILE_harvester_status
    ),
    # Replace NA in PROFIL_nb_esp_cueill with 0 
    PROFILE_nb_harv_species = if_else(is.na(PROFILE_nb_harv_species), "0", PROFILE_nb_harv_species)) %>%
  
  # Pattern-based recoding (variable/stable/etc.)
  mutate(across(-id, ~ case_when(
    str_detect(., "variable") ~ "Variable",
    str_detect(., "stable")   ~ "Stable",
    str_detect(., "decrease")  ~ "Decrease",
    str_detect(., "increase") ~ "Increase",
    TRUE ~ .
  ))) %>%
  
  # Merge SPECIES_market_trend and SPECIES_market_trend_risk
  mutate(
    SPECIES_market_trend = if_else(SPECIES_market_trend == "Trend", SPECIES_market_trend_risk, SPECIES_market_trend),
    PROFILE_harvester_status = if_else(PROFILE_harvester == "Yes", PROFILE_harvester_status, "Non-harvester")
  ) %>%
  dplyr::select(-c(SPECIES_market_trend_risk, PROFILE_harvester)) %>%
  
  # Text cleaning
  mutate(across(everything(), ~ .x %>%
                  str_replace("I don't know", "IDK") %>%
                  str_remove("\\(.*\\)"))) %>%
  
  # Presence recoding
  mutate(
    PROFILE_affiliated_organisation_type = case_when(
      str_detect(PROFILE_affiliated_organisation_type, "No affiliated organisation") ~ "None",
      str_detect(PROFILE_affiliated_organisation_type, "wild plants") ~ "Wild-plant business",
      TRUE ~ PROFILE_affiliated_organisation_type
    )
  ) %>%
  
  # Group harvested parts
  mutate(SPECIES_harv_parts_group = case_when(
    SPECIES_harv_parts %in% c("Leaves", "Buds", "Aerial part", "Seedlings") ~ "Aerial part",
    SPECIES_harv_parts %in% c("Flowers", "Fruits", "Seeds") ~ "Reproductive part",
    SPECIES_harv_parts %in% c("Bark", "Sap") ~ "Other",
    SPECIES_harv_parts == "Whole plant" ~ "Whole plant",
    SPECIES_harv_parts == "Underground part" ~ "Underground part")) %>%
  dplyr::select(-SPECIES_harv_parts) %>%
  unique() %>%
  
  # Wide format: harvested parts
  mutate(presence = "Yes") %>%
  pivot_wider(
    names_from = SPECIES_harv_parts_group,
    values_from = presence,
    names_prefix = "SPECIES_harv_parts_",
    values_fill = "Non"
  ) %>%
  dplyr::select(-matches("SPECIES_harv_parts_NA$")) %>%
  
  # Wide format: usages
  mutate(presence = "Yes") %>%
  pivot_wider(
    names_from = SPECIES_uses,
    values_from = presence,
    names_prefix = "SPECIES_uses_",
    values_fill = "No"
  ) %>%
  dplyr::select(-matches("SPECIES_uses_NA$")) %>%
  
  # Add taxref, Raunkiaer, CSR, species presence (cover %) per département, species status (native...)
  left_join(taxref, by = c("SPECIES_name" = "LB_NOM")) %>%
  left_join(raunkieaer, by = "CD_REF") %>%
  left_join(csr, by = "CD_REF") %>%
  left_join(all_rarity, by = join_by("CD_REF", "SPECIES_dpt"=="dpt_simple")) %>%
  left_join(vascular) %>%
  left_join(departements_simpl_PARIS %>% dplyr::select(dpt_simple, massif), join_by("SPECIES_dpt"=="dpt_simple")) %>%
  dplyr::select(-dpt_name, -geometry) %>%
  
  # Convert to factors (excluding key cols)
  mutate(across(-c(id, SPECIES_name, CD_REF, C, S, R, sp_relative_area, sp_area, sp_area_FR), as.factor))



####_________####
#### % of species identified at the genus level and not-plant species ####
corresp_species <- read.csv("raw_data/corresp_species.csv")
all_data_corresp <- all_data %>%
  left_join(corresp_species %>% 
              dplyr::select(SPECIES_name, Comment),
            by="SPECIES_name") 

number_non_plant <- all_data_corresp[all_data_corresp$Comment %in% c("Mushroom", "Alga", "Lichen"),]
number_non_plant
number_genus <- all_data_corresp[is.na(all_data_corresp$CD_REF) &
                                   !(all_data_corresp$Comment %in% c("Mushroom", "Alga", "Lichen")),]
number_genus
# non plants : 27/1117=2.4%
# genus level : 41/1117=3.7%

length(unique(number_non_plant$id)) # 25 non plant respondents
length(unique(number_genus$id)) # 40 respondents giving species genera only
length(unique(all_data_corresp$id)) # 536 total respondents


####_________####

#### Remove mushrooms, or species identified at the genus level for the rest of the analysis ####
all_data <- all_data %>%
  filter(!is.na(CD_REF)) # 469 total respondents



####_________####
#### APPENDIX A - Analysis of the survey’s representativeness ####

##### Age ####
age <- ggplot(all_data %>% 
         select(id, PROFILE_age) %>% 
         unique() %>% 
         mutate(PROFILE_age = ifelse(is.na(PROFILE_age),
                                     "Prefer not to answer",
                                     as.character(PROFILE_age)),
                PROFILE_age = factor(PROFILE_age, levels=
                                       c("Under 26 years", "26 to 35 years", "36 to 50 years", "Over 50 years", "Prefer not to answer"))),
       aes(x = PROFILE_age)) +
  geom_bar() +
  ylab("Count") +
  xlab("Respondent age") +
  theme_minimal(base_size = 13)


#plot_zoom_png?width=686&height=446
png("plots/AppendixA_age.png", 
    width = 2058,     # pixels
    height = 1338,   # pixels
    res = 300)        # resolution in dpi
age
dev.off()


##### Gender ####
gender <- ggplot(all_data %>% 
         select(id, PROFILE_gender) %>% 
         unique() %>% 
         mutate(PROFILE_gender = ifelse(is.na(PROFILE_gender),
                                        "Prefer not to answer",
                                        as.character(PROFILE_gender))),
       aes(x = PROFILE_gender)) +
  geom_bar() +
  ylab("Count") +
  xlab("Respondent gender") +
  theme_minimal(base_size = 13)

# plot_zoom_png?width=607&height=557
png("plots/AppendixA_gender.png", 
    width = 1821,     # pixels
    height = 1671,   # pixels
    res = 300)        # resolution in dpi
gender
dev.off()


##### Socio-professional category ####
df_sociopro <- all_data %>% 
  select(id, PROFILE_socio_pro_category) %>% 
  unique() %>% 
  mutate(PROFILE_socio_pro_category = ifelse(is.na(PROFILE_socio_pro_category),
                                             "Prefer not to answer",
                                             as.character(PROFILE_socio_pro_category)))

# order by frequency
ord <- names(sort(table(df_sociopro$PROFILE_socio_pro_category), decreasing = TRUE))

# move "Other" and "Prefer not to answer" to the end
ord <- c(setdiff(ord, c("Other", "Prefer not to answer")),
         "Other", "Prefer not to answer")

df_sociopro$PROFILE_socio_pro_category <- factor(df_sociopro$PROFILE_socio_pro_category, levels = ord)

socio_pro <- ggplot(df_sociopro, aes(x = PROFILE_socio_pro_category)) +
  geom_bar() +
  ylab("Count") +
  xlab("Respondent socio-economic category") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 70, hjust=1))


# plot_zoom_png?width=767&height=557
png("plots/AppendixA_sociopro.png", 
    width = 2301,     # pixels
    height = 1671,   # pixels
    res = 300)        # resolution in dpi
socio_pro
dev.off()


##### Harvester status ####
df_harvstatus <- all_data %>% 
  select(id, PROFILE_harvester_status) %>% 
  unique() %>% 
  mutate(PROFILE_harvester_status = ifelse(is.na(PROFILE_harvester_status),
                                           "Prefer not to answer",
                                           as.character(PROFILE_harvester_status)))

# order levels
ord <- names(sort(table(df_harvstatus$PROFILE_harvester_status), decreasing = TRUE))
ord <- c(setdiff(ord, c("Other", "Prefer not to answer")),
         "Other", "Prefer not to answer")

df_harvstatus$PROFILE_harvester_status <- factor(df_harvstatus$PROFILE_harvester_status, levels = ord)

harvstatus <- ggplot(df_harvstatus, aes(x = PROFILE_harvester_status)) +
  geom_bar() +
  ylab("Count") +
  xlab("Respondent harvester status") +
  theme_minimal(base_size = 13)


# plot_zoom_png?width=593&height=557
png("plots/AppendixA_harvstatus.png", 
    width = 1779,     # pixels
    height = 1000,   # pixels
    res = 300)        # resolution in dpi
harvstatus
dev.off()


##### Affiliated organisation type ####
df_affilorga <- all_data %>% 
  select(id, PROFILE_affiliated_organisation_type) %>% 
  unique() %>% 
  mutate(PROFILE_affiliated_organisation_type = ifelse(is.na(PROFILE_affiliated_organisation_type),
                                                    "Prefer not to answer",
                                                    as.character(PROFILE_affiliated_organisation_type)))

# order levels
ord <- names(sort(table(df_affilorga$PROFILE_affiliated_organisation_type), decreasing = TRUE))
ord <- c(setdiff(ord, c("Other", "Prefer not to answer")),
         "Other", "Prefer not to answer")

df_affilorga$PROFILE_affiliated_organisation_type <- factor(df_affilorga$PROFILE_affiliated_organisation_type, levels = ord)

affilorga <- ggplot(df_affilorga, aes(x = PROFILE_affiliated_organisation_type)) +
  geom_bar() +
  ylab("Count") +
  xlab("Affiliated organisation type") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot_zoom_png?width=712&height=557
png("plots/AppendixA_affilorga.png", 
    width = 2136,     # pixels
    height = 1671,   # pixels
    res = 300)        # resolution in dpi
affilorga
dev.off()



##### Demographics (counties covered) ####
process_and_map_expertise <- function(data, shp) {
  data %>%
    dplyr::select(id, SPECIES_dpt) %>%
    group_by(SPECIES_dpt) %>%
    summarise(n_answers = n_distinct(id), .groups = "drop") %>%
    full_join(shp, by = join_by(SPECIES_dpt==dpt_simple)) %>%
    st_as_sf()
}

# Process and prepare data for mapping
dpt_answers <- process_and_map_expertise(all_data, departements_simpl_PARIS)

# Create the departmental plot
dpt_plot <- ggplot(dpt_answers) +
  geom_sf(aes(fill = n_answers), colour = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = n_answers), colour = "black", size = 3) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(9, "Reds"),
    na.value = "grey80",
    n.breaks = 10,
    name = "Answers"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

dpt_plot


# plot_zoom_png?width=762&height=646
png("plots/AppendixA_dptcover.png", 
    width = 2286,     # pixels
    height = 1938,   # pixels
    res = 300)        # resolution in dpi
dpt_plot
dev.off()




####_________####
#### How many species in common with the 1st paper ? ####
list_1st_paper <-  read.csv("raw_data/liste_Lescure_v17.csv") %>%
  filter(Regroupement %in% c("Angiospermes", "Gymnospermes", "Fougères")) %>%
  dplyr::select(CD_REF)

list_1st_paper$first_paper <- "yes"

in_common <- read.csv("raw_data/corresp_species.csv")
in_common$in_survey <- "yes"
in_common <- in_common %>%
  full_join(list_1st_paper)


# Nb of plants in Lescure list
nrow(unique(subset(in_common, !is.na(CD_REF) & first_paper=="yes", select=CD_REF))) #692, excluding mushrooms/lichen/algue

# Nb of plants cited in survey that were in the 1st paper
nrow(unique(subset(in_common, in_survey=="yes" & first_paper=="yes", select=SPECIES_name)))
unique(subset(in_common, in_survey=="yes" & first_paper=="yes", select=SPECIES_name))
# 109 species in common
nrow(unique(subset(in_common, in_survey=="yes" &  !is.na(CD_REF), select=SPECIES_name)))
# 146 species cited (excluding mushrooms/lichen/algue and species identified at the genus level)
# 109/146=74.7% of species cited here are present in the first list
# 109/692=15.7% of species from the first list are cited here

# Nb of new plants
nrow(unique(subset(in_common, !is.na(CD_REF) & in_survey=="yes" & is.na(first_paper), select=SPECIES_name)))
unique(subset(in_common, !is.na(CD_REF) & in_survey=="yes" & is.na(first_paper), select=SPECIES_name))
# 37 new plants
unique(subset(in_common, is.na(CD_REF) & in_survey=="yes" & is.na(first_paper) 
              & !Comment %in%  c("Mushroom", "Alga", "Lichen"), select=SPECIES_name)) # species identified to genus level, 15 "species"


####_________####
#### Number of AFC/Simples answers ####
rattach_gp_cueill <- all_data %>%
  dplyr::select(id, PROFILE_name_affiliated_organisation) %>%
  unique() %>%
  na.omit()

ggplot(rattach_gp_cueill, aes(x=PROFILE_name_affiliated_organisation)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=90, hjust=1))


####_________####
#### APPENDIX E - Bar Plot of all cited species (only >1 citation) ####
df_species <- all_data %>%
  dplyr::select(id, SPECIES_name) %>%
  na.omit() %>%
  distinct() %>%
  count(SPECIES_name, name = "citations") %>%
  filter(citations > 1)

# Identify top 10 species by citations
top10_species <- df_species %>%
  top_n(10, citations) %>%
  pull(SPECIES_name)

# Plot
all_cited <- ggplot(df_species, aes(x = fct_reorder(SPECIES_name, -citations), y = citations)) +
  geom_col() +
  # scale_x_discrete(labels = function(x) ifelse(x %in% top10_species, x, "")) +
  labs(
    x = "",
    y = "Number of mentions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, face="italic", hjust = 1))
all_cited

# plot_zoom_png?width=1000&height=611
png("plots/AppendixE_all_cited.png", 
    width = 2000,     # pixels
    height = 1222,   # pixels
    res = 300)        # resolution in dpi
all_cited
dev.off()


#### APPENDIX ? - Cumulative frequency plot of species ####
plot_cumulative_species <- function(data) {
  
  # Count citations per species
  df_cum <- data %>%
    dplyr::select(id, SPECIES_name) %>%
    unique() %>%   # avoid duplicate id-species pairs
    na.omit() %>%
    count(SPECIES_name, name = "citations") %>%
    arrange(desc(citations)) %>%
    mutate(
      cum_citations = cumsum(citations),
      cum_percent   = cum_citations / sum(citations) * 100,
      rank_species  = row_number()
    )
  
  # Cutoffs at 50%, 75%, 95%
  cutoffs <- c(50, 80, 95)
  cutoff_species <- sapply(cutoffs, function(c) min(df_cum$rank_species[df_cum$cum_percent >= c]))
  cutoff_df <- data.frame(cutoff = cutoffs, species = cutoff_species)
  
  # Knee detection (returns index of cutoff species)
  knee_idx <- inflection::uik(df_cum$rank_species, df_cum$cum_percent)
  cutoff_species_knee <- knee_idx[1]  # first detected inflection point
  
  # Exponential fit: y = a*(1 - exp(-b*x))
  fit <- nls(
    cum_percent ~ a * (1 - exp(-b * rank_species)),
    data = df_cum,
    start = list(a = 100, b = 0.05)  # reasonable starting values
  )
  df_cum$exp_fit <- predict(fit, newdata = df_cum)
  
  # Plot cumulative frequency with exponential fit
  ggplot(df_cum, aes(x = reorder(SPECIES_name, -citations), y = cum_percent, group = 1)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "steelblue", size = 2) +
    
    # Exponential fit line
    geom_line(aes(y = exp_fit), color = "purple", size = 1, linetype = "solid") +
    
    # Vertical cutoffs
    geom_vline(data = cutoff_df, aes(xintercept = species), linetype = "dashed", color = "grey50") +
    geom_text(
      data = cutoff_df, aes(x = species, y = 100,
                            label = paste0(cutoff, "% → ", species, " species")),
      inherit.aes = FALSE, hjust = -0.05, vjust = 0, size = 3, color = "grey30") +
    
    
    # Knee cutoff
    geom_vline(xintercept = cutoff_species_knee, linetype = "dashed", color = "purple") +
    annotate("text", x = cutoff_species_knee, y = 60,
             label = paste("Exponential cutoff →", cutoff_species_knee, "species"),
             hjust = -0.02, vjust = 0, color = "purple") +
    
    labs(
      title = "Cumulative frequency of species citations",
      subtitle = "Exponential fit (purple) with explicit cutoffs at 50%, 80%, 95% (grey)",
      x = "Species (ranked by citation frequency)",
      y = "Cumulative % of citations"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
      plot.title = element_text(hjust = 0.5)
    )
}

plot_cumulative_species(all_data)


# Percentage of species with 1 mention :
sp_mentions <- all_data %>% group_by(CD_REF, SPECIES_name) %>%
  summarise(n_mentions=n())
# Allium ursinum has 101 mentions
nrow(sp_mentions %>% filter(n_mentions==1))
nrow(sp_mentions)
# 62/145=42.7% of species with only one mention


####_________####
#### Figure 2 - Harvesting sustainability of the 20 most cited species ####
plot_sustainability_ratio <- function(data, n = NULL,
                                  min_ratio = NULL, max_ratio = NULL,
                                  min_citations = NULL, max_citations = NULL) {
  
  # Top species by total citations (if n is provided)
  top_species <- data %>%
    filter(!is.na(CD_REF)) %>%
    dplyr::select(SPECIES_name, id) %>%
    unique() %>%
    count(SPECIES_name, name = "total_citations")
  
  if (!is.null(n)) {
    top_species <- top_species %>%
      slice_max(total_citations, n = n)
  }
  
  top_species <- top_species %>% pull(SPECIES_name)
  
  # Summarise counts + ratio
  df_summary <- data %>%
    filter(SPECIES_name %in% top_species) %>%
    distinct(id, SPECIES_name, choix_type_bio, SPECIES_sustainability) %>%
    count(SPECIES_name, choix_type_bio, SPECIES_sustainability) %>%
    group_by(SPECIES_name, choix_type_bio) %>%
    mutate(
      total_citations = sum(n),
      ratio_unsustainable = sum(n[SPECIES_sustainability == "Unsustainable"], na.rm = TRUE) / sum(n)
    ) %>%
    summarise(
      ratio_unsustainable = unique(ratio_unsustainable),
      total_citations = unique(total_citations),
      .groups = "drop"
    ) %>%
    arrange(ratio_unsustainable)
  
  # Apply filtering if requested
  if (!is.null(min_ratio)) {
    df_summary <- df_summary %>% filter(ratio_unsustainable >= min_ratio)
  }
  if (!is.null(max_ratio)) {
    df_summary <- df_summary %>% filter(ratio_unsustainable <= max_ratio)
  }
  if (!is.null(min_citations)) {
    df_summary <- df_summary %>% filter(total_citations >= min_citations)
  }
  if (!is.null(max_citations)) {
    df_summary <- df_summary %>% filter(total_citations <= max_citations)
  }
  
  # Keep ordering for plot
  df_summary <- df_summary %>%
    mutate(SPECIES_name = factor(SPECIES_name, levels = unique(SPECIES_name)))
  
  # Dot plot
  ggplot(df_summary, aes(x = ratio_unsustainable, y = SPECIES_name, size = total_citations)) +
    # Add vertical line at 50%
    geom_vline(xintercept = 0.5, color = "grey70", linewidth = .3, linetype = "solid") +
    geom_point(color = "grey10") +
    # geom_point(aes(color = choix_type_bio)) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1)  # 0% to 100%
    ) +
    scale_size_area(max_size = 10) +
    labs(
      x = "Ratio of 'unsustainable' mentions",
      y = "",
      size = "Total mentions"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12, face = "italic"),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.4)
    )
}

plot_sustainability_ratio(all_data %>% filter(!is.na(SPECIES_name)), n = 20)


# plot_zoom_png?width=786&height=600
png("plots/Figure_2_durability_ratio.png", 
    width = 2358,     # pixels
    height = 1800,   # pixels
    res = 300)        # resolution in dpi
plot_sustainability_ratio(all_data %>% filter(!is.na(SPECIES_name)), n = 20)
dev.off()


#### % of unsustainable species ####
summary_unsustainable_species <- df_all_species_data %>%
  mutate(flag = 1) %>%
  pivot_wider(
    names_from = sustainability,
    values_from = flag,
    values_fill = NA  # fill missing combinations with 0
  ) %>%
  group_by(name) %>%
  summarise(Unsustainable = sum(Unsustainable, na.rm=T),
            Sustainable = sum(Sustainable, na.rm=T),
            Ratio = round(100*Unsustainable/(Unsustainable+Sustainable), 2), 
            .groups = "drop")

total_ratio_unsustainable <- summary_unsustainable_species %>%
  summarise(mean_ratio=mean(Ratio, na.rm=T))
total_ratio_unsustainable

percent_unsustainable <- summary_unsustainable_species %>%
  summarise(percent = round(100 * mean(Ratio >+ 50), 2))
percent_unsustainable # 36% of the species were, on average, perceived as being harvested unsustainably

####_________####
#### Figure 3 - Rare species sustainability issues #####
citations_by_species_dpt <- df_all_species_data %>%
  dplyr::select(id, name, sustainability, dpt) %>% 
  filter(!is.na(dpt) & dpt != "") %>%
  unique() %>%
  filter(!is.na(dpt) & dpt != "") %>%
  group_by(dpt, name, sustainability) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = sustainability,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total_citations = Sustainable + Unsustainable)


#### Plot distribution of species rarity ####
# Prepare datasets
data_rarity_plot <- all_data %>% select(CD_REF, sp_area_FR) %>%
  unique() %>%
  mutate(in_survey="yes") %>%
  full_join(all_rarity %>% select(CD_REF, sp_area_FR) %>%
              unique() %>% mutate(in_survey ="no")) %>% # want to keep all species from all_rarity
  na.omit()

quantiles_all <- quantile(data_rarity_plot$sp_area_FR[data_rarity_plot$in_survey=="no"], 
                          probs = c(0.25, 0.5, 0.75))
quantiles_survey <- quantile(data_rarity_plot$sp_area_FR[data_rarity_plot$in_survey=="yes"], 
                             probs = c(0.25, 0.5, 0.75))

France <- all_rarity %>% group_by(dpt_name) %>% summarise(dpt_area = max(dpt_area)) %>% 
  summarise(total_area = sum(dpt_area)) %>% pull(total_area)



ggplot(data_rarity_plot, aes(x = 100*sp_area_FR/France, fill = in_survey)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  
  geom_vline(xintercept = 100*quantiles_all/France,
             linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = 100*quantiles_survey/France,
             linetype = "dashed", color = "red", size = 1) +
  
  annotate("text",
           x = 100*quantiles_all/France,
           y = Inf,
           label = paste0(names(quantiles_all),
                          "% total flora"),
           angle = 90,
           vjust = -0.1,
           hjust = 1,
           color = "blue") +
  
  annotate("text",
           x = 100*quantiles_survey/France,
           y = Inf,
           label = paste0(names(quantiles_survey),
                          "% survey species"),
           angle = 90,
           vjust = -0.1,
           hjust = 1,
           color = "red") +
  
  scale_x_continuous(breaks = c(0, 5, 10, 20, 25, 50, 75, 100)) +
  
  scale_fill_manual(
    name = "",
    values = c("no" = "blue", "yes" = "red"),
    labels = c("Total flora", "Species in survey")
  ) +
  
  theme_minimal() +
  labs(
    x = "Species area in France (%)",
    y = "Number of species"
  )

#### Plot rare species sustainability ####
France <- all_rarity %>% group_by(dpt_name) %>% summarise(dpt_area = max(dpt_area)) %>% 
  summarise(total_area = sum(dpt_area)) %>% pull(total_area)

plot_sustainability_ratio(all_data %>% mutate(relative_sp_area_FR = 100*sp_area_FR/France) %>%
                        filter(relative_sp_area_FR < 20 &
                                 native=="native"), n = NULL,
                      min_ratio=0, min_citations=2, max_citations=7) # there are no species under 50% unsustainable ratio !
# max citations=7 because the 20 most cited species are cited minimum 8 times (Artemisia genipi)

# plot_zoom_png?width=786&height=416
png("plots/Figure_3_rare_species_20pct.png", 
    width = 2358,
    height = 1248,
    res = 300)
plot_sustainability_ratio(all_data %>% mutate(relative_sp_area_FR = 100*sp_area_FR/France) %>%
                        filter(relative_sp_area_FR < 20 &
                                 native=="native"), n = NULL,
                      min_ratio=0.1, min_citations=2, max_citations=7)
dev.off()



####_________####
#### APPENDIX F - Identifying distinct respondent profiles ####
##### Prepare data ###
all_data_ACM_participant_profile <- all_data %>%
  dplyr::select(id, PROFILE_harvester_status, PROFILE_affiliated_organisation_type,
                PROFILE_age, PROFILE_gender, PROFILE_socio_pro_category, 
                PROFILE_nb_harv_species, PROFILE_education_level) %>%
  unique() %>%
  na.omit()

##### Perform MCA ###
# Keep a reference table for IDs
ref_table_particip <- all_data_ACM_participant_profile %>% dplyr::select(id)
ref_table_particip$row_name <- rownames(ref_table_particip)

# MCA without id
res.mca.particip <- MCA(all_data_ACM_participant_profile %>% dplyr::select(-id),
                        graph = FALSE)

# Quick plots
plot(res.mca.particip)
plot(res.mca.particip, invisible = c("var","quali.sup","quanti.sup"), cex = 0.7)
plotellipses(res.mca.particip)

##### Extract coordinates for each individual ###
ind_coords_particip <- bind_cols(ref_table_particip, 
                                 as.data.frame(res.mca.particip$ind$coord))
head(ind_coords_particip)

##### Perform hierarchical clustering based on MCA coordinates ###
res.hcpc.particip <- HCPC(res.mca.particip, nb.clust = -1, graph = FALSE) 
print(res.hcpc.particip$desc.var)

# Recode cluster numbers
res.hcpc.particip$data.clust$clust <- recode(res.hcpc.particip$data.clust$clust,
                                           `1` = "Non-harvesting_professionals",
                                           `2` = "Recreational_harvesters",
                                           `3` = "Professional_harvesters") %>%
  factor(levels = c("Professional_harvesters",
                    "Recreational_harvesters",
                    "Non-harvesting_professionals"))


##### Plot clusters ###
plot_data_particip <- data.frame(
  row_name = rownames(res.mca.particip$ind$coord),
  Dim.1 = res.mca.particip$ind$coord[,1],
  Dim.2 = res.mca.particip$ind$coord[,2],
  Group = res.hcpc.particip$data.clust$clust
)

plot_MCA_particip <- ggplot(plot_data_particip, aes(Dim.1, Dim.2, color = Group)) +
  geom_point(alpha = 0.5, size = 3) +
  stat_ellipse(aes(group = Group), type = "t", geom = "polygon", alpha = 0.2, linetype = "dashed") +
  labs(
    x = paste0("Dimension 1 (", round(res.mca.particip$eig[1,2], 1), "%)"),
    y = paste0("Dimension 2 (", round(res.mca.particip$eig[2,2], 1), "%)"),
  ) +
  theme_minimal(base_size = 13) +
  scale_color_viridis_d(option = "plasma")

plot_MCA_particip


##### Plot clusters with top contributing variables ###
var_data <- as.data.frame(res.mca.particip$var$coord)
var_contrib <- as.data.frame(res.mca.particip$var$contrib)

# Add contributions and select top 10 per dimension
var_data <- var_data %>%
  mutate(contrib.Dim.1 = var_contrib$`Dim 1`,
         contrib.Dim.2 = var_contrib$`Dim 2`)
var_data <- var_data %>%
  arrange(desc(contrib.Dim.1)) %>%
  slice_head(n = 10) %>%
  bind_rows(var_data %>% arrange(desc(contrib.Dim.2)) %>% slice_head(n = 10)) %>%
  distinct()

# Clean labels
new_labels <- rownames(var_data) %>%
  gsub("Only one$", "Only one harvested species", .) %>%
  gsub("^0$", "0 harvested species", .) %>%
  gsub("^1 to 5$", "1 to 5 harvested species", .) %>%
  gsub("^5 to 30$", "5 to 30 harvested species", .) %>%
  gsub("^30 to 100$", "30 to 100 harvested species", .) %>%
  gsub("PROFILE_", "", .) %>%
  gsub("Professional$", "Professional harvester", .) %>%
  gsub("Recreational$", "Recreational harvester", .)
rownames(var_data) <- new_labels


# Create a named vector with name simplifications
simplifications <- c(
  "Professional harvester" = "Professional harvester",
  "socio_pro_category_Farmers" = "Farmers / farm owners",
  "affiliated_organisation_type_Wild-plant business" = "Wild-plant business",
  "30 to 100 harvested species" = "30-100 species collected",
  "0 harvested species" = "0 species collected",
  "Non-harvester" = "Non-harvester",
  "affiliated_organisation_type_Cooperative or trade union" = "Agricultural cooperative or union",
  "affiliated_organisation_type_Environmental enforcement authorities " = "Environmental police",
  "socio_pro_category_Artisans, traders, business owners" = "Craftspeople, merchants, business owners",
  "Recreational harvester" = "Recreational harvester",
  "socio_pro_category_Retired" = "Retired",
  "affiliated_organisation_type_None" = "No affiliated structure",
  "affiliated_organisation_type_Government department " = "State agency",
  "affiliated_organisation_type_Protected area manager " = "Protected area manager",
  "1 to 5 harvested species" = "1-5 species collected"
)

# Replace row names in var_data
rownames(var_data) <- simplifications[rownames(var_data)]

# Add arrows and labels to MCA plot
plot_MCA_with_vars <- plot_MCA_particip +
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.2) +
  geom_text_repel(data = var_data, aes(x = `Dim 1`, y = `Dim 2`, label = rownames(var_data)),
                  color = "black", size = 4)

plot_MCA_with_vars


# plot_zoom_png?width=937&height=634
png("plots/AppendixF_profile_MCA.png", 
    width = 2811,     # pixels
    height = 1902,   # pixels
    res = 300)        # resolution in dpi
plot_MCA_with_vars
dev.off()

res.hcpc.particip$desc.var

##### Compare the amount of men ####
# Reattach cluster to original participant profile
cluster_gender <- all_data_ACM_participant_profile %>%
  select(id, PROFILE_gender) %>%
  left_join(
    res.hcpc.particip$data.clust %>%
      mutate(row_name = rownames(.)) %>%
      left_join(ref_table_particip, by = "row_name") %>%
      select(id, clust),
    by = "id"
  )

# Contingency table
table_gender_cluster <- table(cluster_gender$clust,
                              cluster_gender$PROFILE_gender)

# Percentages within cluster (rows sum to 100)
prop_gender_cluster <- prop.table(table_gender_cluster, margin = 1) * 100

round(prop_gender_cluster, 1)


##### Analyse performance #####
# Calculate the distance matrix based on the MCA coordinates
dist_matrix <- dist(res.mca.particip$ind$coord)

# Get the cluster assignments from the HCPC result
# Convert the cluster factor to a numeric vector for compatibility
cluster_assignments_numeric <- recode(res.hcpc.particip$data.clust$clust,
                                             "Non-harvesting_professionals" = 3,
                                             "Recreational_harvesters" = 2,
                                             "Professional_harvesters" = 1) %>%
  as.character() %>% as.numeric()

# Compute the silhouette values with the numeric cluster assignments
sil_result <- silhouette(cluster_assignments_numeric, dist_matrix)
plot(sil_result)


# plot_zoom_png?width=793&height=838
png("plots/AppendixF_silhouette.png", 
    width = 2379,     # pixels
    height = 2514,   # pixels
    res = 300)        # resolution in dpi
plot(sil_result)
dev.off()


#### Do respondent profiles impact the perception of sustainability ? ####
##### Sustainability status VS respondent profile ####

all_data_profile_clusters <- plot_data_particip %>%
  dplyr::select(row_name, Group) %>%
  full_join(ref_table_particip, by="row_name") %>%
  left_join(all_data) %>%
  dplyr::select(-row_name)


d <- all_data_profile_clusters %>% 
  dplyr::select(id, SPECIES_name, massif, SPECIES_sustainability, Group) %>%
  unique() %>%
  na.omit()

names(d) <- c("id", "species", "harvest_area", "sustainability", "respondent_profile")

d$sustainability <- ifelse(d$sustainability == "Sustainable", 0, 1)

model1 <- glmer(sustainability ~ respondent_profile + (1|species),
                data = d, family = binomial)
summary(model1)

# Odds ratios and contributions
summary(model1)$coefficients %>%
  as.data.frame() %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`)
  ) %>%
  print()


# table(d$species, d$respondent_profile)


##### Unsustainable known VS respondent profile ####
# Barplot
all_data_unsustainable_known <- all_data %>%
  dplyr::select(id, PROFILE_harvester_status, 
                PROFILE_affiliated_organisation_type,
                PROFILE_age, PROFILE_gender, PROFILE_socio_pro_category, PROFILE_nb_harv_species, 
                PROFILE_education_level,
                SPECIES_known_unsustainable) %>%
  unique() %>%
  mutate(across(-id, as.factor)) %>%  
  dplyr::select(-id) %>%
  na.omit()

ggplot(all_data_unsustainable_known, aes(x = "Total", fill = SPECIES_known_unsustainable)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Knowledge of unsustainable species harvesting",
    x = "",
    y = "Percentage of participants",
    fill = "Species with unsustainable harvesting"
  )


# Generalised linear model
d2 <- all_data_profile_clusters %>% 
  dplyr::select(id, SPECIES_name, massif, SPECIES_known_unsustainable, Group) %>%
  unique() %>%
  na.omit()

names(d2) <- c("id", "species", "harvest_area", "unsustainable_species_known", "respondent_profile")

d2$unsustainable_species_known <- ifelse(d2$unsustainable_species_known == "Yes", 1, 0)
d2$respondent_profile <- as.factor(d2$respondent_profile)

model3 <- glm(unsustainable_species_known ~ respondent_profile,
              data = d2, family = binomial)
summary(model3)



####_________####
#### Figure 4 - What variables impact the perception of sustainability ? ####
# Variable réponse (binaire : "Sustainable" / "Non durable")
data_dfa_durabilite_sp <- all_data %>%
  dplyr::select(id, SPECIES_name, CD_REF,
                SPECIES_sustainability, SPECIES_presence, SPECIES_harv_spread,
                SPECIES_state_resource, SPECIES_harv_variation, SPECIES_harv_intensity,
                starts_with("SPECIES_uses_"), starts_with("SPECIES_harv_parts_"), 
                SPECIES_market_trend, SPECIES_species_regulation) %>%
  unique() %>%
  na.omit() %>%
  dplyr::select(-CD_REF, -id) %>%
  droplevels() %>%
  mutate(across(-c(SPECIES_sustainability), as.factor))


active_vars <- data_dfa_durabilite_sp %>%
  dplyr::select(-SPECIES_sustainability, -SPECIES_name)

grouping_factor <- data_dfa_durabilite_sp$SPECIES_sustainability

# --- Run DFA ---
res.dfa <- discrimin(dudi.acm(active_vars, scannf = FALSE), 
                     fac = grouping_factor, 
                     scannf = FALSE)

# --- Visualise Results ---
plot_data <- data.frame(
  score_dfa = res.dfa$li$DS1,
  sustainability = grouping_factor
)

DFA_distrib <- ggplot(plot_data, aes(x = score_dfa, fill = sustainability)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("Unsustainable" = "#D00F0F", "Sustainable" = "#64A251")) +
  labs(x = "Discriminant Axis 1",
       y = "Density",
       fill = "Sustainability perception") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4))


DFA_distrib




# --- Get Contributions ---
correlations <- as.data.frame(res.dfa$va)

sorted_correlations <- correlations[order(abs(correlations$CS1), decreasing = TRUE), , drop = FALSE]

top_contributors <- head(sorted_correlations, 20) %>%
  arrange(desc(CS1))

print(top_contributors)

# Prepare contributions dataframe
var_contrib <- correlations %>%
  rownames_to_column(var = "Variable") %>%
  dplyr::rename(Coefficient = CS1) %>%
  arrange(Coefficient) %>%
  mutate(sustainability = ifelse(Coefficient > 0, "Unsustainable", "Sustainable"))

# Optional: Top 20 contributors by absolute value
top_var_contrib <- var_contrib %>%
  slice_max(order_by = abs(Coefficient), n = 20)

label_dict <- c(
  "SPECIES_market_trend.Trend.with.risk" = "Trend-driven harvesting (unsustainable)",
  "SPECIES_state_resource.A.decline.of.the.resource" = "Declining resource",
  "SPECIES_presence.Rare" = "Rare species",
  "SPECIES_species_regulation.Yes..it.is.regulated" = "Regulated species",
  "SPECIES_harv_intensity.High" = "High harvesting intensity",
  "SPECIES_harv_spread.Localised." = "Localised harvesting",
  "SPECIES_presence.Extremely.rare" = "Extremely rare species",
  "SPECIES_uses_Food.No" = "No food use",
  "SPECIES_harv_variation.Increase" = "Increasing harvesting levels",
  "SPECIES_uses_Ornamental.Yes" = "Ornamental use",
  "SPECIES_market_trend.Trend.without.risk" = "Trend-driven harvesting (sustainable)",
  "SPECIES_harv_intensity.Low" = "Low harvesting intensity",
  "SPECIES_uses_Ornamental.No" = "No ornamental use",
  "SPECIES_uses_Food.Yes" = "Food use",
  "SPECIES_harv_variation.Stable" = "Stable harvesting levels",
  "SPECIES_harv_spread.Widespread." = "Widespread harvesting",
  "SPECIES_species_regulation.No..no.known.regulation" = "Unregulated species",
  "SPECIES_market_trend.No.trend" = "No trend-driven harvesting",
  "SPECIES_presence.Widely.abundant" = "Widely abundant species",
  "SPECIES_state_resource.Stable" = "Stable resource"
)

# Diverging bar plot
var_contrib <- ggplot(top_var_contrib, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = sustainability)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Unsustainable" = "#D00F0F", "Sustainable" = "#64A251")) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "",
       y = "DFA Coefficient",
       fill = "Sustainability perception") +

  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4)) +
  
  scale_x_discrete(labels = function(x) label_dict[x])

var_contrib



# Extract legend from second plot
var_contrib_legend <- var_contrib
legend_grob <- get_legend(var_contrib_legend + 
                       theme(legend.position = "bottom",
                             legend.direction = "horizontal",
                             legend.justification = "center"))

# Remove legend from plots
DFA_distrib_nolegend <- DFA_distrib + theme(legend.position = "none")
var_contrib_nolegend <- var_contrib + theme(legend.position = "none")

# Combine plots side by side WITHOUT tagging yet
top_plots <- DFA_distrib_nolegend + var_contrib_nolegend

# Stack legend below
combined_with_legend <- top_plots / wrap_elements(legend_grob) + 
  plot_layout(heights = c(10, 1))



# plot_zoom_png?width=941&height=451
png("plots/Figure_4_DFA_sustainability.png", 
    width = 2823,     # pixels
    height = 1353,   # pixels
    res = 300)        # resolution in dpi
combined_with_legend + plot_annotation(tag_levels = "a", tag_suffix = "", tag_prefix = "")
dev.off()
# remove c in gimp


# --- Cohen's d ---
group_stats <- plot_data %>%
  group_by(sustainability) %>%
  summarise(mean_score = mean(score_dfa),
            sd_score = sd(score_dfa),
            n = n())

mean_diff <- diff(group_stats$mean_score)
pooled_sd <- sqrt(((group_stats$sd_score[1]^2)*(group_stats$n[1]-1) +
                     (group_stats$sd_score[2]^2)*(group_stats$n[2]-1)) /
                    (sum(group_stats$n)-2))

cat("Cohen's d (DS1 separation):", round(mean_diff / pooled_sd, 3), "\n")
# Cohen's d (DS1 separation): 2.8 


# --- Classification accuracy ---
# Prepare data for LDA (all predictors as factors)
lda_data <- data_dfa_durabilite_sp %>%
  dplyr::select(-SPECIES_name)

lda_fit <- lda(SPECIES_sustainability ~ ., data = lda_data)
pred <- predict(lda_fit)$class

table(Predicted = pred, Actual = lda_data$SPECIES_sustainability) # confusion matrix

cat("Classification accuracy:", round(mean(pred == lda_data$SPECIES_sustainability)*100, 1), "%\n")
# Classification accuracy: 90.9 %


#### APPENDIX G - Variable contributions to DFA ####
sorted_correlations
write.csv(sorted_correlations, "processed_data/APPENDIXG_variables_contrib_DFA.csv")

#### APPENDIX H - Perceived VS actual species abundance ####
# Do respondents’ perceptions of rarity and abundance correspond to actual species abundance ?
rarity <- all_data %>%
  dplyr::select(id, SPECIES_name, SPECIES_presence, SPECIES_sustainability, SPECIES_dpt, sp_relative_area) %>%
  unique() %>%
  na.omit()

##### Kruskal-Wallis test ###
kruskal.test(sp_relative_area ~ SPECIES_presence, data = rarity)
# Kruskal-Wallis chi-squared = 273.81, df = 4, p-value < 2.2e-16

kruskal_effsize(sp_relative_area ~ SPECIES_presence , data = rarity)
# 0.335 effect size, large

# Ensure SPECIES_presence is a factor in the desired order
ordre_presence <- c("Widely abundant", "Locally abundant", "Rare", "Extremely rare", "IDK")
rarity$SPECIES_presence <- factor(rarity$SPECIES_presence, levels = ordre_presence)

##### Dunn's test for pairwise comparisons (because KW test is significant) ###
rstatix::dunn_test(sp_relative_area ~ SPECIES_presence, p.adjust.method = "bonferroni", data=rarity)

##### Estimates and CI ###
# compute bootstrapped CI for each group
set.seed(123)  # for reproducibility
boot_median <- function(data, indices) {
  median(data[indices], na.rm = TRUE)
}

median_ci <- function(x, R = 2000) {
  b <- boot(x, statistic = boot_median, R = R)
  ci <- boot.ci(b, type = "perc")$percent[c(4, 5)]
  
  tibble(
    median = median(x, na.rm = TRUE),
    CI_low = ci[1],
    CI_high = ci[2] )}

rarity %>%
  group_by(SPECIES_presence) %>%
  summarise(
    n = sum(!is.na(sp_relative_area)),
    median_ci(sp_relative_area),
    .groups = "drop")


##### Plot ###
perceivedVSactual <- ggplot(rarity, aes(x = SPECIES_presence, y = sp_relative_area)) +
  geom_boxplot(fill = "lightgrey", outlier.shape = NA) +
  geom_jitter(aes(color = SPECIES_sustainability), width = 0.2, height = 0) +
  scale_color_manual(
    values = c("Sustainable" = "#64A251", "Non durable" = "#D00F0F"),
    labels = c("Sustainable" = "Sustainable", "Non durable" = "Unsustainable")
  ) + 
  stat_pvalue_manual(
    rstatix::dunn_test(sp_relative_area ~ SPECIES_presence, p.adjust.method = "bonferroni", data=rarity),
    label = "p.adj.signif",
    y.position = 110, # adjust depending on your data range
    hide.ns = TRUE,
    step.increase = 0.05
  ) +
  labs(
    x = "Species perceived abundance category",
    y = "Species coverage per county (%)",
    color = "Perceived sustainability"
  ) +
  scale_x_discrete(labels = c(
    "Abondante large"   = "Widely abudant",
    "Abondante locale"       = "Locally abundant",
    "Rare" = "Rare",
    "Extremement rare"     = "Extremely rare",
    "JNSP"= "IDK"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# plot_zoom_png?width=750&height=487
png("plots/AppendixH_perceivedVSactual_abundance.png", 
    width = 2250,
    height = 1461,
    res = 300)
perceivedVSactual
dev.off()



#### APPENDIX I - Can species perceived sustainability be predicted from CSR strategy, life form, and distribution characteristics? ####
# ##### DFA ####
# # Variable réponse (binaire : "Sustainable" / "Non durable")
# data_dfa_durabilite_sp_bio <- all_data %>%
#   dplyr::select(id, SPECIES_name, CD_REF,
#                 SPECIES_sustainability, sp_relative_area, choix_type_bio) %>%
#   unique() %>%
#   na.omit() %>%
#   dplyr::select(-CD_REF, -id) %>%
#   droplevels() %>%
#   mutate(across(-c(SPECIES_sustainability, sp_relative_area), as.factor))
# 
# data_dfa_durabilite_sp_bio_summar <- data_dfa_durabilite_sp_bio %>%
#   group_by(SPECIES_name, SPECIES_sustainability) %>%
#   summarise(count=n(), choix_type_bio=first(choix_type_bio))
# 
# active_vars <- data_dfa_durabilite_sp_bio %>%
#   dplyr::select(-SPECIES_sustainability, -SPECIES_name)
# 
# grouping_factor <- data_dfa_durabilite_sp_bio$SPECIES_sustainability
# 
# 
# # --- Run DFA ---
# res.dfa <- discrimin(dudi.mix(active_vars, scannf = FALSE), 
#                      fac = grouping_factor, 
#                      scannf = FALSE)
# 
# # --- Visualise Results ---
# plot_data <- data.frame(
#   score_dfa = res.dfa$li$DS1,
#   group = grouping_factor
# )
# 
# ggplot(plot_data, aes(x = score_dfa, fill = group)) +
#   geom_density(alpha = 0.5) +
#   labs( title = "Species distribution by sustainability status",
#         x = "Discriminant Axis 1",
#         y = "Density",
#         fill = "Sustainability perception") +
#   theme_minimal() +
#   scale_fill_manual(values = c("Sustainable" = "#64A251", "Unsustainable" = "#D00F0F"))
# 
# # --- Get Contributions ---
# correlations <- as.data.frame(res.dfa$va)
# 
# sorted_correlations <- correlations[order(abs(correlations$CS1), decreasing = TRUE), , drop = FALSE]
# head(sorted_correlations, 10)
# 
# 
# # --- Cohen's d ---
# group_stats <- plot_data %>%
#   group_by(group) %>%
#   summarise(mean_score = mean(score_dfa),
#             sd_score = sd(score_dfa),
#             n = n())
# 
# mean_diff <- diff(group_stats$mean_score)
# pooled_sd <- sqrt(((group_stats$sd_score[1]^2)*(group_stats$n[1]-1) +
#                      (group_stats$sd_score[2]^2)*(group_stats$n[2]-1)) /
#                     (sum(group_stats$n)-2))
# cohens_d <- mean_diff / pooled_sd
# 
# cat("Cohen's d (DS1 separation):", round(cohens_d, 3), "\n")
# 
# # --- Classification accuracy ---
# # Prepare data for LDA (all predictors as factors)
# lda_data <- data_dfa_durabilite_sp_bio %>%
#   dplyr::select(-SPECIES_name)
# 
# lda_fit <- lda(SPECIES_sustainability ~ ., data = lda_data)
# pred <- predict(lda_fit)$class
# 
# table(Predicted = pred, Actual = lda_data$SPECIES_sustainability) # Confusion matix
# 
# cat("Classification accuracy:", round(mean(pred == lda_data$SPECIES_sustainability)*100, 1), "%\n")



##### GLM #####
# ggplot(data_dfa_durabilite_sp_bio, aes(x=(csr_simple))) +
#   geom_tern # grarphe CSR

# Prepare data
data_dfa_durabilite_sp_bio <- all_data %>%
  dplyr::select(id, SPECIES_name, CD_REF, SPECIES_sustainability, sp_relative_area, choix_type_bio) %>%
  unique() %>%
  na.omit() %>%
  dplyr::select(-CD_REF, -id) %>%
  mutate(
    # Now explicitly set reference level:
    SPECIES_sustainability = relevel(SPECIES_sustainability, ref = "Unsustainable"), # !! the model is predicting the log-odds of being Sustainable
    across(-c(sp_relative_area), as.factor)
  )

# Regression
glm_fit <- glm(SPECIES_sustainability ~ sp_relative_area + choix_type_bio,
               data = data_dfa_durabilite_sp_bio,
               family = binomial(link="logit"))
summary(glm_fit)

# Odds ratios and contributions
glm_res <- summary(glm_fit)$coefficients %>%
  as.data.frame() %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`)
  )

print(glm_res)
# write.csv(glm_res, "processed_data/AppendixI_glm_ecol_contrib.csv")

# Find optimal threshold to culculate model performance
pred_probs <- predict(glm_fit, newdata = data_dfa_durabilite_sp_bio, type = "response")

roc_obj <- roc( # use ROC+AUC
  data_dfa_durabilite_sp_bio$SPECIES_sustainability,
  pred_probs,
  levels = c("Unsustainable", "Sustainable"),
  direction = "<"
)

auc(roc_obj) # auc value

opt_coords <- coords(
  roc_obj,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)
opt_coords
opt_coords["threshold"]

# Evaluate model performance using optimal threshold
# Use the same factor levels and order as the actual data
pred_class <- ifelse(pred_probs > as.numeric(opt_coords["threshold"]), "Sustainable","Unsustainable")
pred_class <- factor(pred_class, levels = levels(data_dfa_durabilite_sp_bio$SPECIES_sustainability))

cm <- confusionMatrix(
  pred_class,
  data_dfa_durabilite_sp_bio$SPECIES_sustainability
)

# Accuracy
acc <- cm$overall['Accuracy']

# 95% Confidence Interval
acc_ci <- cm$overall[c('AccuracyLower', 'AccuracyUpper')]

# Print results
cat("Classification accuracy:", round(acc * 100, 1), "%\n")
cat("95% CI:", 
    paste0("(", round(acc_ci[1] * 100, 1), "%, ",
           round(acc_ci[2] * 100, 1), "%)\n"))



# ##### PCA #####
# data_acp <- all_data %>%
#   dplyr::select(id, SPECIES_name, CD_REF,
#                 SPECIES_sustainability, C, S, R) %>%
#   unique() %>%
#   na.omit() %>%
#   dplyr::select(-CD_REF, -id)
# 
# data_acp$SPECIES_sustainability <- as.factor(data_acp$SPECIES_sustainability)
# 
# pca_res <- prcomp(data_acp[, c("C", "S", "R")],
#                   center = FALSE,
#                   scale. = FALSE)
# 
# pca_scores <- as.data.frame(pca_res$x)
# 
# pca_scores$SPECIES_sustainability <- data_acp$SPECIES_sustainability
# pca_scores$SPECIES_name <- data_acp$SPECIES_name
# 
# 
# ggplot(pca_scores, aes(x = PC1, y = PC2, color = SPECIES_sustainability)) +
#   geom_point(size = 3, alpha = 0.8) +
#   theme_minimal() +
#   labs(title = "PCA on CSR traits",
#        x = "PC1",
#        y = "PC2",
#        color = "Durabilité")
# 
# 
# loadings <- as.data.frame(pca_res$rotation)
# loadings$var <- rownames(loadings)
# 
# ggplot(pca_scores, aes(PC1, PC2, color = SPECIES_sustainability)) +
#   geom_point(size = 3, alpha = 0.8) +
#   geom_segment(data = loadings,
#                aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
#                arrow = arrow(length = unit(0.2, "cm")),
#                color = "black") +
#   geom_text(data = loadings,
#             aes(x = PC1 * 3.2, y = PC2 * 3.2, label = var),
#             color = "black") +
#   theme_minimal()
# 
# summary(pca_res)
# pca_res$rotation
# 
# 
# ggplot(pca_scores, aes(x = SPECIES_sustainability, y = PC2, fill = SPECIES_sustainability)) +
#   geom_boxplot(alpha = 0.7) +
#   theme_minimal() +
#   labs(title = "PC2 by durability class",
#        y = "PC2 (Stress-tolerant ↔ Competitive/Ruderal)")
# 
# ggplot(pca_scores, aes(x = SPECIES_sustainability, y = PC1, fill = SPECIES_sustainability)) +
#   geom_boxplot(alpha = 0.7) +
#   theme_minimal() +
#   labs(title = "PC2 by durability class",
#        y = "PC2 (Stress-tolerant ↔ Competitive/Ruderal)")


####_________####
#### Figure 5 - Spatial distribution of ‘unsustainable’ mentions ####
#### Plot species maps per département ####
# Data is already processed in `df_all_species_data`, now join with `departements`
citations_by_species_massif <- citations_by_species_dpt %>%
  right_join(departements_simpl_PARIS, join_by("dpt"=="dpt_simple")) %>%
  group_by(name, massif) %>%
  summarise(Unsustainable=sum(Unsustainable),
            Sustainable=sum(Sustainable), 
            total_citations=sum(total_citations)) %>%
  ungroup() %>%
  na.omit()

plot_species_map_generic <- function(
    spatial_data,
    data,
    species_name = NULL,
    join_by_expr,
    value_unsustainable,
    value_sustainable,
    value_total,
    global_max = NULL,
    subtitle
) {
  
  filtered_data <- data %>%
    {
      if (!is.null(species_name)) {
        filter(., name == species_name)
      } else {
        .
      }
    }
  
  map_data <- spatial_data %>%
    left_join(filtered_data, by = join_by_expr) %>%
    st_as_sf() %>%
    mutate(
      prop_unsustainable = ifelse(
        {{ value_total }} > 0,
        round({{ value_unsustainable }} / {{ value_total }} * 100, 2),
        NA_real_
      )
    )
  
  centroids <- map_data %>%
    mutate(geometry = st_centroid(geometry))
  
  p <- ggplot(map_data) +
    geom_sf(
      aes(fill = prop_unsustainable),
      colour = "white",
      linewidth = 0.2
    ) +
  
    scale_fill_stepsn(
      colours = c("#6D9E90", "#F7DC58", "#F39A43", "#900202"),
      breaks = c(0, 25, 50, 75, 100),
      limits = c(0, 100),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      na.value = "grey80",
      name = "'Unsustainable' mentions"
    ) +
    
    geom_sf(
      data = centroids,
      aes(size = {{ value_total }}),
      shape = 21,
      fill = "black",
      colour = "white",
      alpha = 0.42,
      stroke = 0.3
    ) +
    theme_void(base_size = 14) +
    theme(
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 13, face="italic"),
      plot.margin = margin(t = 30)
    )
  
  if (!is.null(species_name) && !is.null(global_max)) {
    p <- p +
      scale_size_continuous(
        limits = c(1, global_max),
        range = c(1, 10),
        guide = guide_legend(title = "Total mentions")
      )
  } else {
    p <- p +
      scale_size_continuous(
        range = c(1, 10),
        guide = guide_legend(title = "Total mentions")
      )
  }
  
  p +
    labs(
      title = ifelse(is.null(species_name), "", species_name))
}


plot_species_map_dpt <- function(species_name) {
  
  global_max <- max(
    citations_by_species_dpt$total_citations,
    na.rm = TRUE
  )
  
  plot_species_map_generic(
    spatial_data = departements_simpl_PARIS,
    data = citations_by_species_dpt,
    species_name = species_name,
    join_by_expr = c("dpt_simple" = "dpt"),
    value_unsustainable = Unsustainable,
    value_sustainable = Sustainable,
    value_total = total_citations,
    global_max = global_max )
}




plot_species_map_massif <- function(species_name) {
  
  spatial_massif <- departements_simpl_PARIS %>%
    group_by(massif) %>%
    summarise() %>%
    ungroup()
  
  global_max <- max(
    citations_by_species_massif$total_citations,
    na.rm = TRUE
  )
  
  plot_species_map_generic(
    spatial_data = spatial_massif,
    data = citations_by_species_massif,
    species_name = species_name,
    join_by_expr = "massif",
    value_unsustainable = Unsustainable,
    value_sustainable = Sustainable,
    value_total = total_citations,
    global_max = global_max)
}

species_maps_dpt <- plot_species_map_dpt("Allium ursinum") + plot_species_map_dpt("Filipendula ulmaria") + plot_species_map_dpt("Vaccinium myrtillus") +  plot_species_map_dpt("Hypericum nummularium") + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") & theme(legend.position = "right", legend.box.margin = margin(l = 50))
species_maps_dpt 

# plot_zoom_png?width=905&height=697
png("plots/Figure_5_species_maps_dpt.png", 
    width = 2715,
    height = 2091,
    res = 300)
species_maps_dpt
dev.off()

species_maps_massif <- plot_species_map_massif("Allium ursinum") + plot_species_map_massif("Filipendula ulmaria") + plot_species_map_massif("Vaccinium myrtillus") +  plot_species_map_massif("Hypericum nummularium") + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") & theme(legend.position = "right", legend.box.margin = margin(l = 50))
species_maps_massif

# # plot_zoom_png?width=905&height=697
# png("plots/Figure_4_species_maps_massif.png", 
#     width = 2715,
#     height = 2091,
#     res = 300)
# species_maps_massif
# dev.off()



#### APPENDIX J - Unsustainable mentions across harvesting areas ####
plot_map_France_dpt <- function() {

  plot_species_map_generic(
    spatial_data = departements_simpl_PARIS,
    data = citations_by_species_dpt %>% group_by(dpt) %>% summarise(
      Unsustainable=sum(Unsustainable),
      Sustainable=sum(Sustainable),
      total_citations=sum(total_citations)) %>% ungroup(),
    species_name = NULL,
    join_by_expr =  c("dpt_simple" = "dpt"),
    value_unsustainable = Unsustainable,
    value_sustainable = Sustainable,
    value_total = total_citations,
    global_max = NULL,
    subtitle = "by departement"
  )
}

plot_map_France_massif <- function() {
  
  spatial_massif <- departements_simpl_PARIS %>%
    group_by(massif) %>%
    summarise() %>%
    ungroup()
  
  plot_species_map_generic(
    spatial_data = spatial_massif,
    data = citations_by_species_massif %>% group_by(massif) %>% summarise(
      Unsustainable=sum(Unsustainable),
      Sustainable=sum(Sustainable),
      total_citations=sum(total_citations)) %>% ungroup(),
    species_name = NULL,
    join_by_expr = "massif",
    value_unsustainable = Unsustainable,
    value_sustainable = Sustainable,
    value_total = total_citations,
    global_max = NULL,
    subtitle = "by harvesting area"
  )
}


plot_map_France_dpt() + plot_map_France_massif()

# plot_zoom_png?width=728&height=497
png("plots/AppendixJ_all_France_species_maps.png", 
    width = 2184,
    height = 1491,
    res = 300)
plot_map_France_massif()
dev.off()


#### SD of unsustainable mention ratio across harvesting areas ####
national_summary <- citations_by_species_massif %>%
  group_by(massif) %>%
  summarise(
    ratio_unsustainable = 100 * sum(Unsustainable) / sum(total_citations),
    .groups = "drop"
  )

with(national_summary, {
  m <- mean(ratio_unsustainable)
  se <- sd(ratio_unsustainable) / sqrt(length(ratio_unsustainable))
  c(mean = m, ci_lower = m - 1.96 * se, ci_upper = m + 1.96 * se)})
# mean ci_lower ci_upper 
# 30.72300 25.75785 35.68815 


#### APPENDIX K - Effect of departements on harvesting sustainablity ####
# citations_by_species_dpt$response <- with(citations_by_species_dpt, cbind(Unsustainable, Sustainable))
citations_by_species_dpt$ratio <- round(citations_by_species_dpt$Unsustainable/
                                          citations_by_species_dpt$total_citations,2)


# Fit models for each species
models <- lapply(unique(citations_by_species_dpt$name), function(sp) {
  df_sp <- filter(citations_by_species_dpt, name == sp)
  all_constant <- all(df_sp$Unsustainable == 0 | df_sp$Sustainable == 0)
  
  # Check why we would skip
  if(length(unique(df_sp$dpt)) < 2) {
    return(list(skipped = TRUE, model = NULL, singular = NA, reason_skipped = "only 1 department"))
  }
  if(all_constant) {
    return(list(skipped = TRUE, model = NULL, singular = NA, reason_skipped = "constant response"))
  }
  
  # Fit GLMM
  model <- glmer(cbind(Unsustainable, Sustainable) ~ 1 + (1 | dpt),
                 data = df_sp, family = binomial)
  
  # Check if model is singular
  singular_flag <- isSingular(model)
  
  list(skipped = FALSE, model = model, singular = singular_flag, reason_skipped = NA)
})
names(models) <- unique(citations_by_species_dpt$name)

# Build national-level summary
summary_species <- data.frame(
  species_name = character(),
  observed_ratio = numeric(),
  pred_prob = numeric(),
  pred_prob_logit = numeric(),
  sd_dpt = numeric(),
  MOR_dpt = numeric(),    # median odds ratio
  CI_lower = numeric(),   # IC 95% basé sur SD du random effect
  CI_upper = numeric(),
  skipped = logical(),
  # singular = logical(),
  reason_skipped = character(),
  stringsAsFactors = FALSE
)

for(sp in names(models)) {
  df_sp <- filter(citations_by_species_dpt, name == sp)
  
  # Observed ratio (national)
  total_non <- sum(df_sp$Unsustainable)
  total_all <- sum(df_sp$Unsustainable + df_sp$Sustainable)
  obs_ratio <- ifelse(total_all > 0, total_non / total_all, NA)
  
  if(models[[sp]]$skipped) {
    pred_prob <- NA
    pred_prob_logit <- NA
    sd_dpt <- NA
    MOR_dpt <- NA
    CI_lower <- NA
    CI_upper <- NA
    flagged <- TRUE
    # singular_flag <- NA
    reason <- models[[sp]]$reason_skipped
  } else {
    model <- models[[sp]]$model
    
    # Probabilité nationale
    pred_prob_logit <- fixef(model)        # logit
    pred_prob <- plogis(pred_prob_logit)   # inverse logit
    
    # Variabilité départementale
    var_dpt <- as.numeric(VarCorr(model)$dpt[1])
    sd_dpt <- sqrt(var_dpt)
    
    # MOR
    MOR_dpt <- exp(sqrt(2 * var_dpt) * 0.6745)
    
    # IC 95 % basé sur SD des départements
    CI_lower <- plogis(pred_prob_logit - 1.96 * sd_dpt)
    CI_upper <- plogis(pred_prob_logit + 1.96 * sd_dpt)
    
    flagged <- FALSE
    singular_flag <- models[[sp]]$singular
    reason <- NA
  }
  
  summary_species <- rbind(summary_species,
                           data.frame(
                             species_name = sp,
                             observed_ratio = obs_ratio,
                             pred_prob = pred_prob,
                             pred_prob_logit = pred_prob_logit,
                             sd_dpt = sd_dpt,
                             MOR_dpt = MOR_dpt,
                             CI_lower = CI_lower,
                             CI_upper = CI_upper,
                             skipped = flagged,
                             # singular = singular_flag,
                             reason_skipped = reason,
                             stringsAsFactors = FALSE
                           ))
}

summary_species
# summary(models$`Allium ursinum`$model)

write.csv(summary_species, "processed_data/AppendixK_MOR_calculations.csv", row.names=F)

####_________####
#### Figure 6 - Are the assumptions of the respondents concerning species regulations correct ? #####
BDC_STATUTS_17 <- read_csv("raw_data/BDC-Statuts-v17/BDC_STATUTS_17.csv") %>%
  # filter(REGROUPEMENT_TYPE == "Protection" | CD_TYPE_STATUT %in% c("REGL", "REGLSO")) %>% 
  filter(REGROUPEMENT_TYPE == "Protection" | CD_TYPE_STATUT %in% c("REGL")) %>% 
  mutate(
    LB_ADM_TR_simpl =  LB_ADM_TR,
    # Replace "-" and "'" with space
    LB_ADM_TR_simpl = str_replace_all(LB_ADM_TR_simpl, "[-']", " "),
    # Remove accents
    LB_ADM_TR_simpl = iconv(LB_ADM_TR_simpl, to = "ASCII//TRANSLIT")
  ) %>%
  dplyr::select(CD_REF, REGROUPEMENT_TYPE, LB_TYPE_STATUT, NIVEAU_ADMIN, LB_ADM_TR_simpl, LABEL_STATUT, FULL_CITATION, DOC_URL)

# Species with at least 1 regul or protection
BDC_STATUTS_17_summar <- BDC_STATUTS_17 %>%
  dplyr::select(CD_REF) %>%
  unique() %>%
  mutate(national_status = "At least 1 regulation or protection")


# Join survey data
data_regl_prot <- all_data %>%
  dplyr::select(id, SPECIES_name, CD_REF, SPECIES_species_regulation, SPECIES_regulation_adapted, SPECIES_dpt) %>%
  unique() %>%
  left_join(departements %>% dplyr::select(dpt_simple, region_simple), by=join_by("SPECIES_dpt"=="dpt_simple")) %>%
  dplyr::select(-geometry)

# Separate different levels of protections/regulations
bdc_dpt <- data_regl_prot %>%
  # Join department-specific regulations
  left_join(BDC_STATUTS_17 %>% filter(NIVEAU_ADMIN == "Département"), 
            by = join_by("CD_REF"=="CD_REF", "SPECIES_dpt"=="LB_ADM_TR_simpl"))

bdc_region <- data_regl_prot %>%
  # Join department-specific regulations
  left_join(BDC_STATUTS_17 %>% filter(NIVEAU_ADMIN %in% c("Ancienne région", "Région")), 
            by = join_by("CD_REF"=="CD_REF", "region_simple"=="LB_ADM_TR_simpl"))


bdc_national <- data_regl_prot %>%
  # Join department-specific regulations
  left_join(BDC_STATUTS_17 %>% filter(NIVEAU_ADMIN %in% c("État", "Territoire")), 
            by = "CD_REF") %>%
  dplyr::select(-LB_ADM_TR_simpl)




data_regl_prot_join <- rbind(bdc_dpt, bdc_region) %>%
  rbind(bdc_national) %>%
  dplyr::select(-LABEL_STATUT) %>% # many regulations are cited more than once for a same species
  mutate(
    statut_group = case_when(
      LB_TYPE_STATUT %in% c("Réglementation", "Réglementation sans objet") ~ "Regulation",
      str_detect(LB_TYPE_STATUT, "départementale") ~ "Department-level protection",
      str_detect(LB_TYPE_STATUT, "nationale") ~ "National-level protection",
      str_detect(LB_TYPE_STATUT, "régionale") ~ "Regional-level protection",
      
      TRUE ~ LB_TYPE_STATUT
    )
  ) %>%
  group_by(CD_REF) %>%
  filter(if (any(!is.na(REGROUPEMENT_TYPE))) !is.na(REGROUPEMENT_TYPE) else TRUE) %>%
  ungroup() %>%
  mutate(
    statut_group = replace_na(statut_group, "No regulation/protection"),
    statut_group = factor(
      statut_group,
      levels = c(
        "No regulation/protection",
        "National-level protection",
        "Regional-level protection",
        "Department-level protection",
        "Regulation"
      )
    )
  ) %>%
  filter(!is.na(SPECIES_species_regulation))%>%
  mutate(
    SPECIES_species_regulation = case_when(
      str_detect(tolower(SPECIES_species_regulation), "yes") ~ "Yes",
      str_detect(tolower(SPECIES_species_regulation), "no") ~ "No",
      TRUE ~ SPECIES_species_regulation
    )
  ) %>%
  unique() %>%
  left_join(BDC_STATUTS_17_summar)


data_regl_prot_summar <- data_regl_prot_join %>%
  # Calculate % by ESPECE_espece_reglem nad statut_group
  group_by(SPECIES_species_regulation, statut_group) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(SPECIES_species_regulation) %>%
  mutate(pct = n / sum(n) * 100)

data_regl_prot_summar2 <- data_regl_prot_join %>%
  # Calculate % of each answer
  group_by(SPECIES_species_regulation) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)


# Define the colors you want for each category
colours_regul <- c(
  "No regulation/protection" = "#B3B3B3",
  "National-level protection" = "#54278f",
  "Regional-level protection" = "#756bb1",
  "Department-level protection" = "#9e9ac8",
  "Regulation" = "#4C81A0"
)

# Bar plot
pct_regul_answers <- ggplot(data_regl_prot_summar, aes(x = SPECIES_species_regulation, y = pct, fill = statut_group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Is the species regulated or protected ?", y = "Percentage of respondents", fill = "Status") +
  scale_fill_manual(values = colours_regul) +  # manually set colors
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4)) 

pct_regul_answers

count_regul_answers <- ggplot(data_regl_prot_join, aes(x = SPECIES_species_regulation, fill = statut_group)) +
  geom_bar(position = "stack") +
  labs(x = "Is the species regulated or protected ?", y = "Number of respondents", fill = "Status") +
  scale_fill_manual(values = colours_regul) +  # manually set colors
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4)) 

count_regul_answers

# % réponses correctes / incorrectes
data_regl_eval <- data_regl_prot_join %>%
  mutate(
    correct_answer = case_when(
      grepl("Yes", SPECIES_species_regulation) & statut_group != "No regulation/protection" ~ "Correct",
      grepl("No", SPECIES_species_regulation) & statut_group == "No regulation/protection" ~ "Correct",
      TRUE ~ "Incorrect"
    )
  )

# Résumé en pourcentage
data_regl_eval_summar <- data_regl_eval %>%
  group_by(SPECIES_species_regulation, correct_answer, statut_group) %>%
  summarise(n = n(), .groups = "drop")

data_regl_eval_summar <- data_regl_eval %>%
  group_by(SPECIES_species_regulation, statut_group, correct_answer) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(SPECIES_species_regulation) %>%
  mutate(pct = n / sum(n) * 100)

# Graph
pct_correct_regul <- ggplot(data_regl_eval_summar, aes(x = SPECIES_species_regulation, y = pct, fill = correct_answer)) +
  geom_bar(stat = "identity") +
  labs(x = "Is the species regulated or protected ?", y = "Percentage of respondents", fill = "Answer evaluation") +
  scale_fill_manual(values=c("Incorrect"="#D00F0F", "Correct"="#64A251")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4)) 


nb_correct_regul <- ggplot(data_regl_eval_summar, aes(x = SPECIES_species_regulation, y = n, fill = correct_answer)) +
  geom_bar(stat = "identity") +
  labs(x = "Is the species regulated or protected ?", y = "Number of respondents", fill = "Answer evaluation") +
  scale_fill_manual(values=c("Incorrect"="#D00F0F", "Correct"="#64A251")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right",
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4)) 



# Add annotations only to the top plots
regulation_plot <- count_regul_answers + pct_correct_regul + plot_annotation(tag_levels = "a")

regulation_plot

# plot_zoom_png?width=982&height=515
png("plots/Figure_6_regulations.png", 
    width = 2946,     # pixels
    height = 1143,   # pixels
    res = 300)        # resolution in dpi
regulation_plot
dev.off()

# Find if the species indicated as having a regulation when they don't, are regulated elsewhere
df <- data_regl_prot_join %>%
  subset(SPECIES_species_regulation=="Yes" & statut_group=="No regulation/protection",
         select=c(SPECIES_species_regulation, statut_group, national_status)) %>%
  group_by(national_status) %>%
  summarise(n=n())


#### Does the profile of respondents determine their likelihood of correctly reporting species regulations? ####
# Create a new column by pasting col1 and col2 together
data_regl_eval$reglem_glm <- paste(data_regl_eval$SPECIES_species_regulation, data_regl_eval$correct_answer, sep = "_")

data_regl_eval <- data_regl_eval %>%
  left_join(dplyr::select(all_data_profile_clusters, id, Group)%>%unique())

data_regl_eval$correct_answer <- ifelse(data_regl_eval$correct_answer == "Correct", 1, 0)

glm_fit <- glmer(correct_answer ~ Group +(1|SPECIES_name),
               data = data_regl_eval,
               family = binomial(link="logit"))
summary(glm_fit)

# Odds ratios and contributions
summary(glm_fit)$coefficients %>%
  as.data.frame() %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`)
  ) %>%
  print()


#### % of unsustainable species that are regulated/unregulated ####
sust_regul_status <- data_regl_prot_join %>%
  dplyr::select(id, CD_REF, SPECIES_name, SPECIES_species_regulation, statut_group) %>%
  left_join(dplyr::select(all_data, id, CD_REF, SPECIES_sustainability)) %>%
  unique() %>%
  group_by(SPECIES_sustainability, statut_group) %>%
  summarise(n_species = n(), .groups = "drop")


result <- sust_regul_status %>%
  dplyr::filter(SPECIES_sustainability == "Unsustainable") %>%
  dplyr::summarise(
    total_unsustainable = sum(n_species),
    unsustainable_unregulated = sum(
      n_species[statut_group == "No regulation/protection"],
      na.rm = TRUE
    ),
    percent = 100 * unsustainable_unregulated / total_unsustainable
  )

sust_regul_species2 <- data_regl_prot_join %>%
  dplyr::select(id, CD_REF, SPECIES_name, SPECIES_species_regulation, statut_group) %>%
  left_join(dplyr::select(all_data, id, CD_REF, SPECIES_sustainability)) %>%
  distinct()


unprot <- sust_regul_species2 %>%
  dplyr::filter(
    statut_group == "No regulation/protection"
  )

unsust_ratio <- unprot %>%
  dplyr::group_by(CD_REF, SPECIES_name) %>%
  dplyr::summarise(
    total_citations = n(),
    unsustainable_citations = sum(SPECIES_sustainability == "Unsustainable", na.rm = TRUE),
    ratio_unsustainable = unsustainable_citations / total_citations,
    .groups = "drop"
  )

unsust_ratio %>%
  dplyr::arrange(desc(ratio_unsustainable))