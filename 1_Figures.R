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
raw_data <- read.csv("raw_data/results-survey676868_prefilter.csv")

massifs <- read.csv("raw_data/massifs_cueillette.csv")
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

#### Visualise harvesting areas ####
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


plot_massifs <- ggplot() +
  geom_sf(data = departements_simpl_PARIS, 
          aes(fill=massif), colour="white")+
  ggtitle(label="Massifs de cueillette") +
  scale_fill_manual(values=
                      # c("#004949","#009292","#490092",
                      # "#ff6db6","#006ddb","#b66dff","#6db6ff","#b6dbff",
                      # "#920000","#db6d00","#ffff6d")
                      c(
                        "#003f5c",  # deep midnight blue
                        "#2f4b7c",  # muted violet
                        "#665191",  # slate purple
                        "#a05195",  # mauve‑magenta
                        "#d45087",  # rose‑dust
                        "#f95d6a",  # coral
                        "#ff7c43",  # burnt orange
                        "#ffa600",  # warm gold
                        "#7ac667",  # soft green
                        "#4ac3a7",  # teal‑cyan
                        "#00bfae"),   # bright teal)
  ) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

plot_massifs


# plot_zoom_png?width=777&height=640
png("plots/massifs_cueillette.png", 
    width = 2331,     # pixels
    height = 1920,   # pixels
    res = 300)        # resolution in dpi
plot_massifs
dev.off()



####_________####
#### Prepare main data frame ####

# A single, comprehensive data processing pipeline to avoid redundancy

process_data_helper_1 <- function(data, pattern_one, pattern_two, value_col_name) {
  data %>%
    dplyr::select(id = id__ID_de_la_reponse, 
                  matches(paste0("^G\\d+Q0*", c(pattern_one, pattern_two), "_", collapse = "|"))) %>%
    mutate(across(-id, as.character)) %>%
    pivot_longer(
      cols = -id,
      names_to = "column_name",
      values_to = "value",
      values_drop_na = TRUE) %>%
    
    filter(value == "Oui") %>%
    
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
    dplyr::select(id = id__ID_de_la_reponse, 
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
      
      type = ifelse(!is.na(type) & grepl("Autre", column_name), paste0(type, "_autre"), type),
      
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
    dplyr::select(id__ID_de_la_reponse,
                  matches("^G(?:3|4|6|8|10|12|14|16|18|20)Q000(?:0)?(?:1|2|4|5|6|7|8|9|10|11|12|13|14|18|19|20|21|22|23|24)_"),
                  # careful of the number of zeroes after Q
                  starts_with("GsuppQsupp_")) # GsuppQsupp__Connaissez_vous_une_espece_a_la_cueillette_non_durable
  
  df_long <- data %>%
    # Select all necessary columns at once
    dplyr::select(id = id__ID_de_la_reponse,
                  non_durable_connue = starts_with("GsuppQsupp_"),
                  colnames(species_cols)) %>%
    
    # Pivot to long format for easier manipulation
    pivot_longer(
      cols = -c(id, non_durable_connue),
      names_to = c("group", "qcode"),
      names_pattern = "^G(\\d+)Q0{3,4}(\\d+)_", # careful of the number of zeroes after Q
      values_to = "value",
      values_drop_na = TRUE,
      values_transform = list(value = as.character) # because of the numeric (date columns)
    ) %>%
    
    # Classify values by type (species, presence, etendue_cueill, durability)
    mutate(
      type = case_when(
        qcode == "1" ~ "nom",
        group %in% c("3", "4") & qcode == "2" ~ "dpt",
        !(group %in% c("3", "4")) & qcode == "4" ~ "dpt",
        group %in% c("3", "4") & qcode == "4" ~ "presence",
        !(group %in% c("3", "4")) & qcode == "6" ~ "presence",
        group %in% c("3", "4") & qcode == "5" ~ "etendue_cueill",
        !(group %in% c("3", "4")) & qcode == "7" ~ "etendue_cueill",
        group %in% c("3", "4") & qcode == "10" ~ "debut_obs",
        !(group %in% c("3", "4")) & qcode == "12" ~ "debut_obs",
        group %in% c("3", "4") & qcode == "11" ~ "etat_ressource",
        !(group %in% c("3", "4")) & qcode == "13" ~ "etat_ressource",
        group %in% c("3", "4") & qcode == "12" ~ "variation_prelev",
        !(group %in% c("3", "4")) & qcode == "14" ~ "variation_prelev",
        group %in% c("3", "4") & qcode == "13" ~ "intensite_prelev",
        !(group %in% c("3", "4")) & qcode == "15" ~ "intensite_prelev",
        group %in% c("3", "4") & qcode == "18" ~ "mode",
        !(group %in% c("3", "4")) & qcode == "20" ~ "mode",
        group %in% c("3", "4") & qcode == "19" ~ "mode_risque",
        !(group %in% c("3", "4")) & qcode == "21" ~ "mode_risque",
        group %in% c("3", "4") & qcode == "20" ~ "espece_reglem",
        !(group %in% c("3", "4")) & qcode == "22" ~ "espece_reglem",
        group %in% c("3", "4") & qcode == "21" ~ "reglem_adaptee",
        !(group %in% c("3", "4")) & qcode == "23" ~ "reglem_adaptee")
    ) %>%
    
    # # Determine durability status based on group
    dplyr::group_by(id, group) %>%
    dplyr::mutate(
      rep_durable = dplyr::first(value[qcode == "2"]),
      durabilite = dplyr::case_when(
        group == "3" ~ "Durable",
        group == "4" ~ "Non durable",
        rep_durable == "Oui" ~ "Durable",
        TRUE ~ "Non durable"
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
    separate_rows(nom, sep = ",\\s*") %>%
    mutate(
      nom = str_trim(nom),
      dpt = str_trim(str_remove(dpt, "\\s*\\([0-9AB]+\\)"))
    ) %>%
    
    unnest(everything()) %>%
    
    # Filter out empty species
    filter(nom != "") %>%
    
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
  df_parties_cueill <- process_data_helper_1(species_cols, "6", "8", "parties_cueill")
  df_usages <- process_data_helper_1(species_cols, "7", "9", "usages")
  df_risque <- process_data_helper_1(species_cols, "17", "19", "risque")
  
  
  # Use the helper function 2 to process type of harvesting practise, company type...
  df_type_cueillette <- process_data_helper_2(species_cols, "8", "10", "type_cueillette")
  df_type_entrep <- process_data_helper_2(species_cols, "9", "11", "type_entrep")
  df_cause_reglem_inadaptee <- process_data_helper_2(species_cols, "22", "24", "cause_reglem_inadaptee")
  
  
  # Join the two data frames
  df_final <- left_join(df_long, df_parties_cueill, by = c("id", "group")) %>%
    left_join(df_usages, by = c("id", "group")) %>%
    left_join(df_risque, by = c("id", "group")) %>%
    left_join(df_type_cueillette, by = c("id", "group")) %>%
    left_join(df_type_entrep, by = c("id", "group")) %>%
    left_join(df_cause_reglem_inadaptee, by = c("id", "group")) %>%
    filter(nom != "")
  
  
  # Some participants have wrongly informed that they didn't know of any species harvested unsustainably, so we'll correct this:
  for (i in df_final$id) {
    if ("Non durable" %in% df_final$durabilite[df_final$id==i]) {
      df_final$non_durable_connue[df_final$id==i] = "Oui"
    }
  }
  
  return(df_final)
}

df_all_species_data <-  process_all_species_data(raw_data)




process_all_profile_data <- function(data) {
  # Columns related to species, departments, and harvester type
  species_cols <- data %>%
    dplyr::select(id__ID_de_la_reponse,
                  matches("G21")) # careful of the number of zeroes after Q
  
  df_long <- data %>%
    # Select all necessary columns at once
    dplyr::select(id = id__ID_de_la_reponse,
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
        qcode == "1" ~ "genre",
        qcode == "2" ~ "age",
        qcode == "4" ~ "etudes_evt",
        qcode == "6" ~ "cueilleur",
        qcode == "7" ~ "debut_cueill",
        qcode == "8" ~ "nb_esp_cueill",
        qcode == "9" ~ "statut_cueilleur",
        qcode == "10" ~ "statut_cueilleur_entrep",
        qcode == "11" ~ "groupe_cueill",
        qcode == "12" ~ "nom_groupe_cueill",
        qcode == "14" ~ "nom_orga_rattach",
        qcode == "15" ~ "poste_orga_rattach",
        qcode == "16" ~ "taille_orga_rattach",
        qcode == "17" ~ "nb_esp_orga_rattach")
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
  df_niveau_educ <- process_data_helper_2(species_cols, "3", "3", "niveau_educ")
  df_categ_socio_pro <- process_data_helper_2(species_cols, "5", "5", "categ_socio_pro")
  df_type_orga_rattach <- process_data_helper_2(species_cols, "13", "13", "type_orga_rattach")
  
  # Join the two data frames
  df_final <- left_join(df_long, df_niveau_educ, by = c("id", "group")) %>%
    left_join(df_categ_socio_pro, by = c("id", "group")) %>%
    left_join(df_type_orga_rattach, by = c("id", "group")) %>%
    
    dplyr::select(-group) 
  
  return(df_final)
}

df_all_profile_data <-  process_all_profile_data(raw_data)


##### Join species and profile data ####
# Add prefixes to columns (except id)
df_profile_renamed <- df_all_profile_data %>%
  rename_with(~ paste0("PROFIL_", .), -id)

df_species_renamed <- df_all_species_data %>%
  rename_with(~ paste0("ESPECE_", .), -id)

# Full join
all_data <- df_profile_renamed %>%
  full_join(df_species_renamed, by = "id") %>%
  
  # Simplify categories
  mutate(
    ESPECE_usages = case_when(
      str_detect(ESPECE_usages, "Alimentaire") ~ "Alimentaire",
      str_detect(ESPECE_usages, "Cosmetique")  ~ "Cosmetique",
      str_detect(ESPECE_usages, "Artisanat")   ~ "Artisanat",
      str_detect(ESPECE_usages, "ornemental")  ~ "Ornemental",
      str_detect(ESPECE_usages, "Medical")     ~ "Medical",
      TRUE ~ ESPECE_usages
    ),
    ESPECE_parties_cueill = case_when(
      str_detect(ESPECE_parties_cueill, "aerienne")    ~ "Partie aerienne",
      str_detect(ESPECE_parties_cueill, "souterraine") ~ "Partie souterraine",
      TRUE ~ ESPECE_parties_cueill
    ),
    ESPECE_mode = dplyr::recode(ESPECE_mode, "Oui" = "Mode", "Non" = "Pas de mode"),
    ESPECE_mode_risque = dplyr::recode(ESPECE_mode_risque, "Oui" = "Mode et risque", "Non" = "Mode sans risque"),
    PROFIL_statut_cueilleur = case_when(
      str_detect(PROFIL_statut_cueilleur, "loisir")  ~ "Amateur",
      str_detect(PROFIL_statut_cueilleur, "revenus") ~ "Professionnel",
      TRUE ~ PROFIL_statut_cueilleur
    ),
    # Replace NA in PROFIL_nb_esp_cueill with 0 
    PROFIL_nb_esp_cueill = if_else(is.na(PROFIL_nb_esp_cueill), "0", PROFIL_nb_esp_cueill)) %>%
  
  # Pattern-based recoding (variable/stable/etc.)
  mutate(across(-id, ~ case_when(
    str_detect(., "variable") ~ "Variable",
    str_detect(., "stable")   ~ "Stable",
    str_detect(., "regress")  ~ "Diminue",
    str_detect(., "augment") ~ "Augmente",
    TRUE ~ .
  ))) %>%
  
  # Merge ESPECE_mode and risk
  mutate(
    ESPECE_mode = if_else(ESPECE_mode == "Mode", ESPECE_mode_risque, ESPECE_mode),
    PROFIL_statut_cueilleur = if_else(PROFIL_cueilleur == "Oui", PROFIL_statut_cueilleur, "Non cueilleur")
  ) %>%
  dplyr::select(-c(ESPECE_mode_risque, PROFIL_cueilleur)) %>%
  
  # Text cleaning
  mutate(across(everything(), ~ .x %>%
                  str_replace("Je ne sais pas", "JNSP") %>%
                  str_remove("\\(.*\\)"))) %>%
  
  # Presence recoding
  mutate(
    ESPECE_presence = dplyr::recode(ESPECE_presence,
                                    "Abondante et largement distribuee" = "Abondante large",
                                    "Abondante sur des zones localisees" = "Abondante locale",
                                    .default = ESPECE_presence
    ),
    PROFIL_type_orga_rattach = case_when(
      str_detect(PROFIL_type_orga_rattach, "Aucune") ~ "Aucune",
      str_detect(PROFIL_type_orga_rattach, "plantes sauvages") ~ "Entreprise plantes sauvages",
      TRUE ~ PROFIL_type_orga_rattach
    )
  ) %>%
  
  # Group harvested parts
  mutate(ESPECE_parties_cueill_grp = case_when(
    ESPECE_parties_cueill %in% c("Feuilles", "Bourgeons", "Partie aerienne", "Jeunes pousses") ~ "Partie_aerienne",
    ESPECE_parties_cueill %in% c("Fleurs", "Fruits", "Graines") ~ "Partie_reproductive",
    ESPECE_parties_cueill %in% c("Ecorce", "Seve") ~ "Autre",
    ESPECE_parties_cueill == "Plante entiere" ~ "Plante_entiere",
    ESPECE_parties_cueill == "Partie souterraine" ~ "Partie_souterraine")) %>%
  dplyr::select(-ESPECE_parties_cueill) %>%
  unique() %>%
  
  # Wide format: harvested parts
  mutate(presence = "Oui") %>%
  pivot_wider(
    names_from = ESPECE_parties_cueill_grp,
    values_from = presence,
    names_prefix = "ESPECE_parties_cueill_",
    values_fill = "Non"
  ) %>%
  dplyr::select(-matches("ESPECE_parties_cueill_NA$")) %>%
  
  # Wide format: usages
  mutate(presence = "Oui") %>%
  pivot_wider(
    names_from = ESPECE_usages,
    values_from = presence,
    names_prefix = "ESPECE_usages_",
    values_fill = "Non"
  ) %>%
  dplyr::select(-matches("ESPECE_usages_NA$")) %>%
  
  # Add taxref, Raunkiaer, CSR, species presence (cover %) per département, species status (native...)
  left_join(taxref, by = c("ESPECE_nom" = "LB_NOM")) %>%
  left_join(raunkieaer, by = "CD_REF") %>%
  left_join(csr, by = "CD_REF") %>%
  left_join(all_rarity, by = join_by("CD_REF", "ESPECE_dpt"=="dpt_simple")) %>%
  left_join(vascular) %>%
  left_join(departements_simpl_PARIS %>% dplyr::select(dpt_simple, massif), join_by("ESPECE_dpt"=="dpt_simple")) %>%
  dplyr::select(-dpt_name, -geometry) %>%
  
  # Convert to factors (excluding key cols)
  mutate(across(-c(id, ESPECE_nom, CD_REF, C, S, R, sp_relative_area, sp_area, sp_area_FR), as.factor))


####_________####
#### % of species identified at the genus level and not-plant species ####
corresp_species <- read.csv("raw_data/corresp_species.csv")
all_data_corresp <- all_data %>%
  left_join(corresp_species %>% 
              dplyr::select(ESPECE_nom, Commentaire),
            by="ESPECE_nom") 

number_non_plant <- all_data_corresp[all_data_corresp$Commentaire %in% c("Champignon", "Algue", "Lichen"),]
number_non_plant
number_genus <- all_data_corresp[is.na(all_data_corresp$CD_REF) &
                                   !(all_data_corresp$Commentaire %in% c("Champignon", "Algue", "Lichen")),]
number_genus
# non plants : 27/1117=2.4%
# genus level : 41/1117=3.7%

length(unique(number_non_plant$id)) # 27 non plant respondents
length(unique(number_genus$id)) # 40 respondents giving species genera only
length(unique(all_data_corresp$id)) # 536 total respondents

# remove keep mushrooms, or species identified at the genus level for the rest of the analysis
all_data <- all_data %>%
  filter(!is.na(CD_REF)) # 469 total respondents

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
nrow(unique(subset(in_common, in_survey=="yes" & first_paper=="yes", select=ESPECE_nom)))
unique(subset(in_common, in_survey=="yes" & first_paper=="yes", select=ESPECE_nom))
# 109 species in common
nrow(unique(subset(in_common, in_survey=="yes" &  !is.na(CD_REF), select=ESPECE_nom)))
# 146 species cited (excluding mushrooms/lichen/algue and species identified at the genus level)
# 109/146=74.7% of species cited here are present in the first list
# 109/692=15.7% of species from the first list are cited here

# Nb of new plants
nrow(unique(subset(in_common, !is.na(CD_REF) & in_survey=="yes" & is.na(first_paper), select=ESPECE_nom)))
unique(subset(in_common, !is.na(CD_REF) & in_survey=="yes" & is.na(first_paper), select=ESPECE_nom))
# 37 new plants
unique(subset(in_common, is.na(CD_REF) & in_survey=="yes" & is.na(first_paper) 
              & !Commentaire %in%  c("Champignon", "Algue", "Lichen"), select=ESPECE_nom)) # species identified to genus level, 15 "species"


####_________####
#### Number of AFC/Simples answers ####
rattach_gp_cueill <- all_data %>%
  dplyr::select(id, PROFIL_nom_orga_rattach) %>%
  unique() %>%
  na.omit()

ggplot(rattach_gp_cueill, aes(x=PROFIL_nom_orga_rattach)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=90, hjust=1))


####_________####
#### APPENDIX - Bar Plot of all cited species (only >1 citation) ####
df_species <- all_data %>%
  dplyr::select(id, ESPECE_nom) %>%
  na.omit() %>%
  distinct() %>%
  count(ESPECE_nom, name = "citations") %>%
  filter(citations > 1)

# Identify top 10 species by citations
top10_species <- df_species %>%
  top_n(10, citations) %>%
  pull(ESPECE_nom)

# Plot
all_cited <- ggplot(df_species, aes(x = fct_reorder(ESPECE_nom, -citations), y = citations)) +
  geom_col() +
  scale_x_discrete(labels = function(x) ifelse(x %in% top10_species, x, "")) +
  labs(
    title = "Amount of citations per species",
    subtitle = paste("Showing only species cited more than once (", nrow(df_species), " of 146 species )"),
    x = "",
    y = "Citation count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
all_cited

# 172 different cited species

# plot_zoom_png?width=967&height=661
png("plots/all_cited.png", 
    width = 2901,     # pixels
    height = 1983,   # pixels
    res = 300)        # resolution in dpi
all_cited
dev.off()


#### APPENDIX - Cumulative frequency plot of species ####
plot_cumulative_species <- function(data) {
  
  # Count citations per species
  df_cum <- data %>%
    dplyr::select(id, ESPECE_nom) %>%
    unique() %>%   # avoid duplicate id-species pairs
    na.omit() %>%
    count(ESPECE_nom, name = "citations") %>%
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
  ggplot(df_cum, aes(x = reorder(ESPECE_nom, -citations), y = cum_percent, group = 1)) +
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
sp_mentions <- all_data %>% group_by(CD_REF, ESPECE_nom) %>%
  summarise(n_mentions=n())
# Allium ursinum has 101 mentions
nrow(sp_mentions %>% filter(n_mentions==1))
nrow(sp_mentions)
# 62/145=42.7% of species with only one mention


####_________####
#### Figure 1 - Harvesting sustainability of the 20 most cited species ####
plot_durability_ratio <- function(data, n = NULL,
                                  min_ratio = NULL, max_ratio = NULL,
                                  min_citations = NULL, max_citations = NULL) {
  
  # Top species by total citations (if n is provided)
  top_species <- data %>%
    filter(!is.na(CD_REF)) %>%
    dplyr::select(ESPECE_nom, id) %>%
    unique() %>%
    count(ESPECE_nom, name = "total_citations")
  
  if (!is.null(n)) {
    top_species <- top_species %>%
      slice_max(total_citations, n = n)
  }
  
  top_species <- top_species %>% pull(ESPECE_nom)
  
  # Summarise counts + ratio
  df_summary <- data %>%
    filter(ESPECE_nom %in% top_species) %>%
    distinct(id, ESPECE_nom, ESPECE_durabilite) %>%
    count(ESPECE_nom, ESPECE_durabilite) %>%
    group_by(ESPECE_nom) %>%
    mutate(
      total_citations = sum(n),
      ratio_non_durable = sum(n[ESPECE_durabilite == "Non durable"], na.rm = TRUE) / sum(n)
    ) %>%
    summarise(
      ratio_non_durable = unique(ratio_non_durable),
      total_citations = unique(total_citations),
      .groups = "drop"
    ) %>%
    arrange(ratio_non_durable)
  
  # Apply filtering if requested
  if (!is.null(min_ratio)) {
    df_summary <- df_summary %>% filter(ratio_non_durable >= min_ratio)
  }
  if (!is.null(max_ratio)) {
    df_summary <- df_summary %>% filter(ratio_non_durable <= max_ratio)
  }
  if (!is.null(min_citations)) {
    df_summary <- df_summary %>% filter(total_citations >= min_citations)
  }
  if (!is.null(max_citations)) {
    df_summary <- df_summary %>% filter(total_citations <= max_citations)
  }
  
  # Keep ordering for plot
  df_summary <- df_summary %>%
    mutate(ESPECE_nom = factor(ESPECE_nom, levels = unique(ESPECE_nom)))
  
  # Dot plot
  ggplot(df_summary, aes(x = ratio_non_durable, y = ESPECE_nom, size = total_citations)) +
    # Add vertical line at 50%
    geom_vline(xintercept = 0.5, color = "grey70", linewidth = .3, linetype = "solid") +
    geom_point(color = "grey10") +
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

plot_durability_ratio(all_data %>% filter(!is.na(ESPECE_nom)), n = 20)


# plot_zoom_png?width=786&height=600
png("plots/Figure_1_durability_ratio.png", 
    width = 2358,     # pixels
    height = 1800,   # pixels
    res = 300)        # resolution in dpi
plot_durability_ratio(all_data %>% filter(!is.na(ESPECE_nom)), n = 20)
dev.off()


#### % of unsustainable species ####
summary_unsustainable_species <- df_all_species_data %>%
  mutate(flag = 1) %>%
  pivot_wider(
    names_from = durabilite,
    values_from = flag,
    values_fill = NA  # fill missing combinations with 0
  ) %>%
  group_by(nom) %>%
  summarise(Non_durable = sum(`Non durable`, na.rm=T),
            Durable = sum(Durable, na.rm=T),
            Ratio = round(100*Non_durable/(Non_durable+Durable), 2), 
            .groups = "drop")

total_ratio_unsustainable <- summary_unsustainable_species %>%
  summarise(mean_ratio=mean(Ratio, na.rm=T))
total_ratio_unsustainable

percent_unsustainable <- summary_unsustainable_species %>%
  summarise(percent = round(100 * mean(Ratio >+ 50), 2))
percent_unsustainable # 36% of the species were, on average, perceived as being harvested unsustainably

####_________####
#### Figure 2 - Rare species sustainability issues #####
citations_by_species_dpt <- df_all_species_data %>%
  dplyr::select(id, nom, durabilite, dpt) %>% 
  filter(!is.na(dpt) & dpt != "") %>%
  unique() %>%
  filter(!is.na(dpt) & dpt != "") %>%
  group_by(dpt, nom, durabilite) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = durabilite,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total_citations = Durable + `Non durable`)

#### Effect of departements on harvesting sustainablity ####

# citations_by_species_dpt$response <- with(citations_by_species_dpt, cbind(`Non durable`, Durable))
citations_by_species_dpt$ratio <- round(citations_by_species_dpt$`Non durable`/
                                          citations_by_species_dpt$total_citations,2)


# Step 1: Fit models for each species
models <- lapply(unique(citations_by_species_dpt$nom), function(sp) {
  df_sp <- filter(citations_by_species_dpt, nom == sp)
  all_constant <- all(df_sp$`Non durable` == 0 | df_sp$Durable == 0)
  
  # Check why we would skip
  if(length(unique(df_sp$dpt)) < 2) {
    return(list(skipped = TRUE, model = NULL, singular = NA, reason_skipped = "only 1 department"))
  }
  if(all_constant) {
    return(list(skipped = TRUE, model = NULL, singular = NA, reason_skipped = "constant response"))
  }
  
  # Fit GLMM
  model <- glmer(cbind(`Non durable`, Durable) ~ 1 + (1 | dpt),
                 data = df_sp, family = binomial)
  
  # Check if model is singular
  singular_flag <- isSingular(model)
  
  list(skipped = FALSE, model = model, singular = singular_flag, reason_skipped = NA)
})
names(models) <- unique(citations_by_species_dpt$nom)

# Step 2: Build national-level summary
summary_species <- data.frame(
  nom = character(),
  observed_ratio = numeric(),
  pred_prob = numeric(),
  pred_prob_logit = numeric(),
  sd_dpt = numeric(),
  MOR_dpt = numeric(), # median odds ratio
  skipped = logical(),
  singular = logical(),
  reason_skipped = character(),
  stringsAsFactors = FALSE
)


for(sp in names(models)) {
  df_sp <- filter(citations_by_species_dpt, nom == sp)
  
  # Observed ratio (national)
  total_non <- sum(df_sp$`Non durable`)
  total_all <- sum(df_sp$`Non durable` + df_sp$Durable)
  obs_ratio <- ifelse(total_all > 0, total_non / total_all, NA)
  
  if(models[[sp]]$skipped) {
    pred_prob <- NA
    pred_prob_logit <- NA
    var_dpt <- NA
    sd_dpt <- NA
    MOR_dpt <- NA
    flagged <- TRUE
    singular_flag <- NA
    reason <- models[[sp]]$reason_skipped
  } else {
    model <- models[[sp]]$model
    pred_prob <- plogis(fixef(model))              # national-level predicted probability
    pred_prob_logit <- fixef(model)            # national-level predicted probability
    var_dpt <- as.numeric(VarCorr(model)$dpt[1])  # random effect variance
    sd_dpt <- sqrt(var_dpt)
    MOR_dpt <- exp(sqrt(2 * var_dpt) * 0.6745)
    flagged <- FALSE
    singular_flag <- models[[sp]]$singular
    reason <- NA
  }
  
  summary_species <- rbind(summary_species,
                           data.frame(
                             nom = sp,
                             observed_ratio = obs_ratio,
                             pred_prob = pred_prob,
                             pred_prob_logit = pred_prob_logit,
                             sd_dpt = sd_dpt,
                             MOR_dpt = MOR_dpt,
                             skipped = flagged,
                             singular = singular_flag,
                             reason_skipped = reason,
                             stringsAsFactors = FALSE
                           ))
}

# summary(models$`Allium ursinum`$model)
summary_species

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

plot_durability_ratio(all_data %>% mutate(relative_sp_area_FR = 100*sp_area_FR/France) %>%
                        filter(relative_sp_area_FR < 20 &
                                 native=="native"), n = NULL,
                      min_ratio=0, min_citations=2, max_citations=7) # there are no species under 50% unsustainable ratio !
# max citations=7 because the 20 most cited species are cited minimum 8 times (Artemisia genipi)

# plot_zoom_png?width=786&height=416
png("plots/Figure_3_rare_species_20pct.png", 
    width = 2358,
    height = 1248,
    res = 300)
plot_durability_ratio(all_data %>% mutate(relative_sp_area_FR = 100*sp_area_FR/France) %>%
                        filter(relative_sp_area_FR < 20 &
                                 native=="native"), n = NULL,
                      min_ratio=0.1, min_citations=2, max_citations=7)
dev.off()



###_####
#### Overview of harvesting issues in France ####
# x axis = number of departements with a "non durable" answer / number of departements the species is cited in
data_enjeux <- all_rarity %>%
  inner_join(all_data %>% dplyr::select(-sp_relative_area, -dpt_area, -sp_area_FR), by="CD_REF") %>% # "inner" removes species without CD_REF !
  dplyr::select(CD_REF, id, ESPECE_nom, ESPECE_presence, ESPECE_durabilite, ESPECE_dpt,
                dpt_name, dpt_simple, dpt_area, sp_relative_area, sp_area_FR) %>%
  mutate(sp_relative_area = ifelse(is.na(sp_relative_area), 0, sp_relative_area)) %>%
  unique() %>%
  na.omit() %>%
  mutate(n_answers = ifelse(ESPECE_dpt==dpt_simple, 1, 0)) %>%
  pivot_wider(
    names_from = ESPECE_durabilite,
    values_from = n_answers,
    names_prefix = "",
    names_glue = "{.value}_{gsub(' ', '_', .name)}",
    values_fill = 0
  ) %>%
  # Summarise data per département to get the total number of answers per département, and proportion of non durable answers
  group_by(CD_REF, ESPECE_nom, dpt_name, sp_relative_area, sp_area_FR) %>%
  summarise(across(c(n_answers_Non_durable, n_answers_Durable), sum),
            .groups = "drop") %>%
  mutate(total_answers_all = n_answers_Non_durable + n_answers_Durable,
         proportion_non_durable = ifelse(n_answers_Non_durable==0, NA, 
                                         100* n_answers_Non_durable/
                                           total_answers_all),
         proportion_non_durable = ifelse(n_answers_Non_durable==0 & 
                                           n_answers_Durable>0, 0, 
                                         proportion_non_durable)) %>%
  # Join with the harvesting areas
  full_join(massifs_simple, join_by("dpt_name"=="dpt")) %>%
  # Summarise data per harvesting area to get the % of départements with a citation that have 1 "non durable" citations
  group_by(CD_REF, ESPECE_nom, massif, sp_area_FR) %>%
  summarise(
    # Number of departments where the species has 1 non durable answer
    nb_non_durable_dpt = sum(n_answers_Non_durable > 0),
    # Number of non durable answers per harvesting area
    nb_non_durable = sum(n_answers_Non_durable),
    # Number of departments where the species has been cited in
    nb_cited = sum(total_answers_all > 0),
    # Number of departements where the species is present
    n_present = sum(sp_relative_area > 20), #############################
    # Number of departements where the species is present and has at least 1 answer
    n_present_and_answer = sum(sp_relative_area > 0 &
                                 total_answers_all > 0),
    # Total number of answers per harvesting area
    total_answers_all=sum(total_answers_all),
    # Non durable answers ratio
    non_durable_ratio1 = ifelse(total_answers_all > 0,
                                100 * nb_non_durable
                                / total_answers_all,
                                NA),
    # Number of departments where the species has 1 non durable answer
    # divided by the number of departments where the species has been cited in
    non_durable_ratio2 = ifelse(nb_cited > 0,
                                100 * nb_non_durable_dpt
                                / nb_cited,
                                NA),
    # Answer coverage ratio (Number of departments where the species is present and has at least one answer divided by the number of départements where the species is present)
    answer_coverage = ifelse(n_present > 0,
                             100 * n_present_and_answer /
                               n_present,
                             NA),
    .groups = "drop"
  ) %>%
  # Round proportions
  mutate(across(c(non_durable_ratio1, non_durable_ratio2, answer_coverage),
                ~ round(.x, 1))) %>%
  
  # Add species presence statuses from the list of French vascular plants
  left_join(vascular) 


data_enjeux_general <- data_enjeux %>%
  group_by(CD_REF, ESPECE_nom, sp_area_FR, native) %>%
  summarise(
    # % of harvesting areas that agree with the unsustainability score
    agreement = sd(non_durable_ratio1, na.rm = TRUE),
    agreement = ifelse(is.na(agreement), 0, agreement),
    
    # % of areas in which the species is cited as "non durable" on average 
    # over the number of areas in which the species was cited
    non_durable_ratio3 = ifelse(sum(total_answers_all > 0, na.rm = T) > 0,
                                100 * sum(non_durable_ratio1 > 50, na.rm = T) 
                                / sum(total_answers_all > 0, na.rm = T),
                                NA),
    # Average % of non durable answers / harvesting area
    non_durable_ratio1 = ifelse(sum(total_answers_all > 0, na.rm = T) > 0,
                                mean(non_durable_ratio1, na.rm = T),
                                NA),
    # Number of harvesting areas where the species has 1 non durable answer
    # divided by the number of departments where the species has been cited in
    non_durable_ratio2 = ifelse(sum(total_answers_all > 0, na.rm = T) > 0,
                                100 * sum(non_durable_ratio2 > 0, na.rm = T) 
                                / sum(total_answers_all > 0, na.rm = T),
                                NA),
    # Answer coverage ratio (number of harvesting areas where the species is present and has at least 1 answer, divided by the number of areas where the species is present in)
    answer_coverage_harv_area = ifelse(sum(n_present > 0) > 0,
                                       100 * sum(n_present > 0 &
                                                   total_answers_all > 0) /
                                         sum(n_present > 0),
                                       NA),
    # Answer coverage ratio (number of departements where the species is present and has at least 1 answer, divided by the number of areas where the species is present in))
    answer_coverage_dpt = ifelse(sum(n_present > 0) > 0,
                                 100* sum(n_present_and_answer) / sum(n_present),
                                 NA),
    
    # Number of harvesting areas where the species is present
    n_present = sum(n_present > 0),
    
    # Total number of all answers per species
    total_answers_all = sum(total_answers_all),
    .groups = "drop") %>%
  # Round proportions
  mutate(across(c(non_durable_ratio1, non_durable_ratio2, non_durable_ratio3,
                  answer_coverage_harv_area, answer_coverage_dpt, agreement),
                ~ round(.x, 1)))


##### Plot number of citations against species area ####
data_enjeux_general$non_durable_cat <- cut(
  data_enjeux_general$non_durable_ratio2,
  breaks = c(0, 25, 50, 75, 100),  # define 4 bins
  labels = c("0-25", "25-50", "50-75", "75-100"),
  include.lowest = TRUE
)


ggplot(data_enjeux_general,
       aes(x = log10(sp_area_FR), y = total_answers_all)) +
  geom_point(aes(size = agreement, colour = non_durable_cat),
             alpha = 0.8) +
  geom_vline(xintercept = log10(quantiles_all), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = log10(quantiles_survey), linetype = "dashed", color = "blue", size = 1) +
  scale_colour_manual(
    values = c("lightgreen", "gold", "orange", "darkred"),
    name = "Non-sustainable ratio"
  ) +
  scale_size_continuous(
    range = c(1, 10),       # min and max point sizes
    name = "Disagreement (SD of non-sustainable ratio)"
  ) +
  geom_text_repel(
    data = subset(data_enjeux_general, total_answers_all > 7),
    aes(label = ESPECE_nom),
    size = 3,
    max.overlaps = 1000,      # ensure all labels are attempted
    nudge_y = 1,            # move labels slightly up
    nudge_x = 0.0,            # optional horizontal adjustment
    segment.color = "grey50", # color of connecting line
    segment.size = 0.5,       # line thickness
    box.padding = 0.5,        # space around text to avoid overlapping
    point.padding = 0.5,      # extra distance from point
    force = 2,                # stronger repulsion to avoid overlap
    direction = "both",       # labels can move vertically and horizontally
    min.segment.length = 0    # draw line even if text is very close to point
  ) +
  theme_minimal()


# questions: do i calculate sd among harvesting areas, or directly among dpts
# is it worth increasing precision of species area calculation (taraxacum vs thymus for example)



ggplot(subset(data_enjeux_general, total_answers_all <= 7 & native=="native" 
              & non_durable_ratio2>0), # non-native vs invasive ?
       aes(x = 100*sp_area_FR/France, y = total_answers_all)) +
  geom_point(aes(size = agreement, colour = non_durable_cat),
             alpha = 0.8) +
  geom_vline(xintercept = 100*quantiles_all/France, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = 100*quantiles_survey/France, linetype = "dashed", color = "blue", size = 1) +
  scale_colour_manual(
    values = c("lightgreen", "gold", "orange", "darkred"),
    name = "Non-sustainable ratio"
  ) +
  scale_size_continuous(
    range = c(1, 10),       # min and max point sizes
    name = "Disagreement (SD of non-sustainable ratio)"
  ) +
  geom_text_repel(data=subset(data_enjeux_general, total_answers_all>1 & total_answers_all <= 7 & native=="native" 
                              & non_durable_ratio2>0),
                  aes(label = ESPECE_nom),
                  size = 3,
                  max.overlaps = 1000,      # ensure all labels are attempted
                  nudge_y = 0.1,            # move labels slightly up
                  nudge_x = 0.0,            # optional horizontal adjustment
                  segment.color = "grey50", # color of connecting line
                  segment.size = 0.5,       # line thickness
                  box.padding = 0.5,        # space around text to avoid overlapping
                  point.padding = 0.5,      # extra distance from point
                  force = 2,                # stronger repulsion to avoid overlap
                  direction = "both",       # labels can move vertically and horizontally
                  min.segment.length = 0    # draw line even if text is very close to point
  ) +
  theme_minimal()




# ggplot(subset(data_enjeux_general, total_answers_all <= 7 & native=="native" 
#               & non_durable_ratio1>0), # non-native vs invasive ?
#        aes(x = log10(species_area_FR), y = non_durable_ratio2)) +
#   geom_point(aes(size = agreement),
#              alpha = 0.8) +
#   scale_size_continuous(
#     range = c(1, 10),       # min and max point sizes
#     name = "Disagreement (SD of non-sustainable ratio)"
#   ) +
#   geom_text_repel(
#     aes(label = ESPECE_nom),
#     size = 3,
#     max.overlaps = 1000,      # ensure all labels are attempted
#     nudge_y = 1,            # move labels slightly up
#     nudge_x = 0.0,            # optional horizontal adjustment
#     segment.color = "grey50", # color of connecting line
#     segment.size = 0.5,       # line thickness
#     box.padding = 0.5,        # space around text to avoid overlapping
#     point.padding = 0.5,      # extra distance from point
#     force = 2,                # stronger repulsion to avoid overlap
#     direction = "both",       # labels can move vertically and horizontally
#     min.segment.length = 0    # draw line even if text is very close to point
#   ) +
#   theme_minimal()
# 
# 
# ggplot(subset(data_enjeux_general, total_answers_all <= 7 & native=="native" 
#               & non_durable_ratio1>0), # non-native vs invasive ?
#        aes(x = agreement, y = total_answers_all)) +
#   geom_point(aes(size = agreement, colour = non_durable_cat),
#              alpha = 0.8) +
#   scale_colour_manual(
#     values = c("lightgreen", "gold", "orange", "darkred"),
#     name = "Non-sustainable ratio"
#   ) +
#   geom_text_repel(
#     aes(label = ESPECE_nom),
#     size = 3,
#     max.overlaps = 1000,      # ensure all labels are attempted
#     nudge_y = 1,            # move labels slightly up
#     nudge_x = 0.0,            # optional horizontal adjustment
#     segment.color = "grey50", # color of connecting line
#     segment.size = 0.5,       # line thickness
#     box.padding = 0.5,        # space around text to avoid overlapping
#     point.padding = 0.5,      # extra distance from point
#     force = 2,                # stronger repulsion to avoid overlap
#     direction = "both",       # labels can move vertically and horizontally
#     min.segment.length = 0    # draw line even if text is very close to point
#   ) +
#   theme_minimal()


####_________####
#### Identifying distinct respondent profiles ####
##### Prepare data ###
all_data_ACM_participant_profile <- all_data %>%
  dplyr::select(id, PROFIL_statut_cueilleur, PROFIL_type_orga_rattach,
                PROFIL_age, PROFIL_genre, PROFIL_categ_socio_pro, 
                PROFIL_nb_esp_cueill, PROFIL_niveau_educ) %>%
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
res.hcpc.particip$data.clustclust <- recode(res.hcpc.particip$data.clust$clust,
                                           `1` = 3,
                                           `2` = 2,
                                           `3` = 1) %>%
  as.factor()


##### Plot clusters ###
plot_data_particip <- data.frame(
  row_name = rownames(res.mca.particip$ind$coord),
  Dim.1 = res.mca.particip$ind$coord[,1],
  Dim.2 = res.mca.particip$ind$coord[,2],
  Cluster = as.factor(res.hcpc.particip$data.clust$clust)
)

plot_MCA_particip <- ggplot(plot_data_particip, aes(Dim.1, Dim.2, color = Cluster)) +
  geom_point(alpha = 0.8, size = 3) +
  stat_ellipse(aes(group = Cluster), type = "t", geom = "polygon", alpha = 0.2, linetype = "dashed") +
  labs(
    title = "ACM des profils de répondants et classification en clusters",
    subtitle = paste0("Variance expliquée par Dim.1 et Dim.2 : ", 
                      round(res.mca.particip$eig[1,2], 1), "% et ", 
                      round(res.mca.particip$eig[2,2], 1), "%"),
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
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
  gsub("Une seule$", "Une seule espece cueillie", .) %>%
  gsub("^0$", "0 especes cueillies", .) %>%
  gsub("^1 a 5$", "1 a 5 especes cueillies", .) %>%
  gsub("^5 a 30$", "5 a 30 especes cueillies", .) %>%
  gsub("^30 a 100$", "30 a 100 especes cueillies", .) %>%
  gsub("PROFIL_", "", .) %>%
  gsub("Professionnel$", "Cueilleur professionnel", .) %>%
  gsub("Amateur$", "Cueilleur amateur", .)
rownames(var_data) <- new_labels

# Add arrows and labels to MCA plot
plot_MCA_with_vars <- plot_MCA_particip +
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  geom_text_repel(data = var_data, aes(x = `Dim 1`, y = `Dim 2`, label = rownames(var_data)),
                  color = "black", size = 3)

plot_MCA_with_vars

# plot_zoom_png?width=861&height=641
png("plots/profile_MCA.png", 
    width = 2583,     # pixels
    height = 1923,   # pixels
    res = 300)        # resolution in dpi
plot_MCA_with_vars
dev.off()



##### Analyse performance #####
# Calculate the distance matrix based on the MCA coordinates
dist_matrix <- dist(res.mca.particip$ind$coord)

# Get the cluster assignments from the HCPC result
# Convert the cluster factor to a numeric vector for compatibility
cluster_assignments_numeric <- as.numeric(as.character(res.hcpc.particip$data.clust$clust))

# Compute the silhouette values with the numeric cluster assignments
sil_result <- silhouette(cluster_assignments_numeric, dist_matrix)

# Print a summary of the silhouette results
summary(sil_result)

# Plot the silhouette
plot(sil_result)


#### Do respondent profiles impact the perception of sustainability ? ####
##### Sustainability status VS respondent profile ####

all_data_profile_clusters <- plot_data_particip %>%
  dplyr::select(row_name, Cluster) %>%
  full_join(ref_table_particip, by="row_name") %>%
  left_join(all_data) %>%
  dplyr::select(-row_name)


d <- all_data_profile_clusters %>% 
  dplyr::select(id, ESPECE_nom, massif, ESPECE_durabilite, Cluster) %>%
  unique() %>%
  na.omit()

names(d) <- c("id", "species", "harvest_area", "sustainability", "respondent_profile")

d$sustainability <- ifelse(d$sustainability == "Durable", 1, 0)

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

# Count number of respondents per species and profile
table(d$species, d$respondent_profile)


##### Unsustainable known VS respondent profile ####
# Barplot
all_data_non_durable_connue <- all_data %>%
  dplyr::select(id, PROFIL_statut_cueilleur, 
                PROFIL_type_orga_rattach,
                PROFIL_age, PROFIL_genre, PROFIL_categ_socio_pro, PROFIL_nb_esp_cueill, PROFIL_niveau_educ,
                ESPECE_non_durable_connue) %>%
  unique() %>%
  mutate(across(-id, as.factor)) %>%  
  dplyr::select(-id) %>%
  na.omit()

ggplot(all_data_non_durable_connue, aes(x = "Total", fill = ESPECE_non_durable_connue)) +
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
  dplyr::select(id, ESPECE_nom, massif, ESPECE_non_durable_connue, Cluster) %>%
  unique() %>%
  na.omit()

names(d2) <- c("id", "species", "harvest_area", "unsustainable_species_known", "respondent_profile")

d2$unsustainable_species_known <- ifelse(d2$unsustainable_species_known == "Oui", 1, 0)
d2$respondent_profile <- as.factor(d2$respondent_profile)

model3 <- glm(unsustainable_species_known ~ respondent_profile,
              data = d2, family = binomial)
summary(model3) # Profile 3 bordeline significant



####_________####
#### Figure 3 - What variables impact the perception of sustainability ? ####
# Variable réponse (binaire : "Durable" / "Non durable")
data_dfa_durabilite_sp <- all_data %>%
  dplyr::select(id, ESPECE_nom, CD_REF,
                ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
                ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
                starts_with("ESPECE_usages_"), starts_with("ESPECE_parties_cueill_"), 
                ESPECE_mode, ESPECE_espece_reglem) %>%
  unique() %>%
  na.omit() %>%
  dplyr::select(-CD_REF, -id) %>%
  droplevels() %>%
  mutate(across(-c(ESPECE_durabilite), as.factor)) %>%
  mutate(ESPECE_durabilite = forcats::fct_recode(
    ESPECE_durabilite,
    "Sustainable" = "Durable",
    "Unsustainable" = "Non durable"
  ))


active_vars <- data_dfa_durabilite_sp %>%
  dplyr::select(-ESPECE_durabilite, -ESPECE_nom)

grouping_factor <- data_dfa_durabilite_sp$ESPECE_durabilite

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
  "ESPECE_mode.Mode.et.risque" = "Trend-driven harvesting (unsustainable)",
  "ESPECE_etat_ressource.Diminue" = "Declining resource",
  "ESPECE_presence.Rare" = "Rare species",
  "ESPECE_espece_reglem.Oui..elle.est.reglementee" = "Regulated species",
  "ESPECE_intensite_prelev.Forte" = "High harvesting intensity",
  "ESPECE_etendue_cueill.Localisee." = "Localised harvesting",
  "ESPECE_presence.Extremement.rare" = "Extremely rare species",
  "ESPECE_usages_Alimentaire.Non" = "No food use",
  "ESPECE_variation_prelev.Augmentent" = "Increasing harvesting levels",
  "ESPECE_usages_Ornemental.Oui" = "Ornamental use",
  "ESPECE_mode.Mode.sans.risque" = "Trend-driven harvesting (sustainable)",
  "ESPECE_intensite_prelev.Faible" = "Low harvesting intensity",
  "ESPECE_usages_Ornemental.Non" = "No ornamental use",
  "ESPECE_usages_Alimentaire.Oui" = "Food use",
  "ESPECE_variation_prelev.Stable" = "Stable harvesting levels",
  "ESPECE_etendue_cueill.Generalisee." = "Widespread harvesting",
  "ESPECE_espece_reglem.Non..aucune.reglementation.connue" = "Unregulated species",
  "ESPECE_mode.Pas.de.mode" = "No trend-driven harvesting",
  "ESPECE_presence.Abondante.large" = "Widely abundant species",
  "ESPECE_etat_ressource.Stable" = "Stable resource"
)

# Diverging bar plot
var_contrib <- ggplot(top_var_contrib, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = sustainability)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Unsustainable" = "#D00F0F", "Sustainable" = "#64A251")) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "",
       y = "FDA Coefficient",
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

# Add annotations only to the top plots
FDA_plot <- combined_with_legend + plot_annotation(tag_levels = "a", tag_suffix = "", tag_prefix = "")

FDA_plot



# plot_zoom_png?width=941&height=451
png("plots/Figure_2_DFA_sustainability.png", 
    width = 2823,     # pixels
    height = 1353,   # pixels
    res = 300)        # resolution in dpi
FDA_plot
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
  dplyr::select(-ESPECE_nom)

lda_fit <- lda(ESPECE_durabilite ~ ., data = lda_data)
pred <- predict(lda_fit)$class

table(Predicted = pred, Actual = lda_data$ESPECE_durabilite) # confusion matrix

cat("Classification accuracy:", round(mean(pred == lda_data$ESPECE_durabilite)*100, 1), "%\n")
# Classification accuracy: 90.9 %


#### Do respondents’ perceptions of rarity and abundance correspond to actual species abundance ? ####
rarity <- all_data %>%
  dplyr::select(id, ESPECE_nom, ESPECE_presence, ESPECE_durabilite, ESPECE_dpt, sp_relative_area) %>%
  unique() %>%
  na.omit()

##### Kruskal-Wallis test ###
kruskal.test(sp_relative_area ~ ESPECE_presence, data = rarity)
# Kruskal-Wallis chi-squared = 273.81, df = 4, p-value < 2.2e-16

kruskal_effsize(sp_relative_area ~ ESPECE_presence , data = rarity)
# 0.335 effect size, large

# Ensure ESPECE_presence is a factor in the desired order
ordre_presence <- c("Abondante large", "Abondante locale", "Rare", "Extremement rare", "JNSP")
rarity$ESPECE_presence <- factor(rarity$ESPECE_presence, levels = ordre_presence)

##### Dunn's test for pairwise comparisons (because KW test is significant) ###
rstatix::dunn_test(sp_relative_area ~ ESPECE_presence, p.adjust.method = "bonferroni", data=rarity)

##### Estimates and CI ###
# compute bootstrapped CI for each group
set.seed(123)  # for reproducibility
rarity %>%
  group_by(ESPECE_presence) %>%
  summarise(
    n = n(),
    median = median(sp_relative_area, na.rm = TRUE),
    CI_low = {
      b <- boot(sp_relative_area, 
                function(data, indices) { # function to bootstrap the median
        median(data[indices], na.rm = TRUE)
      }, R = 2000)
      boot.ci(b, type = "perc")$percent[4]
    },
    CI_high = {
      b <- boot(sp_relative_area, boot_median, R = 2000)
      boot.ci(b, type = "perc")$percent[5]
    }
  )

##### Plot ###
ggplot(rarity, aes(x = ESPECE_presence, y = sp_relative_area)) +
  geom_boxplot(fill = "lightgrey", outlier.shape = NA) +
  geom_jitter(aes(color = ESPECE_durabilite), width = 0.2, height = 0) +
  scale_color_manual(
    values = c("Durable" = "#64A251", "Non durable" = "#D00F0F"),
    labels = c("Durable" = "Sustainable", "Non durable" = "Unsustainable")
  ) + 
  stat_pvalue_manual(
    dunn_result,
    label = "p.adj.signif",
    y.position = 110, # adjust depending on your data range
    hide.ns = TRUE,
    step.increase = 0.05
  ) +
  labs(
    title = "Comparison between perceived and actual species abundance",
    x = "Species abundance category",
    y = "Species coverage of per department (%)",
    color = "Sustainability status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


#### Can species perceived sustainability be predicted from CSR strategy, life form, and distribution characteristics? ####
##### DFA ####
# Variable réponse (binaire : "Durable" / "Non durable")
data_dfa_durabilite_sp_bio <- all_data %>%
  dplyr::select(id, ESPECE_nom, CD_REF,
                ESPECE_durabilite, C, S, R, csr_simple, sp_relative_area, choix_type_bio) %>%
  unique() %>%
  na.omit() %>%
  dplyr::select(-CD_REF, -id) %>%
  droplevels() %>%
  mutate(across(-c(ESPECE_durabilite, C, S, R, csr_simple, sp_relative_area), as.factor)) %>%
  mutate(ESPECE_durabilite = forcats::fct_recode(
    ESPECE_durabilite,
    "Sustainable" = "Durable",
    "Non sustainable" = "Non durable"
  ))

data_dfa_durabilite_sp_bio_summar <- data_dfa_durabilite_sp_bio %>%
  group_by(ESPECE_nom, ESPECE_durabilite) %>%
  summarise(count=n(), csr_simple=first(csr_simple), choix_type_bio=first(choix_type_bio))

active_vars <- data_dfa_durabilite_sp_bio %>%
  dplyr::select(-ESPECE_durabilite, -ESPECE_nom)

grouping_factor <- data_dfa_durabilite_sp_bio$ESPECE_durabilite


# --- Run DFA ---
res.dfa <- discrimin(dudi.mix(active_vars, scannf = FALSE), 
                     fac = grouping_factor, 
                     scannf = FALSE)

# --- Visualise Results ---
plot_data <- data.frame(
  score_dfa = res.dfa$li$DS1,
  group = grouping_factor
)

ggplot(plot_data, aes(x = score_dfa, fill = group)) +
  geom_density(alpha = 0.5) +
  labs( title = "Species distribution by sustainability status",
        x = "Discriminant Axis 1",
        y = "Density",
        fill = "Sustainability perception") +
  theme_minimal() +
  scale_fill_manual(values = c("Sustainable" = "#64A251", "Non sustainable" = "#D00F0F"))

# --- Get Contributions ---
correlations <- as.data.frame(res.dfa$va)

sorted_correlations <- correlations[order(abs(correlations$CS1), decreasing = TRUE), , drop = FALSE]
head(sorted_correlations, 10)


# --- Cohen's d ---
group_stats <- plot_data %>%
  group_by(group) %>%
  summarise(mean_score = mean(score_dfa),
            sd_score = sd(score_dfa),
            n = n())

mean_diff <- diff(group_stats$mean_score)
pooled_sd <- sqrt(((group_stats$sd_score[1]^2)*(group_stats$n[1]-1) +
                     (group_stats$sd_score[2]^2)*(group_stats$n[2]-1)) /
                    (sum(group_stats$n)-2))
cohens_d <- mean_diff / pooled_sd

cat("Cohen's d (DS1 separation):", round(cohens_d, 3), "\n")

# --- Classification accuracy ---
# Prepare data for LDA (all predictors as factors)
lda_data <- data_dfa_durabilite_sp_bio %>%
  dplyr::select(-ESPECE_nom)

lda_fit <- lda(ESPECE_durabilite ~ ., data = lda_data)
pred <- predict(lda_fit)$class

table(Predicted = pred, Actual = lda_data$ESPECE_durabilite) # Confusion matix

cat("Classification accuracy:", round(mean(pred == lda_data$ESPECE_durabilite)*100, 1), "%\n")


##### GLM #####
# Prepare data
data_dfa_durabilite_sp_bio <- all_data %>%
  dplyr::select(id, ESPECE_nom, CD_REF, ESPECE_durabilite, C, S, R, 
                csr_simple, sp_relative_area, choix_type_bio) %>%
  unique() %>%
  na.omit() %>%
  dplyr::select(-CD_REF, -id) %>%
  mutate(
    ESPECE_durabilite = fct_recode(ESPECE_durabilite,
                                   "Sustainable" = "Durable",
                                   "Non sustainable" = "Non durable"),
    # Now explicitly set reference level:
    ESPECE_durabilite = relevel(ESPECE_durabilite, ref = "Non sustainable"),
    across(-c(C, S, R, sp_relative_area), as.factor)
  )

# Regression
glm_fit <- glm(ESPECE_durabilite ~ sp_relative_area + csr_simple + choix_type_bio,
               data = data_dfa_durabilite_sp_bio,
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

# Find optimal threshold to culculate model performance
pred_probs <- predict(glm_fit, newdata = data_dfa_durabilite_sp_bio, type = "response")

roc_obj <- roc( # use ROC+AUC
  data_dfa_durabilite_sp_bio$ESPECE_durabilite,
  pred_probs,
  levels = c("Non sustainable", "Sustainable"),
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
print(as.numeric(opt_coords["threshold"]))

# Evaluate model performance using optimal threshold
# Use the same factor levels and order as the actual data
pred_class <- ifelse(pred_probs > opt_thresh, "Sustainable","Non sustainable")
pred_class <- factor(pred_class, levels = levels(data_dfa_durabilite_sp_bio$ESPECE_durabilite))
cconfusionMatrix(pred_class, data_dfa_durabilite_sp_bio$ESPECE_durabilite)
print(conf_mat)

# Extract and print accuracy directly.
cat("Classification accuracy:", round(conf_mat$overall['Accuracy'] * 100, 1), "%\n")




####_________####
#### Figure 4 - Spatial distribution of ‘unsustainable’ mentions ####
#### Plot species maps per département ####
# Data is already processed in `df_all_species_data`, now join with `departements`
citations_by_species_massif <- citations_by_species_dpt %>%
  right_join(departements_simpl_PARIS, join_by("dpt"=="dpt_simple")) %>%
  group_by(nom, massif) %>%
  summarise(Non_durable=sum(`Non durable`),
            Durable=sum(Durable), 
            total_citations=sum(total_citations)) %>%
  ungroup() %>%
  na.omit()

plot_species_map_generic <- function(
    spatial_data,
    data,
    species_name = NULL,
    join_by_expr,
    value_non_durable,
    value_durable,
    value_total,
    global_max = NULL,
    subtitle
) {
  
  filtered_data <- data %>%
    {
      if (!is.null(species_name)) {
        filter(., nom == species_name)
      } else {
        .
      }
    }
  
  map_data <- spatial_data %>%
    left_join(filtered_data, by = join_by_expr) %>%
    st_as_sf() %>%
    mutate(
      prop_non_durable = ifelse(
        {{ value_total }} > 0,
        round({{ value_non_durable }} / {{ value_total }} * 100, 2),
        NA_real_
      )
    )
  
  centroids <- map_data %>%
    mutate(geometry = st_centroid(geometry))
  
  p <- ggplot(map_data) +
    geom_sf(
      aes(fill = prop_non_durable),
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
    value_non_durable = `Non durable`,
    value_durable = Durable,
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
    value_non_durable = Non_durable,
    value_durable = Durable,
    value_total = total_citations,
    global_max = global_max)
}

species_maps_dpt <- plot_species_map_dpt("Allium ursinum") + plot_species_map_dpt("Filipendula ulmaria") + plot_species_map_dpt("Vaccinium myrtillus") +  plot_species_map_dpt("Hypericum nummularium") + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") & theme(legend.position = "right", legend.box.margin = margin(l = 50))
species_maps_dpt 

# plot_zoom_png?width=905&height=697
png("plots/Figure_3_species_maps_dpt.png", 
    width = 2715,
    height = 2091,
    res = 300)
species_maps_dpt
dev.off()


species_maps_massif <- plot_species_map_massif("Allium ursinum") + plot_species_map_massif("Filipendula ulmaria") + plot_species_map_massif("Vaccinium myrtillus") +  plot_species_map_massif("Hypericum nummularium") + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") & theme(legend.position = "right", legend.box.margin = margin(l = 50))
species_maps_massif

# plot_zoom_png?width=905&height=697
png("plots/Figure_3_species_maps_massif.png", 
    width = 2715,
    height = 2091,
    res = 300)
species_maps_massif
dev.off()


plot_map_France_dpt <- function() {

  plot_species_map_generic(
    spatial_data = departements_simpl_PARIS,
    data = citations_by_species_dpt %>% group_by(dpt) %>% summarise(
      Non_durable=sum(`Non durable`),
      Durable=sum(Durable),
      total_citations=sum(total_citations)) %>% ungroup(),
    species_name = NULL,
    join_by_expr =  c("dpt_simple" = "dpt"),
    value_non_durable = Non_durable,
    value_durable = Durable,
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
      Non_durable=sum(Non_durable),
      Durable=sum(Durable),
      total_citations=sum(total_citations)) %>% ungroup(),
    species_name = NULL,
    join_by_expr = "massif",
    value_non_durable = Non_durable,
    value_durable = Durable,
    value_total = total_citations,
    global_max = NULL,
    subtitle = "by harvesting area"
  )
}


plot_map_France_dpt() + plot_map_France_massif()

# plot_zoom_png?width=1272&height=480
png("plots/Figure_3_all_France_species_maps.png", 
    width = 3816,
    height = 1440,
    res = 300)
plot_map_France_dpt() + plot_map_France_massif() + plot_annotation(tag_levels = "a") 
dev.off()


#### SD of unsustainable mention ratio across harvesting areas ####
national_summary <- citations_by_species_massif %>%
  group_by(massif) %>%
  summarise(
    ratio_unsustainable = 100 * sum(Non_durable) / sum(total_citations),
    .groups = "drop"
  )

with(national_summary, {
  m <- mean(ratio_unsustainable)
  se <- sd(ratio_unsustainable) / sqrt(length(ratio_unsustainable))
  c(mean = m, ci_lower = m - 1.96 * se, ci_upper = m + 1.96 * se)})
# mean ci_lower ci_upper 
# 30.72300 25.75785 35.68815 



####_________####
#### Figure 5 - Are the assumptions of the respondents concerning species regulations correct ? #####
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
  dplyr::select(id, ESPECE_nom, CD_REF, ESPECE_espece_reglem, ESPECE_reglem_adaptee, ESPECE_dpt) %>%
  unique() %>%
  left_join(departements %>% dplyr::select(dpt_simple, region_simple), by=join_by("ESPECE_dpt"=="dpt_simple")) %>%
  dplyr::select(-geometry)

# Separate different levels of protections/regulations
bdc_dpt <- data_regl_prot %>%
  # Join department-specific regulations
  left_join(BDC_STATUTS_17 %>% filter(NIVEAU_ADMIN == "Département"), 
            by = join_by("CD_REF"=="CD_REF", "ESPECE_dpt"=="LB_ADM_TR_simpl"))

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
    ESPECE_espece_reglem = case_when(
      str_detect(ESPECE_espece_reglem, "Non") ~ "No",
      str_detect(ESPECE_espece_reglem, "Oui") ~ "Yes",
      str_detect(ESPECE_espece_reglem, "JNSP") ~ "IDK",
      TRUE ~ ESPECE_espece_reglem),
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
  filter(!is.na(ESPECE_espece_reglem))%>%
  unique() %>%
  left_join(BDC_STATUTS_17_summar)


data_regl_prot_summar <- data_regl_prot_join %>%
  # Calculate % by ESPECE_espece_reglem nad statut_group
  group_by(ESPECE_espece_reglem, statut_group) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(ESPECE_espece_reglem) %>%
  mutate(pct = n / sum(n) * 100)

data_regl_prot_summar2 <- data_regl_prot_join %>%
  # Calculate % of each answer
  group_by(ESPECE_espece_reglem) %>%
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
pct_regul_answers <- ggplot(data_regl_prot_summar, aes(x = ESPECE_espece_reglem, y = pct, fill = statut_group)) +
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

count_regul_answers <- ggplot(data_regl_prot_join, aes(x = ESPECE_espece_reglem, fill = statut_group)) +
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
    reponse_correcte = case_when(
      ESPECE_espece_reglem == "Yes" & statut_group != "No regulation/protection" ~ "Correct",
      ESPECE_espece_reglem == "No" & statut_group == "No regulation/protection" ~ "Correct",
      TRUE ~ "Incorrect"
    )
  )

# Résumé en pourcentage
data_regl_eval_summar <- data_regl_eval %>%
  group_by(ESPECE_espece_reglem, reponse_correcte, statut_group) %>%
  summarise(n = n(), .groups = "drop")

data_regl_eval_summar <- data_regl_eval %>%
  group_by(ESPECE_espece_reglem, statut_group, reponse_correcte) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(ESPECE_espece_reglem) %>%
  mutate(pct = n / sum(n) * 100)

# Graph
pct_correct_regul <- ggplot(data_regl_eval_summar, aes(x = ESPECE_espece_reglem, y = pct, fill = reponse_correcte)) +
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


nb_correct_regul <- ggplot(data_regl_eval_summar, aes(x = ESPECE_espece_reglem, y = n, fill = reponse_correcte)) +
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
png("plots/Figure_4_regulations.png", 
    width = 2946,     # pixels
    height = 1143,   # pixels
    res = 300)        # resolution in dpi
regulation_plot
dev.off()

# Find if the species indicated as having a regulation when they don't are regulated elsewhere
df <- data_regl_prot_join %>%
  subset(ESPECE_espece_reglem=="Yes" & statut_group=="No regulation/protection",
         select=c(ESPECE_espece_reglem, statut_group, national_status)) %>%
  group_by(national_status) %>%
  summarise(n=n())


#### Does the profile of respondents determine their likelihood of correctly reporting species regulations? ####
# Create a new column by pasting col1 and col2 together
data_regl_eval$reglem_glm <- paste(data_regl_eval$ESPECE_espece_reglem, data_regl_eval$reponse_correcte, sep = "_")

data_regl_eval <- data_regl_eval %>%
  left_join(dplyr::select(all_data_profile_clusters, id, Cluster)%>%unique())

data_regl_eval$reponse_correcte <- ifelse(data_regl_eval$reponse_correcte == "Correct", 1, 0)

glm_fit <- glm(reponse_correcte ~ Cluster,
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
  dplyr::select(id, CD_REF, ESPECE_nom, ESPECE_espece_reglem, statut_group) %>%
  left_join(dplyr::select(all_data, id, CD_REF, ESPECE_durabilite)) %>%
  unique() %>%
  group_by(ESPECE_durabilite, statut_group) %>%
  summarise(n_species = n(), .groups = "drop")


result <- sust_regul_status %>%
  dplyr::filter(ESPECE_durabilite == "Non durable") %>%
  dplyr::summarise(
    total_non_durable = sum(n_species),
    non_durable_unregulated = sum(
      n_species[statut_group == "No regulation/protection"],
      na.rm = TRUE
    ),
    percent = 100 * non_durable_unregulated / total_non_durable
  )

sust_regul_species2 <- data_regl_prot_join %>%
  dplyr::select(id, CD_REF, ESPECE_nom, ESPECE_espece_reglem, statut_group) %>%
  left_join(dplyr::select(all_data, id, CD_REF, ESPECE_durabilite)) %>%
  distinct()


unprot <- sust_regul_species2 %>%
  dplyr::filter(
    statut_group == "No regulation/protection"
  )

unsust_ratio <- unprot %>%
  dplyr::group_by(CD_REF, ESPECE_nom) %>%
  dplyr::summarise(
    total_citations = n(),
    unsustainable_citations = sum(ESPECE_durabilite == "Non durable", na.rm = TRUE),
    ratio_unsustainable = unsustainable_citations / total_citations,
    .groups = "drop"
  )

unsust_ratio %>%
  dplyr::arrange(desc(ratio_unsustainable))



####_________####

###################################################













#### Number of harvesting areas per species ####
data_enjeux_summary <- data_enjeux %>%
  mutate(sustainability = ifelse(non_durable_ratio1 > 50, "Unsustainable", "Sustainable")) %>%
  group_by(ESPECE_nom, sustainability) %>%
  summarise(
    area_count = n_distinct(massif),
    citations = sum(total_answers_all),
    .groups = "drop"
  ) %>%
  filter(citations >= 3)

# Step 2: compute unsustainable ratio per species for ordering
unsustainable_ratio <- data_enjeux_summary %>%
  group_by(ESPECE_nom) %>%
  summarise(
    total_unsustainable = sum(area_count[sustainability == "Unsustainable"], na.rm = TRUE),
    total_areas = sum(area_count, na.rm = TRUE)
  ) %>%
  mutate(unsustainable_ratio = total_unsustainable / total_areas)

# Step 3: join back and order factor levels
data_enjeux_summary <- data_enjeux_summary %>%
  left_join(unsustainable_ratio %>% dplyr::select(ESPECE_nom, unsustainable_ratio, total_areas), by = "ESPECE_nom") %>%
  mutate(ESPECE_nom = reorder(ESPECE_nom, unsustainable_ratio, decreasing = TRUE))

# Step 4: plot
ggplot(data_enjeux_summary %>%
         dplyr::select(ESPECE_nom, total_areas) %>%
         unique() %>%
         mutate(ESPECE_nom = reorder(ESPECE_nom, -total_areas)), # reorder by descending total_areas
       aes(x = ESPECE_nom, y = total_areas)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Harvesting Areas per Species",
    x = "Species",
    y = "Number of Harvesting Areas"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


ggplot(data_enjeux_summary %>%
         dplyr::select(ESPECE_nom, unsustainable_ratio, total_areas) %>%
         unique() %>%
         mutate(ESPECE_nom = reorder(ESPECE_nom, -total_areas)), # reorder by descending total_areas
       aes(x = ESPECE_nom, y = unsustainable_ratio)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Harvesting Areas per Species",
    x = "Species",
    y = "Number of Harvesting Areas"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


ggplot(data_enjeux_summary, aes(x = ESPECE_nom, y = area_count, fill = sustainability)) +
  geom_col(position = "stack") +
  labs(
    title = "Sustainable vs Unsustainable Harvesting Areas per Species",
    x = "Species",
    y = "Number of Harvesting Areas"
  ) +
  scale_fill_manual(values = c("Sustainable" = "seagreen3", "Unsustainable" = "tomato")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


#### Harvesting issues ####
# For each species, for each harvesting area, get the ratio of unsustainable citations
# When there are less than 2 answers in the harvesting area, write "DD"
# When the species is absent from the area, write "Abs

data_enjeux_2 <- data_enjeux %>%
  dplyr::select(CD_REF, ESPECE_nom, massif, n_present, total_answers_all, non_durable_ratio1) %>%
  mutate(sustainability = ifelse(non_durable_ratio1 > 50, "Unsustainable", "Sustainable"),
         sustainability = ifelse(non_durable_ratio1 == "Abs", "Absent", sustainability),
         non_durable_ratio1 = ifelse(total_answers_all<3, "DD", non_durable_ratio1),
         non_durable_ratio1 = ifelse(n_present==0, "Abs", non_durable_ratio1),
         sustainability = ifelse(non_durable_ratio1 == "DD", "Data deficient", sustainability)) %>%
  group_by(ESPECE_nom, sustainability) %>%
  summarise(
    area_count = n_distinct(massif),
    citations = sum(total_answers_all),
    .groups = "drop"
  )

# Step 2: compute unsustainable ratio per species for ordering
unsustainable_ratio_2 <- data_enjeux_2 %>%
  group_by(ESPECE_nom) %>%
  summarise(
    total_unsustainable = sum(area_count[sustainability == "Unsustainable"], na.rm = TRUE),
    total_sustainable = sum(area_count[sustainability == "Sustainable"], na.rm = TRUE),
    total_areas_enough_cited = total_unsustainable+total_sustainable,
    total_areas_presence = sum(area_count[!is.na(sustainability)], na.rm = TRUE)) %>%
  mutate(unsustainable_ratio = total_unsustainable / total_areas_enough_cited)

# Step 3: join back and order factor levels
data_enjeux_2_summary <- data_enjeux_2 %>%
  left_join(unsustainable_ratio_2 %>% 
              dplyr::select(ESPECE_nom, unsustainable_ratio, 
                            total_unsustainable, total_sustainable,
                            total_areas_enough_cited, 
                            total_areas_presence), by = "ESPECE_nom")


massif_levels <- c(
  "Alpes", "Pyrénées", "Jura-Alpes Nord", "Massif Central",
  "Méditerranée", "Massif Corse",
  "Bassin Parisien Nord", "Bassin Parisien Sud", "Nord-Est", "Massif Armoricain", "Sud-Ouest"
)

data_enjeux_2_summary %>%
  # Keep only relevant sustainability statuses
  filter(sustainability %in% c("Sustainable", "Unsustainable", "Data deficient")) %>%
  na.omit() %>%
  # Remove species that only have Data deficient areas
  group_by(ESPECE_nom) %>%
  filter(any(sustainability != "Data deficient")) %>%
  ungroup() %>%
  # Reorder species by unsustainable_ratio
  mutate(ESPECE_nom = fct_reorder(ESPECE_nom, unsustainable_ratio, .desc = TRUE)) %>%
  ggplot(aes(x = ESPECE_nom, y = area_count, fill = sustainability)) +
  geom_col(position = "stack") +
  labs(
    title = "Sustainable vs Unsustainable Harvesting Areas per Species",
    x = "Species",
    y = "Number of Harvesting Areas"
  ) +
  scale_fill_manual(values = c("Sustainable" = "seagreen3", 
                               "Unsustainable" = "tomato", 
                               "Data deficient" = "grey")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

### Identifying local issues ####
# Define color palettes for harvesting areas
colors_montagne <- c("#4d004b", "#810f7c", "#88419d", "#8c6bb1")   # Mountain areas
colors_med_corse <- c("#FD8D3C", "#fdd0a2")                         # Mediterranean & Corsica
colors_plaine   <- c("#023858", "#045a8d", "#0570b0", "#3690c0", "#74a9cf") # Lowland areas

harvesting_area_levels <- c(
  "Alps", "Pyrenees", "Jura–Northern Alps", "Central Massif",
  "Mediterranean", "Corsica",
  "Northern Paris Basin", "Southern Paris Basin", "North–East", "Armorican Massif", "South–West"
)

massif_colors <- c(colors_montagne, colors_med_corse, colors_plaine)
names(massif_colors) <- harvesting_area_levels
harvesting_area_letters <- setNames(LETTERS[1:length(harvesting_area_levels)], harvesting_area_levels)


## Prepare spatial data
departements_simpl_PARIS$harvesting_area <- factor(
  departements_simpl_PARIS$massif,
  levels = massif_levels,
  labels = harvesting_area_levels
)

departements_simpl_PARIS$area_type <- case_when(
  departements_simpl_PARIS$harvesting_area %in% colors_montagne ~ "Mountain areas",
  departements_simpl_PARIS$harvesting_area %in% colors_med_corse ~ "Mediterranean & Corsica",
  TRUE ~ "Lowland areas"
)

# Centroids for labeling each harvesting area
centroids <- departements_simpl_PARIS %>%
  group_by(harvesting_area) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  mutate(letter = harvesting_area_letters[harvesting_area])

# Boundaries of harvesting areas
harvesting_area_boundaries <- departements_simpl_PARIS %>%
  group_by(harvesting_area) %>%
  summarize(geometry = st_union(geometry))


# Map of harvesting areas
plot_harvest_areas <- ggplot() +
  geom_sf(data = departements_simpl_PARIS, aes(fill = harvesting_area), colour = "white", alpha = 0.8) +
  geom_sf(data = harvesting_area_boundaries, fill = NA, colour = "black", size = 1) +
  geom_label(data = centroids, aes(label = letter, geometry = geometry),
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


## Prepare sustainability data
data_enjeux_3 <- data_enjeux %>%
  dplyr::select(CD_REF, ESPECE_nom, massif, n_present, total_answers_all, non_durable_ratio1) %>%
  mutate(
    non_durable_flag = case_when(
      n_present == 0 ~ "Abs",
      n_present != 0 & total_answers_all == 0 ~ "ND",
      TRUE ~ "Value"
    ),
    sustainability = case_when(
      non_durable_flag == "Abs" ~ "Absent",
      non_durable_flag == "ND" ~ "No data",
      total_answers_all < 3 & non_durable_ratio1 <= 50 ~ "Sustainable (<3 citations)",
      total_answers_all < 3 & non_durable_ratio1 > 50 ~ "Unsustainable (<3 citations)",
      total_answers_all >= 3 & non_durable_ratio1 <= 50 ~ "Sustainable (≥3 citations)",
      total_answers_all >= 3 & non_durable_ratio1 > 50 ~ "Unsustainable (≥3 citations)",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(ESPECE_nom) %>%
  filter(any(sustainability %in% c("Sustainable (<3 citations)", "Unsustainable (<3 citations)",
                                   "Sustainable (≥3 citations)", "Unsustainable (≥3 citations)"))) %>%
  ungroup() %>%
  mutate(
    massif = factor(massif, levels = massif_levels, labels = harvesting_area_levels),
    massif_letter = harvesting_area_letters[massif]
  )

# Order species by number of unsustainable areas
species_order <- data_enjeux_3 %>%
  group_by(ESPECE_nom) %>%
  filter(any(sustainability == "Unsustainable (≥3 citations)")) %>%
  summarize(n_unsustainable = sum(sustainability == "Unsustainable (≥3 citations)")) %>%
  arrange(n_unsustainable) %>%
  pull(ESPECE_nom)

data_enjeux_3_ordered <- data_enjeux_3 %>%
  filter(ESPECE_nom %in% species_order) %>%
  mutate(ESPECE_nom = factor(ESPECE_nom, levels = species_order),
         x_pos = as.numeric(factor(massif, levels = harvesting_area_levels)))

# Introduce gaps in x-axis after certain harvesting areas
gap_positions <- c(4, 6)
gap_size <- 0.5
data_enjeux_3_ordered <- data_enjeux_3_ordered %>%
  mutate(x_pos_gap = x_pos + sapply(x_pos, function(x) sum(x > gap_positions) * gap_size))

# Prepare letter labels for harvesting areas
max_y <- nlevels(data_enjeux_3_ordered$ESPECE_nom)
label_df <- data.frame(
  x = 1:length(harvesting_area_levels),
  letter = harvesting_area_letters,
  y = max_y + 0.5
) %>%
  mutate(x_gap = x + sapply(x, function(x) sum(x > gap_positions) * gap_size))

# Colored rectangles to group harvesting areas
group_df <- data.frame(
  xmin = c(0.5, 4.5, 6.5),
  xmax = c(4.001, 6.001, 11.5),
  y = max_y + 0.5,
  group_color = c("#4d004b", "#FD8D3C", "#023858")
) %>%
  mutate(
    xmin = xmin + sapply(xmin, function(x) sum(x > gap_positions) * gap_size),
    xmax = xmax + sapply(xmax, function(x) sum(x > gap_positions) * gap_size)
  )

group_labels <- data.frame(
  label = c("Mountains", "Mediterranean", "Lowlands"),
  x = (group_df$xmin + group_df$xmax) / 2,
  y = max_y + 2
)


## Plot sustainability heatmap
plot_enjeux_custom <- ggplot(data_enjeux_3_ordered, aes(x = x_pos_gap, y = ESPECE_nom)) +
  geom_rect(data = group_df, aes(xmin = xmin, xmax = xmax, ymin = y + 0.1, ymax = y + 1.1, fill = group_color),
            color = NA, inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_identity() +
  geom_tile(fill = "white", colour = "grey80", alpha = 0.6) +
  new_scale_fill() +
  geom_tile(aes(fill = sustainability), width = 0.6, height = 0.6, colour = "black") +
  scale_fill_manual(
    name = "Sustainability",
    values = c(
      "Sustainable (≥3 citations)" = "#006400",
      "Sustainable (<3 citations)" = "#CCFFCC",
      "Unsustainable (≥3 citations)" = "#8B0000",
      "Unsustainable (<3 citations)" = "#FFCCCC",
      "Absent" = NA,
      "No data" = "#D3D3D3"
    ),
    na.value = "transparent"
  ) +
  geom_tile(data = label_df, aes(x = x_gap, y = y + 0.6), width = 0.6, height = 0.6,
            fill = "white", color = "black", inherit.aes = FALSE) +
  geom_text(data = label_df, aes(x = x_gap, y = y + 0.6, label = letter), fontface = "bold", inherit.aes = FALSE) +
  geom_text(data = group_labels, aes(x = x, y = y, label = label), fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.25)))


## Display combined plots
plot_enjeux_custom + plot_harvest_areas


# plot_zoom_png?width=1945&height=814
png("local_issues.png", 
    width = 5835,     # pixels
    height = 2442,   # pixels
    res = 300)        # resolution in dpi
plot_enjeux_custom + plot_harvest_areas
dev.off()







##########################

length(data_enjeux_general$non_durable_ratio1[data_enjeux_general$non_durable_ratio1<50])
length(data_enjeux_general$non_durable_ratio1[data_enjeux_general$non_durable_ratio1>=50])


#### Plot ####
data_enjeux_general_plot <- function(data, ratio, xlab, top_n_species = 10) {
  # 
  # data <- data %>%
  #   # Reorder species by ratio for plotting
  #   arrange({{ ratio }}, answer_coverage_dpt, total_answers_all) %>%
  #   mutate(ESPECE_nom = factor(ESPECE_nom, levels = unique(ESPECE_nom)))
  
  # Select top N species by ratio
  data <- data %>%
    # Keep only top N species with highest ratio
    arrange(desc({{ ratio }}), desc(answer_coverage_dpt), desc(total_answers_all)) %>%
    slice_head(n = top_n_species) %>%
    # Reorder factor for plotting
    mutate(ESPECE_nom = factor(ESPECE_nom, levels = rev(ESPECE_nom)))
  
  
  ggplot(data, aes(x = {{ ratio }},
                   y = ESPECE_nom,
                   colour = answer_coverage_dpt,
                   size = total_answers_all)) +
    geom_point() +
    # geom_text(aes(label = total_answers_all),
    # geom_text(aes(label = answer_coverage_dpt),
    #           hjust = 0, vjust = 0.5, size = 3) +
    scale_color_gradient(
      low = "#fc9272",
      high = "#67000d"
    ) +
    labs(
      # title = "Proportion of unsustainable assessments across species",
      x = xlab,
      y = "",
      size = "Total number of citations",
      colour = "Geographical response coverage\n(% of departments within species’ range)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 9),
      legend.position = "right"
    )
}

# data_enjeux_general_filter <- data_enjeux_general %>%
#   # Keep only the species that have been cited at least twice
#   filter(total_answers_all > 2, answer_coverage_dpt>10)
# 
# data_enjeux_general_plot(data_enjeux_general_filter, non_durable_ratio1, 
#                          xlab="'Unsustainable' citations (%)", 100)
# data_enjeux_general_plot(data_enjeux_general_filter, non_durable_ratio2, 
#                          xlab="'Unsustainable' citations (%)", 20)


# plot_zoom_png?width=730&height=421
png("enjeux_general.png", 
    width = 2190,     # pixels
    height = 1263,   # pixels
    res = 300)        # resolution in dpi

# Your plot code
data_enjeux_general_plot(data_enjeux_general, non_durable_ratio1, 
                         xlab="'Unsustainable' citations (%)", 40)
# Close the device
dev.off()



prop_over_50_non_sust <- length(data_enjeux_general$ESPECE_nom[data_enjeux_general$non_durable_ratio1>50]) /
  length(data_enjeux_general$ESPECE_nom)


data_enjeux_panel_plot <- function(data, ratio, xlab) {
  data <- data %>%
    # Keep only the species that have been cited at least twice in the harvesting area
    filter(total_answers_all > 2) %>%
    # Reorder species for better plotting
    group_by(massif) %>%
    arrange({{ ratio }}, answer_coverage, total_answers_all) %>%
    mutate(order_id = row_number()) %>%
    # mutate(ESPECE_nom = factor(ESPECE_nom, levels = unique(ESPECE_nom))) %>%
    ungroup()
  
  ggplot(data, aes(x = {{ ratio }},  
                   y = reorder_within(ESPECE_nom, order_id, massif),
                   size = total_answers_all)) +
    
    geom_point() +
    
    labs(
      title = "Proportion of unsustainable assessments across species and harvesting areas",
      x = xlab,
      y = "Species names",
      size = "Total number of citations",
    ) +
    
    facet_wrap(~massif, scales = "free_y") +
    scale_y_reordered() +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_text(size = 9),
      legend.position = "right"
    )
}

data_enjeux_panel_plot(data_enjeux, non_durable_ratio1, 
                       xlab="'Unsustainable responses' per harvesting area (%)")
data_enjeux_panel_plot(data_enjeux, non_durable_ratio2,
                       xlab="Departements with ≥1 'unsustainable' response (%)")