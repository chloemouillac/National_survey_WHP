#### Load packages ####
library(dplyr)
library(ggplot2)
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

#### Import data ####
raw_data <- read.csv("raw_data/results-survey676868_prefilter.csv")

massifs <- read.csv("massifs_cueillette.csv")

departements <- read_sf("raw_data/departements_detail_paris.shp") %>%
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

raunkieaer <- read.csv("processed_data/type_bio.csv") %>%
  dplyr::select(CD_REF, choix_type_bio) %>%
  filter(!is.na(choix_type_bio))

csr <- read.csv("processed_data/CSR_clean.csv") %>%
  dplyr::select(CD_REF, C, S, R, strategy_class) %>%
  mutate(csr_simple = str_split_i(strategy_class, "/", 1)) %>%
  dplyr::select(-strategy_class)



#### Visualise biogeographical clusters of departements ####
clusters <- read.csv("raw_data/7clusters_chisq_no_transfo_20.csv")

departements_simpl_PARIS <- departements %>%
  # Create a grouping variable: "IDF" for Ile-de-France, and the original "code" for others
  mutate(group = if_else(region == "Ile-de-France", "IDF", code)) %>%
  # Group the data by the new "group" variable
  group_by(group) %>%
  # Summarise the data by fusing geometries and assigning new values for Ile-de-France
  summarize(
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

departements_clus <- full_join(departements_simpl_PARIS, clusters, 
                               by=join_by("code"=="departement"))

departements_clus$clus_ward_cut <- as.factor(departements_clus$clus_ward_cut)


plot_clus <- ggplot() +
  geom_sf(data = departements_clus, 
          aes(fill=clus_ward_cut), colour="white")+
  ggtitle(label="Clustering") +
  scale_fill_manual(name="Cluster", values=c('#117733','#CC6677','#44AA99','#DDCC77','#332288','#88CCEE', '#882255','#AA4499',"black",'#999933')) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

plot_clus


#### Visualise harvesting areas ####
plot_massifs <- ggplot() +
  geom_sf(data = departements_simpl_PARIS, 
          aes(fill=massif), colour="white")+
  ggtitle(label="Massifs de cueillette") +
  # scale_fill_brewer() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

plot_massifs

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
        !(group %in% c("3", "4")) & qcode == "23" ~ "reglem_adaptee"),
      
      
      # Determine durability status based on group
      durabilite = case_when(
        group %in% c("3", "4") ~ ifelse(group == "3", "Durable", "Non durable"),
        !(group %in% c("3", "4")) ~ ifelse(value == "Oui", "Durable", "Non durable")
      )
    ) %>%
    
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
    str_detect(., "augmente") ~ "Augmente",
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
  
  # Add clusters
  left_join(departements_clus %>% 
              st_drop_geometry() %>%
              dplyr::select(dpt_simple, clus_ward_cut),
            by = c("ESPECE_dpt" = "dpt_simple")) %>%
  mutate(biogeo_cluster = clus_ward_cut) %>%
  dplyr::select(-clus_ward_cut) %>%
  
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
  
  # Add taxref, Raunkiaer, CSR, species presence (cover %) per département
  left_join(taxref, by = c("ESPECE_nom" = "LB_NOM")) %>%
  left_join(raunkieaer, by = "CD_REF") %>%
  left_join(csr, by = "CD_REF") %>%
  left_join(all_rarity, by = join_by("CD_REF", "ESPECE_dpt"=="dpt_simple")) %>%
  dplyr::select(-dpt_name) %>%
  
  # Convert to factors (excluding key cols)
  mutate(across(-c(id, ESPECE_nom, CD_REF, C, S, R, sp_relative_area), as.factor))


#### Bar Plot of all cited species (only >1 citation) ####
df_species <- all_data %>%
  dplyr::select(id, ESPECE_nom) %>%
  na.omit() %>%
  distinct() %>%
  count(ESPECE_nom, name = "citations") %>%
  filter(citations > 1)

ggplot(df_species, aes(y = fct_reorder(ESPECE_nom, citations), x = citations)) +
  geom_col() +
  labs(
    title = "Amount of citations per species",
    subtitle = paste("Showing only species cited more than once (", nrow(df_species), "of 172 species )"),
    x = "Citation count",
    y = "Species name"
  ) +
  theme_minimal()


# 172 different cited species


#### Species durability diverging plot ####

plot_durability_diverging <- function(data, n = 10) {
  
  # Top n species by total citations
  top_species <- data %>%
    count(ESPECE_nom, PROFIL_statut_cueilleur, name = "cit") %>%
    filter(!is.na(PROFIL_statut_cueilleur)) %>%
    count(ESPECE_nom, name = "total_citations") %>%
    slice_max(total_citations, n = n) %>%
    pull(ESPECE_nom)
  
  # Summarise counts + ratio
  df_summary <- data %>%
    filter(ESPECE_nom %in% top_species) %>%
    distinct(id, ESPECE_nom, ESPECE_durabilite) %>%
    count(ESPECE_nom, ESPECE_durabilite) %>%
    mutate(
      count_signed = ifelse(ESPECE_durabilite == "Non durable", -n, n),  # for plotting
      count = n                                                          # for labels
    ) %>%
    group_by(ESPECE_nom) %>%
    mutate(ratio_non_durable = sum(n[ESPECE_durabilite == "Non durable"], na.rm = TRUE) / sum(n),
           count_non_durable = sum(n[ESPECE_durabilite == "Non durable"], na.rm = TRUE),
           count_durable = sum(n[ESPECE_durabilite == "Durable"], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(ratio_non_durable, count_non_durable, -count_durable) %>%
    mutate(ESPECE_nom = factor(ESPECE_nom, levels = unique(ESPECE_nom)))
  
  # Plot
  ggplot(df_summary, aes(x = ESPECE_nom, y = count_signed, fill = ESPECE_durabilite)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_y_continuous(
      labels = abs,  # show only positive values
      breaks = scales::pretty_breaks(n = 8)
    ) +
    scale_fill_manual(
      values = c("Durable" = "darkgreen", "Non durable" = "red"),
      labels = c("Durable" = "Sustainable", "Non durable" = "Non sustainable")
    ) +
    labs(
      title = paste("Perceived harvesting sustainability across the", n, "most cited species"),
      subtitle = "Species ordered by decreasing ratio of non-sustainable citations",
      x = "Species name", 
      y = "Number of citations", 
      fill = "Perceived harvesting sustainability"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(hjust = 0.5)
    )
}
plot_durability_diverging(all_data %>% filter(!is.na(ESPECE_nom)), n = 40)



#### Cumulative frequency plot of species ####
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


#### Overview of harvesting issues in France ####
data_enjeux <- all_rarity %>%
  full_join(all_data %>% dplyr::select(-sp_relative_area), by="CD_REF") %>%
  dplyr::select(CD_REF, id, ESPECE_nom, ESPECE_presence, ESPECE_durabilite, ESPECE_dpt,
                dpt_name, dpt_simple, sp_relative_area) %>%
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
  group_by(CD_REF, ESPECE_nom, dpt_name, sp_relative_area) %>%
  summarise(n_answers_Non_durable = sum(n_answers_Non_durable),
            n_answers_Durable = sum(n_answers_Durable)) %>%
  mutate(proportion_non_durable = ifelse(n_answers_Non_durable==0, NA, 
                                         100* n_answers_Non_durable/
                                           (n_answers_Non_durable+n_answers_Durable)),
         proportion_non_durable = ifelse(n_answers_Non_durable==0 & 
                                           n_answers_Durable>0, 0, 
                                         proportion_non_durable),
         proportion_non_durable = round(proportion_non_durable,1))




data_enjeux_filtered <- data_enjeux %>%
  group_by(ESPECE_nom) %>%
  mutate(
    median_non_durable = median(proportion_non_durable, na.rm = TRUE),
    mean_non_durable = mean(proportion_non_durable, na.rm = TRUE),
    max_non_durable = max(proportion_non_durable, na.rm = TRUE),
    
    # Number of departments with non-durable answers
    num_non_durable_dpts = sum(n_answers_Non_durable > 0, na.rm = TRUE),
    
    # Number of departments with durable answers
    num_durable_dpts = sum(n_answers_Durable > 0, na.rm = TRUE),
    
    # Number of departments where the species is present
    num_present_dpts = sum(sp_relative_area > 0, na.rm = TRUE),
    
    # Number of departments with at least one answer (durable or non-durable)
    num_dpts_with_any_answer = sum((n_answers_Non_durable > 0) | (n_answers_Durable > 0), na.rm = TRUE),
    
    # Answer coverage ratio (Number of departments where the species is present and has at least one answer)
    answer_coverage = 100 * sum(sp_relative_area > 0 & 
                                  ((n_answers_Non_durable > 0) | (n_answers_Durable > 0)),
                                na.rm = TRUE) / num_present_dpts,
    
    # Total number of durable answers
    total_answers_durable = sum(n_answers_Durable, na.rm = TRUE),
    
    # Total number of non-durable answers
    total_answers_non_durable = sum(n_answers_Non_durable, na.rm = TRUE),
    
    # Total number of all answers (durable + non-durable)
    total_answers_all = sum(n_answers_Durable, na.rm = TRUE) + 
      sum(n_answers_Non_durable, na.rm = TRUE)) %>%
  
  ungroup() %>%
  
  # Keep only the species with at least 3 citations
  filter(total_answers_all>2) %>%
  
  # Calculate the ratio for the graph's x-axis
  mutate(
    non_durable_ratio1 = 100 * num_non_durable_dpts / num_dpts_with_any_answer,
    non_durable_ratio2 = 100 * num_non_durable_dpts / num_present_dpts,
    durable_ratio1 = 100 * num_durable_dpts / num_dpts_with_any_answer,
    durable_ratio2 = 100 * num_durable_dpts / num_present_dpts
  ) %>%
  # 1. Arrange the dataframe by your desired order (first by answer_ratio, then by non_durable_ratio)
  arrange(non_durable_ratio1, answer_coverage) %>%
  # 2. Reorder the factor levels to match the new row order
  # This is the crucial step to ensure the plot is ordered correctly
  mutate(ESPECE_nom = factor(ESPECE_nom, levels = unique(ESPECE_nom))) %>%
  
  na.omit() %>%
  
  left_join(departements_clus %>% dplyr::select(dpt, clus_ward_cut),
            join_by("dpt_name"=="dpt")) %>%
  st_drop_geometry()




ggplot(data_enjeux_filtered, aes(x = non_durable_ratio1,
                                 y = ESPECE_nom,
                                 colour = answer_coverage,
                                 size = total_answers_all)) +
  
  geom_point() +
  
  scale_color_gradient(
    low = "#fc9272",   # medium red
    high = "#67000d"   # dark red
  )+
  
  labs(
    title = "Share of 'unsustainable' responses by species",
    x = "Departments with ≥1 'unsustainable' response (%)",
    y = "Species names",
    size = "Total number of citations",
    colour = "Answer coverage (% of departments\nwith species that provided citations)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 9),
    legend.position = "right"
  )
