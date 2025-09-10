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

#### Import data ####
raw_data <- read.csv("raw_data/results-survey676868_prefilter.csv")

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
  )

taxref <-  read.delim("raw_data/TAXREF_v17_2024/TAXREFv17.txt") %>%
  subset((GROUP1_INPN=="Trachéophytes") &
           (REGNE=="Plantae") &
           # (CD_REF==CD_NOM) &
           (RANG=="ES"),
         select=c(CD_REF, CD_NOM, LB_NOM)) %>%
  unique()

all_rarity <-  read.csv("raw_data/OpenObs+GBIF_RARITY_20km.csv")

raunkieaer <- read.csv("raw_data/Raunkieaer_data_REVIEWED_WHP.csv") %>%
  select(CD_REF, choix_type_bio) %>%
  unique()
  

#### Prepare main data frame ####

# A single, comprehensive data processing pipeline to avoid redundancy

process_data_helper_1 <- function(data, pattern_one, pattern_two, value_col_name) {
  data %>%
    select(id = id__ID_de_la_reponse, 
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
    select(id, group, !!sym(value_col_name))
}

process_data_helper_2 <- function(data, pattern_one, pattern_two, value_col_name) {
  data %>%
    select(id = id__ID_de_la_reponse, 
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
    
    select(id, group, matches(value_col_name)) %>%
    
    # Step to remove columns that are entirely NA
    select(where(~ any(!is.na(.)))) %>%
    
    # Combine multiple rows with the same identifier into a single row
    group_by(id) %>%
    summarise(across(everything(), ~first(na.omit(.))))
  
}

process_all_species_data <- function(data) {
  # Columns related to species, departments, and harvester type
  species_cols <- data %>%
    select(id__ID_de_la_reponse,
           matches("^G(?:3|4|6|8|10|12|14|16|18|20)Q000(?:0)?(?:1|2|4|5|6|7|8|9|10|11|12|13|14|18|19|20|21|22|23|24)_"),
           # careful of the number of zeroes after Q
           starts_with("GsuppQsupp_")) # GsuppQsupp__Connaissez_vous_une_espece_a_la_cueillette_non_durable
  
  df_long <- data %>%
    # Select all necessary columns at once
    select(id = id__ID_de_la_reponse,
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
        qcode == "1" ~ "species",
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
    select(-qcode) %>%
    
    # Pivot wider to get species, presence, and etendue_cueill in columns
    pivot_wider(
      names_from = type,
      values_from = value,
      values_fn = list
    ) %>%
    
    # Clean and process species and type_cueilleur
    separate_rows(species, sep = ",\\s*") %>%
    mutate(
      species = str_trim(species),
      dpt = str_trim(str_remove(dpt, "\\s*\\([0-9AB]+\\)"))
    ) %>%
    
    unnest(everything()) %>%
    
    # Filter out empty species
    filter(species != "") %>%
    
    # Keep only one record per ID, species, and group to avoid double-counting
    unique() %>%
    
    # Group Ile de France departments
    mutate(
      dpt = ifelse(
        dpt %in% c("Essonne", "Hauts-de-Seine", "Paris", "Seine-Saint-Denis", "Seine-et-Marne", "Val-d'Oise", "Val-de-Marne", "Yvelines"),
        "Ile-de-France",
        dpt
      )
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
    filter(species != "")

  
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
    select(id__ID_de_la_reponse,
           matches("G21")) # careful of the number of zeroes after Q
  
  df_long <- data %>%
    # Select all necessary columns at once
    select(id = id__ID_de_la_reponse,
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
    select(-qcode) %>%
    
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
    
    select(-group) 
  
  return(df_final)
}

df_all_profile_data <-  process_all_profile_data(raw_data)


#### Multivariate : plant species profiles ####
##### Prepare data ####
# Add prefixes to columns (except id)
df_profile_renamed <- df_all_profile_data %>%
  rename_with(~ paste0("PROFIL_", .), -id)

df_species_renamed <- df_all_species_data %>%
  rename_with(~ paste0("ESPECE_", .), -id)

# Full join
all_data <- full_join(df_profile_renamed, df_species_renamed, by = "id")

# Standardise text categories in one go
all_data <- all_data %>%
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
    ESPECE_mode = recode(ESPECE_mode, "Oui" = "Mode", "Non" = "Pas de mode"),
    ESPECE_mode_risque = recode(ESPECE_mode_risque,
                                "Oui" = "Mode et risque",
                                "Non" = "Mode sans risque"),
    ESPECE_espece_reglem = recode(ESPECE_espece_reglem,
                                  "Oui" = "Oui",
                                  "Non" = "Non"),
    PROFIL_statut_cueilleur = case_when(
      str_detect(PROFIL_statut_cueilleur, "loisir")   ~ "Amateur",
      str_detect(PROFIL_statut_cueilleur, "revenus")  ~ "Professionnel",
      TRUE ~ PROFIL_statut_cueilleur
    ),
    # Replace NA in PROFIL_nb_esp_cueill with 0
    PROFIL_nb_esp_cueill = if_else(is.na(PROFIL_nb_esp_cueill), "0", PROFIL_nb_esp_cueill)
  ) %>%
  
  # Apply recoding for variable/stable/etc. patterns
  mutate(across(-id, ~ case_when(
    str_detect(., "variable") ~ "Variable",
    str_detect(., "stable")   ~ "Stable",
    str_detect(., "regress")  ~ "Diminue",
    str_detect(., "augmente") ~ "Augmente",
    TRUE ~ .
  ))) %>%
  
  # Adjust ESPECE_mode with ESPECE_mode_risque, drop redundant col
  mutate(ESPECE_mode = if_else(ESPECE_mode == "Mode", ESPECE_mode_risque, ESPECE_mode)) %>%
  select(-ESPECE_mode_risque) %>%
  
  # Cueilleur status refinement
  mutate(PROFIL_statut_cueilleur = if_else(PROFIL_cueilleur == "Oui",
                                           PROFIL_statut_cueilleur,
                                           "Non cueilleur")) %>%
  select(-PROFIL_cueilleur) %>%
  
  # Clean text
  mutate(across(everything(), ~ str_replace(.x, "Je ne sais pas", "JNSP")),
         across(everything(), ~ str_replace(.x, "\\(.*\\)", ""))) %>%
  
  # More condensed replacements
  mutate(
    ESPECE_presence = recode(ESPECE_presence,
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
  left_join(taxref, join_by("ESPECE_species"=="LB_NOM"))


# ACM data prep
all_data_ACM_species_profile <- all_data %>%
  select(
    id, ESPECE_species,
    ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
    ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
    ESPECE_usages, ESPECE_parties_cueill, ESPECE_mode, ESPECE_espece_reglem, ESPECE_debut_obs
  ) %>%
  drop_na() %>%
  mutate(ESPECE_parties_cueill_grp = case_when(
    ESPECE_parties_cueill %in% c("Feuilles", "Bourgeons", "Partie aerienne", "Jeunes pousses") ~ "Partie_aerienne",
    ESPECE_parties_cueill %in% c("Fleurs", "Fruits", "Graines") ~ "Partie_reproductive", # grouper simplement en partie aérienne ?
    ESPECE_parties_cueill %in% c("Ecorce", "Seve") ~ "Autre",
    ESPECE_parties_cueill %in% c("Plante entiere") ~ "Plante_entiere",
    ESPECE_parties_cueill %in% c("Partie souterraine") ~ "Partie_souterraine"
  )) %>%
  select(-ESPECE_parties_cueill) %>%
  unique() %>%
  mutate(presence = "Oui") %>%
  pivot_wider(
    names_from = ESPECE_parties_cueill_grp,
    values_from = presence,
    names_prefix = "ESPECE_parties_cueill_",
    values_fill = "Non"
  ) %>%
  mutate(across(-c(id, ESPECE_species), as.factor)) %>%
  unique() %>%
  na.omit()


##### Perform MCA ####
# Keep a reference table for IDs and species
ref_table_species <- all_data_ACM_species_profile %>% select(id, ESPECE_species)

# Perform MCA without id and species
res.mca.species <- MCA(all_data_ACM_species_profile %>% select(-id, -ESPECE_species),
               graph = FALSE)

plot(res.mca.species)
plot(res.mca.species,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plotellipses(res.mca.species)

##### Extract coordinates for each individual ####
# Get the coordinates of individuals (rows)
ind_coords_species <- as.data.frame(res.mca.species$ind$coord)
# Add id and species back to the MCA coordinates
ind_coords_species <- bind_cols(ref_table_species, ind_coords_species)
# Now we have a table linking each row's MCA coordinates to id and species
head(ind_coords_species)

##### Perform hierarchical clustering based on the coordinates ####
res.hcpc.species <- HCPC(res.mca.species, nb.clust = -1, graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal
print(res.hcpc.species$desc.var)

##### Plot clusters ####
plot_data_species <- data.frame(
  id = rownames(res.mca.species$ind$coord),
  Dim.1 = res.mca.species$ind$coord[,1],
  Dim.2 = res.mca.species$ind$coord[,2],
  Cluster = as.factor(res.hcpc.species$data.clust$clust) # Le cluster est dans l'objet HCPC
)

plot_MCA_species <- ggplot(plot_data_species, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  geom_point(alpha = 0.8, size = 3) +
  stat_ellipse(aes(group = Cluster), type = "norm", geom = "polygon", alpha = 0.2, linetype = "dashed") +
  labs(
    title = "ACM des profils d'espèces et classification en clusters",
    subtitle = paste0("Variance expliquée par Dim.1 et Dim.2 : ", 
                      round(res.mca.species$eig[1,2], 1), "% et ", 
                      round(res.mca.species$eig[2,2], 1), "%"),
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") # Utilisation d'une palette de couleurs plus lisible

print(plot_MCA_species)


##### Results ####
# 
# Cluster 1 : Les espèces perçues comme durables
# Ce cluster, avec 588 réponses, regroupe des espèces que les participants considèrent comme durables. C'est le cluster le plus représenté. Les caractéristiques dominantes de ces espèces sont :
# 
# Durabilité : 96,7 % sont perçues comme durables, ce qui est une sur-représentation massive par rapport à la moyenne globale de 68,4 %.
# État de la ressource : La ressource est perçue comme stable pour 74 % des espèces de ce groupe, contre 47,9 % globalement.
# Présence et étendue : 56 % des espèces sont perçues comme ayant une présence abondante et large, et l'étendue de la cueillette est perçue comme généralisée dans 81,5 % des cas.
# Intensité et variation de prélèvement : L'intensité de prélèvement est principalement faible (35,3 %) ou moyenne (30,3 %). Les variations sont perçues comme stables dans 43,5 % des cas.
# Effet de mode : 81,9 % ne subissent pas d'effet de mode, ce qui est très supérieur à la moyenne de 65 % globalement.
# Parties cueillies : On y cueille principalement des feuilles (48,7 %), des fruits (18,2 %), et des jeunes pousses (17,6 %).
#
# Espèces phares : Des espèces comme Urtica dioica et le Thymus vulgaris sont fortement associées à ce profil.
# 
# 
# Cluster 2 : Les espèces avec un manque d'information
# Ce cluster regroupe 197 réponses pour lesquelles les participants n'ont pas pu fournir d'information. Il met en lumière des lacunes dans les connaissances. Les caractéristiques de ce cluster sont :
#
#   Intensité de prélèvement : 67,4 % des espèces du cluster ont une intensité de prélèvement non connue (JNSP), contre 15,5 % au niveau global.
# Variation de prélèvement : 67,9 % ont une variation de prélèvement non renseignée, contre 22 % au global.
# État de la ressource : 50 % ont un état de ressource non documenté, contre 14,6 % au global.
# Étendue de cueillette : 44 % ont une étendue de cueillette inconnue, contre 12,7 % au global.
#
# Sous-représentation : Les espèces de ce cluster sont sous-représentées pour les intensités de prélèvement moyenne ou forte, un état de ressource stable ou en diminution, et une présence abondante et large.
# 
# Espèces phares : Allium ursinum est fortement représenté ici, tout comme Urtica dioica, ce qui suggère que même pour des espèces réputées communes, il y a des manques de connaissance.
# 
# 
# Cluster 3 : Les espèces perçues comme non durables
# Ce cluster de 323 réponses est caractérisé par une perception de non-durabilité de la cueillette. Les traits sur-représentés sont :
# 
#   Durabilité : 87,4 % des espèces du cluster sont considérées comme non durables, soit une sur-représentation massive par rapport à la moyenne de 31,5 %.
# État de la ressource : 77,5 % ont un état de ressource perçu comme en diminution, contre 34,5 % globalement.
# Intensité de prélèvement : 38,7 % des espèces subissent un prélèvement fort, bien supérieur à la moyenne globale de 11,9 %.
# Effet de mode : 66,2 % des espèces de ce cluster sont perçues comme subissant un effet de mode, contre 25,6 % au global.
# Présence et étendue : Une grande partie des espèces sont perçues comme rares (33,4 %) ou extrêmement rares (6,6 %). L'étendue de la cueillette est principalement localisée (43,7 %).
# Parties cueillies et usages : On y cueille des parties souterraines (18,9 %) et des fleurs (49,3 %). L'usage ornemental (17,5 %) est sur-représenté.
#
# Espèces phares : Des espèces comme Arnica montana et la Gentiana lutea sont majoritairement associées à ce profil.
# 
# Divergences de perception
# L'analyse révèle que certaines espèces sont présentes dans plusieurs clusters, indiquant des divergences de perception parmi les répondants. C'est particulièrement le cas pour Allium ursinum, qui apparaît presque également dans le Cluster 1 (perçu comme durable) et le Cluster 3 (perçu comme non durable). Cela suggère un désaccord notable sur son statut entre les participants. De même, des espèces comme Vaccinium myrtillus et Narcissus pseudonarcissus, bien que principalement perçues comme durables, apparaissent également dans le Cluster 3, soulignant des perceptions contrastées de leur durabilité.


##### Associate species to clusters ####
species_by_cluster_sorted <- all_data_ACM_species_profile %>%
  # Associate species with cluster
  mutate(Cluster = res.hcpc.species$data.clust$clust) %>%
  # Count number of responses per species per cluster
  count(ESPECE_species, Cluster, name = "n") %>%
  # Sort by species and within cluster by count
  group_by(Cluster) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup()

print(species_by_cluster_sorted, n = Inf)

# Cluster 1
# Espèces types (nombre de citations) :
# Urtica dioica (96)
# Thymus vulgaris (75)
# Allium ursinum (44)
# Sambucus nigra (41)
# Salvia rosmarinus (40)
# Taraxacum officinale (25)
# Filipendula ulmaria (23)
# Rubus fruticosus (16)
# Vaccinium myrtillus (15)

# Cluster 2
# Espèces types (nombre de citations) :
# Allium ursinum (24)
# Gentiana lutea (14)
# Thymus vulgaris (14)
# Urtica dioica (10)
# Filipendula ulmaria (8)
# Arnica montana (7)
# Sambucus nigra (6)

# Cluster 3
# Espèces types (nombre de citations) :
# Arnica montana (48)
# Allium ursinum (36)
# Gentiana lutea (19)
# Vaccinium myrtillus (18)
# Narcissus pseudonarcissus (11)
# Thymus vulgaris (10)
# Artemisia umbelliformis (9)
# Lavandula angustifolia (8)
# Artemisia genipi (7)
# Filipendula ulmaria (7)

# Espèces aux perceptions variables
# Ces espèces sont citées dans plusieurs clusters, ce qui indique que la perception de leur statut et de leur pratique de cueillette peut varier considérablement selon les personnes interrogées.

# Urtica dioica : Cluster 1 (96), Cluster 2 (10)
# Thymus vulgaris : Cluster 1 (75), Cluster 2 (14), Cluster 3 (10)
# Allium ursinum : Cluster 1 (44), Cluster 2 (24), Cluster 3 (36)
# Filipendula ulmaria : Cluster 1 (23), Cluster 2 (8), Cluster 3 (7)
# Gentiana lutea : Cluster 1 (8), Cluster 2 (14), Cluster 3 (19)
# Vaccinium myrtillus : Cluster 1 (15), Cluster 2 (3), Cluster 3 (18)
# Arnica montana : Cluster 2 (7), Cluster 3 (48)
# Taraxacum officinale : Cluster 1 (25), Cluster 2 (4)


#### Multivariate : participant profiles ####
##### Prepare data ####
all_data_ACM_participant_profile <- all_data %>%
  select(id, PROFIL_statut_cueilleur, PROFIL_type_orga_rattach,
         PROFIL_age, PROFIL_genre, PROFIL_categ_socio_pro, PROFIL_nb_esp_cueill, PROFIL_niveau_educ) %>%
  mutate(across(-id, as.factor)) %>%
  unique() %>%
  na.omit()

##### Perform MCA ####
# Keep a reference table for IDs and species
ref_table_particip <- all_data_ACM_participant_profile %>% select(id)

# Perform MCA without id and species
res.mca.particip <- MCA(all_data_ACM_participant_profile %>% select(-id),
                        graph = FALSE)

plot(res.mca.particip)
plot(res.mca.particip,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plotellipses(res.mca.particip)

##### Extract coordinates for each individual ####
# Get the coordinates of individuals (rows)
ind_coords_particip <- as.data.frame(res.mca.particip$ind$coord)

# Add id and species back to the MCA coordinates
ind_coords_particip <- bind_cols(ref_table_particip, ind_coords_particip)
# Now we have a table linking each row's MCA coordinates to id and species
head(ind_coords_particip)


##### Perform hierarchical clustering based on the coordinates ####
res.hcpc.particip <- HCPC(res.mca.particip, nb.clust = -1 , graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal
print(res.hcpc.particip$desc.var)

##### Plot clusters ####
plot_data_particip <- data.frame(
  id = rownames(res.mca.particip$ind$coord),
  Dim.1 = res.mca.particip$ind$coord[,1],
  Dim.2 = res.mca.particip$ind$coord[,2],
  Cluster = as.factor(res.hcpc.particip$data.clust$clust) # Le cluster est dans l'objet HCPC
)

plot_MCA_particip <- ggplot(plot_data_particip, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  geom_point(alpha = 0.8, size = 3) +
  stat_ellipse(aes(group = Cluster), type = "t", geom = "polygon", alpha = 0.2, linetype = "dashed") + 
  # type="t" instead of "norm" is more robust for small groups
  labs(
    title = "ACM des profils de répondants et classification en clusters",
    subtitle = paste0("Variance expliquée par Dim.1 et Dim.2 : ", 
                      round(res.mca.particip$eig[1,2], 1), "% et ", 
                      round(res.mca.particip$eig[2,2], 1), "%"),
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") # Utilisation d'une palette de couleurs plus lisible

print(plot_MCA_particip)



#### AFCM durability ####
##### Run AFCM (with durability as supplementary variable) ####
res.afcm.durability <- MCA(all_data_ACM_species_profile %>% select(-id, -ESPECE_species),
                           quali.sup = which(names(all_data_ACM_species_profile 
                                                   %>% select(-id, -ESPECE_species)) == "ESPECE_durabilite"),
                           graph = FALSE)

##### Visualise individuals (species) colored by durability ####
fviz_mca_ind(res.afcm.durability, 
             label = "none",   # hide individul labels
             habillage = all_data_ACM_species_profile$ESPECE_durabilite, # colored by durability
             addEllipses = TRUE, ellipse.type = "confidence")

##### Visualise variables ####
fviz_mca_var(res.afcm.durability, repel = TRUE) 

##### Show only well-represented modalities ####
fviz_mca_var(res.afcm.durability, repel = TRUE, select.var = list(cos2 = 0.2)) # only modalities with cos2 > 0.2 (explaining at leats 20 % variability)


#### AFCM non sutainable species known ####
##### Prepare data ####
all_data_non_durable_connue <- all_data %>%
  select(id, PROFIL_statut_cueilleur, 
         PROFIL_type_orga_rattach,
         PROFIL_age, PROFIL_genre, PROFIL_categ_socio_pro, PROFIL_nb_esp_cueill, PROFIL_niveau_educ,
         ESPECE_non_durable_connue) %>%
  unique() %>%
  mutate(across(-id, as.factor)) %>%  
  select(-id) %>%
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

##### Run AFCM (with durability as supplementary variable) ####
res.afcm.non_durable_connue <- MCA(all_data_non_durable_connue,
                                   quali.sup = which(names(all_data_non_durable_connue) == "ESPECE_non_durable_connue"),
                                   graph = FALSE)

##### Visualise individuals (species) colored by durability ####
fviz_mca_ind(res.afcm.non_durable_connue, 
             label = "none",   # hide individul labels
             habillage = all_data_non_durable_connue$ESPECE_non_durable_connue, # colored by durability
             addEllipses = TRUE, ellipse.type = "confidence")

##### Visualise variables ####
fviz_mca_var(res.afcm.non_durable_connue, repel = TRUE) 

##### Show only well-represented modalities ####
fviz_mca_var(res.afcm.non_durable_connue, repel = TRUE, select.var = list(cos2 = 0.2)) # only modalities with cos2 > 0.2 (explaining at leats 20 % variability)



#### Compute actual rarity for all species ####
all_rarity$dpt_name_simpl <- iconv(all_rarity$dpt_name, to = "ASCII//TRANSLIT")
rarity <- left_join(all_data, all_rarity, by=join_by("CD_REF", "ESPECE_dpt"=="dpt_name_simpl")) %>%
  select(id, ESPECE_species, ESPECE_presence, ESPECE_durabilite, dpt_name, sp_relative_area) %>%
  unique() %>%
  na.omit()

##### Perform the Kruskal-Wallis test #####
kruskal_result <- kruskal.test(sp_relative_area ~ ESPECE_presence, data = rarity)

# Print the results
print(kruskal_result)

# If the test is significant (p-value < 0.05), perform a post-hoc test
if (kruskal_result$p.value < 0.05) {
  print("The groups are significantly different. Performing Dunn's post-hoc test:")
  
  library(dunn.test)
  
  dunn_result <- dunn.test(rarity$sp_relative_area, rarity$ESPECE_presence, method = "bonferroni")
  print(dunn_result)
} else {
  print("The groups are not significantly different.")
}

# Store the Kruskal-Wallis p-value in a variable
p_value <- kruskal_result$p.value

# Create a text label for the p-value
p_label <- paste("Kruskal-Wallis, p =", format(p_value, digits = 3))

# Prepare the data: Ensure ESPECE_presence is a factor
ordre_presence <- c("Abondante large", "Abondante locale", "Rare", "Extremement rare", "JNSP")
rarity$ESPECE_presence <- factor(rarity$ESPECE_presence, levels = ordre_presence)

##### Perform the Dunn's test and get pairwise comparisons ####
# Note: You can also use rstatix::dunn_test() which is a more direct function.
dunn_result <- rarity %>%
  rstatix::dunn_test(sp_relative_area ~ ESPECE_presence, p.adjust.method = "bonferroni")

##### Create the plot with the p-values ####
ggplot(rarity, aes(x = ESPECE_presence, y = sp_relative_area)) +
  geom_boxplot() +
  geom_point(aes(color = ESPECE_durabilite), position = "jitter") +
  scale_color_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  stat_pvalue_manual(
    dunn_result,
    label = "p.adj.signif", # Use asterisks (*, **, ***) to show significance
    y.position = 110, # Adjust this value to control the height of the p-value labels
    hide.ns = TRUE, # Hides non-significant comparisons for a cleaner plot
    step.increase	=0.05
  ) +
  labs(
    title = "Comparaison de la perception et de la réalité de la présence des espèces",
    x = "Catégorie de présence",
    y = "% de recouvrement du département",
    color = "Durabilité"
  ) +
  theme_minimal()

accspecies_id <- read.csv("raw_data/list_species_TRY_French_Flora.csv")
csr_already_calc <- read.csv("raw_data/CSR_calculations_ALL.csv")
csr_already_calc$csr_simple <- str_split_i(csr_already_calc$strategy_class, pattern="/", i=1)





#### Grime strategy and durability ####
  # PCA data prep
all_data_ACM_species_profile_CSR <- left_join(all_data, csr_already_calc) %>%
  select(
    id, ESPECE_species,
    ESPECE_durabilite, C, S, R, csr_simple
  ) %>%  
  mutate(across(ESPECE_durabilite, as.factor)) %>%
  na.omit() %>%
  unique()

all_data_PCA_CSR <- all_data_ACM_species_profile_CSR %>% 
  select(-id, -ESPECE_species, -csr_simple)

# Run the FAMD analysis
res.pca.csr <- PCA(all_data_PCA_CSR,
                 quali.sup = which(names(all_data_PCA_CSR) == "ESPECE_durabilite"),
                 graph = FALSE)


##### Biplot ####
fviz_pca_biplot(res.pca.csr, habillage = all_data_PCA_CSR$ESPECE_durabilite,
             repel = TRUE,  label = c("quali", "var"), addEllipses = TRUE, ellipse.type = "confidence") 




#### % durable/non durable species per CSR strategy ####
csr_summary <- all_data_ACM_species_profile_CSR %>%
  group_by(csr_simple) %>%
  summarise(
    n = n(),
    non_durable = sum(ESPECE_durabilite == "Non durable"),
    prop_non_durable = non_durable / n,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci_low = binom.test(non_durable, n)$conf.int[1],
    ci_high = binom.test(non_durable, n)$conf.int[2]
  )

# Plot proportions with CI
ggplot(csr_summary, aes(x = csr_simple, y = prop_non_durable)) +
  geom_col(fill = "red") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_text(aes(label = scales::percent(prop_non_durable, accuracy = 0.1)), 
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "% Non durable species (with 95% CI)",
    x = "CSR strategy"
  ) +
  theme_minimal()


#### % durable/non durable species per Raunkiear form ####
all_data_ACM_species_profile_Raunk <- 
  left_join(all_data, raunkieaer) %>%
  select(
    id, ESPECE_species,
    ESPECE_durabilite, choix_type_bio
  ) %>%  
  mutate(across(choix_type_bio, as.factor)) %>%
  na.omit() %>%
  unique()


# Binary response
all_data_ACM_species_profile_Raunk$dur_bin <- ifelse(df$ESPECE_durabilite == "Non durable", 1, 0)

# Logistic regression
mod <- glm(dur_bin ~ choix_type_bio, data = df, family = binomial)

summary(mod)

# Odds ratios
exp(cbind(OR = coef(mod), confint(mod)))


all_data_ACM_species_profile_Raunk_summary <- all_data_ACM_species_profile_Raunk %>%
  group_by(choix_type_bio) %>%
  summarise(
    n = n(),
    non_durable = sum(dur_bin),
    prop_non_durable = non_durable / n,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci_low = binom.test(non_durable, n)$conf.int[1],
    ci_high = binom.test(non_durable, n)$conf.int[2]
  )

ggplot(all_data_ACM_species_profile_Raunk_summary, 
       aes(x = choix_type_bio, y = prop_non_durable)) +
  geom_col(fill = "red") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_text(aes(label = scales::percent(prop_non_durable, accuracy = 0.1)), 
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "% Non durable citings (with 95% CI)", 
       x = "Raunkiaer life form") +
  theme_minimal()
