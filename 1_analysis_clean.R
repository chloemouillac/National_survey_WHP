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


#### Cluster of departements ####
clusters <- read.csv("raw_data/7clusters_chisq_no_transfo_20.csv")

departements_simpl_PARIS <- departements %>%
  # Create a grouping variable: "IDF" for Ile-de-France, and the original "code" for others
  mutate(group = if_else(region == "Ile-de-France", "IDF", code)) %>%
  # Group the data by the new "group" variable
  group_by(group) %>%
  # Summarize the data by fusing geometries and assigning new values for Ile-de-France
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


#### Plot massifs ####
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



#### Multivariate : plant species profiles ####
##### Prepare data ###

# ACM data prep
all_data_ACM_species_profile <- all_data %>%
  dplyr::select(
    id, ESPECE_nom, CD_REF,
    ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
    ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
    # starts_with("ESPECE_usages_"), starts_with("ESPECE_parties_cueill_"),
    # ESPECE_debut_obs,
    # ESPECE_dpt,
    ESPECE_mode, ESPECE_espece_reglem
   ) %>%
  unique() %>%
  na.omit()

##### Perform MCA ###
# Keep a reference table for IDs and species
ref_table_species <- all_data_ACM_species_profile %>% dplyr::select(id, ESPECE_nom, CD_REF)

# Perform MCA without id and species
res.mca.species <- MCA(all_data_ACM_species_profile %>% dplyr::select(-id, -ESPECE_nom, -CD_REF),
               graph = FALSE)

plot(res.mca.species)
plot(res.mca.species,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plotellipses(res.mca.species)

##### Extract coordinates for each individual ###
# Get the coordinates of individuals (rows)
ind_coords_species <- as.data.frame(res.mca.species$ind$coord)
# Add id and species back to the MCA coordinates
ind_coords_species <- bind_cols(ref_table_species, ind_coords_species)
# Now we have a table linking each row's MCA coordinates to id and species
head(ind_coords_species)

##### Perform hierarchical clustering based on the coordinates ###
res.hcpc.species <- HCPC(res.mca.species, nb.clust = -1, graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal
print(res.hcpc.species$desc.var)

##### Plot clusters ###
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


##### Results ###
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
  count(ESPECE_nom, Cluster, name = "n") %>%
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
##### Prepare data ###
all_data_ACM_participant_profile <- all_data %>%
  dplyr::select(id, PROFIL_statut_cueilleur, PROFIL_type_orga_rattach,
         PROFIL_age, PROFIL_genre, PROFIL_categ_socio_pro, 
         PROFIL_nb_esp_cueill, PROFIL_niveau_educ) %>%
  unique() %>%
  na.omit()

##### Perform MCA ###
# Keep a reference table for IDs and species
ref_table_particip <- all_data_ACM_participant_profile %>% dplyr::select(id)

# Perform MCA without id and species
res.mca.particip <- MCA(all_data_ACM_participant_profile %>% dplyr::select(-id),
                        graph = FALSE)

plot(res.mca.particip)
plot(res.mca.particip,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plotellipses(res.mca.particip)

##### Extract coordinates for each individual ###
# Get the coordinates of individuals (rows)
ind_coords_particip <- as.data.frame(res.mca.particip$ind$coord)

# Add id and species back to the MCA coordinates
ind_coords_particip <- bind_cols(ref_table_particip, ind_coords_particip)
# Now we have a table linking each row's MCA coordinates to id and species
head(ind_coords_particip)


##### Perform hierarchical clustering based on the coordinates ###
res.hcpc.particip <- HCPC(res.mca.particip, nb.clust = -1 , graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal
print(res.hcpc.particip$desc.var)

##### Plot clusters ###
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



#### AFD durability #####
# --- Data Preparation ---
data_dfa1 <- all_data %>%
  dplyr::select(id, ESPECE_nom, CD_REF,
         ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
         ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
         starts_with("ESPECE_usages_"), starts_with("ESPECE_parties_cueill_"), 
         ESPECE_mode, ESPECE_espece_reglem, 
         # ESPECE_debut_obs,
         ESPECE_dpt) %>%
  unique() %>%
  na.omit() %>%
  # filter(ESPECE_nom=="Allium ursinum") %>%
  dplyr::select(-id, -ESPECE_nom, -CD_REF) %>%
  droplevels()

active_vars1 <- data_dfa1 %>%
  dplyr::select(-ESPECE_durabilite)

grouping_factor1 <- data_dfa1$ESPECE_durabilite

# --- Run DFA ---
res.dfa1 <- discrimin(dudi.acm(active_vars1, scannf = FALSE), 
                      fac = grouping_factor1, 
                      scannf = FALSE)

# --- Visualise Results ---
plot_data1 <- data.frame(
  score_dfa = res.dfa1$li$DS1,
  group = grouping_factor1
)

ggplot(plot_data1, aes(x = score_dfa, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Species Groups by Durability",
       x = "Discriminant Axis 1 (CS1)",
       y = "Density",
       fill = "Durability") +
  theme_minimal() +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red"))

# --- Get Contributions ---
correlations1 <- as.data.frame(res.dfa1$va)

sorted_correlations1 <- correlations1[order(abs(correlations1$CS1), decreasing = TRUE), , drop = FALSE]

top_contributors1 <- head(sorted_correlations1, 10)

print("Top 10 contributing variables for Durability analysis:")
print(top_contributors1)


#### GLM logistique pour la durabilité ####

# Variable réponse (binaire : "Durable" / "Non durable")
data_glm <- all_data %>%
  dplyr::select(id, ESPECE_nom, CD_REF,
         ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
         ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
         starts_with("ESPECE_usages_"), starts_with("ESPECE_parties_cueill_"), 
         ESPECE_mode, ESPECE_espece_reglem) %>%
  unique() %>%
  na.omit() %>%
  dplyr::select(-id, -CD_REF) %>%
  droplevels()

# Ajustement du modèle logistique
glm_durability <- glm(
  ESPECE_durabilite ~ . -ESPECE_nom, 
  data = data_glm,
  family = binomial(link = "logit")
)
summary(glm_durability)

glmm_durability <- glmer(
  ESPECE_durabilite ~ . - ESPECE_nom + (1 | ESPECE_nom),
  data = data_glm, 
  family = binomial(link = "logit")
)
summary(glmm_durability)

# Résumé du modèle
summary(glm_durability)



#### AFD mix (incl. rarity) durability #####
# --- Data Preparation ---
data_dfa_mix1 <- all_data %>%
  dplyr::select(id, ESPECE_nom, CD_REF,
         ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
         ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
         # starts_with("ESPECE_usages_"), starts_with("ESPECE_parties_cueill_"), 
         ESPECE_mode, ESPECE_espece_reglem, ESPECE_dpt,
         # ESPECE_debut_obs,
         C, S, R, choix_type_bio, sp_relative_area) %>%
  unique() %>%
  na.omit() %>%
  # filter(ESPECE_nom=="Allium ursinum") %>%
  filter(!ESPECE_nom %in% c("Allium ursinum", "Urtica dioica", "Thymus vulgaris")) %>%
  dplyr::select(-id, -ESPECE_nom, -CD_REF, -ESPECE_dpt) %>%
  droplevels()

active_vars_mix1 <- data_dfa_mix1 %>%
  dplyr::select(-ESPECE_durabilite)

grouping_factor_mix1 <- data_dfa_mix1$ESPECE_durabilite

# --- Run DFA ---
res.dfa.mix1 <- discrimin(dudi.mix(active_vars_mix1, scannf = FALSE), 
                      fac = grouping_factor_mix1, 
                      scannf = FALSE)

# --- Visualise Results ---
plot_data_mix1 <- data.frame(
  score_dfa = res.dfa.mix1$li$DS1,
  group = grouping_factor_mix1
)

ggplot(plot_data_mix1, aes(x = score_dfa, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Species Groups by Durability",
       x = "Discriminant Axis 1 (CS1)",
       y = "Density",
       fill = "Durability") +
  theme_minimal() +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red"))

# --- Get Contributions ---
correlations_mix1 <- as.data.frame(res.dfa.mix1$va)

sorted_correlations_mix1 <- correlations_mix1[order(abs(correlations_mix1$CS1), 
                                                    decreasing = TRUE), , drop = FALSE]

top_contributors_mix1 <- head(sorted_correlations_mix1, 10)

print("Top 10 contributing variables for Durability analysis:")
print(top_contributors_mix1)


# #### GLM mix ####
# 
# # Variable réponse (binaire : "Durable" / "Non durable")
# data_glm_mix <- all_data %>%
#   dplyr::select(id, ESPECE_nom, CD_REF,
#          ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill,
#          ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev,
#          # starts_with("ESPECE_usages_"), starts_with("ESPECE_parties_cueill_"), 
#          ESPECE_mode, ESPECE_espece_reglem, ESPECE_dpt,
#          # ESPECE_debut_obs,
#          C, S, R, choix_type_bio, sp_relative_area) %>%
#   unique() %>%
#   na.omit() %>%
#   # filter(ESPECE_nom=="Allium ursinum") %>%
#   # filter(!ESPECE_nom %in% c("Allium ursinum", "Urtica dioica", "Thymus vulgaris")) %>%
#   dplyr::select(-id, -CD_REF, -ESPECE_dpt) %>%
#   droplevels()
# 
# # Ajustement du modèle
# glm_mix_durability <- glm(
#   ESPECE_durabilite ~ . -ESPECE_nom, 
#   data = data_glm_mix,
#   family = binomial(link = "logit")
# )
# # Résumé du modèle
# summary(glm_mix_durability)
# 
# glmm_mix_durability <- glmer(
#   ESPECE_durabilite ~ . + (1 | ESPECE_nom),
#   data = data_glm_mix, 
#   family = binomial(link = "logit")
# )
# 
# # Résumé du modèle
# summary(glmm_mix_durability)
# 
# 
# 
# #### GLM mix ####
# 
# # Variable réponse (binaire : "Durable" / "Non durable")
# data_glm_mix <- all_data %>%
#   dplyr::select(id, ESPECE_nom, CD_REF, ESPECE_durabilite,
#                 C, S, R, choix_type_bio, sp_relative_area) %>%
#   unique() %>%
#   na.omit() %>%
#   # filter(ESPECE_nom=="Allium ursinum") %>%
#   # filter(!ESPECE_nom %in% c("Allium ursinum", "Urtica dioica", "Thymus vulgaris")) %>%
#   dplyr::select(-id, -CD_REF) %>%
#   droplevels()
# 
# # Ajustement du modèle
# glm_mix_durability <- glm(
#   ESPECE_durabilite ~ . -ESPECE_nom, 
#   data = data_glm_mix,
#   family = binomial(link = "logit")
# )
# # Résumé du modèle
# summary(glm_mix_durability)
# 
# glmm_mix_durability <- glmer(
#   ESPECE_durabilite ~ . + (1 | ESPECE_nom),
#   data = data_glm_mix, 
#   family = binomial(link = "logit")
# )
# 
# # Résumé du modèle
# summary(glmm_mix_durability)



#### AFD non sustainable species known #####
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

# --- Data Preparation ---
data_dfa2 <- all_data_non_durable_connue %>%
  droplevels()

active_vars2 <- data_dfa2 %>%
  dplyr::select(-ESPECE_non_durable_connue)

grouping_factor2 <- data_dfa2$ESPECE_non_durable_connue

# --- Run DFA ---
res.dfa2 <- discrimin(dudi.acm(active_vars2, scannf = FALSE), 
                      fac = grouping_factor2, 
                      scannf = FALSE)

# --- Visualise Results ---
plot_data2 <- data.frame(
  score_dfa = res.dfa2$li$DS1,
  group = grouping_factor2
)

ggplot(plot_data2, aes(x = score_dfa, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Groups by Knowledge of Unsustainable Species",
       x = "Discriminant Axis 1 (CS1)",
       y = "Density",
       fill = "Knowledge") +
  theme_minimal()

# --- Get Contributions ---
correlations2 <- as.data.frame(res.dfa2$va)

sorted_correlations2 <- correlations2[order(abs(correlations2$CS1), decreasing = TRUE), , drop = FALSE]

top_contributors2 <- head(sorted_correlations2, 10)

print("Top 10 contributing variables for Unsustainable Species Knowledge analysis:")
print(top_contributors2)

#### AFD durability vs Profile #####
all_data_non_durable_profil <- all_data %>%
  dplyr::select(id, ESPECE_nom, ESPECE_dpt, PROFIL_statut_cueilleur, 
                PROFIL_type_orga_rattach,
                # PROFIL_age, 
                PROFIL_categ_socio_pro, 
                PROFIL_nb_esp_cueill,
                ESPECE_durabilite) %>%
  unique() %>%
  mutate(across(-id, as.factor)) %>%  
  dplyr::select(-id) %>%
  na.omit()

ggplot(all_data_non_durable_profil, aes(x = "Total", fill = ESPECE_durabilite)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Sustainability assignement",
    x = "",
    y = "Percentage of participants",
    fill = "Species with unsustainable harvesting"
  )

# --- Data Preparation ---
data_dfa2 <- all_data_non_durable_profil %>%
  droplevels()

active_vars2 <- data_dfa2 %>%
  dplyr::select(-ESPECE_durabilite)

grouping_factor2 <- data_dfa2$ESPECE_durabilite

# --- Run DFA ---
res.dfa2 <- discrimin(dudi.acm(active_vars2, scannf = FALSE), 
                      fac = grouping_factor2, 
                      scannf = FALSE)

# --- Visualise Results ---
plot_data2 <- data.frame(
  score_dfa = res.dfa2$li$DS1,
  group = grouping_factor2
)

ggplot(plot_data2, aes(x = score_dfa, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Groups by Knowledge of Unsustainable Species",
       x = "Discriminant Axis 1 (CS1)",
       y = "Density",
       fill = "Knowledge") +
  theme_minimal()

# --- Get Contributions ---
correlations2 <- as.data.frame(res.dfa2$va)

sorted_correlations2 <- correlations2[order(abs(correlations2$CS1), decreasing = TRUE), , drop = FALSE]

top_contributors2 <- head(sorted_correlations2, 10)

print("Top 10 contributing variables for Unsustainable Species Knowledge analysis:")
print(top_contributors2)


#### Compute actual rarity for all species ####
rarity <- all_data %>%
  dplyr::select(id, ESPECE_nom, ESPECE_presence, ESPECE_durabilite, ESPECE_dpt, sp_relative_area) %>%
  unique() %>%
  na.omit()

##### Kruskal-Wallis test #####
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

##### Dunn's test and get pairwise comparisons ####
# Note: You can also use rstatix::dunn_test() which is a more direct function.
dunn_result <- rarity %>%
  rstatix::dunn_test(sp_relative_area ~ ESPECE_presence, p.adjust.method = "bonferroni")

##### Plot ####
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




#### Grime strategy and durability ####
  # PCA data prep
all_data_ACM_species_profile_CSR <- all_data %>%
  dplyr::select(
    id, ESPECE_nom,
    ESPECE_durabilite, C, S, R, csr_simple
  ) %>%  
  na.omit() %>%
  unique()

all_data_PCA_CSR <- all_data_ACM_species_profile_CSR %>% 
  dplyr::select(-id, -ESPECE_nom, -csr_simple)

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
  dplyr::select(
    id, ESPECE_nom,
    ESPECE_durabilite, choix_type_bio
  ) %>%  
  mutate(across(choix_type_bio, as.factor)) %>%
  na.omit() %>%
  unique()


# Binary response
all_data_ACM_species_profile_Raunk$dur_bin <- ifelse(
  all_data_ACM_species_profile_Raunk$ESPECE_durabilite == "Non durable", 1, 0)

# Logistic regression
mod <- glm(dur_bin ~ choix_type_bio, data = all_data_ACM_species_profile_Raunk, family = binomial)

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



#### Enjeux par département ####
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




ggplot(data_enjeux_filtered, aes(x = ESPECE_nom,
                                 y = proportion_non_durable)) +
 
  geom_boxplot(fill = "grey90", outlier.alpha = 0.3) +
  
  geom_point(aes(x = ESPECE_nom, 
                 y = proportion_non_durable),
             alpha = 0.1) +
  
  geom_point(aes(x = ESPECE_nom, 
                 y = non_durable_ratio1,
                 colour = answer_coverage,
                 size = total_answers_all),
               alpha = 0.8) +
  
  scale_color_distiller(type="seq", palette="Reds", direction=1) +
  labs(
    title = "Proportion non durable par espèce",
    x = "Espèce",
    y = "Proportion de dpts avec 1 réponse non durable (%)",
    size = "Nombre total de réponses",
    colour = "Couverture des reponses"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





#### Enjeux par massif ####
massifs_simple <- read.csv("massifs_cueillette_simple.csv")

data_enjeux_massif <- all_rarity %>%
  full_join(all_data %>% dplyr::select(-sp_relative_area, -dpt_area, -sp_area), 
            by="CD_REF") %>%
  dplyr::select(CD_REF, id, ESPECE_nom, ESPECE_presence, ESPECE_durabilite, ESPECE_dpt,
                dpt_name, dpt_simple, dpt_area, sp_area) %>%
  left_join(massifs_simple, join_by("dpt_name"=="dpt")) %>%
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
  group_by(CD_REF, ESPECE_nom, massif) %>%
  summarise(sp_relative_area = round(100*sum(sp_area)/sum(dpt_area),2),
            n_answers_Non_durable = sum(n_answers_Non_durable),
            n_answers_Durable = sum(n_answers_Durable),
            n_answers_all = n_answers_Durable + n_answers_Non_durable) %>%
  mutate(proportion_non_durable = ifelse(n_answers_Non_durable==0, NA, 
                                         100* n_answers_Non_durable/
                                           (n_answers_Non_durable+n_answers_Durable)),
         proportion_non_durable = ifelse(n_answers_Non_durable==0 & 
                                           n_answers_Durable>0, 0, 
                                         proportion_non_durable),
         proportion_non_durable = round(proportion_non_durable,1)) %>%
  filter(n_answers_all>1) %>%
  arrange(proportion_non_durable, n_answers_all) %>%
  mutate(ESPECE_nom = factor(ESPECE_nom, levels = unique(ESPECE_nom)))
  



ggplot(data_enjeux_massif, aes(x = reorder_within(ESPECE_nom, proportion_non_durable, massif),
                                        y = proportion_non_durable)) +
  
  geom_point(aes(size = n_answers_all),
             alpha = 0.8) +
  
  # scale_color_distiller(type = "seq", palette = "Reds", direction = 1) +
  labs(
    title = "Proportion non durable par espèce et massif",
    x = "Espèce",
    y = "Proportion de dpts avec ≥1 réponse non durable (%)",
    size = "Nombre total de réponses",
    colour = "Couverture des réponses"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)) +
  
  facet_wrap(~ massif, scales = "free_x") +
  
  scale_x_reordered()




#### Maps for specific species ####
# Data is already processed in `df_all_species_data`, now join with `departements`
citations_by_species_dpt <- all_data %>%
  dplyr::select(id, ESPECE_nom, ESPECE_durabilite, ESPECE_dpt) %>% 
  filter(!is.na(ESPECE_dpt) & ESPECE_dpt != "") %>%
  unique() %>%
  filter(!is.na(ESPECE_dpt) & ESPECE_dpt != "") %>%
  group_by(ESPECE_dpt, ESPECE_nom, ESPECE_durabilite) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = ESPECE_durabilite,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total_citations = Durable + `Non durable`)


global_max_total <- max(citations_by_species_dpt$total_citations, na.rm = TRUE)

plot_species_map <- function(species_name) {
  map_data <- departements_simpl_PARIS %>%
    left_join(data_enjeux %>% filter(ESPECE_nom == species_name), 
              by = join_by("dpt"=="dpt_name")) %>%
    st_as_sf() %>%
    mutate(total_citations = n_answers_Non_durable + n_answers_Durable,
           proportion_non_durable = ifelse(total_citations > 0,
                                           round(100 * n_answers_Non_durable / total_citations, 1),
                                           NA),
           # Encode fill vars
           fill_total = ifelse(sp_relative_area == 0 &
                                 total_citations == 0, NA, total_citations),
           fill_prop  = ifelse(sp_relative_area == 0 &
                                 total_citations == 0, NA, proportion_non_durable),
           fill_prop  = ifelse(sp_relative_area > 0 &
                                 total_citations == 0, 0, fill_prop))
  
  # Plot 1: Total citations
  plot_total <- ggplot(map_data) +
    geom_sf(aes(fill = fill_total)) +
    geom_sf(aes(), fill = NA, colour = "grey70", linewidth = 0.3) +  # grey borders
    geom_sf_text(aes(label = ifelse(total_citations > 0, total_citations, "")),
                 colour = "black", size = 3) +
    scale_fill_gradient(
      low = "white", high = "#08519c",   # white = present but 0 citations
      na.value = "grey80",               # species absent
      limits = c(0, global_max_total),
      oob = scales::squish
    ) +
    theme_void() +
    labs(
      title = "Total citations",
      fill = "Number of citations"
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Plot 2: Proportion of non-durable citations
  plot_prop <- ggplot(map_data) +
    geom_sf(aes(fill = fill_prop)) +
    geom_sf(aes(), fill = NA, colour = "grey70", linewidth = 0.3) +  # grey borders
    geom_sf_text(
      data = subset(map_data, !is.na(proportion_non_durable) & total_citations > 0),
      aes(label = paste0(proportion_non_durable, "%")),
      colour = "black", size = 3
    ) +
    scale_fill_gradient(
      low = "white", high = "#a50f15",   # white = present but 0 citations
      na.value = "grey80",               # species absent
      limits = c(0, 100),
      oob = scales::squish
    ) +
    theme_void() +
    labs(
      title = "Proportion of non-durable citations",
      fill = "Percentage"
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Combine and display plots
  plot_grid(
    plot_total,
    plot_prop,
    align = "hv",
    labels = paste("Citations for", species_name, "by Department"),
    label_size = 16,
    label_fontface = "bold",
    nrow = 1
  )
}


# plot_species_map("Urtica dioica")
plot_species_map("Allium ursinum")
# plot_species_map("Arnica montana")
# plot_species_map("Gentiana lutea")
# plot_species_map("Thymus vulgaris")
plot_species_map("Euphorbia spinosa")
plot_species_map("Rhodiola rosea")

# plot_species_map("Artemisia umbelliformis")
# plot_species_map("Sambucus nigra")
# plot_species_map("Vaccinium myrtillus")
# plot_species_map("Filipendula ulmaria")
plot_species_map("Narcissus pseudonarcissus")



#### Map durable/non-durable citings ####
# This plot requires a specific data aggregation
plot_durability_diverging <- function(data, n = 10) {
  
  # Calculate top n species
  top_species <- data %>%
    dplyr::select(id, ESPECE_nom, PROFIL_statut_cueilleur) %>%
    filter(!is.na(PROFIL_statut_cueilleur)) %>%
    distinct() %>%
    group_by(ESPECE_nom) %>%
    summarise(total_citations = n(), .groups = "drop") %>%
    slice_max(order_by = total_citations, n = n) %>%
    pull(ESPECE_nom)
  
  # Aggregate counts by species + durability
  df_summary <- data %>%
    dplyr::select(id, ESPECE_nom, ESPECE_durabilite) %>%
    filter(ESPECE_nom %in% top_species) %>%
    distinct() %>%
    group_by(ESPECE_nom, ESPECE_durabilite) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      # Make non-durable counts negative so they plot downwards
      count = ifelse(ESPECE_durabilite == "Non durable", -count, count)
    )
  
  # Plot
  ggplot(df_summary, aes(x = reorder(ESPECE_nom, abs(count)), y = count, fill = ESPECE_durabilite)) +
    geom_col(width = 0.7) +
    coord_flip() +  # optional: flip for easier labels
    scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
    labs(
      title = paste("Top", n, "Species cited by durability status"),
      x = "Species",
      y = "Number of Citations",
      fill = "Durability"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
}
# Example usage to show the top 10 species
plot_durability_diverging(all_data, n = 40)



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
  
  # 95% cutoff species
  cutoff_species_95 <- df_cum %>%
    filter(cum_percent <= 75) %>%
    nrow()
  
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
    
    # 95% cutoff
    geom_hline(yintercept = 95, linetype = "dashed", color = "darkgreen") +
    geom_vline(xintercept = cutoff_species_95, linetype = "dashed", color = "darkgreen") +
    annotate("text", x = cutoff_species_95, y = 97,
             label = paste(cutoff_species_95, "species = 95% of citations"),
             hjust = -0.1, vjust = 0, color = "darkgreen") +
    
    # Knee cutoff
    geom_vline(xintercept = cutoff_species_knee, linetype = "dashed", color = "purple") +
    annotate("text", x = cutoff_species_knee, y = 85,
             label = paste("Exponential cutoff ~", cutoff_species_knee, "species"),
             hjust = -0.1, vjust = 0, color = "purple") +
    
    labs(
      title = "Cumulative frequency of species citations with exponential fit",
      x = "Species (ordered by frequency)",
      y = "Cumulative % of citations"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5)
    )
}


plot_cumulative_species(all_data)



##### Plot of departement coverage ####
dc <- data_enjeux_filtered %>%
  dplyr::select(ESPECE_nom, num_present_dpts) %>%
  unique()

# Density plot of the number of departments
# compute the median of your variable
median_val <- median(dc$num_present_dpts, na.rm = TRUE)

# plot with vertical line
ggplot(dc, aes(x = num_present_dpts)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = median_val, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Density of Species Distribution Across Departments",
    x = "Number of Departments",
    y = "Density"
  ) +
  theme_minimal()




##### GLM tests ########


####################################""""
data_glm_durabilite$ESPECE_durabilite <- ifelse(data_glm_durabilite$ESPECE_durabilite == "Durable", 1, 0)
data_glm_durabilite <- data_glm_durabilite 

# model4 <- glmer(ESPECE_durabilite ~ . + (1|ESPECE_nom), family = binomial(link = "logit"), data=data_glm_durabilite) # failure to converge

model5 <- glm(ESPECE_durabilite ~ . -ESPECE_nom, family = binomial(link = "logit"), data=data_glm_durabilite)
summary(glm_durability)

# 1. Stepwise AIC selection
model_full <- glm(
  ESPECE_durabilite ~ . - ESPECE_nom,
  family = binomial(link = "logit"),
  data = data_glm_durabilite
)

model_step <- stepAIC(model_full, direction = "both", trace = FALSE)
summary(model_step)

vars_step <- names(coef(model_step))[coef(model_step) != 0]
vars_step <- vars_step[vars_step != "(Intercept)"]


# 2. LASSO (cv.glmnet)
# glmnet requires matrix input
X <- model.matrix(ESPECE_durabilite ~ . - ESPECE_nom, data = data_glm_durabilite)[, -1]
y <- data_glm_durabilite$ESPECE_durabilite

set.seed(123) # for reproducibility
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)

plot(cv_lasso)

coef_lasso <- coef(cv_lasso, s = "lambda.min")
vars_lasso <- rownames(coef_lasso)[as.vector(coef_lasso != 0)]
vars_lasso <- vars_lasso[vars_lasso != "(Intercept)"]


# 3. Compare selected variables
list(
  Stepwise_AIC = vars_step,
  LASSO = vars_lasso
)


# LASSO selected variables
vars_lasso
colnames(data_glm_durabilite)


# Initialize vector to store matched column names

matched_cols <- colnames(data_glm_durabilite)[
  sapply(colnames(data_glm_durabilite), function(col) {
    any(grepl(col, vars_lasso))
  })
]

matched_cols


# Build formula dynamically
formula_lasso <- as.formula(
  paste("ESPECE_durabilite ~", paste(matched_cols, collapse = " + "))
)


# Fit GLM
model_lasso_selected <- glm(
  formula_lasso,
  family = binomial(link = "logit"),
  data = data_glm_durabilite
)

summary(model_lasso_selected)

# Coefficients and p-values
coefs <- summary(model_lasso_selected)$coefficients
coefs

odds_ratios <- exp(coefs[, "Estimate"])
confint_glm <- exp(confint(model_lasso_selected))  # 95% CI

