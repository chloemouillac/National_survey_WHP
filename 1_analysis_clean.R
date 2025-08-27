#### Load packages ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(sf)
library(cowplot)
library(patchwork)


#### Import data ####
raw_data <- read.csv("raw_data/results-survey676868_prefilter.csv") %>%
  mutate(across(where(is.character), ~case_when( # change any occurrence of the strings below to "Ile de France"
    . %in% c("Essonne", "Hauts de Seine", "Paris",
             "Seine Saint Denis", "Seine et Marne", "Val d Oise",
             "Val de Marne", "Yvelines") ~ "Ile de France",
    TRUE ~ . # Keep the original value if it's not in the list
  )))


departements <- read_sf("raw_data/departements_detail_paris.shp") %>%
  mutate(
    dpt_simple = gsub("-", " ", dpt),
    region_simple = gsub("-", " ", region)
  ) %>%
  mutate(
    dpt_simple = gsub("'", " ", dpt_simple),
    region_simple = gsub("'", " ", region_simple)
  ) %>%
  mutate(
    dpt_simple = iconv(dpt_simple, to = "ASCII//TRANSLIT"),
    region_simple = iconv(region_simple, to = "ASCII//TRANSLIT")
  )

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
    select(where(~ any(!is.na(.))))
  }



process_all_data <- function(data) {
  # Columns related to species, departments, and harvester type
  species_cols <- data %>%
    select(id__ID_de_la_reponse,
           matches("^G(?:3|4|6|8|10|12|14|16|18|20)Q000(?:0)?(?:1|2|4|5|6|7|8|9|10|11|12|13|14|18|19|20|21|22|23|24)_")) # careful of the number of zeroes after Q
  
  df_long <- data %>%
    # Select all necessary columns at once
    select(id = id__ID_de_la_reponse,
           type_cueilleur = starts_with("G21Q00009_"),
           colnames(species_cols)) %>%
    
    # Pivot to long format for easier manipulation
    pivot_longer(
      cols = -c(id, type_cueilleur),
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
      type_cueilleur = word(type_cueilleur, 1, 2),
      dpt = str_trim(str_remove(dpt, "\\s*\\([0-9AB]+\\)"))
    ) %>%
    
    unnest(everything()) %>%
    
    # Filter out empty species
    filter(species != "") %>%
    
    # Keep only one record per ID, species, and group to avoid double-counting
    unique()
  
  
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
  
  return(df_final)
}

df_all_data <- process_all_data(raw_data)



  #### Map the number of answers per department and region ####

# Helper function to process and map data by location
process_and_map_expertise <- function(data, pattern, name_col, join_col, shp, shp_col) {
  data %>%
    pivot_longer(cols = starts_with(pattern), names_to = name_col, values_to = "response") %>%
    filter(response == "Oui") %>%
    mutate(
      !!name_col := case_when(
        pattern == "G1Q00002_" ~ sub(".*___(.*?)__\\d+[AB]?$", "\\1", .data[[name_col]]),
        pattern == "G1Q00003_" ~ sub(".*___(.*)$", "\\1", .data[[name_col]])
      ),
      !!name_col := gsub("_", " ", .data[[name_col]])
    ) %>%
    group_by(!!sym(name_col)) %>%
    summarise(n_answers = n_distinct(id__ID_de_la_reponse), .groups = "drop") %>%
    full_join(shp, by = setNames(shp_col, name_col)) %>%
    st_as_sf()
}

# Process and prepare data for mapping
dpt_answers <- process_and_map_expertise(
  raw_data, "G1Q00002_", "dpt_simple", "dpt_simple", departements, "dpt_simple")

region_answers <- process_and_map_expertise(
  raw_data, "G1Q00003_", "region_simple", "region_simple", 
  departements %>% group_by(region_simple) %>% summarise(geometry = st_union(geometry)), 
  "region_simple")

national_answers <- raw_data %>%
  filter(G1Q00001__A_quelle_echelle_spatiale_vous_situez_vous == "National (France entiere)")

n_national_answers <- n_distinct(national_answers$id__ID_de_la_reponse)

total_answers <- n_distinct(raw_data$id__ID_de_la_reponse)

# Calculate common max for colour scale
max_answers <- max(dpt_answers$n_answers, region_answers$n_answers, na.rm = TRUE)

# Create the departmental plot
dpt_plot <- ggplot(dpt_answers) +
  geom_sf(aes(fill = n_answers), colour = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = n_answers), colour = "black", size = 3) +
  scale_fill_distiller(
    name = "Answers",
    palette = "Reds",
    direction = 1,
    limits = c(0, max_answers),
    na.value = "grey80"
  ) +
  theme_void() +
  labs(title = "Survey answers per department") +
  theme(plot.title = element_text(hjust = 0.5))

# Create the regional plot
region_plot <- ggplot(region_answers) +
  geom_sf(aes(fill = n_answers), colour = "white", linewidth = 0.2, show.legend = FALSE) +
  geom_sf_text(aes(label = n_answers), colour = "black", size = 3) +
  scale_fill_distiller(
    name = "Answers",
    palette = "Reds",
    direction = 1,
    limits = c(0, max_answers),
    na.value = "grey80"
  ) +
  theme_void() +
  labs(title = "Survey answers per region") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine plots with a shared legend
combined_plot <- dpt_plot + region_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = paste("Total national level answers:", n_national_answers, "\n",
                    "Total survey answers:", total_answers),
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 12))
  )

combined_plot

  
  #### Bar Plots: species from G2 section ####
# Helper function for consistent bar plots
plot_bar <- function(data, title, fill_var = NULL) {
  p <- ggplot(data, aes(x = reorder(species, -table(species)[species]))) +
    labs(title = title, x = "Species", y = "Number of Citations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (!is.null(fill_var)) {
    p <- p + geom_bar(aes(fill = !!sym(fill_var)))
  } else {
    p <- p + geom_bar()
  }
  return(p)
}

# Bar plots for G2 section
df_g2_bar <- raw_data %>%
  select(matches("^G2Q0000(2|3|4|5)_SQ001"),
         id = id__ID_de_la_reponse,
         type_cueilleur=starts_with("G21Q00009_")) %>%
  pivot_longer(cols = matches("^G2Q0000(2|3|4|5)_SQ001"), 
                              names_to = "qcode",
                              names_pattern = "(^G\\d+Q\\d+)",
                              values_to = "species") %>%
  mutate(type_cueilleur = word(type_cueilleur, 1, 2)) %>%
  filter(!is.na(species)) %>%
  separate_rows(species, sep = ",\\s*") %>%
  unique()

plot_bar(df_g2_bar %>% filter(qcode == "G2Q00002"), 
         "The most harvested species", "type_cueilleur")
plot_bar(df_g2_bar %>% filter(qcode == "G2Q00003"), 
         "The most expensive species", "type_cueilleur")
plot_bar(df_g2_bar %>% filter(qcode == "G2Q00004"), 
         "The most difficult species to get", "type_cueilleur")
plot_bar(df_g2_bar %>% filter(qcode == "G2Q00005"), 
         "The species most subject to trends", "type_cueilleur")


  
  #### Map: Number of species per department ####
# Data for this map is already in `df_all_data`
departements_sans_accent <- read_sf("raw_data/departements_detail_paris.shp") %>%
  mutate(dpt = iconv(dpt, to = "ASCII//TRANSLIT"))
  
  
answers_by_dpt <- df_all_data %>%
  full_join(departements_sans_accent, by = "dpt") %>% # we want the empty departements too!
  group_by(dpt, code, region, geometry) %>%
  summarise(n = n_distinct(species, na.rm = T), .groups = "drop") %>%
  mutate(n = if_else(n == 0, NA_integer_, n)) %>%
  st_as_sf()

ggplot(answers_by_dpt) +
  geom_sf(aes(fill = n), colour = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = n), colour = "black", size = 3) +
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80") +
  theme_void() +
  labs(
    fill = "Number of species",
    title = "Number of different species per department"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


  
  #### Map durable/non-durable citings per harvester type ####
# This plot requires a specific data aggregation
plot_durability_pro_bar <- function(data) {
  df_summary <- data %>%
    filter(!is.na(type_cueilleur)) %>% # to keep only answers which have the harvester type associated
    group_by(species, type_cueilleur, durabilite) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = durabilite,
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(total = Durable + `Non durable`) %>%
    pivot_longer(
      cols = c(Durable, `Non durable`),
      names_to = "statut_durabilite",
      values_to = "count"
    )
  
  ggplot(df_summary, aes(x = reorder(species, -total), y = count, fill = statut_durabilite)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(type_cueilleur ~ ., scales = "free_y", space = "free_y") +
    scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
    labs(
      title = "Species cited by durability status",
      x = "Species",
      y = "Number of Citations",
      fill = "Durability"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5)
    )
}

plot_durability_pro_bar(df_all_data)

# Bar plot of all cited species
plot_bar(df_all_data, "All cited species", "type_cueilleur")

  
  #### Maps for specific species ####
# Data is already processed in `df_all_data`, now join with `departements`
citations_by_species_dpt <- df_all_data %>%
  filter(!is.na(dpt) & dpt != "") %>%
  group_by(dpt, species, durabilite) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = durabilite,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total_citations = Durable + `Non durable`)

# Calculate global max for common colour scale limits
global_max_total <- max(citations_by_species_dpt$total_citations, na.rm = TRUE)

plot_species_map <- function(species_name) {
  map_data <- departements_sans_accent %>%
    left_join(citations_by_species_dpt %>% filter(species == species_name), by = "dpt") %>%
    st_as_sf() %>%
    mutate(prop_non_durable = ifelse(total_citations > 0, round(`Non durable` / total_citations * 100, 2), 0))
  
  # Plot 1: Total citations
  plot_total <- ggplot(map_data) +
    geom_sf(aes(fill = total_citations), colour = "white", linewidth = 0.2) +
    geom_sf_text(aes(label = total_citations), colour = "black", size = 3) +
    scale_fill_distiller(
      palette = "Blues",
      direction = 1,
      na.value = "grey80",
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
    geom_sf(aes(fill = prop_non_durable), colour = "white", linewidth = 0.2) +
    geom_sf_text(
      data = subset(map_data, !is.na(prop_non_durable)),
      aes(label = paste0(prop_non_durable, "%")),
      colour = "black", size = 3
    ) +
    scale_fill_distiller(
      palette = "Reds",
      direction = 1,
      na.value = "grey80",
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

plot_species_map("Allium ursinum")
plot_species_map("Arnica montana")
plot_species_map("Gentiana lutea")
plot_species_map("Thymus vulgaris")
plot_species_map("Euphorbia spinosa")
plot_species_map("Artemisia umbelliformis")
plot_species_map("Sambucus nigra")
plot_species_map("Vaccinium myrtillus")
plot_species_map("Filipendula ulmaria")
plot_species_map("Narcissus pseudonarcissus")


  
  #### Bar Plots: species-specific questions ####
# Data is already processed in `df_all_data`

##### Plot 1: Presence ####
ggplot(df_all_data %>% filter(!is.na(presence)), 
       aes(x = reorder(presence, -table(presence)[presence]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "How widespread is this species within the department ?",
    y = "Number of responses",
    x = "Presence"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 2: Harvesting extent ####
ggplot(df_all_data %>% filter(!is.na(etendue_cueill)), 
       aes(x = reorder(etendue_cueill, -table(etendue_cueill)[etendue_cueill]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "How would you describe the extent of the harvesting of this species ?",
    y = "Number of responses",
    x = "Harvesting extent"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 3: Harvested parts ####
ggplot(df_all_data %>% filter(!is.na(parties_cueill)), 
       aes(x = reorder(parties_cueill, -table(parties_cueill)[parties_cueill]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "What parts of this species are collected ?",
    y = "Number of responses",
    x = "Collected part"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 4: Harvesting uses ####
ggplot(df_all_data %>% filter(!is.na(usages)), 
       aes(x = reorder(usages, -table(usages)[usages]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "What are the uses of this species ?",
    y = "Number of responses",
    x = "Use"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 5: Harvesting type (professional/ local) ####
df_type_cueillette <- df_all_data %>%
  select(id, species, type_cueillette_Cueillette_amatrice_familiale, type_cueillette_Cueillette_professionnelle, durabilite) %>%
  unique() %>%
  pivot_longer(cols = c(type_cueillette_Cueillette_amatrice_familiale, type_cueillette_Cueillette_professionnelle),
               names_to= "type_cueillette",
               values_drop_na = T) %>%
  mutate(type_cueillette = sub("type_cueillette_", "", type_cueillette)) %>%
  group_by(type_cueillette, value, durabilite) %>%
  summarise(count=n())

ggplot(df_type_cueillette, 
       aes(x = value,
           y = count,
           fill = durabilite)) +
  geom_col() + facet_grid(.~type_cueillette) +
  labs(
    title = "How would you describe the harvesting of this species ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 6: Type of companies ####
df_type_entrep <- df_all_data %>%
  select(id, species, type_entrep_Microentreprises_lt_10_salaries, 
         type_entrep_PME_10_50_salaries,
         type_entrep_Grandes_entreprises_gt_50_salaries,
         durabilite) %>%
  unique() %>%
  pivot_longer(cols = c(type_entrep_Microentreprises_lt_10_salaries, 
                        type_entrep_PME_10_50_salaries,
                        type_entrep_Grandes_entreprises_gt_50_salaries),
               names_to= "type_entrep",
               values_drop_na = T) %>%
  mutate(type_entrep = sub("type_entrep_", "", type_entrep)) %>%
  group_by(type_entrep, value, durabilite) %>%
  summarise(count=n())

ggplot(df_type_entrep, 
       aes(x = value,
           y = count,
           fill = durabilite)) +
  geom_col() + facet_grid(.~type_entrep) +
  labs(
    title = "What type of companies are professional harvests intended for ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 7: Start of observation ####
ggplot(df_all_data %>% filter(!is.na(debut_obs)) %>%
         mutate(debut_obs = as.numeric(debut_obs)), 
       aes(x = reorder(debut_obs, -table(debut_obs)[debut_obs]))) +
  geom_bar() +
  labs(
    title = "When did you first start observing the harvesting of this species ?",
    y = "Number of responses",
    x = "Start date"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 8: State of resource ####
ggplot(df_all_data %>% filter(!is.na(etat_ressource)), 
       aes(x = reorder(etat_ressource, -table(etat_ressource)[etat_ressource]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "Have you noticed an increase or decrease in the availability of the resource ?",
    y = "Number of responses",
    x = "Use"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 9: Variation of harvesting intensity ####
ggplot(df_all_data %>% filter(!is.na(variation_prelev)), 
       aes(x = reorder(variation_prelev, -table(variation_prelev)[variation_prelev]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "At the department level, over the last 5 years, do you think that harvesting...",
    y = "Number of responses",
    x = "Use"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 10: Harvesting intensity ####
ggplot(df_all_data %>% filter(!is.na(intensite_prelev)), 
       aes(x = reorder(intensite_prelev, -table(intensite_prelev)[intensite_prelev]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "How would you describe the intensity of current harvesting ?",
    y = "Number of responses",
    x = "Use"
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 11: Risk factors ####
ggplot(df_all_data %>% filter(!is.na(risque)), 
       aes(x = reorder(risque, -table(risque)[risque]))) +
  geom_bar() +
  labs(
    title = "What are the risk factors that can lead to unsustainable harvesting ?",
    y = "Number of responses",
    x = "Risk factors"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 12: Trend effect ####
ggplot(df_all_data %>% filter(!is.na(mode)), 
       aes(x = reorder(mode, -table(mode)[mode]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "Is this species subject to trends ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 13: Trend effect : a risk factor for unsustainable harvesting ####
ggplot(df_all_data %>% filter(!is.na(mode_risque)), 
       aes(x = reorder(mode_risque, -table(mode_risque)[mode_risque]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "Does this trend pose a risk to the sustainability of harvesting for this species ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 14: Regulations ####
ggplot(df_all_data %>% filter(!is.na(espece_reglem)), 
       aes(x = reorder(espece_reglem, -table(espece_reglem)[espece_reglem]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "Do you know if the harvesting of this species is regulated ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df_all_data %>% filter(!is.na(reglem_adaptee)), 
       aes(x = reorder(reglem_adaptee, -table(reglem_adaptee)[reglem_adaptee]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "Do you consider the current regulations to be appropriate for the harvesting of this species ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df_all_data %>% filter(!is.na(cause_reglem_inadaptee)), 
       aes(x = reorder(cause_reglem_inadaptee, -table(cause_reglem_inadaptee)[cause_reglem_inadaptee]), fill = durabilite)) +
  geom_bar() +
  labs(
    title = "If no, why ?",
    y = "Number of responses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))









#### Bar Plots: harvester profiles ####
