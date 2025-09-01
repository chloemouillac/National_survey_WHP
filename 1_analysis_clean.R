#### Load packages ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(sf)
library(cowplot)
library(patchwork)
library(FactoMineR)


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
    select(where(~ any(!is.na(.)))) %>%
    
    # Combine multiple rows with the same identifier into a single row
    group_by(id) %>%
    summarise(across(everything(), ~first(na.omit(.))))
  
  }



 process_all_species_data <- function(data) {
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

df_all_species_data <-  process_all_species_data(raw_data)



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
departements_sans_accent <- read_sf("raw_data/departements_detail_paris.shp") %>%
  mutate(dpt = iconv(dpt, to = "ASCII//TRANSLIT"))
  
  
answers_by_dpt <- df_all_species_data %>%
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
plot_durability_pro_bar <- function(data, n = 10) { # Add 'n' as a function argument with a default value
  
  # Calculate total citations per species and select the top n
  top_species <- data %>%
    select(id, species, type_cueilleur) %>%
    filter(!is.na(type_cueilleur)) %>%
    unique() %>%
    group_by(species) %>%
    summarise(total_citations = n(), .groups = "drop") %>%
    slice_max(order_by = total_citations, n = n) %>%
    pull(species)
  
  # Filter the main data frame to include only the top species
  df_summary <- data %>%
    select(id, species, durabilite, type_cueilleur) %>%
    filter(!is.na(type_cueilleur) & species %in% top_species) %>%
    unique() %>%
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
  
  # Plotting
  ggplot(df_summary, aes(x = reorder(species, -total), y = count, fill = statut_durabilite)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(type_cueilleur ~ ., scales = "free_y", space = "free_y") +
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
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5)
    )
}

# Example usage to show the top 10 species
plot_durability_pro_bar(df_all_species_data, n = 20)


  #### Bar Plot of all cited species ####
plot_bar(df_all_species_data %>%
           select(id, species, type_cueilleur) %>% 
           unique(),
         "All cited species", "type_cueilleur")

nrow(df_all_species_data %>%
         select(species) %>% 
         unique()) # 191 different cited species

  
  #### Maps for specific species ####
# Data is already processed in `df_all_species_data`, now join with `departements`
citations_by_species_dpt <- df_all_species_data %>%
  select(id, species, durabilite, dpt) %>% 
  filter(!is.na(dpt) & dpt != "") %>%
  unique() %>%
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
# Data is already processed in `df_all_species_data`

##### Plot 1: Presence ####
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, presence) %>% 
         filter(!is.na(presence)) %>%
         unique(), 
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, etendue_cueill) %>% 
         filter(!is.na(etendue_cueill)) %>%
         unique(),
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, parties_cueill) %>% 
         filter(!is.na(parties_cueill)) %>%
         unique(),
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, usages) %>% 
         filter(!is.na(usages)) %>%
         unique(),
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
df_type_cueillette <- df_all_species_data %>%
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
df_type_entrep <- df_all_species_data %>%
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
           y = count)) +
  geom_col() + facet_grid(.~type_entrep) +
  labs(
    title = "What type of companies are professional harvests intended for ?",
    y = "Number of responses",
    x = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 7: Start of observation ####
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, debut_obs) %>% 
         filter(!is.na(debut_obs)) %>%
         unique() %>%
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, etat_ressource) %>% 
         filter(!is.na(etat_ressource)) %>%
         unique(), 
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, variation_prelev) %>% 
         filter(!is.na(variation_prelev)) %>%
         unique(), 
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, intensite_prelev) %>% 
         filter(!is.na(intensite_prelev)) %>%
         unique(), 
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, risque) %>% 
         filter(!is.na(risque)) %>%
         unique(), 
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
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, mode) %>% 
         filter(!is.na(mode)) %>%
         unique(), 
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


ggplot(df_all_species_data %>% 
         select(id, species, durabilite, mode_risque) %>% 
         filter(!is.na(mode_risque)) %>%
         unique(), 
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


##### Plot 13: Regulations ####
ggplot(df_all_species_data %>% 
         select(id, species, durabilite, espece_reglem) %>% 
         filter(!is.na(espece_reglem)) %>%
         unique(), 
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

ggplot(df_all_species_data %>% 
         select(id, species, durabilite, reglem_adaptee) %>% 
         filter(!is.na(reglem_adaptee)) %>%
         unique(), 
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

ggplot(df_all_species_data %>% 
         select(id, species, durabilite, cause_reglem_inadaptee) %>% 
         filter(!is.na(cause_reglem_inadaptee)) %>%
         unique(), 
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


##### Plot 1: Gender ####
ggplot(df_all_profile_data %>% 
         select(id, genre) %>% 
         filter(!is.na(genre)) %>%
         unique(), 
       aes(x = reorder(genre, -table(genre)[genre]))) +
  geom_bar() +
  labs(
    title = "What is your gender ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 2: Age ####
ggplot(df_all_profile_data %>% 
         select(id, age) %>% 
         filter(!is.na(age)) %>%
         unique(), 
       aes(x = reorder(age, -table(age)[age]))) +
  geom_bar() +
  labs(
    title = "What is your age ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### Plot 3: Studies ####
ggplot(df_all_profile_data %>% 
         select(id, niveau_educ) %>% 
         filter(!is.na(niveau_educ)) %>%
         unique(), 
       aes(x = reorder(niveau_educ, -table(niveau_educ)[niveau_educ]))) +
  geom_bar() +
  labs(
    title = "What is your level of studies ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df_all_profile_data %>% 
         select(id, etudes_evt) %>% 
         filter(!is.na(etudes_evt)) %>%
         unique(), 
       aes(x = reorder(etudes_evt, -table(etudes_evt)[etudes_evt]))) +
  geom_bar() +
  labs(
    title = "Are your studies linked with the environment ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 4: CSP ####
ggplot(df_all_profile_data %>% 
         select(id, categ_socio_pro) %>% 
         filter(!is.na(categ_socio_pro)) %>%
         unique(), 
       aes(x = reorder(categ_socio_pro, -table(categ_socio_pro)[categ_socio_pro]))) +
  geom_bar() +
  labs(
    title = "What socio-professional category do you belong to ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 5: Do you harvest wild plants ? ####
ggplot(df_all_profile_data %>% 
         select(id, cueilleur) %>% 
         filter(!is.na(cueilleur)) %>%
         unique(), 
       aes(x = reorder(cueilleur, -table(cueilleur)[cueilleur]))) +
  geom_bar() +
  labs(
    title = "Do you harvest wild plants ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df_all_profile_data %>% 
         select(id, debut_cueill) %>% 
         filter(!is.na(debut_cueill)) %>%
         unique(), 
       aes(x = reorder(debut_cueill, -table(debut_cueill)[debut_cueill]))) +
  geom_bar() +
  labs(
    title = "Since when do you harvest wild plants ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df_all_profile_data %>% 
         select(id, nb_esp_cueill) %>% 
         filter(!is.na(nb_esp_cueill)) %>%
         unique(), 
       aes(x = reorder(nb_esp_cueill, -table(nb_esp_cueill)[nb_esp_cueill]))) +
  geom_bar() +
  labs(
    title = "How many species do you harvest ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 6: Harvester status ####
ggplot(df_all_profile_data %>% 
         select(id, statut_cueilleur) %>% 
         filter(!is.na(statut_cueilleur)) %>%
         unique(), 
       aes(x = reorder(statut_cueilleur, -table(statut_cueilleur)[statut_cueilleur]))) +
  geom_bar() +
  labs(
    title = "What type of harvester are you ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df_all_profile_data %>% 
         select(id, statut_cueilleur_entrep) %>% 
         filter(!is.na(statut_cueilleur_entrep)) %>%
         unique(), 
       aes(x = reorder(statut_cueilleur_entrep, -table(statut_cueilleur_entrep)[statut_cueilleur_entrep]))) +
  geom_bar() +
  labs(
    title = "What role do you have in your company (professional harvesting) ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 7: Harvester group ####
ggplot(df_all_profile_data %>% 
         select(id, groupe_cueill) %>% 
         filter(!is.na(groupe_cueill)) %>%
         unique(), 
       aes(x = reorder(groupe_cueill, -table(groupe_cueill)[groupe_cueill]))) +
  geom_bar() +
  labs(
    title = "Are you part of a group of harvesters ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df_all_profile_data %>% 
         select(id, nom_groupe_cueill) %>% 
         filter(!is.na(nom_groupe_cueill)) %>%
         unique(), 
       aes(x = reorder(nom_groupe_cueill, -table(nom_groupe_cueill)[nom_groupe_cueill]))) +
  geom_bar() +
  labs(
    title = "What is the name of your group ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 8: Organism ####
ggplot(df_all_profile_data %>% 
         select(id, type_orga_rattach) %>% 
         filter(!is.na(type_orga_rattach)) %>%
         unique(), 
       aes(x = reorder(type_orga_rattach, -table(type_orga_rattach)[type_orga_rattach]))) +
  geom_bar() +
  labs(
    title = "What organism do you belong to ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(df_all_profile_data %>% 
         select(id, taille_orga_rattach) %>% 
         filter(!is.na(taille_orga_rattach)) %>%
         unique(), 
       aes(x = reorder(taille_orga_rattach, -table(taille_orga_rattach)[taille_orga_rattach]))) +
  geom_bar() +
  labs(
    title = "What is the size of this organism ?",
    y = "Number of responses",
    x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Multivariate analysis ####
# Rename columns in df_all_profile_data with "PROFILE_" prefix
df_profile_renamed <- df_all_profile_data %>%
  rename_with(~ paste0("PROFIL_", .), .cols = -id)

# Rename columns in df_all_species_data with "SPECIES_" prefix
df_species_renamed <- df_all_species_data %>%
  rename_with(~ paste0("ESPECE_", .), .cols = -id)

# Perform the full join on the renamed data frames
all_data <- full_join(df_profile_renamed, df_species_renamed, by = "id")

all_data$ESPECE_usages[grepl("Alimentaire", all_data$ESPECE_usages)] <- "Alimentaire"
all_data$ESPECE_usages[grepl("Cosmetique", all_data$ESPECE_usages)] <- "Cosmetique"
all_data$ESPECE_usages[grepl("Artisanat", all_data$ESPECE_usages)] <- "Artisanat"
all_data$ESPECE_usages[grepl("ornemental", all_data$ESPECE_usages)] <- "Ornemental"
all_data$ESPECE_usages[grepl("Medical", all_data$ESPECE_usages)] <- "Medical"

all_data$ESPECE_parties_cueill[grepl("aerienne", all_data$ESPECE_parties_cueill)] <- "Partie aerienne"
all_data$ESPECE_parties_cueill[grepl("souterraine", all_data$ESPECE_parties_cueill)] <- "Partie souterraine"


all_data <- all_data %>%
  mutate(across(!starts_with("id"), ~ case_when(
    str_detect(., "variable") ~ "Variable",
    str_detect(., "stable") ~ "Stable",
    str_detect(., "regress") ~ "Diminue",
    str_detect(., "augmente") ~ "Augmente",
    TRUE ~ . # Keep the original value if no pattern matches
  ))) %>%
  
  mutate(across(everything(), ~ str_replace(.x, "Je ne sais pas", "JNSP") )) %>%
  
  # Remove text in brackets from all columns
  mutate(across(everything(), ~ str_replace(.x, "\\(.*\\)", "") ))


all_data$ESPECE_presence[grepl("Abondante et largement distribuee", all_data$ESPECE_presence)] <- "Abondante large"
all_data$ESPECE_presence[grepl("Abondante sur des zones localisees", all_data$ESPECE_presence)] <- "Abondante locale"

all_data$PROFIL_type_orga_rattach[grepl("Aucune", all_data$PROFIL_type_orga_rattach)] <- "Aucune"
all_data$PROFIL_type_orga_rattach[grepl("plantes sauvages", all_data$PROFIL_type_orga_rattach)] <- "Entreprise plantes sauvages"


##### Multivariate : durable vs non durable species profiles ####
all_data_ACM_profil <- all_data %>%
  select(id, ESPECE_species,
         ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill, 
         ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev, 
         ESPECE_usages, ESPECE_parties_cueill,ESPECE_mode
         # ESPECE_espece_reglem, ESPECE_reglem_adaptee, ESPECE_risque,
         # ESPECE_mode_risque,
         # ESPECE_type_cueillette_Cueillette_amatrice_familiale,
         # ESPECE_type_cueillette_Cueillette_professionnelle
         ) %>%
  na.omit()  %>%
  
  mutate(presence = "Oui") %>%
  
  pivot_wider(
    names_from =  ESPECE_parties_cueill,
    values_from = presence,
    names_prefix = "ESPECE_parties_cueill_",
    values_fill = "Non")
  
all_data_ACM_profil <- all_data_ACM_profil %>%
  mutate(across(c(ESPECE_durabilite:ESPECE_mode, starts_with("ESPECE_parties_cueill_")), as.factor))

res.mca <- MCA(all_data_ACM_profil %>% select(-id, -ESPECE_species),
               graph = FALSE)

plot(res.mca)
plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plotellipses(res.mca)

# Extraire les coordonnées des individus
ind_coords <- res.mca$ind$coord

# Réaliser une CAH sur les coordonnées des individus
res.hcpc <- HCPC(res.mca, nb.clust = -1, graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal
print(res.hcpc$desc.var)

# Créer un dataframe pour la visualisation
# On récupère les coordonnées des individus et on ajoute leur groupe d'appartenance
plot_data <- data.frame(
  id = rownames(res.mca$ind$coord),
  Dim.1 = res.mca$ind$coord[,1],
  Dim.2 = res.mca$ind$coord[,2],
  Cluster = as.factor(res.hcpc$data.clust$clust) # Le cluster est dans l'objet HCPC
)

# Créer le graphique
p <- ggplot(plot_data, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  geom_point(alpha = 0.8, size = 3) +
  stat_ellipse(aes(group = Cluster), type = "norm", geom = "polygon", alpha = 0.2, linetype = "dashed") +
  labs(
    title = "ACM des profils d'espèces et classification en clusters",
    subtitle = paste0("Variance expliquée par Dim.1 et Dim.2 : ", 
                      round(res.mca$eig[1,2], 1), "% et ", 
                      round(res.mca$eig[2,2], 1), "%"),
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") # Utilisation d'une palette de couleurs plus lisible

print(p)


# Résultat : 
#
# Cluster 1 : Les espèces communes et durables
# Ce cluster, le plus grand avec 588 réponses, représente le profil de perception le plus dominant et le plus positif. Il est caractérisé par une sur-représentation de modalités décrivant un état de ressource sain et une cueillette durable, comme une durabilité perçue comme Durable (97,3%, v.test=23.47) et un état de ressource stable (74,4%, v.test=19.31). Les espèces les plus fortement associées à ce profil sont l'Ortie dioïque (Urtica dioica) avec 92 réponses et le Thym vulgaire (Thymus vulgaris) avec 70 réponses, ce qui confirme leur statut perçu comme abondant et non menacé.
# 
# Cluster 2 : Les espèces à l'information manquante
# Ce cluster de 197 réponses ne reflète pas un profil écologique, mais une incertitude. Il est défini par une concentration de réponses JNSP (Je Ne Sais Pas) pour les variables clés telles que l'intensité de prélèvement (68,0%, v.test=19.78) et la variation du prélèvement (70,6%, v.test=16.63). La présence d'espèces comme l'Ail des ours (Allium ursinum) et l'Ortie dioïque (Urtica dioica) dans ce cluster, bien que moins fréquente que dans le Cluster 1, met en évidence des lacunes dans les connaissances des participants, même pour des espèces réputées communes.
# 
# Cluster 3 : Les espèces vulnérables
# Ce cluster, composé de 323 réponses, présente un profil de risque et de déclin. Il est fortement caractérisé par une sur-représentation de perceptions négatives, notamment une durabilité perçue comme Non durable (83,3%, v.test=23.86) et un état de ressource jugé Diminué (77,1%, v.test=19.06). Certaines espèces y sont majoritairement associées, comme l'Arnica des montagnes (Arnica montana) et la Gentiane jaune (Gentiana lutea), confirmant leur statut perçu comme vulnérable.
# 
# Divergences de perception
# L'analyse révèle une variabilité de perception significative, car plusieurs espèces sont présentes dans différents profils. Le cas de l'Ail des ours (Allium ursinum) est particulièrement illustratif : il est presque également représenté dans le Cluster 1 (37 réponses) et le Cluster 3 (40 réponses), indiquant un désaccord profond sur son statut entre les répondants. De même, des espèces comme le Myrtillier (Vaccinium myrtillus) et le Narcisse des prés (Narcissus pseudonarcissus), bien que majoritairement perçues comme durables, apparaissent également dans le Cluster 3, suggérant que pour une partie des répondants, leur perception s'oriente vers un déclin ou une vulnérabilité. 



# Créer un dataframe avec le nom de l'espèce et le cluster
species_clusters <- data.frame(
  ESPECE_species = all_data_ACM_profil$ESPECE_species,
  Cluster = res.hcpc$data.clust$clust
)
# Calculer le nombre de réponses pour chaque espèce dans chaque cluster
species_by_cluster <- species_clusters %>%
  group_by(ESPECE_species, Cluster) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(ESPECE_species)

# Afficher les 20 espèces les plus représentées dans chaque cluster
species_by_cluster_sorted <- species_by_cluster %>%
  group_by(Cluster) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  ungroup()

print(species_by_cluster_sorted, n = Inf)



##### PERSONAL NOTES ####
# Faire un chi2 ? beacoup d'individus seuls...
# Dégrader au genre pour certaines espèces (genepi...) pour les grouper et leur donner plus de poids


##### Multivariate : why differing answers ####

all_data_ACM <- all_data %>%
  select(id,PROFIL_cueilleur, PROFIL_type_orga_rattach,
         PROFIL_age, PROFIL_genre, PROFIL_categ_socio_pro, PROFIL_nb_esp_cueill, PROFIL_niveau_educ,
         ESPECE_species,
         ESPECE_durabilite, ESPECE_presence, ESPECE_etendue_cueill, 
         ESPECE_etat_ressource, ESPECE_variation_prelev, ESPECE_intensite_prelev, 
         ESPECE_usages, ESPECE_parties_cueill,ESPECE_mode) %>%
  na.omit()  %>%
  
  mutate(presence = "Oui") %>%
  
  pivot_wider(
    names_from =  ESPECE_parties_cueill,
    values_from = presence,
    names_prefix = "ESPECE_parties_cueill_",
    values_fill = "Non") %>%
  
  mutate(presence = "Oui") %>%
  
  pivot_wider(
    names_from =  ESPECE_usages,
    values_from = presence,
    names_prefix = "ESPECE_usages_",
    values_fill = "Non")




res.mca <- MCA(all_data_ACM %>% select(-id, -ESPECE_species),
               graph = FALSE)

# 5. Interprétation des résultats
plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)
plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)
dimdesc(res.mca)
plotellipses(res.mca)
# plotellipses(res.mca, keepvar=c("PROFIL_type_orga_rattach", "ESPECE_durabilite"))
# plotellipses(res.mca, keepvar=c("ESPECE_durabilite",
#                                 "PROFIL_type_orga_rattach", "ESPECE_variation_prelev",
#                                 "ESPECE_intensite_prelev", "ESPECE_etat_ressource", "ESPECE_mode"),
#              autoLab = "yes",
#              cex = 1.25)




# Extraire les coordonnées des individus
ind_coords <- res.mca$ind$coord

# Réaliser une CAH sur les coordonnées des individus
res.hcpc <- HCPC(res.mca, nb.clust = -1, graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal

res.hcpc <- HCPC(res.mca, nb.clust = 3, graph = FALSE) # nb.clust = -1 laisse FactoMineR choisir le nombre optimal

print(res.hcpc$desc.var)

# Créer un dataframe pour la visualisation
# On récupère les coordonnées des individus et on ajoute leur groupe d'appartenance
plot_data <- data.frame(
  id = rownames(res.mca$ind$coord),
  Dim.1 = res.mca$ind$coord[,1],
  Dim.2 = res.mca$ind$coord[,2],
  Cluster = as.factor(res.hcpc$data.clust$clust) # Le cluster est dans l'objet HCPC
)

# Créer le graphique
p <- ggplot(plot_data, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
  geom_point(alpha = 0.8, size = 3) +
  stat_ellipse(aes(group = Cluster), type = "norm", geom = "polygon", alpha = 0.2, linetype = "dashed") +
  labs(
    title = "ACM des profils d'espèces et classification en clusters",
    subtitle = paste0("Variance expliquée par Dim.1 et Dim.2 : ", 
                      round(res.mca$eig[1,2], 1), "% et ", 
                      round(res.mca$eig[2,2], 1), "%"),
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") # Utilisation d'une palette de couleurs plus lisible

print(p)











#######################################################

# Example: MCA on a dataset
# res.mca <- MCA(your_data, quali.sup = ..., graph = FALSE)

# Extract individual coordinates
ind_coords <- as.data.frame(res.mca$ind$coord)

# Add grouping variable(s) from your dataset
ind_coords$group <- all_data_ACM$PROFIL_type_orga_rattach  # or another factor
colnames(ind_coords) <- sub(" ", ".", colnames(ind_coords))

# Basic scatter plot with ellipses
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, color = group)) +
  geom_point(alpha = 0.6) +
  stat_ellipse(level = 0.95, type = "norm") +
  theme_minimal() +
  labs(title = "MCA with Ellipses",
       x = paste0("Dim 1 (", round(res.mca$eig[1,2],1), "%)"),
       y = paste0("Dim 2 (", round(res.mca$eig[2,2],1), "%)"))


library(tidyr)

ind_long <- ind_coords %>%
  mutate(PROFIL = all_data_ACM$PROFIL_type_orga_rattach,
         DURABILITE = all_data_ACM$ESPECE_durabilite,
         VARIATION = all_data_ACM$ESPECE_variation_prelev,
         INTENSITE = all_data_ACM$ESPECE_intensite_prelev,
         ETAT = all_data_ACM$ESPECE_etat_ressource,
         MODE = all_data_ACM$ESPECE_mode) %>%
  pivot_longer(cols = PROFIL:MODE, names_to = "Variable", values_to = "Category")

ggplot(ind_long, aes(x = Dim.1, y = Dim.2, color = Category)) +
  geom_point(alpha = 0.6) +
  stat_ellipse(level = 0.95) +
  facet_wrap(~ Variable) +
  theme_minimal()

# Results :
# Cluster 1 : Le cluster des "Optimistes éclairés" 🌱
# Ce cluster est associé à une perception très positive de la durabilité et de la ressource. Ses caractéristiques révèlent un profil de répondants avec une expertise de terrain et une vision pragmatique.
# 
# Une perception positive et stable : Les répondants de ce cluster décrivent les espèces comme étant majoritairement durables (v.test=19.6), avec une ressource stable (v.test=14.4) et une présence abondante (v.test=14.0). Ils estiment que la cueillette a un impact faible (v.test=6.56) ou moyen (v.test=5.20), et que la variation de prélèvement est stable (v.test=8.92).
# 
# Profil du répondant : Il s'agit en grande majorité de personnes qui pratiquent la cueillette de manière régulière (non-cueilleurs sous-représentés, v.test=−11.08). Le fait que les retraités (v.test=2.95) soient sur-représentés peut suggérer que cette perception positive est le fruit d'une expérience à long terme et d'une connaissance transmise. L'absence de rattachement à un organisme (v.test=2.16) renforce l'idée d'une connaissance empirique, non-institutionnalisée, acquise au fil du temps.
# 
# Lien entre usage et perception : La sur-représentation de la cueillette de feuilles (v.test=6.03) et de jeunes pousses (v.test=3.70) et l'usage alimentaire (v.test=7.33) sont des informations clés. Ces pratiques, souvent associées à des plantes communes et non protégées, ainsi qu'à des méthodes de prélèvement non destructives (ne pas arracher la plante entière), peuvent expliquer une perception rassurante de la ressource.
# 
# Cluster 2 : Le cluster des "Prudents" ou "Non-initiés" 🤷
# Ce cluster est caractérisé par un manque de connaissances ou une grande prudence dans les réponses.
# 
# Profil du répondant : Ce groupe se compose principalement d'individus ne se considérant pas comme cueilleurs (non-cueilleurs sur-représentés, v.test=−3.41 dans le cluster 3). Les jeunes de moins de 26 ans (v.test=5.91) et les personnes n'ayant que peu d'expérience de la cueillette (1 à 5 espèces, v.test=5.77) sont fortement sur-représentés. Cela indique un fossé générationnel et d'expérience entre les profils.
# 
# Une approche professionnelle et scientifique : La sur-représentation des personnes rattachées à la Police de l'environnement (v.test=9.58) ou à des organismes de gestion d'espaces (v.test=4.58) est la conclusion la plus notable. Plutôt que de donner une perception, ces professionnels ont un v.test positif pour la modalité "Je ne sais pas" (JNSP), ce qui est la signature de ce cluster. Cela n'est pas une "méconnaissance", mais plutôt un refus de l'extrapolation. Ils ne s'aventurent pas à donner un avis sur l'état d'une ressource sans données scientifiques précises.
# 
# Caractère des espèces : Les espèces associées à ce cluster sont souvent perçues comme ayant une variation de prélèvement variable (v.test=7.29) ou une intensité de prélèvement variable (v.test=7.41), ce qui renforce l'idée de l'incertitude. L'usage ornemental (v.test=6.86) et la cueillette de la sève (v.test=2.66) sont également sur-représentés. Ces pratiques peuvent concerner des espèces moins communes ou avec des modes de prélèvement plus délicats.
# 
# Cluster 3 : Le cluster des "Pessimistes engagés" 📉
# Ce cluster reflète une perception négative, souvent associée à un statut de ressource en déclin.
# 
# Une perception négative : Ce groupe perçoit majoritairement les espèces comme non durables (v.test=17.06), en déclin (v.test=11.33) et rares (v.test=9.87). L'intensité de prélèvement est perçue comme forte (v.test=7.53) ou en augmentation (v.test=6.56).
# 
# Profil du répondant : Ce cluster est sur-représenté par des profils de non-cueilleurs, ce qui peut être lié à la perception d'une ressource rare ou non disponible. Les agriculteurs exploitants (v.test=3.67) et les artisans, commerçants ou chefs d'entreprise (v.test=3.18) sont sur-représentés. Cela peut indiquer une perception liée à des enjeux socio-économiques (compétition pour la ressource, transformation des paysages agricoles) ou à une observation du déclin de certaines espèces sauvages dans leur environnement professionnel.
# 
# Usages et modes de prélèvement : La cueillette de fleurs (v.test=4.20) et de parties souterraines (v.test=3.09) est sur-représentée. Ces pratiques, considérées comme plus invasives ou potentiellement destructrices pour la plante (en particulier la cueillette des racines), peuvent être associées à une perception du déclin de la ressource.











