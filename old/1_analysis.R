#### Load packages ####
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(stringr)
library(sf)
library(cowplot)
library(patchwork)

#### Import data ####
raw_data <- read.csv(here::here("raw_data/results-survey676868_prefilter.csv"))

departements <- read_sf("raw_data/departements_FR.shp") %>%
  mutate(
    dpt_simple = gsub("-", " ", dpt),
    region_simple = gsub("-", " ", region)
  )


#### Map the number of answers per departement #####
##### Helper function to process expertise levels ####
process_expertise <- function(data, pattern, name_col, join_col, shp, shp_col) {
  data %>%
    pivot_longer(cols = starts_with(pattern), names_to = name_col, values_to = "Réponse") %>%
    filter(Réponse == "Oui") %>%
    mutate(
      !!name_col := case_when(
        pattern == "G1Q00002_" ~ sub(".*___(.*?)__\\d+[AB]?$", "\\1", .data[[name_col]]),
        pattern == "G1Q00003_" ~ sub(".*___(.*)$", "\\1", .data[[name_col]])
      ),
      !!name_col := gsub("_", " ", .data[[name_col]])
    ) %>%
    select(id__ID_de_la_réponse,
           G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous,
           !!sym(name_col)) %>%
    full_join(shp, join_by(!!sym(name_col) == !!sym(shp_col))) %>%
    select(id__ID_de_la_réponse,
           G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous,
           dpt, region, geometry)
}

##### Process departements and regions ####
raw_data_departements <- process_expertise(raw_data, "G1Q00002_", "Departement_expertise", "dpt_simple", departements, "dpt_simple")
raw_data_regions      <- process_expertise(raw_data, "G1Q00003_", "Region_expertise", "region_simple", departements, "region_simple")

##### National level (not used for these maps) ####
raw_data_national <- raw_data %>%
  filter(G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous == "National (France entière)") %>%
  transmute(
    id__ID_de_la_réponse,
    G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous)

n_national_answers <- n_distinct(raw_data_national$id__ID_de_la_réponse)

answers_per_dpt <- raw_data_departements %>%
  filter(G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous == "Départemental") %>%
  filter(!is.na(dpt)) %>%
  group_by(dpt, geometry,G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous) %>%
  summarise(n_answers = n_distinct(id__ID_de_la_réponse), .groups = "drop") %>%
  full_join(departements) %>%
  select(dpt, geometry, n_answers) %>%
  st_as_sf()


# Your existing code to prepare the regional map data
regional_geometries <- departements %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()

answers_per_region <- raw_data_regions %>%
  filter(G1Q00001__À_quelle_échelle_spatiale_vous_situez_vous == "Régional (anciennes régions)") %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(n_answers = n_distinct(id__ID_de_la_réponse), .groups = "drop")

answers_per_region_sf <- regional_geometries %>%
  left_join(answers_per_region, by = "region")

# Calculate the total number of unique survey answers
total_answers <- n_distinct(raw_data$id__ID_de_la_réponse)

# Calculate the common range for the color scale
max_answers <- max(answers_per_dpt$n_answers, answers_per_region_sf$n_answers)


# Create the departmental plot with the shared color aesthetic
dpt_plot <- ggplot(answers_per_dpt) +
  geom_sf(aes(fill = n_answers), color = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = n_answers), color = "black", size = 3) +
  scale_fill_distiller(
    name = "Answers",
    palette = "Reds",
    direction = 1,
    limits = c(0, max_answers),
    na.value = "grey80" # Ensures departments with 0 answers are grey80
  ) +
  theme_void() +
  labs(
    title = "Survey answers per département"
  )

# Create the regional plot WITHOUT a legend
region_plot <- ggplot(answers_per_region_sf) +
  geom_sf(aes(fill = n_answers), color = "white", linewidth = 0.2, show.legend = FALSE) + # Here's the key change
  geom_sf_text(aes(label = n_answers), color = "black", size = 3) +
  scale_fill_distiller(
    name = "Answers",
    palette = "Reds",
    direction = 1,
    limits = c(0, max_answers),
    na.value = "grey80" # Ensures regions with 0 answers are grey80
  ) +
  theme_void() +
  labs(
    title = "Survey answers per region"
  )

# Combine the plots using patchwork and add the combined caption
combined_plot <- dpt_plot + region_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = paste("Total national level answers:", n_national_answers, "\n",
                    "Total survey answers:", total_answers),
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 12))
  )

# Display the combined plot
combined_plot




#### Barplots G2 section #####
# Function to create barplots with prefix matching
plot_species_bar <- function(data, species_prefix, title) {
  
  # Select the species column(s) starting with the prefix + type_cueilleur
  df <- data %>%
    select(starts_with(species_prefix), starts_with("G21Q00009_"))
  
  # Rename columns
  colnames(df) <- c("species", "type_cueilleur")
  
  # Keep first two words of type_cueilleur
  df$type_cueilleur <- word(df$type_cueilleur, 1, 2)
  
  # Filter NAs and split multiple species
  df_long <- df %>%
    filter(!is.na(species)) %>%
    separate_rows(species, sep = ",\\s*")
  
  # Plot
  ggplot(df_long, aes(x = reorder(species, -table(species)[species]))) +
    geom_bar(aes(fill = type_cueilleur)) +
    labs(
      title = title,
      x = "Species",
      y = "Number of citations"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Generate all plots
plot_species_bar(raw_data, "G2Q00002_SQ001_", "The most harvested species")
plot_species_bar(raw_data, "G2Q00003_SQ001_", "The most expensive species")
plot_species_bar(raw_data, "G2Q00004_SQ001_", "The most difficult species to get")
plot_species_bar(raw_data, "G2Q00005_SQ001_", "The species most subject to trends")




#### Barplots : durable/NOT durable species ####
# Function to create durable/non-durable barplots with type_cueilleur
plot_durability_bar <- function(data, species_prefix, durability_flag = "Oui", title) {
  
  # select relevant columns + type_cueilleur
  df <- data %>%
    select(
      id__ID_de_la_réponse,
      starts_with(species_prefix),
      matches("^G(6|8|10|12|14|16|18|20)Q0000[12]_"),
      type_cueilleur=starts_with("G21Q00009_") # type_cueilleur
    )
  
  df$type_cueilleur <- word(df$type_cueilleur, 1, 2)
  
  # pivot longer for species columns
  df_durability <- df %>%
    pivot_longer(
      cols = -c(id__ID_de_la_réponse, type_cueilleur),
      names_to = c("group", ".value"),
      names_pattern = "(G\\d+)Q0000(\\d).*",
      values_drop_na = TRUE
    ) %>%
    filter(`2` == durability_flag | is.na(`2`)) %>%   # keep durable or non-durable
    transmute(
      id__ID_de_la_réponse,
      species = `1`,
      type_cueilleur
    ) %>%
    separate_rows(species, sep = ",\\s*") %>%
    mutate(species = str_trim(species)) %>%
    filter(species != "")
  
  # plot
  ggplot(df_durability, aes(x = reorder(species, -table(species)[species]))) +
    geom_bar(aes(fill = type_cueilleur)) +
    labs(
      title = title,
      x = "Species",
      y = "Number of citations"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Durable species
plot_durability_bar(raw_data, "G3Q00001_", durability_flag = "Oui", 
                    title = "Species with durable harvesting")

# Non-durable species
plot_durability_bar(raw_data, "G4Q00001_", durability_flag = "Non", 
                    title = "Species with NON durable harvesting")


#### Total number of cited species ####
all_species <- raw_data %>%
  select(id__ID_de_la_réponse,
         matches("^G(3|4|6|8|10|12|14|16|18|20)Q0000[1]_"),
         type_cueilleur=starts_with("G21Q00009_")) %>%
  pivot_longer(cols = -c(id__ID_de_la_réponse, type_cueilleur),
               values_to = "species") %>%  # gather into one column
  filter(!is.na(species)) %>%                # remove NA
  separate_rows(species, sep = ",\\s*") %>%  # split multiple species
  mutate(species = str_trim(species)) %>%    # remove extra spaces
  filter(species != "") %>%                 # remove empty strings
  select(-name) %>%
  unique() # to no count when the same person has cited a species multiple times

all_species$type_cueilleur <- word(all_species$type_cueilleur, 1, 2)


ggplot(all_species, aes(x = reorder(species, -table(species)[species]), fill = type_cueilleur)) +
  geom_bar() +
  labs(
    title = "All species harvested",
    x = "Species",
    y = "Number of citations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Map the number of species per département ####
# and when group =2 ???

all_species_dpts <- raw_data %>%
  select(id__ID_de_la_réponse,
         matches("^G(3|4|6|8|10|12|14|16|18|20)Q0000[124]_")) %>%
  pivot_longer(
    cols = -id__ID_de_la_réponse,
    names_to = c("group", "qcode"),
    names_pattern = "^G(\\d+)Q0000(\\d)_",
    values_to = "value") %>%
  na.omit() %>%
  # now classify into species or dpt depending on group & qcode
  mutate(type = case_when(
    qcode == "1" ~ "species",
    group %in% c("3", "4") & qcode == "2" ~ "dpt",
    !(group %in% c("3", "4")) & qcode == "4" ~ "dpt")) %>%
  filter(!is.na(type)) %>%
  select(-qcode) %>%
  pivot_wider(
    names_from = type,
    values_from = value) %>%
  select(-group) %>%
  separate_rows(species, sep = ",\\s*") %>%
  mutate(species = str_trim(species),
         dpt = str_trim(str_remove(dpt, "\\s*\\([0-9AB]+\\)"))) %>%
  unique() %>% # to no count when the same person has cited a species multiple times
  inner_join(departements, join_by(dpt)) %>% # remove the "Autre"
  group_by(dpt, code, region, geometry) %>%
  summarise(n=n(), .groups = "drop") %>%
  st_as_sf()


# Plot
ggplot(all_species_dpts) +
  geom_sf(aes(fill = n), color = "white", size = 0.2) +
  geom_sf_text(aes(label = n), color = "black", size = 3) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  theme_void() +
  labs(
    fill = "Number of species",
    title = "Number of different species per département")




#### Map durable/non durable citings separated by pro/non pro ####

plot_durability_pro_bar <- function(data) {
  
  df_g3 <- data %>%
    select(id__ID_de_la_réponse, starts_with("G3Q00001_"),
           type_cueilleur = starts_with("G21Q00009_")) %>%
    pivot_longer(cols = starts_with("G3Q00001_"), names_to = "species", values_to = "val") %>%
    filter(!is.na(val) & val != "") %>%
    separate_rows(val, sep = ",\\s*") %>%
    mutate(durability = "Durable", species = str_trim(val))
  
  df_g4 <- data %>%
    select(id__ID_de_la_réponse, starts_with("G4Q00001_"),
           type_cueilleur = starts_with("G21Q00009_")) %>%
    pivot_longer(cols = starts_with("G4Q00001_"), names_to = "species", values_to = "val") %>%
    filter(!is.na(val) & val != "") %>%
    separate_rows(val, sep = ",\\s*") %>%
    mutate(durability = "Non durable", species = str_trim(val))
  
  df_other <- data %>%
    select(
      id__ID_de_la_réponse,
      matches("^G(6|8|10|12|14|16|18|20)Q0000[12]_"),
      type_cueilleur = starts_with("G21Q00009_")
    ) %>%
    pivot_longer(
      cols = -c(id__ID_de_la_réponse, type_cueilleur),
      names_to = c("group", "coltype"),
      names_pattern = "(G\\d+)Q0000(\\d).*",
      values_drop_na = TRUE
    ) %>%
    pivot_wider(names_from = coltype, values_from = value) %>%
    filter(!is.na(`1`) & `1` != "") %>%
    separate_rows(`1`, sep = ",\\s*") %>%
    mutate(
      species = str_trim(`1`),
      durability = ifelse(`2` == "Oui", "Durable", "Non durable")
    ) %>%
    select(id__ID_de_la_réponse, species, durability, type_cueilleur)
  
  df_long <- bind_rows(df_g3, df_g4, df_other) %>%
    mutate(type_cueilleur = word(type_cueilleur, 1, 2)) %>%
    filter(!is.na(type_cueilleur), species != "")
  
  # Get the facet labels and their order
  facet_labels <- unique(df_long$type_cueilleur)
  
  # Create a separate data frame for annotations
  annotations <- data.frame(
    type_cueilleur = facet_labels,
    my_label = c("Cueillette non professionnelle", "Cueillette professionnelle"),
    x_pos = 50, # Adjust the x-position as needed
    y_pos = Inf, # Use Inf to place the label at the top of each facet
    stringsAsFactors = FALSE
  )
  
  # ---- Plot ----
  common_breaks <- seq(0, max(table(df_long$species)), by = 5)
  
  ggplot(df_long, aes(x = reorder(species, -table(species)[species]), fill = durability)) +
    geom_bar(position = "stack") +
    facet_grid(type_cueilleur ~ ., scales = "free_y", space = "free_y") +
    geom_label(
      data = annotations,
      aes(x = x_pos, y = y_pos, label = my_label),
      hjust = 0, vjust = 1.1, # Adjust justification to place the text just inside the top
      size = 5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
    scale_y_continuous(breaks = common_breaks) +
    labs(
      title = "Species cited by durability status",
      x = "Species",
      y = "Number of citations",
      fill = "Durability"
    ) +
    theme_minimal() +
    theme(
      legend.position = c(0.9, 0.8),
      legend.background = element_rect(fill = "white", colour = "grey70"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = element_line(colour = "grey80"),
      panel.grid.minor.x = element_line(colour = "grey90", linetype = "dotted"),
      # Remove the original facet labels
      strip.text.y = element_blank(),
      strip.background = element_blank()
    )
}

plot_durability_pro_bar(raw_data)



#### Map durable/non durable citings for a single species ####
# Full data processing to get global max values
df_g3 <- raw_data %>%
  select(id__ID_de_la_réponse, starts_with("G3Q00001_"),
         type_cueilleur = starts_with("G21Q00009_")) %>%
  pivot_longer(cols = starts_with("G3Q00001_"), names_to = "species", values_to = "val") %>%
  filter(!is.na(val) & val != "") %>%
  separate_rows(val, sep = ",\\s*") %>%
  mutate(durability = "Durable", species = str_trim(val), group = "G4")%>%
  select(-val)

df_g4 <- raw_data %>%
  select(id__ID_de_la_réponse, starts_with("G4Q00001_"),
         type_cueilleur = starts_with("G21Q00009_")) %>%
  pivot_longer(cols = starts_with("G4Q00001_"), names_to = "species", values_to = "val") %>%
  filter(!is.na(val) & val != "") %>%
  separate_rows(val, sep = ",\\s*") %>%
  mutate(durability = "Non durable", species = str_trim(val), group = "G4")%>%
  select(-val)

df_other <- raw_data %>%
  select(
    id__ID_de_la_réponse,
    matches("^G(6|8|10|12|14|16|18|20)Q0000[12]_"),
    type_cueilleur = starts_with("G21Q00009_")
  ) %>%
  pivot_longer(
    cols = -c(id__ID_de_la_réponse, type_cueilleur),
    names_to = c("group", "coltype"),
    names_pattern = "(G\\d+)Q0000(\\d).*",
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = coltype, values_from = value) %>%
  filter(!is.na(`1`) & `1` != "") %>%
  separate_rows(`1`, sep = ",\\s*") %>%
  mutate(
    species = str_trim(`1`),
    durability = ifelse(`2` == "Oui", "Durable", "Non durable")
  ) %>%
  select(id__ID_de_la_réponse, species, durability, type_cueilleur, group)

df_dpts <- raw_data %>%
  select(id__ID_de_la_réponse,
         matches("^G(3|4|6|8|10|12|14|16|18|20)Q0000[124]_")) %>%
  pivot_longer(
    cols = -id__ID_de_la_réponse,
    names_to = c("group", "qcode"),
    names_pattern = "(G\\d+)Q0000(\\d).*",
    values_to = "value") %>%
  na.omit() %>%
  mutate(type = case_when(
    qcode == "1" ~ "species",
    group %in% c("G3", "G4") & qcode == "2" ~ "dpt",
    !(group %in% c("G3", "G4")) & qcode == "4" ~ "dpt")) %>%
  filter(!is.na(type)) %>%
  select(-qcode) %>%
  pivot_wider(
    names_from = type,
    values_from = value) %>%
  separate_rows(species, sep = ",\\s*") %>%
  mutate(species = str_trim(species),
         dpt = str_replace_all(dpt, "\\s*\\(.*\\)", "")) %>%
  unique()

df_all_species <- bind_rows(df_g3, df_g4, df_other) %>%
  inner_join(df_dpts, by = c("id__ID_de_la_réponse", "species", "group"))

citations_all <- df_all_species %>%
  group_by(dpt, species, durability) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = durability,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    Total_Citations = Durable + `Non durable`,
  )

# Get the global max for each metric
global_max_total <- max(citations_all$Total_Citations, na.rm = TRUE)


plot_species_map <- function(raw_data, departements, species_name) {
  
  # ---- 1. Data Processing to combine species, durability, and dpt information ----
  
  # Process G3 data for durable species
  df_g3 <- raw_data %>%
    select(id__ID_de_la_réponse, starts_with("G3Q00001_")) %>%
    pivot_longer(cols = starts_with("G3Q00001_"), names_to = "species", values_to = "val") %>%
    filter(!is.na(val) & val != "") %>%
    separate_rows(val, sep = ",\\s*") %>%
    mutate(durability = "Durable", species = str_trim(val), group = "G3") %>%
    select(-val)
  
  # Process G4 data for non-durable species
  df_g4 <- raw_data %>%
    select(id__ID_de_la_réponse, starts_with("G4Q00001_")) %>%
    pivot_longer(cols = starts_with("G4Q00001_"), names_to = "species", values_to = "val") %>%
    filter(!is.na(val) & val != "") %>%
    separate_rows(val, sep = ",\\s*") %>%
    mutate(durability = "Non durable", species = str_trim(val), group = "G4") %>%
    select(-val)
  
  # Process "other" data for both durable and non-durable species
  df_other <- raw_data %>%
    select(
      id__ID_de_la_réponse,
      matches("^G(6|8|10|12|14|16|18|20)Q0000[12]_")
    ) %>%
    pivot_longer(
      cols = -id__ID_de_la_réponse,
      names_to = c("group", "coltype"),
      names_pattern = "(G\\d+)Q0000(\\d).*",
      values_drop_na = TRUE
    ) %>%
    pivot_wider(names_from = coltype, values_from = value) %>%
    filter(!is.na(`1`) & `1` != "") %>%
    separate_rows(`1`, sep = ",\\s*") %>%
    mutate(
      species = str_trim(`1`),
      durability = ifelse(`2` == "Oui", "Durable", "Non durable")
    ) %>%
    select(id__ID_de_la_réponse, species, durability, group)
  
  # Combine all species data into one dataframe
  df_long <- bind_rows(df_g3, df_g4, df_other)
  
  # Get the department for each citation ID
  df_dpts <- raw_data %>%
    select(id__ID_de_la_réponse,
           matches("^G(3|4|6|8|10|12|14|16|18|20)Q0000[124]_")) %>%
    pivot_longer(
      cols = -id__ID_de_la_réponse,
      names_to = c("group", "qcode"),
      names_pattern = "(G\\d+)Q0000(\\d).*",
      values_to = "value") %>%
    na.omit() %>%
    mutate(type = case_when(
      qcode == "1" ~ "species",
      group %in% c("G3", "G4") & qcode == "2" ~ "dpt",
      !(group %in% c("G3", "G4")) & qcode == "4" ~ "dpt")) %>%
    filter(!is.na(type)) %>%
    select(-qcode) %>%
    pivot_wider(
      names_from = type,
      values_from = value) %>%
    separate_rows(species, sep = ",\\s*") %>%
    mutate(species = str_trim(species),
           dpt = str_replace_all(dpt, "\\s*\\(.*\\)", "")) %>%
    unique()
  
  # Join species/durability data with department data
  df_full <- df_long %>%
    inner_join(df_dpts, by = c("id__ID_de_la_réponse", "species", "group")) %>%
    filter(species == species_name)
  
  # ---- 2. Summarise and create two separate plots ----
  
  # Summarise data to calculate total citations and proportion of non-durable
  citations_wide <- df_full %>%
    group_by(dpt, durability) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = durability,
      values_from = n,
      values_fill = 0
    )
  
  # Ensure 'Durable' and 'Non durable' columns exist before calculations
  if (!"Durable" %in% names(citations_wide)) {
    citations_wide$Durable <- 0
  }
  if (!"Non durable" %in% names(citations_wide)) {
    citations_wide$`Non durable` <- 0
  }
  
  citations_wide <- citations_wide %>%
    mutate(
      Total_Citations = Durable + `Non durable`,
      Prop_Non_Durable = ifelse(Total_Citations > 0, 
                                round(`Non durable` / Total_Citations * 100,2), 0)
    )
  
  # Join with spatial data for plotting
  map_data <- departements %>%
    left_join(citations_wide, by = "dpt") %>%
    st_as_sf()
  
  # Create the total citations plot
  plot_total <- ggplot(map_data) +
    geom_sf(aes(fill = Total_Citations), color = "white", size = 0.2) +
    geom_sf_text(aes(label = Total_Citations), color = "black", size = 3) +
    scale_fill_distiller(
      palette = "Blues", 
      direction = 1, 
      na.value = "grey80",
      limits = c(0, global_max_total), # Use the common maximum value
      oob = scales::squish # Handles values outside the limit
    ) +
    theme_void() +
    labs(
      title = "Total citations",
      fill = "Number of citations"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  # Create the proportion non-durable plot
  plot_prop <- ggplot(map_data) +
    geom_sf(aes(fill = Prop_Non_Durable), color = "white", size = 0.2) +
    geom_sf_text(
      data = subset(map_data, !is.na(Prop_Non_Durable)),
      aes(label = paste0(Prop_Non_Durable, "%")),
      color = "black", size = 3
    ) +
    scale_fill_distiller(
      palette = "Reds", 
      direction = 1, 
      na.value = "grey80",
      limits = c(0, 100), # Use the common maximum value
      oob = scales::squish # Handles values outside the limit
    ) +
    theme_void() +
    labs(
      title = "Proportion non-durable",
      fill = "Percentage"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  
  # Combine the two plots
  combined_plot <- plot_grid(
    plot_total,
    plot_prop,
    align = "h",
    labels = paste("Citations for", species_name, "by department"),
    label_size = 16,
    label_fontface = "bold",
    nrow = 1
  )
  
  return(combined_plot)
}

plot_species_map(raw_data, departements, "Allium ursinum")
plot_species_map(raw_data, departements, "Arnica montana")
plot_species_map(raw_data, departements, "Gentiana lutea")
plot_species_map(raw_data, departements, "Thymus vulgaris")
plot_species_map(raw_data, departements, "Euphorbia spinosa") ## pas de durable
plot_species_map(raw_data, departements, "Artemisia umbelliformis")
plot_species_map(raw_data, departements, "Sambucus nigra")
plot_species_map(raw_data, departements, "Vaccinium myrtillus")
plot_species_map(raw_data, departements, "Filipendula ulmaria")
plot_species_map(raw_data, departements, "Narcissus pseudonarcissus")



#### Specific questions species ####
# Data preparation
df_all <- raw_data %>%
  # Select all necessary columns at once
  select(id__ID_de_la_réponse,
         type_cueilleur = starts_with("G21Q00009_"),
         matches("^G(3|4|6|8|10|12|14|16|18|20)Q0000(1|2|4|5|6|7)_")) %>%
  
  # Pivot to long format for easier manipulation
  pivot_longer(
    cols = -c(id__ID_de_la_réponse, type_cueilleur),
    names_to = c("group", "qcode"),
    names_pattern = "^G(\\d+)Q0000(\\d)_",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  
  # Classify values by type (species, presence, etendue_cueill, durability)
  mutate(
    type = case_when(
      qcode == "1" ~ "species",
      group %in% c("3", "4") & qcode == "4" ~ "presence",
      !(group %in% c("3", "4")) & qcode == "6" ~ "presence",
      group %in% c("3", "4") & qcode == "5" ~ "etendue_cueill",
      !(group %in% c("3", "4")) & qcode == "7" ~ "etendue_cueill",
      group %in% c("3", "4") & qcode == "2" ~ "dpt",
      !(group %in% c("3", "4")) & qcode == "4" ~ "dpt"),
    durability = case_when(
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
    type_cueilleur = word(type_cueilleur, 1, 2)
  ) %>%
  
  unnest(everything()) %>%
  
  # Filter out empty species
  filter(species != "")


# PART 2: Process `parties_cueill` columns separately
df_parts_collected <- raw_data %>%
  select(id__ID_de_la_réponse, matches("Q0000(6|8)_")) %>%
  pivot_longer(
    cols = -id__ID_de_la_réponse,
    names_to = "column_name",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  filter(value == "Oui") %>%
  mutate(
    group = str_extract(column_name, "(?<=^G)\\d+"),
    # This regex removes everything up to the final "__"
    parties_cueill = str_remove(column_name, ".*__"),
    # Convert to sentence case and remove underscores
    parties_cueill = str_to_sentence(str_replace_all(parties_cueill, "_", " "))
  ) %>%
  select(id__ID_de_la_réponse, group, parties_cueill)

# PART 3: Join the two datasets
df_final <- left_join(df_all, df_parts_collected, by = c("id__ID_de_la_réponse", "group")) %>%
  mutate(
    type_cueilleur = word(type_cueilleur, 1, 2)
  )


##### Plot 1: Presence ####
# Plot the data using the pre-calculated counts
ggplot(df_all, aes(x = reorder(presence, -table(presence)[presence]),
                   fill = durability)) +
  geom_bar() +
  labs(
    title = "Quelle est la présence de cette espèce à l'échelle du département ?",
    y = "Nombre de réponses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 2: Harvesting extent ####
df_harv_ext <- df_final %>%
  filter(!is.na(etendue_cueill))

ggplot(df_harv_ext, aes(x = reorder(etendue_cueill, -table(etendue_cueill)[etendue_cueill]), fill = durability)) +
  geom_bar() +
  labs(
    title = "Comment caractérisez vous l'étendue de la cueillette de cette espèce ?",
    y = "Nombre de réponses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### Plot 3: Harvested parts ####
df_harv_parts <- df_final %>%
  filter(!is.na(parties_cueill))

ggplot(df_harv_parts, aes(x = reorder(parties_cueill, -table(parties_cueill)[parties_cueill]), 
                          fill = durability)) +
  geom_bar() +
  labs(
    title = "Quelles parties de cette espèce sont collectées ?",
    y = "Nombre de réponses",
    x = ""
  ) +
  scale_fill_manual(values = c("Durable" = "darkgreen", "Non durable" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


