##### Group species ########
data_species <- data_enjeux_2_summary %>%
  subset(select=c(ESPECE_nom, unsustainable_ratio, total_areas_presence, total_unsustainable, total_sustainable),
         total_unsustainable>0) %>%
  unique() %>%
  mutate(
    range_type = ifelse(total_areas_presence >= median(total_areas_presence), "Widespread", "Local"),
    problem_type = ifelse(total_unsustainable >= 3, "Widely problematic", "Locally problematic")
    # problem_type = ifelse(total_unsustainable == 0, "Not problematic", problem_type)
    
  )


data_species <- data_species %>%
  mutate(
    category = case_when(
      range_type == "Widespread" & problem_type == "Widely problematic" ~ "Widespread & Widely problematic",
      range_type == "Widespread" & problem_type == "Locally problematic" ~ "Widespread & Locally problematic",
      range_type == "Local" & problem_type == "Widely problematic" ~ "Local & Widely problematic",
      range_type == "Local" & problem_type == "Locally problematic" ~ "Local & Locally problematic",
      TRUE ~ "Not problematic"
    )
  )



species_cluster <- data_species

# Standardize the data
species_scaled <- scale(species_cluster[, c("total_areas_presence", "total_unsustainable")])

# Run k-means with 4 clusters
set.seed(123)
kmeans_result <- kmeans(species_scaled, centers = 4)

species_cluster$cluster <- factor(kmeans_result$cluster)


ggplot(species_cluster, aes(x = total_areas_presence, y = total_unsustainable, color = cluster, label = ESPECE_nom)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, vjust = 1.5) +
  theme_minimal()


# Make sure 'cluster' is numeric
cluster_numeric <- as.numeric(as.character(species_cluster$cluster))
# Compute silhouette
sil <- silhouette(cluster_numeric, dist(species_cluster[, c("total_areas_presence", "total_unsustainable")]))
mean(sil[, 3])  # average silhouette width
plot(sil)
