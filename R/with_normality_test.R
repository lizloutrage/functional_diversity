station_sp <- rbind(data_biomass_2002_2019, data_biomass_2021_2022) %>%
  as.data.frame() %>%
  left_join(metadata) %>%
  select(species, biomass_sp, volume_filtered, station) %>%
  # Divide biomass by the volume filtered at each trawl (g.m3)
  mutate(biomass_cpu = (biomass_sp / volume_filtered) * 1000) %>%
  select(species, biomass_cpu, station) %>%
  replace(is.na(.), 0) %>%
  group_by(species, station) %>%
  mutate(biomass = sum(biomass_cpu)) %>%
  select(-biomass_cpu) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = species, values_from = biomass) %>%
  replace(is.na(.), 0) %>%
  arrange(station) %>%
  filter(!station %in% c("H0411", "L0731", "L0736")) %>%
  tibble::column_to_rownames(var = "station") %>%
  select(order(colnames(.))) %>%
  as.matrix()


# Model parameters ----
n_simulations <- 999

# Correction method for non-Euclidean distances
corr_method <- "lingoes"
depth_layers <- rownames(station_sp)  

# Indices to calculate
indices <- c("FRic", "FDis", "FDiv", "FEve")

# Data definition: list with depth and station matrices ----
analysis_levels <- list(
  depth_layer = depth_fish_biomass,
  station = station_sp
)

# Store results ----
all_results <- list()
dbFD_result_sim_list <- list()

# Function to calculate FD indices and SES ----
calculate_FD_and_SES <- function(data_matrix, traits_data, n_simulations, indices, corr_method) {
  layers <- rownames(data_matrix)
  dbFD_result_obs <- matrix(NA, nrow = length(layers), ncol = length(indices), 
                            dimnames = list(layers, indices))
  dbFD_result_sim <- array(NA, dim = c(length(layers), n_simulations, length(indices)),
                           dimnames = list(layers, paste0("Sim.", 1:n_simulations), indices))
  
  # Loop through each layer
  for (layer in layers) {
    species_in_layer <- colnames(data_matrix)[data_matrix[layer, ] > 0]
    traits_layer <- traits_data[species_in_layer, , drop = FALSE] %>%
      select(where(~ length(unique(.)) > 1)) %>%
      droplevels()
    
    if (ncol(traits_layer) == 0) next
    
    biomass_layer <- matrix(as.numeric(data_matrix[layer, species_in_layer, drop = FALSE]),
                            nrow = 1, dimnames = list(layer, species_in_layer))
    
    # Calculate FD indices for observed data
    dbFD_result <- FD::dbFD(x = traits_layer, w.abun = TRUE, m = 4, a = biomass_layer,
                            messages = FALSE, corr = corr_method)
    
    # Verify structure of dbFD_result
    indices_names <- names(dbFD_result)  
    
    # Extract valid indices
    valid_indices <- intersect(indices, indices_names)
    dbFD_result_obs[layer, valid_indices] <- dbFD_result[valid_indices]
    
    # Random simulations
    for (sim in 1:n_simulations) {
      randomized_traits <- randomize_traits(traits_layer)
      dbFD_sim_result <- FD::dbFD(x = randomized_traits, w.abun = TRUE, m = 4, a = biomass_layer,
                                  messages = FALSE, corr = corr_method)
      
      # Verify structure of dbFD_sim_result
      indices_names_sim <- names(dbFD_sim_result)
      valid_indices_sim <- intersect(indices, indices_names_sim)
      
      # Store simulation results
      dbFD_result_sim[layer, sim, valid_indices_sim] <- dbFD_sim_result[valid_indices_sim]
    }
  }
  
  # Calculate SES (Standardized Effect Size) ----
  SES_results <- data.frame(name = rownames(dbFD_result_obs))
  for (index in indices) {
    meanNullFD <- rowMeans(dbFD_result_sim[, , index], na.rm = TRUE)
    sdNullFD <- apply(dbFD_result_sim[, , index], 1, sd, na.rm = TRUE)
    SES_results[[index]] <- (dbFD_result_obs[, index] - meanNullFD) / sdNullFD
  }
  
  return(list(dbFD_result_obs = dbFD_result_obs, dbFD_result_sim = dbFD_result_sim, SES_results = SES_results))
}

# Loop through analysis levels
for (level in names(analysis_levels)) {
  data_matrix <- analysis_levels[[level]]
  layers <- rownames(data_matrix)
  
  # Matrices for observed and simulated results
  dbFD_result_obs <- matrix(NA, nrow = length(layers), ncol = length(indices),
                            dimnames = list(layers, indices))
  
  dbFD_result_sim <- array(NA, dim = c(length(layers), n_simulations, length(indices)),
                           dimnames = list(layers, paste0("Sim.", 1:n_simulations), indices))
  
  # Loop through each layer
  for (layer in layers) {
    species_in_layer <- colnames(data_matrix)[data_matrix[layer, ] > 0]
    traits_layer <- fish_traits[species_in_layer, , drop = FALSE] %>%
      select(where(~ length(unique(.)) > 1)) %>%
      droplevels()
    
    if (ncol(traits_layer) == 0) next
    
    biomass_layer <- matrix(as.numeric(data_matrix[layer, species_in_layer, drop = FALSE]),
                            nrow = 1, dimnames = list(layer, species_in_layer))
    
    # Calculate FD indices for the observed data
    dbFD_result <- FD::dbFD(x = traits_layer, w.abun = TRUE, m = 4, a = biomass_layer,
                            messages = FALSE, corr = corr_method)
    
    # Store observed results
    dbFD_result_obs[layer, "FRic"] <- dbFD_result$FRic
    dbFD_result_obs[layer, "FDis"] <- dbFD_result$FDis
    dbFD_result_obs[layer, "FEve"] <- dbFD_result$FEve
    dbFD_result_obs[layer, "FDiv"] <- dbFD_result$FDiv
    
    # Perform random simulations
    for (sim in 1:n_simulations) {
      randomized_traits <- randomize_traits(traits_layer)
      
      dbFD_sim_result <- FD::dbFD(x = randomized_traits, w.abun = TRUE, m = 4, a = biomass_layer,
                                  messages = FALSE, corr = corr_method)
      
      # Store simulated results
      dbFD_result_sim[layer, sim, "FRic"] <- dbFD_sim_result$FRic
      dbFD_result_sim[layer, sim, "FDis"] <- dbFD_sim_result$FDis
      dbFD_result_sim[layer, sim, "FEve"] <- dbFD_sim_result$FEve
      dbFD_result_sim[layer, sim, "FDiv"] <- dbFD_sim_result$FDiv
    }
  }
  
  # Save simulated results for normality tests
  dbFD_result_sim_list[[level]] <- dbFD_result_sim
  
  # Calculate SES (Standardized Effect Size)
  SES_results <- data.frame(level = level, name = rownames(dbFD_result_obs))
  for (index in indices) {
    meanNullFD <- rowMeans(dbFD_result_sim[, , index], na.rm = TRUE)
    sdNullFD <- apply(dbFD_result_sim[, , index], 1, sd, na.rm = TRUE)
    SES_results[[index]] <- (dbFD_result_obs[, index] - meanNullFD) / sdNullFD
  }
  
  all_results[[level]] <- SES_results
}

# Normality and symmetry tests ----
library(nortest)
library(moments)

# Normality and symmetry results
normality_results <- list()

for (level in names(all_results)) {
  SES_data <- all_results[[level]]
  simulated_values <- dbFD_result_sim_list[[level]]
  
  normality_test <- data.frame(level = level, name = SES_data$name)
  
  for (index in indices) {
    # Normality test (Shapiro-Wilk)
    normality_test[[paste0(index, "_shapiro_p")]] <- apply(simulated_values[, , index], 1, function(x) shapiro.test(x)$p.value)
    
    # Skewness 
    normality_test[[paste0(index, "_skewness")]] <- apply(simulated_values[, , index], 1, skewness)
  }
  
  normality_results[[level]] <- normality_test
}

# Stockage final des résultats
normality_results_combined <- bind_rows(normality_results$station, normality_results$depth_layer, .id = "level")

# Combine results
SES_results_depth_layer <- all_results$depth_layer %>%
  tidyr::pivot_longer(cols = -c(name, level), names_to = "index", values_to = "SES_FD") %>%
  mutate(depth_layer=name)

SES_results_station <- all_results$station %>%
  rename(station=name) %>%
  tidyr::pivot_longer(cols = -c(station, level), names_to = "index", values_to = "SES_FD") %>%
  inner_join(metadata %>% select(station, depth), by = "station") %>%
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  )

# Combine datasets
SES_results_combined <- bind_rows(SES_results_station, SES_results_depth_layer)

SES_results_combined$depth_layer <- factor(
  SES_results_combined$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

SES_results_combined$index <- factor(
  SES_results_combined$index,
  levels = c("FRic", "FDis", "FDiv", "FEve"),
  labels = c(
    "Functional richness",
    "Functional dispersion",
    "Functional divergence",
    "Functional evenness"
  )
)

# Plot ----
ggplot(SES_results_combined, aes(x = depth_layer, y = SES_FD)) +
  geom_boxplot(data = SES_results_combined %>% filter(level == "station"),
               aes(color = depth_layer, fill = depth_layer),
               alpha = 0.08, width = 0.5, outlier.shape = NA) +
  geom_point(data = SES_results_combined %>% filter(level == "depth_layer"),
             aes(color = depth_layer),
             size = 5, shape = 18) +
  geom_jitter(data = SES_results_combined %>% filter(level == "station"),
              aes(color = depth_layer),
              size = 1, width = 0.2, alpha = 0.35) +
  geom_hline(yintercept = c(1.96, -1.96), linetype = "dashed", color = "gray40", size = 0.8) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  facet_wrap(~ index, labeller = labeller(index = c(
    FRic = "Functional richness",
    FDis = "Functional dispersion",
    FDiv = "Functional divergence",
    FEve = "Functional evenness"
  ))) +
  labs(x = NULL, y = "Standard Effect Size (SES)") +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 14, color = "black"),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13)
  ) +
  guides(col = "none", fill = "none")

#ggsave("SES_dbFD_two_levels.png", path = "figures", dpi = 700, height = 7, width = 9)

#save(SES_results_combined, normality_results_combined, file = "data.RData")
