# Load required libraries
library(mFD)
library(tibble)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(FD)
library(ade4)
library(geometry)

# Source external functions (Ensure these files exist!)
source("R/Anik/sp_tr_en_tr_cat.R")  
source("R/Anik/comparaison_chull.R")  

# Load data
depth_fish_biomass <- read.csv(here::here("data", "depth_fish_biomass.csv"), row.names = 1, sep = ";", dec = ".")
fish_traits <- read.csv(here::here("data", "fish_traits.csv"), row.names = 1, sep = ";", dec = ".")

# Convert character traits to factor
fish_traits <- fish_traits %>% mutate(across(where(is.character), as.factor))

# Function to randomize traits while preserving factor levels
randomize_traits <- function(traits_mat) {
  randomized_mat <- traits_mat
  for (trait in colnames(traits_mat)) {
    if (is.factor(traits_mat[[trait]])) {
      randomized_mat[[trait]] <- factor(sample(traits_mat[[trait]], replace = FALSE), levels = levels(traits_mat[[trait]]))
    } else {
      randomized_mat[[trait]] <- sample(traits_mat[[trait]], replace = FALSE)
    }
  }
  return(randomized_mat)
}

# Number of simulations
n_simulations <- 999
indice_name <- c("fric")

# Depth layers
depth_layers <- rownames(depth_fish_biomass)

# Initialize results storage
obsFD <- setNames(numeric(length(depth_layers)), depth_layers)
convexhull_obs <- setNames(numeric(length(depth_layers)), depth_layers)
dbFD_result_obs <- setNames(numeric(length(depth_layers)), depth_layers)
FD_convhull_obs <- setNames(numeric(length(depth_layers)), depth_layers)

# Randomization results storage
resultsRandomFD <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))
resultsRandomconvhull <- resultsRandomFD
resultsRandomFD_convhull <- resultsRandomFD
dbFD_result_sim <- resultsRandomFD

# Loop through each depth layer
for (layer in depth_layers) {
  
  # Get species present in this layer
  species_in_layer <- colnames(depth_fish_biomass)[depth_fish_biomass[layer, ] > 0]
  
  # Filter trait matrix for present species & remove constant traits
  traits_layer <- fish_traits[species_in_layer, , drop = FALSE] %>%
    select(where(~ length(unique(.)) > 1))  
  
  # Skip if no variable traits remain
  if (ncol(traits_layer) == 0) next  
  
  # Assurer le bon format de biomass_layer
  biomass_layer <- matrix(as.numeric(depth_fish_biomass[layer, species_in_layer, drop = FALSE]), 
                          nrow = 1, dimnames = list(layer, species_in_layer))
  
  # Convert categorical traits
  fish_traits_cat <- sp_tr_en_tr_cat(traits_layer)[, -3]  
  
  # Compute functional distance
  sp_dist <- mFD::funct.dist(
    sp_tr = traits_layer,
    tr_cat = fish_traits_cat,
    metric = "gower",
    scale_euclid = "scale_center",
    ordinal_var = "classic",
    weight_type = "equal",
    stop_if_NA = TRUE
  )
  
  # Define functional space
  fspaces_quality <- mFD::quality.fspaces(
    sp_dist = sp_dist,
    maxdim_pcoa = 10,
    deviation_weighting = "absolute",
    fdist_scaling = FALSE,
    fdendro = "average"
  )
  
  sp_faxes_coord <- fspaces_quality$details_fspaces$sp_pc_coord
  
  # Compute observed functional richness (FRic)
  obsFD[layer] <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord[species_in_layer, 1:4, drop = FALSE],
    asb_sp_w =biomass_layer,
    ind_vect = indice_name,
    scaling = TRUE,
    check_input = TRUE,
    details_returned = FALSE
  )$functional_diversity_indices[1, indice_name]
  
  # Compute convex hull observed values
  convexhull_obs[layer] <- calculate_convex_hull(traits_layer, plot_result = FALSE)$volume
  FD_convhull_obs[layer] <- convhulln(as.matrix(sp_faxes_coord[, 1:4, drop = FALSE]), options = "FA")$vol
  dbFD_result_obs[layer] <- dbFD(x = traits_layer, w.abun = TRUE, m = 4, a = biomass_layer)$FRic
  
  print(paste0("Starting simulations for: ", layer))
  
  # Run randomizations
  for (sim in 1:n_simulations) {
    randomized_traits <- randomize_traits(traits_layer)
    
    sp_dist_random <- mFD::funct.dist(
      sp_tr = randomized_traits,
      tr_cat = fish_traits_cat,
      metric = "gower",
      scale_euclid = "scale_center",
      ordinal_var = "classic",
      weight_type = "equal",
      stop_if_NA = TRUE
    )
    
    fspaces_quality_random <- mFD::quality.fspaces(
      sp_dist = sp_dist_random,
      maxdim_pcoa = 10,
      deviation_weighting = "absolute",
      fdist_scaling = FALSE,
      fdendro = "average"
    )
    
    sp_faxes_coord_random <- fspaces_quality_random$details_fspaces$sp_pc_coord
    
    resultsRandomFD_convhull[layer, sim] <- convhulln(sp_faxes_coord_random[, 1:4], options = "FA")$vol
    dbFD_result_sim[layer, sim] <- dbFD(x = randomized_traits, w.abun = TRUE, m = 4, a = biomass_layer)$FRic
    resultsRandomconvhull[layer, sim] <- calculate_convex_hull(randomized_traits, plot_result = FALSE)$volume
    
    resultsRandomFD[layer, sim] <- mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_random[species_in_layer, 1:4, drop = FALSE],
      asb_sp_w = as.matrix(biomass_layer),
      ind_vect = indice_name,
      scaling = TRUE,
      check_input = TRUE,
      details_returned = FALSE
    )$functional_diversity_indices[1, indice_name]
  }  
}

# Compute SES statistics
meanNullFD <- rowMeans(resultsRandomconvhull, na.rm = TRUE)
sdNullFD <- apply(resultsRandomconvhull, 1, sd, na.rm = TRUE)
SES_FD <- (convexhull_obs - meanNullFD) / sdNullFD

# Convert results to dataframe
results_df <- data.frame(
  depth_layer = factor(names(convexhull_obs), levels = c("Epipelagic", "Upper mesopelagic", "Lower mesopelagic", "Bathypelagic")),
  obsFD = convexhull_obs,
  SES_FD = SES_FD
)

# Plot results
ggplot(results_df, aes(x = depth_layer, y = SES_FD, color = depth_layer)) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(1.96, -1.96), linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(x = "", y = "Standard Effect Size (SES)") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, vjust = 1))
