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

# Initialize results storage for dbFD
dbFD_result_obs <- setNames(numeric(length(depth_layers)), depth_layers)
dbFD_result_sim <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))

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
  
  # Compute dbFD observed values
  dbFD_result_obs[layer] <- dbFD(x = traits_layer, w.abun = TRUE, m = 4, a = biomass_layer)$FRic
  
  print(paste0("Starting simulations for: ", layer))
  
  # Run randomizations for dbFD
  for (sim in 1:n_simulations) {
    randomized_traits <- randomize_traits(traits_layer)
    
    dbFD_result_sim[layer, sim] <- dbFD(x = randomized_traits, w.abun = TRUE, m = 4, a = biomass_layer)$FRic
  }  
}

# Compute SES statistics for dbFD method
meanNullFD <- rowMeans(dbFD_result_sim, na.rm = TRUE)
sdNullFD <- apply(dbFD_result_sim, 1, sd, na.rm = TRUE)
SES_FD <- (dbFD_result_obs - meanNullFD) / sdNullFD

# Convert results to dataframe for dbFD
results_df <- data.frame(
  depth_layer = factor(names(dbFD_result_obs), levels = c("Epipelagic", "Upper mesopelagic", "Lower mesopelagic", "Bathypelagic")),
  obsFD = dbFD_result_obs,
  SES_FD = SES_FD
)

# Plot results for dbFD
ggplot(results_df, aes(x = depth_layer, y = SES_FD, color = depth_layer)) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(1.96, -1.96), linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(x = "", y = "Standard Effect Size (SES)") +
  theme_light() +
  theme(axis.text.x = element_blank())

#ggsave("SES_FRic_dbFD.png", path = "figures", dpi = 700, height = 5, width = 7)
