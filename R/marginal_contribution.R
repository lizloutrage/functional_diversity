library(geometry)  # Pour les enveloppes convexes
library(dplyr)
library(ggplot2)

# Fonction pour calculer FRic
calc_fric <- function(points) {
  if (nrow(points) > 3) {
    hull <- geometry::convhulln(points, options = "FA")  # Enveloppe convexe
    return(hull$vol)  # Volume de l'enveloppe
  } else {
    return(0)  # Pas d'enveloppe possible si <4 points
  }
}

# Fonction pour calculer la contribution marginale des espèces
calc_marginal_contribution <- function(points) {
  base_volume <- calc_fric(points)  # Volume de l'espace fonctionnel avec toutes les espèces
  marginal_contributions <- numeric(nrow(points))  # Stocker les contributions marginales
  
  for (i in 1:nrow(points)) {
    points_without_i <- points[-i, , drop = FALSE]  # Enlever une espèce
    marginal_contributions[i] <- base_volume - calc_fric(points_without_i)  # Différence de volume
  }
  
  return(marginal_contributions)
}

# Fonction pour appliquer la méthode aux assemblages
compute_functional_depth <- function(sp_faxes_coord, asb_matrix) {
  results <- list()
  
  for (i in 1:nrow(asb_matrix)) {
    asb <- asb_matrix[i, ]
    species_present <- which(asb > 0)
    points <- sp_faxes_coord[species_present, , drop = FALSE]
    
    if (nrow(points) > 3) {
      marginal_contributions <- calc_marginal_contribution(points)
      results[[rownames(asb_matrix)[i]]] <- marginal_contributions
    } else {
      results[[rownames(asb_matrix)[i]]] <- rep(NA, length(species_present))
    }
  }
  
  return(results)
}

# Appliquer la méthode aux assemblages
functional_depth <- compute_functional_depth(sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")], depth_fish_biomass)

generate_null_model_marginal <- function(asb_matrix, sp_faxes_coord, n_sim) {
  null_fric_marginal <- list()
  
  for (sim in 1:n_sim) {
    sim_results <- list()
    
    for (i in 1:nrow(asb_matrix)) {
      richness <- sum(asb_matrix[i, ] > 0)
      selected_species <- sample(rownames(sp_faxes_coord), richness)
      points <- sp_faxes_coord[selected_species, , drop = FALSE]
      
      if (nrow(points) > 3) {
        sim_results[[rownames(asb_matrix)[i]]] <- calc_marginal_contribution(points)
      } else {
        sim_results[[rownames(asb_matrix)[i]]] <- rep(NA, richness)
      }
    }
    
    null_fric_marginal[[sim]] <- sim_results
  }
  
  return(null_fric_marginal)
}

# Générer le modèle nul
n_sim <- 99
null_fric_marginal <- generate_null_model_marginal(depth_fish_biomass, sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")], n_sim)

# Fonction pour calculer SES par assemblage
compute_ses <- function(observed, null_model) {
  ses_results <- list()
  
  for (assemblage in names(observed)) {
    obs_values <- observed[[assemblage]]
    null_values <- sapply(null_model, function(x) x[[assemblage]])
    
    mean_null <- rowMeans(null_values, na.rm = TRUE)
    sd_null <- apply(null_values, 1, sd, na.rm = TRUE)
    
    ses_values <- (obs_values - mean_null) / sd_null
    ses_results[[assemblage]] <- ses_values
  }
  
  return(ses_results)
}

# Calculer SES
ses_fric_marginal <- compute_ses(functional_depth, null_fric_marginal)
# Préparation des données pour ggplot
ses_df <- do.call(rbind, lapply(names(ses_fric_marginal), function(assemblage) {
  data.frame(assemblage = assemblage, SES = ses_fric_marginal[[assemblage]])
}))

# Ajouter les strates de profondeur
ses_df <- ses_df %>%
  mutate(depth_layer = case_when(
    assemblage %in% "Epipelagic" ~ "Epipelagic",
    assemblage %in% "Upper mesopelagic" ~ "Upper mesopelagic",
    assemblage %in% "Lower mesopelagic" ~ "Lower mesopelagic",
    assemblage %in% "Bathypelagic" ~ "Bathypelagic"
  ))


ses_df$depth_layer <- factor(
  ses_df$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

# Visualisation des SES
ggplot(ses_df, aes(x = depth_layer, y = SES, fill = depth_layer)) +
  geom_point(size = 3, position = position_jitter(width = 0.2)) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(
    x = "",
    y = "SES of Functional Richness (Marginal Contribution)",
    title = "Functional Depth by Depth Layer"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = "none"
  )
