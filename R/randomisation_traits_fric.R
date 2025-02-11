library(mFD)
library(tibble)
library(tidyr)
library(ggplot2)

# Nombre de simulations
n_simulations <- 99

indice_name <- c("fric")

# liste des couches de profondeur
depth_layers <- rownames(depth_fish_biomass)

# listes pour stocker les résultats
obsFD <- numeric(length(depth_layers))
names(obsFD) <- depth_layers
resultsRandomFD <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, 
                          dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))

# Fonction pour randomiser les traits d'une seule couche
randomize_traits <- function(traits_mat) {
  randomized_mat <- traits_mat
  for (trait in colnames(traits_mat)) {
    randomized_mat[, trait] <- sample(traits_mat[, trait], replace = FALSE)  # Randomisation des colonnes
  }
  return(randomized_mat)
}

# Boucle sur chaque couche
for (layer in depth_layers) {
  
  # Sélectionner les espèces présentes dans cette couche
  species_in_layer <- colnames(depth_fish_biomass)[depth_fish_biomass[layer, ] > 0]
  
  # Filtrer les matrices pour ne garder que les espèces présentes
  traits_layer <- fish_traits[species_in_layer, , drop = FALSE]
  biomass_layer <- depth_fish_biomass[layer, species_in_layer, drop = FALSE]
  
  # Calculer la richesse fonctionnelle observée
  obsFD[layer] <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_fish[species_in_layer, c("PC1", "PC2", "PC3", "PC4"), drop = FALSE],
    asb_sp_w = biomass_layer,
    ind_vect = indice_name,
    scaling = TRUE,
    check_input = TRUE,
    details_returned = FALSE
  )$functional_diversity_indices[1, indice_name] 
  
  # Boucle de simulations
  for (sim in 1:n_simulations) {
    
    # Randomiser les traits des espèces présentes
    randomized_traits <- randomize_traits(traits_layer)
    
    # Calculer la distance avec traits randomisés
    sp_dist_random <- mFD::funct.dist(
      sp_tr         = randomized_traits,
      tr_cat        = fish_traits_cat,
      metric        = "gower",
      scale_euclid  = "scale_center",
      ordinal_var   = "classic",
      weight_type   = "equal",
      stop_if_NA    = TRUE
    )
    
    # Définir l'espace fonctionnel
    fspaces_quality_random <- mFD::quality.fspaces(
      sp_dist             = sp_dist_random,
      maxdim_pcoa         = 10,
      deviation_weighting = "absolute",
      fdist_scaling       = FALSE,
      fdendro             = "average"
    )
    
    sp_faxes_coord_random <- fspaces_quality_random$"details_fspaces"$"sp_pc_coord"
    
    #  Calculer la richesse fonctionnelle
    resultsRandomFD[layer, sim] <- mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_random[species_in_layer, c("PC1", "PC2", "PC3", "PC4"), drop = FALSE],
      asb_sp_w = biomass_layer,
      ind_vect = indice_name,
      scaling = TRUE,
      check_input = TRUE,
      details_returned = FALSE
    )$functional_diversity_indices[1, indice_name]  # Prendre la valeur pour toute la couche
  }
}

# Calculer les statistiques SES par couche
meanNullFD <- rowMeans(resultsRandomFD, na.rm = TRUE)
sdNullFD <- apply(resultsRandomFD, 1, sd, na.rm = TRUE)
ES_FD <- obsFD - meanNullFD
SES_FD <- ES_FD / sdNullFD

# Transformer les résultats en dataframe
results_df <- data.frame(
  depth_layer = names(obsFD),
  obsFD = obsFD,
  meanNullFD = meanNullFD,
  sdNullFD = sdNullFD,
  ES_FD = ES_FD,
  SES_FD = SES_FD
)

results_df$depth_layer <- factor(
  results_df$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)


# plot
ggplot(results_df, aes(x = depth_layer, y = SES_FD)) +
  geom_point(size = 3, aes(color = depth_layer)) +
  geom_hline(yintercept = c(1.96, -1.96), linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  ylim(-4, 4) +
  labs(x = "", y = "Standard Effect Size (SES)") +
  theme_light()

