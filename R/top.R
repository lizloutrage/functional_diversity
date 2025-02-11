library(geometry)  # Pour calculer les enveloppes convexes
library(tibble)
library(ggplot2)

onion_peeling <- function(points) {
  layers <- list()  # Stocker les couches
  areas <- c()      # Stocker les aires des enveloppes
  layer_index <- 1
  
  while (nrow(points) > 3) {  # Minimum 3 points pour former une enveloppe convexe
    hull <- geometry::convhulln(points)  # Calcul de l'enveloppe convexe
    hull_indices <- unique(as.vector(hull))  # Extraire les indices des points formant l'enveloppe
    
    layers[[layer_index]] <- points[hull_indices, ]  # Enregistrer les points de l'enveloppe
    areas[layer_index] <- geometry::convhulln(points, options = "FA")$area  # Calculer l'aire
    points <- points[-hull_indices, ]  # Retirer les points de l'enveloppe
    layer_index <- layer_index + 1
  }
  
  return(list(layers = layers, areas = areas))
}


# Appliquer l'onion peeling par assemblage
functional_diversity_onion <- function(sp_faxes_coord, asb_matrix) {
  results <- data.frame(
    assemblage = rownames(asb_matrix),
    total_area = NA,  # Aire totale de l'espace fonctionnel occupé
    layers_count = NA # Nombre de couches (pelures)
  )
  
  for (i in 1:nrow(asb_matrix)) {
    asb <- asb_matrix[i, ]  # Sélectionner un assemblage
    species_present <- which(asb > 0)  # Filtrer les espèces présentes
    points <- sp_faxes_coord[species_present, , drop = FALSE]  # Extraire les coordonnées fonctionnelles
    
    if (nrow(points) > 3) {  # Vérifier qu'il y a au moins 3 points
      peeling <- onion_peeling(points)
      results$total_area[i] <- sum(peeling$areas)  # Somme des aires des couches
      results$layers_count[i] <- length(peeling$areas)  # Nombre de couches
    } else {
      results$total_area[i] <- 0
      results$layers_count[i] <- 0
    }
  }
  
  return(results)
}

# Appliquer la méthode à vos données
# Matrice des coordonnées fonctionnelles (PC1-PC4)
sp_faxes_coord <- sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")]

# Matrice des assemblages
asb_matrix <- depth_fish_biomass

# Calculer la diversité fonctionnelle
fd_results <- functional_diversity_onion(sp_faxes_coord, asb_matrix)

# Ajouter les couches de profondeur pour la visualisation
fd_results <- fd_results %>%
  mutate(depth_layer = case_when(
    rownames(asb_matrix) %in% "Epipelagic" ~ "Epipelagic",
    rownames(asb_matrix) %in% "Upper mesopelagic" ~ "Upper mesopelagic",
    rownames(asb_matrix) %in% "Lower mesopelagic" ~ "Lower mesopelagic",
    rownames(asb_matrix) %in% "Bathypelagic" ~ "Bathypelagic"
  ))


fd_results$depth_layer <- factor(
  fd_results$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

# Visualisation des résultats ----
# Aire totale de l'espace fonctionnel par couche de profondeur
ggplot(fd_results, aes(x = depth_layer, y = total_area, fill = depth_layer)) +
  geom_point(size = 3,aes(col=depth_layer)) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(
    x = "",
    y = "Total Functional Richness (Area)",
    title = "Functional Richness by Depth Layer (Onion Peeling)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = "none"
  )

# Nombre de couches par assemblage
ggplot(fd_results, aes(x = depth_layer, y = layers_count, fill = depth_layer)) +
  geom_point(size = 3, aes(col=depth_layer)) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(
    x = "",
    y = "Number of Functional Layers",
    title = "Functional Layers by Depth Layer (Onion Peeling)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = "none"
  )
