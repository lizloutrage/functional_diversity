
# Installer et charger ADE4
# Fonction pour calculer l'aire ou le volume du convex hull
calculate_convex_hull <- function(mat, plot_result = TRUE) {
  # Si non installé
  library(ade4)
  library(geometry)

  dudimat <- dudi.mix(mat, scannf = FALSE, nf = 4 )
   
    # Extraire les coordonnées des individus dans l'espace des composantes principales
  ind_coords <- dudimat$li
  
  # Calculer les indices du convex hull en 2D
  hull_indices <- chull(ind_coords)
  
  # Extraire les coordonnées du convex hull
  hull_coords <- ind_coords[hull_indices, , drop = FALSE]
  
  # Calculer l'aire ou le volume du convex hull
  hull_result <- NULL
  
  if (nrow(ind_coords) >= ncol(ind_coords) + 1) {
    hull_result <- convhulln(ind_coords, options = "FA")
  } else {
    warning("Pas assez de points pour former un simplex dans cet espace")
  }
  
  # Vérifier si hull_result est défini avant de l'utiliser
  if (!is.null(hull_result)) {
    print(hull_result)
  } else {
    print("Convex hull non calculé.")
  }
  
  # Afficher l'aire ou le volume et le graphique si demandé
  if (plot_result) {
    plot(hull_result$hull, main = "Convex Hull")
    title(sub = paste("Aire du Convex Hull : ", hull_result$area))
  }
  
  # Retourner l'aire et le volume
  return(list(
    area = hull_result$area,
    volume = hull_result$vol,
    hull = hull_result$hull
  ))
}

# Exemple d'utilisation de la fonction avec traitsL (résultats de dudi.mix)
# traitsL est le résultat de l'analyse dudi.mix() que tu as effectuée précédemment

randomize_esp <- function(traits_mat) {
  randomized_mat <- traits_mat
  for (sp in rownames(traits_mat)) {
    for (trait in colnames(traits_mat)) {
      if (is.factor(traits_mat[[trait]])) {
        # Si le trait est un facteur, échantillonnage parmi les niveaux du facteur
        randomized_mat[sp, trait] <- sample(levels(traits_mat[[trait]]), size = 1)
      } else {
        # Si le trait est numérique, échantillonnage parmi les valeurs numériques
        randomized_mat[sp, trait] <- sample(traits_mat[sp, trait], size = 1)  # Assurer un seul élément échantillonné
      }
    }
  }
  return(randomized_mat)
}
