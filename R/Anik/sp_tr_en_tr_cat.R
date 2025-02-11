if(F) {
sp_tr_en_tr_cat <- function(sp_tr) {

  num_traits <- sapply(sp_tr, is.numeric)   
  cat_traits <- sapply(sp_tr, is.character) 
  
  # Séparer les traits continus et catégoriels
  sp_tr_cont <- sp_tr[, num_traits, drop = FALSE]
  sp_tr_cat  <- sp_tr[, cat_traits, drop = FALSE]
  
  # Convertir les traits catégoriels en facteurs
  if (ncol(sp_tr_cat) > 0) {
    sp_tr_cat <- data.frame(lapply(sp_tr_cat, as.factor))
  }
  
  # Vérifier que les espèces sont bien en lignes
  rownames(sp_tr_cont) <- rownames(sp_tr)
  rownames(sp_tr_cat)  <- rownames(sp_tr)
  
  # Charger le package nécessaire
  if (!requireNamespace("mFD", quietly = TRUE)) {
    stop("Le package 'mFD' est requis mais n'est pas installé. Installez-le avec install.packages('mFD').")
  }
  
  # Convertir avec les fonctions mFD si les matrices ne sont pas vides
  tr_cont <- if (ncol(sp_tr_cont) > 0) mFD::as.sp.tr.cont(sp_tr_cont) else NULL
  tr_cat  <- if (ncol(sp_tr_cat) > 0) mFD::as.sp.tr.cat(sp_tr_cat) else NULL
  
  # Fusionner les matrices si les deux existent
  if (!is.null(tr_cont) && !is.null(tr_cat)) {
    tr_cat_final <- cbind(tr_cont, tr_cat)
  } else if (!is.null(tr_cont)) {
    tr_cat_final <- tr_cont
  } else {
    tr_cat_final <- tr_cat
  }
  
  # Retourner la matrice finale
  return(tr_cat_final)
}
}

# Fonction pour générer la matrice de traits catégoriels avec type et fuzzy_name
# Fonction pour générer la matrice des traits avec le type et fuzzy_name
sp_tr_en_tr_cat <- function(traits_df) {
    # Identifier les types de variables
    is_numeric <- sapply(traits_df, is.numeric)
    is_character <- sapply(traits_df, is.character)
    is_factor <- sapply(traits_df, is.factor)
    is_ordered <- sapply(traits_df, function(x) inherits(x, "ordered"))
    
    # Création du tableau
    traits_matrix <- data.frame(
      trait_name = colnames(traits_df),
      trait_type = NA, 
      fuzzy_name = NA, 
      stringsAsFactors = FALSE
    )
    
    # Attribution des types
    traits_matrix$trait_type[is_numeric] <- "Q"   # Quantitatif
    traits_matrix$trait_type[is_ordered] <- "O"   # Ordinal
    traits_matrix$trait_type[is_factor & !is_ordered] <- "N"  # Nominal
    traits_matrix$trait_type[is_character] <- "N" # Catégoriel (Non factorisé)
    
    # Détection des groupes flous (ex. "Use.raw", "Use.pastry", "Use.jam")
    fuzzy_groups <- list(
      Use = c("Use.raw", "Use.pastry", "Use.jam")
    )
    
    for (group_name in names(fuzzy_groups)) {
      fuzzy_traits <- fuzzy_groups[[group_name]]
      traits_matrix$fuzzy_name[traits_matrix$trait_name %in% fuzzy_traits] <- group_name
      traits_matrix$trait_type[traits_matrix$trait_name %in% fuzzy_traits] <- "F"
    }
    
    return(traits_matrix)
  }
  
  