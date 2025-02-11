library(mFD)
library(tibble)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(FD)
library(ade4)
library(geometry)


source("R/Anik/sp_tr_en_tr_cat.R")  # fct pour convertir la matrice fish_traits en traits-cat
source("R/Anik/comparaison_chull.R")  # fct pour convertir la matrice fish_traits en traits-cat

depth_fish_biomass <- utils::read.csv(here::here("data","depth_fish_biomass.csv"), row.names=1, h=T, dec=".", sep=";")
fish_traits <- utils::read.csv(here::here("data","fish_traits.csv"), row.names=1, h=T, dec=".", sep=";")

# modifier les traits character en factor pour analyse dudi.mix
fish_traits <- fish_traits %>% mutate(across(where(is.character), as.factor))


# Fonction pour randomiser les traits d'une seule couche
randomize_traits <- function(traits_mat) {
  randomized_mat <- traits_mat
  for (trait in colnames(traits_mat)) {
    randomized_mat[, trait] <- sample(traits_mat[, trait], replace = FALSE)  # Randomisation des colonnes
  }
  return(randomized_mat)
}

# Nombre de simulations
n_simulations <- 99

indice_name <- c("fric")

# liste des couches de profondeur
depth_layers <- rownames(depth_fish_biomass)

# listes pour stocker les résultats
obsFD <- numeric(length(depth_layers))  
names(obsFD) <- depth_layers

convexhull_obs <- numeric(length(depth_layers)) 
names(convexhull_obs) <- depth_layers

dbFD_result_obs <-numeric(length(depth_layers)) 
names(dbFD_result_obs) <- depth_layers

FD_convhull_obs <-numeric(length(depth_layers)) 
names(FD_convhull_obs) <- depth_layers


resultsRandomFD <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, 
                          dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))

resultsRandomconvhull <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, 
                          dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))

resultsRandomFD_convhull <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, 
                                   dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))

dbFD_result_sim <- matrix(NA, nrow = length(depth_layers), ncol = n_simulations, 
                         dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations)))


# Boucle sur chaque couche
for (layer in depth_layers) {
  
  # SÃ©lectionner les espÃ¨ces prÃ©sentes dans cette couche
  species_in_layer <- colnames(depth_fish_biomass)[depth_fish_biomass[layer, ] > 0]
  
  # Filtrer les matrices pour ne garder que les espÃ¨ces prÃ©sentes
  traits_layer <- fish_traits[species_in_layer, , drop = FALSE] %>%
    select(where(~ length(unique(.)) > 1))  # Retirer les traits constants pour les catégoriels
  
  biomass_layer <- depth_fish_biomass[layer, species_in_layer, drop = FALSE]
  fish_traits_cat <- sp_tr_en_tr_cat(traits_layer)[,-3]    ####TODO ABA
  
## calcul obs à partir des axes principaux calculés à partir de mFD
  # Calculer la distance avec traits randomisÃ©s
  sp_dist <- mFD::funct.dist(
    sp_tr         = traits_layer,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE
  )
  
  # DÃ©finir l'espace fonctionnel
  fspaces_quality <- mFD::quality.fspaces(
    sp_dist             = sp_dist,
    maxdim_pcoa         = 10,
    deviation_weighting = "absolute",
    fdist_scaling       = FALSE,
    fdendro             = "average"
  )
  
  sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"
  
  #  Calculer la richesse fonctionnelle
  obsFD[layer] <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord[species_in_layer, c("PC1", "PC2", "PC3", "PC4"), drop = FALSE],
    asb_sp_w = as.matrix(biomass_layer),
    ind_vect = indice_name,
    scaling = TRUE,
    check_input = TRUE,
    details_returned = FALSE
  )$functional_diversity_indices[1, indice_name]  # Prendre la valeur pour toute la couche
 
#calcul des valeurs observées à partir des différentes approches  
  convexhull_obs[layer] <- calculate_convex_hull(traits_layer, plot_result = FALSE)$volume
  FD_convhull_obs[layer] <- convhulln(sp_faxes_coord[,1:4], options = "FA")$vol
  dbFD_result_obs[layer] <- dbFD(x = as.data.frame(traits_layer), w.abun = TRUE , m = 4, a = as.vector(biomass_layer))$FRic
  
  print(paste0("debut des simulations: ", layer))

  ################
 
  # Boucle de simulations
  for (sim in 1:n_simulations) {
    
    # Randomiser les traits des espÃ¨ces prÃ©sentes
    randomized_traits <- randomize_traits(traits_layer)
    
    # Calculer la distance avec traits randomisÃ©s
    sp_dist_random <- mFD::funct.dist(
      sp_tr         = randomized_traits,
      tr_cat        = fish_traits_cat,
      metric        = "gower",
      scale_euclid  = "scale_center",
      ordinal_var   = "classic",
      weight_type   = "equal",
      stop_if_NA    = TRUE
    )
    
    # DÃ©finir l'espace fonctionnel
    fspaces_quality_random <- mFD::quality.fspaces(
      sp_dist             = sp_dist_random,
      maxdim_pcoa         = 10,
      deviation_weighting = "absolute",
      fdist_scaling       = FALSE,
      fdendro             = "average"
    )
 
    # ABA: récupération des simulations avec 
    #1- convexhull sur les axes générés par mFD  :resultsRandomFD_convhull
    #2- calcul convexhull avec calculate_convex_hull, une fct qui fait appel à dudi.mix directement sur la matrice de traits  :resultsRandomconvhull
    #3- calcul de liz: resultsRandomFD
    #4- calcul FRic à partir du package dbFD sur la matrice de traits directement pondéré par biomasse
    
    sp_faxes_coord_random <- fspaces_quality_random$"details_fspaces"$"sp_pc_coord"
    resultsRandomFD_convhull[layer, sim] <- convhulln(sp_faxes_coord_random[,1:4], options = "FA")$vol
    dbFD_result_sim[layer, sim] <- dbFD(x = randomized_traits, w.abun = TRUE , m = 4, a = biomass_layer)$FRic
    resultsRandomconvhull[layer, sim] <- calculate_convex_hull(randomized_traits, plot_result = FALSE)$volume
    
    
    #  Calculer la richesse fonctionnelle
    resultsRandomFD[layer, sim] <- mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_random[species_in_layer, c("PC1", "PC2", "PC3", "PC4"), drop = FALSE],
      asb_sp_w = as.matrix(biomass_layer),
      ind_vect = indice_name,
      scaling = TRUE,
      check_input = TRUE,
      details_returned = FALSE
    )$functional_diversity_indices[1, indice_name]  # Prendre la valeur pour toute la couche

    toto <- mFD::funct.space.plot(
      sp_faxes_coord = sp_faxes_coord_random[species_in_layer, c("PC1", "PC2"), drop = FALSE], 
      faxes  = NULL,
      faxes_nm = NULL)
    
    print(toto)
    
    }  
  
}


if(T) {
# Calculer les statistiques SES par couche
meanNullFD_liz <- rowMeans(resultsRandomFD, na.rm = TRUE)
sdNullFD_liz <- apply(resultsRandomFD, 1, sd, na.rm = TRUE)
ES_FD_liz <- obsFD - sdNullFD_liz
SES_FD_liz <- ES_FD_liz / sdNullFD_liz
}

# Calculer les statistiques SES par couche
meanNullFD <- rowMeans(resultsRandomconvhull, na.rm = TRUE)
sdNullFD <- apply(resultsRandomconvhull, 1, sd, na.rm = TRUE)
ES_FD <- convexhull_obs - meanNullFD
SES_FD <- ES_FD / sdNullFD

# Calculer les statistiques SES par couche
meanNullFD_db <- rowMeans(dbFD_result_sim, na.rm = TRUE)
sdNullFD_db <- apply(dbFD_result_sim, 1, sd, na.rm = TRUE)
ES_FD_db <- dbFD_result_obs - meanNullFD_db
SES_FD_db <- ES_FD_db / sdNullFD_db

# Transformer les rÃ©sultats en dataframe
results_df <- data.frame(
  depth_layer = names(convexhull_obs),
  obsFD = convexhull_obs,
  meanNullFD = meanNullFD,
  sdNullFD = sdNullFD,
  ES_FD = ES_FD,
  SES_FD = SES_FD
)

# Transformer les rÃ©sultats en dataframe
results_df_db <- data.frame(
  depth_layer = names(dbFD_result_obs),
  obsFD = dbFD_result_obs,
  meanNullFD = meanNullFD_db,
  sdNullFD = sdNullFD_db,
  ES_FD = ES_FD_db,
  SES_FD = SES_FD_db
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

results_df_db$depth_layer <- factor(
  results_df_db$depth_layer,
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
  ylim(-15, 15) +
  labs(x = "", y = "Standard Effect Size (SES)") +
  theme_light()

ggplot(results_df_db, aes(x = depth_layer, y = SES_FD)) +
  geom_point(size = 3, aes(color = depth_layer)) +
  geom_hline(yintercept = c(1.96, -1.96), linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  ylim(-15, 15) +
  labs(x = "", y = "Standard Effect Size (SES) - dbFD package") +
  theme_light()



################################
################################
## test de comparaison

#sur les randomized results
results_FRIC_comp <- list(resultsRandomFD=resultsRandomFD, resultsRandomconvhull=resultsRandomconvhull, 
                          resultsRandomFD_convhull=resultsRandomFD_convhull, dbFD_result_sim=dbFD_result_sim)

meanNullFD_liz <- rowMeans(resultsRandomFD, na.rm = TRUE)
meanNull_rand_vol <- rowMeans(resultsRandomconvhull, na.rm = TRUE)
meanNullFD_vol <- rowMeans(resultsRandomFD_convhull, na.rm = TRUE)
meanNullFD_dbric <- rowMeans(dbFD_result_sim, na.rm = TRUE)

results_comp <- rbind(meanNullFD_liz,meanNull_rand_vol,meanNullFD_vol,meanNullFD_dbric)

results_comp_t <- t(results_comp) 

# Calculer la matrice de corrélation entre les approches
cor_matrix <- cor(results_comp_t)

# Convertir la matrice en format long
cor_long <- melt(cor_matrix)
colnames(cor_long) <- c("Approach1", "Approach2", "Correlation")

# Exclure la diagonale (corrélation de 1 avec soi-même)
cor_long <- cor_long[cor_long$Approach1 != cor_long$Approach2, ]

# Créer la heatmap avec ggplot2
ggplot(cor_long, aes(x = Approach1, y = Approach2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap de la matrice de corrélation entre approches", fill = "Corrélation")

#################################################################
## sur les observed results
results_comp <- rbind(convexhull_obs,dbFD_result_obs,FD_convhull_obs)

results_comp_t <- t(results_comp)  

# Calculer la matrice de corrélation entre les approches
cor_matrix <- cor(results_comp_t)

# Convertir la matrice en format long
cor_long <- melt(cor_matrix)
colnames(cor_long) <- c("Approach1", "Approach2", "Correlation")

# Exclure la diagonale (corrélation de 1 avec soi-même)
cor_long <- cor_long[cor_long$Approach1 != cor_long$Approach2, ]

# Créer la heatmap 
ggplot(cor_long, aes(x = Approach1, y = Approach2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap de la matrice de corrélation entre approches", fill = "Corrélation")
