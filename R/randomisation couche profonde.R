########################
## Code pour randomiser les traits ind?pendamment pour briser les relations entre esp?ces et traits
########################

#exemple au lieu de la matrice de valeurs observ?es
set.seed(123)
species <- 15 # nombre d'esp?ces
traits <- 5   # nombre de traits

mat_traits <- matrix(runif(species * traits, 0, 1), nrow = species, ncol = traits)
rownames(mat_traits) <- paste0("Species_", 1:species)
colnames(mat_traits) <- paste0("Trait_", 1:traits)

## fonction pour randomiser les valeurs des traits entre les esp?ces
randomize_matrix_swap_traits <- function(mat) {
  n_species <- nrow(mat)
  n_traits <- ncol(mat)
  
  randomized_mat <- mat
  for (i in 1:n_traits) {
    randomized_mat[, i] <- sample(mat[, i])  # Je rendomise les valeurs de chaque colonne
  }
  
  return(randomized_mat)
}

## simulation des 1000 matrices
n_simulations <- 10
randomized_matrices <- vector("list", n_simulations)

# Appliquer la randomisation 1000 fois et stocker les r?sultats
for (k in 1:n_simulations) {
  randomized_matrices[[k]] <- randomize_matrix_swap_traits(mat)
}

#Tests ----

# Define the randomization function for traits
randomize_matrix_swap_traits <- function(mat) {
  n_species <- nrow(mat)
  n_traits <- ncol(mat)
  
  randomized_mat <- mat
  for (i in 1:n_traits) {
    randomized_mat[, i] <- sample(mat[, i])  # Randomize the values in each column
  }
  
  return(randomized_mat)
}

# Number of simulations
n_simulations <- 10000

# Create a list to store random matrices
randomized_matrices <- vector("list", n_simulations)

# Generate randomized matrices
for (k in 1:n_simulations) {
  randomized_matrices[[k]] <- randomize_matrix_swap_traits(sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")])
}

# Initialize a list to store results for each functional diversity index
indices_names <- colnames(obsFD$functional_diversity_indices)
resultsRandomFD <- list()

for (index_name in indices_names) {
  resultsRandomFD[[index_name]] <- matrix(
    NA,
    nrow = nrow(depth_fish_biomass),
    ncol = n_simulations,
    dimnames = list(rownames(depth_fish_biomass), paste0("Sim.", 1:n_simulations))
  )
}

# Perform functional diversity calculation for each randomized matrix
for (sim in 1:n_simulations) {
  simFD_cal <- mFD::alpha.fd.multidim(
    sp_faxes_coord = randomized_matrices[[sim]],
    asb_sp_w = depth_fish_biomass,
    ind_vect = c("fdis", "feve", "fric", "fdiv"),
    scaling = TRUE,
    check_input = TRUE,
    details_returned = FALSE
  )
  
  simFD_div <- simFD_cal$functional_diversity_indices
  
  for (index_name in indices_names) {
    simFD_index <- simFD_div[, index_name]
    
    if (length(simFD_index) == nrow(depth_fish_biomass)) {
      resultsRandomFD[[index_name]][, sim] <- simFD_index
    } else {
      stop(paste("The length of", index_name, "does not match the number of rows in depth_fish_biomass"))
    }
  }
}

# Initialize dataframes to store mean, standard deviation, effect size, and standardized effect size
meanNullFD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))
sdNullFD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))
ES_FD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))
SES_FD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))

# Set column names for the dataframes
colnames(meanNullFD) <- indices_names
colnames(sdNullFD) <- indices_names
colnames(ES_FD) <- indices_names
colnames(SES_FD) <- indices_names

# Dataframe to store SES values for testing normality
SES_values_reps <- data.frame()

# Calculate statistics and SES for each index
for (index_name in indices_names) {
  # Calculate mean and standard deviation of null model FD values
  meanNullFD[, index_name] <- rowMeans(resultsRandomFD[[index_name]], na.rm = TRUE)
  sdNullFD[, index_name] <- apply(resultsRandomFD[[index_name]], 1, sd, na.rm = TRUE)
  
  # Calculate effect size and standardized effect size
  ES_FD[, index_name] <- obsFD_div[, index_name] - meanNullFD[, index_name]
  SES_FD[, index_name] <- ES_FD[, index_name] / sdNullFD[, index_name]
  
  # Add SES for each simulation to SES_values_reps dataframe
  for (sim in 1:n_simulations) {
    SES_repetition <- (resultsRandomFD[[index_name]][, sim] - meanNullFD[, index_name]) / sdNullFD[, index_name]
    SES_values_reps <- rbind(
      SES_values_reps,
      data.frame(
        depth_layer = rownames(depth_fish_biomass),
        SES = SES_repetition,
        index = index_name,
        repetition = paste0("Sim_", sim)
      )
    )
  }
}

# Combine all results into a single dataframe
results_df <- cbind(
  obsFD_div,
  meanNullFD = meanNullFD,
  sdNullFD = sdNullFD,
  ES_FD = ES_FD,
  SES_FD = SES_FD
)

# Add row names
rownames(results_df) <- rownames(depth_fish_biomass)

# Plot results
results_df_plot <- results_df %>%
  tibble::rownames_to_column(var = "depth_layer") %>%
  tidyr::pivot_longer(!depth_layer,
                      values_to = "values",
                      names_to = "indice") %>%
  mutate(indice = stringr::str_replace(indice, "^SES_", "")) %>%
  filter(indice %in% c("FD.fric", "FD.fdis", "FD.feve", "FD.fdiv"))

results_df_plot$depth_layer <- factor(
  results_df_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)
results_df_plot$indice <- factor(
  results_df_plot$indice,
  levels = c("FD.fric", "FD.fdis", "FD.fdiv", "FD.feve"),
  labels = c(
    "Functional richness",
    "Functional dispersion",
    "Functional divergence",
    "Functional evenness"
  )
)

ggplot(results_df_plot, aes(x = depth_layer, y = values, fill = depth_layer)) +
  facet_wrap(~indice) +
  geom_point(size = 3, aes(col=depth_layer)) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(
    x = "",
    y = "Standard Effect Size (SES)") +
  theme_light() +
  theme(axis.text.x = element_blank(),
        strip.text.x = element_text(size = 14, color = "black"),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13)) +
  guides(col="none", fill="none")

