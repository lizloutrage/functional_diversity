
# Define randomization function for traits
randomize_matrix_swap_traits <- function(mat) {
  n_species <- nrow(mat)
  n_traits <- ncol(mat)
  
  randomized_mat <- mat
  for (i in 1:n_traits) {
    randomized_mat[, i] <- sample(mat[, i])  # Randomize each column independently
  }
  
  return(randomized_mat)
}

# Number of simulations
n_simulations <- 10  # Set to 999 for robust null model results

# Names of functional diversity indices
indices_names <- c("fdis", "feve", "fric", "fdiv")  # Indices to compute

# Calculate functional diversity for the observed data ----
obsFD <- mFD::alpha.fd.multidim(
  sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w = depth_fish_biomass,
  ind_vect = indices_names,
  scaling = TRUE,
  check_input = TRUE,
  details_returned = F
)

# Initialize a list to store results for each index
resultsRandomFD <- lapply(indices_names, function(index_name) {
  matrix(
    NA,
    nrow = nrow(depth_fish_biomass),  # Number of assemblages
    ncol = n_simulations,             # Number of simulations
    dimnames = list(rownames(depth_fish_biomass), paste0("Sim.", 1:n_simulations))
  )
})
names(resultsRandomFD) <- indices_names

# Perform simulations
for (sim in 1:n_simulations) {
  # Step 1: Randomize the traits
  randomized_traits <- randomize_matrix_swap_traits(fish_traits)
  
  # Step 2: Compute functional distance between species
  sp_dist_fish_random <- mFD::funct.dist(
    sp_tr         = randomized_traits,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE
  )
  
  # Step 3: Compute the functional space
  fspaces_quality_fish_random <- mFD::quality.fspaces(
    sp_dist             = sp_dist_fish_random,
    maxdim_pcoa         = 10,
    deviation_weighting = "absolute",
    fdist_scaling       = FALSE,
    fdendro             = "average"
  )
  
  sp_faxes_coord_fish_random <- fspaces_quality_fish_random$"details_fspaces"$"sp_pc_coord"
  
  # Step 4: Compute functional diversity indices for the random space
  simFD_cal <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_fish_random[, c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w = depth_fish_biomass,
    ind_vect = indices_names,
    scaling = TRUE,
    check_input = TRUE,
    details_returned = FALSE
  )
  
  # Step 5: Store results for each index
  simFD_div <- simFD_cal$functional_diversity_indices
  for (index_name in indices_names) {
    resultsRandomFD[[index_name]][, sim] <- simFD_div[, index_name]
  }
}

# Initialize dataframes to store mean, standard deviation, effect size, and SES
meanNullFD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))
sdNullFD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))
ES_FD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))
SES_FD <- data.frame(matrix(NA, nrow = nrow(depth_fish_biomass), ncol = length(indices_names)))

# Add column names
colnames(meanNullFD) <- indices_names
colnames(sdNullFD) <- indices_names
colnames(ES_FD) <- indices_names
colnames(SES_FD) <- indices_names


# Calculate SES for each index
for (index_name in indices_names) {
  # Mean and standard deviation of null model results
  meanNullFD[, index_name] <- rowMeans(resultsRandomFD[[index_name]], na.rm = TRUE)
  sdNullFD[, index_name] <- apply(resultsRandomFD[[index_name]], 1, sd, na.rm = TRUE) 
  
  # Effect size (ES) and standardized effect size (SES)
  ES_FD[, index_name] <- obsFD$functional_diversity_indices[, index_name] - meanNullFD[, index_name]
  SES_FD[, index_name] <- ES_FD[, index_name] / sdNullFD[, index_name]
}
# Combine results into a single dataframe
results_df <- cbind(
  obsFD$functional_diversity_indices,
  meanNullFD = meanNullFD,
  sdNullFD = sdNullFD,
  ES_FD = ES_FD,
  SES_FD = SES_FD
)

# Prepare data for plotting
results_df_plot <- results_df %>%
  tibble::rownames_to_column(var = "depth_layer") %>%
  tidyr::pivot_longer(!depth_layer,
                      values_to = "values",
                      names_to = "indice") %>%
  filter(indice =="SES_FD.fric")

results_df_plot$depth_layer <- factor(
  results_df_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

# Plot SES results
ggplot(results_df_plot, aes(x = depth_layer, y = values)) +
  geom_point(size = 5, aes(color = depth_layer)) +
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "gray50", linewidth = 0.8)+
  geom_hline(yintercept = -1.96, linetype = "dashed", color = "gray50", linewidth = 0.8)+
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  ylim(-3, 3) +
  labs(
    x = "",
    y = "Standard Effect Size (SES)") +
  guides(col="none")+
  theme_light()+
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 13))
