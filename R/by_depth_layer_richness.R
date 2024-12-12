# Model of randomization = "richness" & by depth layer 
# la biomasse des espèces est donc randomisée par couche de profondeur (seules les espèces présentes dans la couche de profondeur)
# Dans ce cas, je n'obtiens pas de valeurs de SES pour la richesse fonctionelle car cet indice 
# ne prend pas en compte la biomasse des espèces, seulement leur présence et donc j'obtiens
# les mêmes valeurs de Functional richness pour les valeurs observées et les valeurs simulées
# donc les valeurs de SES sont toujours = 0 (que les valeurs de biomasses soient randomisées ou pas ça ne change rien)

# Calculate functional diversity for the observed data ----
obsFD <- mFD::alpha.fd.multidim(
  sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w = depth_fish_biomass,
  ind_vect = c("fdis", "feve", "fdiv"),
  scaling = TRUE,
  check_input = TRUE,
  details_returned = FALSE
)

obsFD_div <- obsFD$functional_diversity_indices

# Null model ----
# Define the number of replications
nb_rep <- 1000
null_model_used <- "richness"

# Initialize a list to store results of random functional diversity calculations for each index
indices_names <- colnames(obsFD_div)
resultsRandomFD <- list()
randomized_matrices <- list()  # List to store randomized matrices

for (index_name in indices_names) {
  resultsRandomFD[[index_name]] <- matrix(
    NA,
    nrow = nrow(depth_fish_biomass),
    ncol = nb_rep,
    dimnames = list(rownames(depth_fish_biomass), paste0("Sim.", 1:nb_rep))
  )
}

# Perform randomization and calculate functional diversity for each depth layer
for (depth_layer in unique(rownames(depth_fish_biomass))) {
  # Subset biomass matrix for the current depth layer
  sub_biomass <- depth_fish_biomass[rownames(depth_fish_biomass) == depth_layer, , drop = FALSE]
  
  # Filter species with biomass > 0
  sub_biomass <- sub_biomass[, colSums(sub_biomass) > 0, drop = FALSE]
  
  for (rep in 1:nb_rep) {
    randomize_mx <- picante::randomizeMatrix(
      samp = sub_biomass,
      null.model = null_model_used,
      iterations = 1
    )
    
    # Store the randomized matrix
    randomized_matrices[[paste0(depth_layer, "_Rep_", rep)]] <- randomize_mx
    
    simFD_cal <- mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
      asb_sp_w = randomize_mx,
      ind_vect = c("fdis", "feve", "fdiv"),
      scaling = TRUE,
      check_input = TRUE,
      details_returned = FALSE
    )
    
    simFD_div <- simFD_cal$functional_diversity_indices
    
    for (index_name in indices_names) {
      simFD_index <- simFD_div[, index_name]
      
      if (length(simFD_index) == nrow(sub_biomass)) {
        resultsRandomFD[[index_name]][rownames(sub_biomass), rep] <- simFD_index
      } else {
        stop(
          paste(
            "The length of",
            index_name,
            "does not match the number of rows in sub_biomass for depth layer", depth_layer
          )
        )
      }
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

# Calculate statistics and SES for each index and store SES in both dataframes
for (index_name in indices_names) {
  # Calculate mean and standard deviation of null model FD values for each index
  meanNullFD[, index_name] <- rowMeans(resultsRandomFD[[index_name]], na.rm = TRUE)
  sdNullFD[, index_name] <- apply(resultsRandomFD[[index_name]], 1, sd, na.rm = TRUE)
  
  # Calculate effect size and standardized effect size for each index
  ES_FD[, index_name] <- obsFD_div[, index_name] - meanNullFD[, index_name]
  SES_FD[, index_name] <- ES_FD[, index_name] / sdNullFD[, index_name]
  
  # Calculate SES for each repetition and add to SES_values_reps dataframe
  for (rep in 1:nb_rep) {
    SES_repetition <- (resultsRandomFD[[index_name]][, rep] - meanNullFD[, index_name]) / sdNullFD[, index_name]
    SES_values_reps <- rbind(
      SES_values_reps,
      data.frame(
        depth_layer = rownames(depth_fish_biomass),
        SES = SES_repetition,
        index = index_name,
        repetition = paste0("Rep_", rep)
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

# Plot ----
# Output the results dataframe
results_df_plot <- results_df %>%
  tibble::rownames_to_column(var = "depth_layer") %>%
  tidyr::pivot_longer(!depth_layer,
                      values_to = "values",
                      names_to = "indice") %>%
  mutate(indice = stringr::str_replace(indice, "^SES_", "")) %>%
  filter(indice %in% c("FD.fdis",
                       "FD.fdiv",
                       "FD.feve"))

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
  levels = c("FD.fdis",
             "FD.fdiv",
             "FD.feve"),
  labels = c(
    "Functional dispersion",
    "Functional divergence",
    "Functional evenness"
  )
)

# Define number of repetitions and model used for randomization
n_rep <- nb_rep
null_model_used <- "richness"

ggplot(results_df_plot, aes(x = depth_layer, y = values, fill = depth_layer)) +
  facet_wrap(~indice, ncol=2) +
  geom_point(size = 3, aes(col = depth_layer)) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(
    x = "",
    y = "Standard Effect Size (SES)",
    title = paste("nb_rep =", nb_rep, "| Model =", null_model_used)
  ) +
  theme_light() +
  ylim(c(-3,3))+
  theme(
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 14, color = "black"),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13)
  ) +
  guides(col = "none", fill = "none")

ggsave("SES_by_depth_layer_richness.png", path = "figures", dpi = 700, height = 6, width = 8)

# normality and symetry tests ----
test_norm  <- SES_values_reps %>%
  tibble::remove_rownames() %>%
  filter(index %in% c("fdis", "fdiv", "feve"))

# Compute skewness and Lilliefors tests to check normaltiy and symetry 
stat_indices <- test_norm %>% 
  group_by(index, depth_layer) %>% 
  summarise(
    skewness = round(e1071::skewness(SES),2),
    Lilliefors_pvalue = round(nortest::lillie.test(SES)$p.value,2)
  )

print(stat_indices)