#' Randomization of the species trait matrix
#'
#' @param traits_mat : species trait matrix 
#'
#' @returns
#' @export
#'
#' @examples

randomize_traits <- function(traits_mat) {
  randomized_mat <- traits_mat
  for (trait in colnames(traits_mat)) {
    trait_values <- traits_mat[[trait]]
    randomized_mat[[trait]] <- sample(trait_values, size = length(trait_values), replace = TRUE)
  }
  randomized_mat <- droplevels(randomized_mat)
  return(randomized_mat)
}

#' Run dbFD simulations
#'
#' @param depth_fish_biomass biomass matrix
#' @param fish_traits trait data frame
#' @return simulated dbFD values

run_simulations <- function(depth_fish_biomass, fish_traits) {
  n_simulations <- 999
  corr_method <- "lingoes"
  depth_layers <- rownames(depth_fish_biomass)
  
  dbFD_result_sim <- array(NA, dim = c(length(depth_layers), n_simulations, 4),
                           dimnames = list(depth_layers, paste0("Sim.", 1:n_simulations), c("FRic", "FDis", "FEve", "FDiv")))
  for (layer in depth_layers) {
    species_in_layer <- colnames(depth_fish_biomass)[depth_fish_biomass[layer, ] > 0]
    traits_layer <- fish_traits[species_in_layer, , drop = FALSE] %>%
      select(where(~ length(unique(.)) > 1)) %>%
      droplevels()
    if (ncol(traits_layer) == 0) next
    biomass_layer <- matrix(as.numeric(depth_fish_biomass[layer, species_in_layer, drop = FALSE]),
                            nrow = 1, dimnames = list(layer, species_in_layer))
    for (sim in 1:n_simulations) {
      randomized_traits <- randomize_traits(traits_layer)
      dbFD_sim_result_tmp <- FD::dbFD(x = randomized_traits, w.abun = TRUE, m = 4, a = biomass_layer, messages = FALSE, corr = corr_method)
      dbFD_result_sim[layer, sim, "FRic"] <- dbFD_sim_result_tmp$FRic
      dbFD_result_sim[layer, sim, "FDis"] <- dbFD_sim_result_tmp$FDis
      dbFD_result_sim[layer, sim, "FEve"] <- dbFD_sim_result_tmp$FEve
      dbFD_result_sim[layer, sim, "FDiv"] <- dbFD_sim_result_tmp$FDiv
    }
  }
  return(dbFD_result_sim)
}

#' Compute SES values
#'
#' @param dbFD_result_sim simulation results array
#' @param fish_traits trait data frame
#' @param depth_fish_biomass biomass matrix
#' @return SES values data frame

compute_SES <- function(dbFD_result_sim, fish_traits, depth_fish_biomass) {
  corr_method <- "lingoes"
  
  # Calculate observed (real) values
  observed_values <- FD::dbFD(x = fish_traits, w.abun = TRUE, m = 4, a = depth_fish_biomass,
                              messages = FALSE, corr = corr_method)
  
  # format the dataframe 
  observed_values <- observed_values[c("FRic", "FDiv", "FDis", "FEve")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "depth_layer") %>%
    tidyr::pivot_longer(!depth_layer, values_to = "observed_values", names_to = "indice")
  
  # format the dataframe 
  simulated_values <- dbFD_result_sim[, , ] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "depth_layer") %>%
    tidyr::pivot_longer(!depth_layer, values_to = "values", names_to = "indice") %>%
    mutate(indice = gsub("Sim\\.\\d+\\.", "", indice))
  
  # test the normality distribution of the simulated values
  stat_test <- simulated_values %>%
    group_by(indice, depth_layer) %>%
    mutate(
      skewness_values = e1071::skewness(values),
      shapiro_p_values = rstatix::shapiro_test(values)$p.value
    ) %>%
    select(-values) %>%
    distinct() %>%
    mutate(normality = case_when(
      shapiro_p_values > 0.05 ~ "normal",
      shapiro_p_values <= 0.05 ~ "non_normal"
    ))
  
  # Try log-transforming the values to see if this makes the distribution normal
  test_values_trans <- simulated_values %>%
    inner_join(stat_test %>% select(normality)) %>%
    filter(normality == "non_normal") %>%
    mutate(values_log = log10(values)) %>%
    group_by(indice, depth_layer) %>%
    mutate(
      skewness_values = e1071::skewness(values_log),
      shapiro_p_values = rstatix::shapiro_test(values_log)$p.value
    ) %>%
    select(depth_layer, indice, shapiro_p_values) %>%
    distinct() %>%
    # which simulated diversity indices have normal distribution
    mutate(normality = case_when(
      shapiro_p_values > 0.05 ~ "normal",
      shapiro_p_values <= 0.05 ~ "non_normal"
    ))
  
  # Are the log-transformed observed values of the indices outside
  # the limits (2.5-97.5%) of the distribution of simulated transformed 
  # values for each index in each layer?
  values_percentile <- simulated_values %>%
    inner_join(stat_test %>% select(normality)) %>%
    filter(normality == "non_normal") %>%
    mutate(values_log = log10(values)) %>% 
    group_by(depth_layer, indice) %>%  
    summarise(quantile = scales::percent(c(0.025, 0.975), na.rm=T),
              percentile = quantile(values_log, c(0.025, 0.975), na.rm=T)) %>% 
    distinct() %>% 
    inner_join(observed_values) %>% 
    mutate(observed_values_trans=(log10(observed_values))) %>% 
    select(-observed_values) %>% 
    filter(indice=="FRic")
  
  # SES calcul ----
  ## calcul mean and sd of simlauted values not transform
  sum_simulated_values <- simulated_values %>%
    # simulated values of indices normaly distributed (or not but for which the log doesn't change it)
    filter(indice %in% c("FDiv", "FDis", "FEve")) %>%
    group_by(indice, depth_layer) %>%
    summarise(meanNullFD = mean(values), sdNullFD = sd(values), .groups = "drop")
  
  # Calculation of SES values for normally distributed simulated index value
  SES_calcul <- observed_values %>%
    inner_join(sum_simulated_values) %>%
    mutate(SES = (observed_values - meanNullFD) / sdNullFD) %>%
    filter(indice %in% c("FDis", "FEve", "FDiv"))
  
  # For simulated functional diversity index values 
  # that are not normally distributed, log transformation is required.
  sum_simulated_values_log <- simulated_values %>%
    filter(indice == "FRic") %>%
    mutate(values_log = log10(values)) %>%
    group_by(depth_layer) %>%
    summarise(meanNullFD = mean(values_log), sdNullFD = sd(values_log), .groups = "drop")
  
  SES_calcul_log <- observed_values %>%
    filter(indice=="FRic") %>% 
    inner_join(sum_simulated_values_log) %>%
    mutate(SES = (log10(observed_values) - meanNullFD) / sdNullFD, indice = "FRic")
  
  # combined all results 
  SES_combined <- SES_calcul_log %>%
    full_join(SES_calcul) %>%
    select(depth_layer, indice, SES)
  
  return(SES_combined)
}

#' plot SES
#'
#' @param SES_combined 
#'
#' @returns
#' @export
#'
#' @examples

plot_SES <- function(SES_combined) {
  SES_combined$depth_layer <- factor(
    SES_combined$depth_layer,
    levels = c("Epipelagic", "Upper mesopelagic", "Lower mesopelagic", "Bathypelagic")
  )
  SES_combined$indice <- factor(
    SES_combined$indice,
    levels = c("FRic", "FDis", "FDiv", "FEve"),
    labels = c("Functional richness", "Functional dispersion", "Functional divergence", "Functional evenness")
  )
  
  p <- ggplot(SES_combined, aes(x = depth_layer, y = SES, color = depth_layer)) +
    geom_point(size = 3) +
    facet_wrap(~indice) +
    geom_hline(yintercept = c(1.96, -1.96), linetype = "dashed", color = "gray40", size = 0.8) +
    scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
    labs(x = "Depth layers", y = "Standard Effect Size (SES)") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          strip.text.x = element_text(size = 14, color = "black"),
          strip.background = element_rect(fill = "white"),
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 13)) +
    guides(col = "none", fill = "none")
  
  return(p)
}
