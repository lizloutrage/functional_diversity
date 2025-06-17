#' Format the community and morpho data to match the format of the traitstrap R package.
#' DOI ref package : 10.1111/2041-210X.14160
#'
#' @param morpho_data 
#' @param data_biomass_2002_2019 
#' @param data_biomass_2021_2022 
#'
#' @returns
#'
#' @examples

format_data_CWM <- function(morpho_data,
                            data_biomass_2002_2019,
                            data_biomass_2021_2022) {
  
  # Trait data: We want to keep the data for all individuals,
  # rather than the mean values for each index
  trait_boot <- morpho_data %>%
    mutate(
      eye_size = eye_diameter / head_depth,
      orbital_length = eye_diameter / standard_length,
      oral_gape_surface = mouth_width * mouth_depth / body_width * body_depth,
      oral_gape_shape = mouth_depth / mouth_width,
      oral_gape_position = distance_upper_jaws_bottom_head / head_depth,
      lower_jaw_length = lower_jaw_length / standard_length,
      head_length = head_length / standard_length,
      body_depth = body_depth / standard_length,
      pectoral_fin_position = distance_pectoral_bottom_body / body_depth_pectoral_insertion,
      pectoral_fin_insertion = prepectoral_length / standard_length,
      transversal_shape = body_depth / body_width,
      dorsal_fin_insertion = predorsal_length / standard_length,
      eye_position = eye_height / head_depth,
      operculum_volume = operculum_depth / operculum_width,
      gill_outflow = operculum_width,
      caudal_throttle_width = caudal_peduncle_min_depth
    ) %>%
    select(
      depth,
      species,
      eye_size,
      orbital_length,
      gill_outflow,
      oral_gape_surface,
      oral_gape_shape,
      oral_gape_position,
      lower_jaw_length,
      head_length,
      body_depth,
      pectoral_fin_position,
      pectoral_fin_insertion,
      transversal_shape,
      caudal_throttle_width,
      dorsal_fin_insertion,
      eye_position,
      operculum_volume,
      ventral_photophores,
      gland_head,
      chin_barbel,
      small_teeth,
      large_teeth,
      fang_teeth,
      retractable_teeth,
      internal_teeth
    ) %>%
    mutate_at(
      vars(
        ventral_photophores,
        gland_head,
        chin_barbel,
        small_teeth,
        large_teeth,
        fang_teeth,
        retractable_teeth,
        internal_teeth
      ),
      funs(ifelse(. == "P", 1, ifelse(. == "A", 0, .)))
    ) %>%
    mutate(across(all_of(
      c(
        "ventral_photophores",
        "gland_head",
        "chin_barbel",
        "small_teeth",
        "large_teeth",
        "fang_teeth",
        "retractable_teeth",
        "internal_teeth"
      )
    ), as.numeric)) %>%
    tidyr::pivot_longer(!c(species, depth),
                        names_to = "trait",
                        values_to = "values") %>%
    # add column with depth layer
    mutate(
      depth_layer = case_when(
        between(depth, 0, 174) ~ "Epipelagic",
        between(depth, 175, 699) ~ "Upper mesopelagic",
        between(depth, 700, 999) ~ "Lower mesopelagic",
        between(depth, 1000, 2000) ~ "Bathypelagic"
      )
    )
  
  
  # Community data
  community <-  rbind(data_biomass_2002_2019, data_biomass_2021_2022) %>%
    as.data.frame() %>%
    mutate(volume_filtered = 24 * 58 * distance_trawled_m) %>%
    select(-c(distance_trawled_m)) %>%
    # divise biomass by the volume filtered at each trawl (g.m3)
    mutate(biomass_cpu = (biomass_sp / volume_filtered)) %>%
    # add column with depth layer
    mutate(
      depth_layer = case_when(
        between(depth, 0, 174) ~ "Epipelagic",
        between(depth, 175, 699) ~ "Upper mesopelagic",
        between(depth, 700, 999) ~ "Lower mesopelagic",
        between(depth, 1000, 2000) ~ "Bathypelagic"
      )
    ) %>%
    replace(is.na(.), 0) %>%
    group_by(species, depth) %>%
    mutate(biomass = sum(biomass_cpu)) %>%
    select(-c(biomass_cpu, biomass_sp)) %>%
    distinct() %>%
    select(-c(volume_filtered)) %>%
    filter(biomass > 0) %>% 
    distinct()
  
  return(list(trait_boot = trait_boot, community = community))
}

#' Cacul of CWM
#'
#' @param community 
#' @param trait_boot 
#'
#' @returns A data frame with summarized bootstrapped trait moments.
#'
#' @examples

CWM_calcul <- function(community, trait_boot){
  
  trait_filling <- traitstrap::trait_fill(
    # input data (mandatory)
    comm = community,
    traits = trait_boot,
    
    # specifies columns in your data (mandatory)
    abundance_col = "biomass",
    taxon_col = "species",
    trait_col = "trait",
    value_col = "values",
    
    # specifies sampling hierarchy
    scale_hierarchy = c("depth_layer", "depth"),
    
    # min number of samples
    min_n_in_sample = 5
  )
  
  # run nonparametric bootstrapping
  np_bootstrapped_moments <- traitstrap::trait_np_bootstrap(
    trait_filling, 
    nrep = 100
  )
  
  sum_boot_moment <- traitstrap::trait_summarise_boot_moments(
    np_bootstrapped_moments
  ) %>% 
    mutate(trait= gsub("_"," ", trait)) %>% 
    mutate(trait=stringr::str_to_sentence(trait)) 
  sum_boot_moment$depth_layer <- factor(
    sum_boot_moment$depth_layer,
    levels = c(
      "Epipelagic",
      "Upper mesopelagic",
      "Lower mesopelagic",
      "Bathypelagic"
    )
  ) 
  
  # order traits 
  sum_boot_moment$trait <- factor(
    sum_boot_moment$trait,
    levels = c(
      "Caudal throttle width",
      "Oral gape surface",
      "Gill outflow",
      "Large teeth",
      "Eye size",
      "Orbital length",
      "Small teeth",
      "Transversal shape",
      "Body depth",
      "Dorsal fin insertion",
      "Eye position",
      "Oral gape shape",
      "Oral gape position",
      "Internal teeth",
      "Lower jaw length",
      "Pectoral fin position",
      "Ventral photophores",
      "Operculum volume",
      "Pectoral fin insertion",
      "Head length",
      "Chin barbel",
      "Fang teeth",
      "Gland head",
      "Retractable teeth"
    )
  ) 
  return(list(sum_boot_moment=sum_boot_moment, np_bootstrapped_moments= np_bootstrapped_moments))
}

#' Format the result to plot CWM 
#'
#' @param sum_boot_moment 

plot_CWM <- function(sum_boot_moment){
  
  ggplot(sum_boot_moment, aes(x = depth_layer, y = mean)) + 
    geom_point(alpha = 0.5, size = 1, position = position_jitter(width = 0.2), aes(col=depth_layer)) +
    geom_boxplot(aes(group=depth_layer, col=depth_layer, fill=depth_layer), alpha=0.1)+
    scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
    scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
    facet_wrap(~trait, scales = "free", ncol = 4) +
    theme_light() +
    labs(y="Community biomass-weighted mean (CWM) ")+
    guides(col="none", fill="none")+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.grid.major = element_blank(),
          axis.title.y.left = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.y = element_text(size = 15),
          strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 16, color = "black"))
  
  ggsave("CWM_boot_sum.png", path = "figures", dpi = 800, height = 13, width = 12)
  
}

#' ACP on the CWM values 
#'
#' @param np_bootstrapped_moments 
#'
#' @returns A ACP on variables (CWM mean values by depth layer) and an individuals ACP (depth layer)
#'
#' @examples

ACP_function <- function(np_bootstrapped_moments){
  
  sum_boot_moment_pca <- traitstrap::trait_summarise_boot_moments(
    np_bootstrapped_moments
  ) %>% 
    ungroup() %>% 
    mutate(trait= gsub("_"," ", trait)) %>% 
    mutate(trait=stringr::str_to_sentence(trait)) %>% 
    select(c(trait, depth_layer, mean)) %>% 
    group_by(
      depth_layer, trait
    ) %>% 
    summarise(median_value= median(mean)) %>% 
    distinct() %>% 
    ungroup() %>% 
    tidyr::pivot_wider(id_cols=depth_layer, values_from = "median_value", names_from = "trait") %>% 
    tibble::column_to_rownames(var = "depth_layer")
  
  
  res.pca <- FactoMineR::PCA(sum_boot_moment_pca, graph = FALSE)
  
  pca_var <- factoextra::fviz_pca_var(
    res.pca,
    repel = TRUE,
    pointsize = 2,
    arrowsize = 0.1,
    label = "var",
    title = "",
    col.circle = "gray80",
    col.var = "black",
    labelsize=5,
    alpha.arrow=0.5
  ) +
    labs(x = "", y = "") +
    theme_light() +
    theme(
      aspect.ratio = 1,
      panel.grid.minor =  element_blank(),
      panel.grid.major = element_blank())
  
  pca_ind <- factoextra::fviz_pca_ind(res.pca, title="") +
    theme_light() +
    xlim(c(-7,7))+
    labs(x="", y="")+
    theme(aspect.ratio = 1)
  
  combined_plot <- gridExtra::grid.arrange(pca_var, pca_ind, ncol = 2)
  
  ggsave("PCA_CWM.png", plot = combined_plot, path = "figures", dpi = 700, height = 6, width = 11)
  
}