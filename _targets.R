#' Targets plan
#'
#'@author Liz Loutrage, \email{liz.loutrage@gmail.com}
#'        Anik Brind'amour, \email {Anik.Brindamour@ifremer.fr}
#'

## Attach required packages ----
library(targets)
library(dplyr)
targets::tar_option_set(packages = c("dplyr", "ggplot2", "traitstrap", "tidyr"))

# load functions
targets::tar_source(files = "R")

list(
  # 1. Morphometric data ----
  # Load morphometric data (DOI:)
  tar_target(
    morpho_data,
    readxl::read_excel(here::here("data", "Morphometric_data_42_deep_pelagic_fish.xlsx"),
                       sheet = 3) %>%
      select(-c(`Individual code`, `Station label`, `Individual weight [g]`)) %>%
      rename(depth = `Trawling depth [m]`, 
             species = Species) %>% 
      rename_all(~ .x %>% 
                   stringr::str_remove_all(" \\[mm\\]") %>% 
                   stringr::str_to_lower() %>%    
                   stringr::str_replace_all(" ", "_")) %>% 
      # Eurypharynx pelecanoides (7 missing traits)
      filter(species !="Eurypharynx_pelecanoides")%>%
     # mutate(across(everything(), ~ na_if(., ""))) %>%
    mutate(across(3:21, as.numeric))
  ),
  
  # Imputation
  tar_target(
    original_data,
    morpho_data %>%
      filter(species != "Eurypharynx_pelecanoides") %>%
      # select only numeric traits 
      select(1:23)
  ),
  
  tar_target(
    imputation,
    mice::mice(
      original_data,
      m = 5,
      maxit = 50,
      printFlag = F
    )
  ) ,
  
  tar_target(imputed_data, mice::complete(imputation)),
  
  # calculate functional numeric traits
  tar_target(
    numeric_traits,
    imputed_data %>%
      na.omit() %>%
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
        operculum_volume
      ) %>%
      group_by(species) %>%
      summarise(across(everything(), mean, na.rm = TRUE)) %>%
      arrange(species)
  ),
  
  # categorical traits for species without NA
  tar_target(
    cat_morpho,
    morpho_data %>%
      select(
        species,
        ventral_photophores,
        gland_head,
        chin_barbel,
        small_teeth,
        large_teeth,
        fang_teeth,
        retractable_teeth,
        internal_teeth,
        gill_raker_types,
        oral_gape_axis
      ) %>%
      na.omit() %>%
      distinct() %>%
      arrange(species)
  ),
  
  # combined the two data frames
  tar_target(
    fish_traits,
    numeric_traits %>%
      inner_join(cat_morpho, by = "species") %>%
      arrange(species) %>%
      mutate(
        species = stringr::str_replace(species, "Cyclothone_sp", "Cyclothone")
      ) %>%
      filter(species != "Eurypharynx_pelecanoides") %>%
      tibble::column_to_rownames("species") %>%
      # assign trait type
      # as.factor for qualitative traits
      mutate_if(is.character, as.factor) %>%
      # as.ordered for ordered variables
      mutate_at(c("gill_raker_types", "oral_gape_axis"), as.ordered)
  ),
  
  # trait category
  tar_target(
    fish_traits_cat,
    fish_traits_cat <- as.data.frame(
      list(
        trait_name = c("eye_size",
                       "orbital_length",
                       "gill_outflow",
                       "oral_gape_surface",
                       "oral_gape_shape",
                       "oral_gape_position",
                       "lower_jaw_length",
                       "head_length",
                       "body_depth",
                       "pectoral_fin_position",
                       "pectoral_fin_insertion",
                       "transversal_shape",
                       "caudal_throttle_width",
                       "dorsal_fin_insertion",
                       "eye_position",
                       "operculum_volume",
                       "ventral_photophores",
                       "gland_head",
                       "chin_barbel",
                       "small_teeth",
                       "large_teeth",
                       "fang_teeth",
                       "retractable_teeth",
                       "internal_teeth",
                       "gill_raker_types",
                       "oral_gape_axis"),
        
        trait_type = c("Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q", 
                       "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q",
                       "N", "N", "N", "N", "N", "N", "N", "N",
                       "O", "O")))
  ),
  
  # Names of species measured in a vector 
  tar_target(sp_names, c(rownames(fish_traits))),
  
  # 1. Biomass data ----
  # Load species biomass data by depth layer 
  
  # 2002-2019 data DOI : https://doi.pangaea.de/10.1594/PANGAEA.967132
  tar_target(data_biomass_2002_2019, utils::read.csv(
    here::here("data", "Spitz-etal_2024_corrected.csv"),
    sep = ";",
    header = T,
    dec = ","
  ) %>% 
    # calculate the distance trawled 
    rowwise() %>%
    mutate(
      distance_trawled_m = round(geosphere::distHaversine(
        c(Longitude, Latitude),
        c(Longitude.2, Latitude.2)
      ))) %>%
    ungroup() %>% 
    select(-c(2:7)) %>% 
    # new taxonomic name 
    rename("Lampanyctus_ater" = "Nannobrachium_atrum") %>% 
    tidyr::pivot_longer(cols = -c(Event, distance_trawled_m, depth),
                        names_to = "species",
                        values_to = "biomass_sp") %>%
    filter(species %in% sp_names) %>%
    replace(is.na(.), 0) %>% 
    select(-Event)
  ),

  # 2021-2022 data DOI : https://doi.org/10.48579/PRO/AIKOEB
  tar_target(data_biomass_2021_2022, readxl::read_excel(
    here::here(
      "data",
      "biomass_abundance_deep_pelagic_fish_Bay_Biscay.xlsx"
    ),
    sheet = 3
  ) %>% 
    select(Species, `Station label`,`Trawl sum biomass [kg]`,`Distance trawled [m]`,
           `Trawling depth [m]`) %>% 
    distinct() %>%
    mutate(Species = gsub(" ", "_", Species)) %>%
    filter(Species %in% sp_names) %>%
    rename("species" = "Species",
           "station" = "Station label",
           "distance_trawled_m"="Distance trawled [m]",
           "biomass_sp" = "Trawl sum biomass [kg]",
           "depth"="Trawling depth [m]") %>%
    mutate(
      biomass_sp = as.numeric(biomass_sp),      # Convert to numeric
      biomass_sp = tidyr::replace_na(biomass_sp, 0),  # Replace NA by 0 (optional, already done by replace(is.na(.), 0))
      biomass_sp = biomass_sp * 1000
    ) %>% 
    select(-station)), 
  
  # Combine all biomass data 
  tar_target(depth_fish_biomass, rbind(data_biomass_2002_2019, data_biomass_2021_2022) %>%
               # volume filtered = 
               # mean vertical opening (24 m) × mean horizontal opening (58 m) × distance trawled 
               mutate(volume_filtered = 24*58*distance_trawled_m) %>% 
               select(-c(distance_trawled_m)) %>% 
               as.data.frame() %>%
               # divise biomass by the volume filtered at each trawl (g.m3)
               mutate(biomass_cpu = (biomass_sp / volume_filtered)) %>%
               select(species, depth, biomass_cpu) %>%
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
               select(-depth) %>%
               group_by(species, depth_layer) %>%
               mutate(biomass = sum(biomass_cpu)) %>%
               select(-c(biomass_cpu)) %>%
               distinct() %>%
               arrange(species) %>% 
               tidyr::pivot_wider(names_from = species, values_from = biomass) %>%
               replace(is.na(.), 0) %>%
               tibble::column_to_rownames(var = "depth_layer") %>%
               as.matrix()
  ),
  
  # 3. Functional space ----
  # Distance matrix

  tar_target(sp_dist_fish, mFD::funct.dist(
    sp_tr         = fish_traits,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE)),

  # quality of functional spaces
  tar_target(
    fspaces_quality_fish,
    mFD::quality.fspaces(
      sp_dist             = sp_dist_fish,
      maxdim_pcoa         = 10,
      deviation_weighting = "absolute",
      fdist_scaling       = FALSE,
      fdendro             = "average"
    )
  ),
  
  # axes coordinates
  tar_target(
    sp_faxes_coord_fish,
    fspaces_quality_fish$"details_fspaces"$"sp_pc_coord"
  ),
    
  # plot functional space
  tar_target(functional_space, functional_space_plot(sp_faxes_coord_fish, depth_fish_biomass)),
  
  # 4. Standardized Effect Size (SES)----
  tar_target(dbFD_result_sim, run_simulations(depth_fish_biomass, fish_traits)),
  
  tar_target(SES_combined, compute_SES(dbFD_result_sim, fish_traits, depth_fish_biomass)),
  
  tar_target(plot_SES_fig, plot_SES(SES_combined)),
  
  tar_target(save_plot, ggsave("figures/SES_dbFD.png", plot_SES_fig, dpi = 700, height = 6, width = 7)),
  
  # 5. CWM ----
  # format data
  tar_target(data_CWM, format_data_CWM(morpho_data, data_biomass_2002_2019, data_biomass_2021_2022)),
  
  # calcul CWM
  tar_target(CWM_results, CWM_calcul(data_CWM$community, data_CWM$trait_boot)),
  
  # plot
  tar_target(final_CWM_plot, plot_CWM(CWM_results$sum_boot_moment)),
  
  # ACP of the CWM values 
  tar_target(ACP_plot, ACP_function(CWM_results$np_bootstrapped_moments)),
  
  # 6. Functional rarity ----
  tar_target(taxonomic_families, sp_names %>%
      as.data.frame() %>%
      `colnames<-`("species") %>% 
      mutate(
        family = case_when(
          species %in%
            c(
              "Benthosema_glaciale",
              "Ceratoscopelus_maderensis",
              "Diaphus_metopoclampus",
              "Lampanyctus_ater",
              "Lampanyctus_crocodilus",
              "Lampanyctus_macdonaldi",
              "Lobianchia_gemellarii",
              "Myctophum_punctatum",
              "Notoscopelus_bolini",
              "Notoscopelus_kroyeri",
              "Bolinichthys_supralateralis"
            ) ~ "Myctophidae",
          species %in% c(
            "Borostomias_antarcticus",
            "Chauliodus_sloani",
            "Malacosteus_niger",
            "Melanostomias_bartonbeani",
            "Stomias_boa"
          ) ~ "Stomiidae",
          species %in% c(
            "Holtbyrnia_anomala",
            "Holtbyrnia_macrops",
            "Maulisia_argipalla",
            "Maulisia_mauli",
            "Maulisia_microlepis",
            "Normichthys_operosus",
            "Searsia_koefoedi",
            "Sagamichthys_schnakenbecki"
          ) ~ "Platytroctidae",
          species %in% c("Sigmops_bathyphilus",
                         "Gonostoma_elongatum") ~ "Gonostomatidae",
          species %in% c(
            "Argyropelecus_hemigymnus",
            "Maurolicus_muelleri",
            "Argyropelecus_olfersii"
          ) ~ "Sternoptychidae",
          species == "Anoplogaster_cornuta" ~ "Anoplogastridae",
          species %in% c("Arctozenus_risso", "Paralepis_coregonoides") ~ "Paralepididae",
          species == "Bathylagus_euryops" ~ "Bathylagidae",
          species == "Cyclothone" ~ "Gonostomatidae",
          species == "Derichthys_serpentinus" ~ "Derichthyidae",
          species == "Eurypharynx_pelecanoides" ~ "Eurypharyngidae",
          species == "Evermannella_balbo" ~ "Evermannellidae",
          species == "Lestidiops_sphyrenoides" ~ "Lestidiidae",
          species == "Melanostigma_atlanticum" ~ "Zoarcidae",
          species %in% c("Photostylus_pycnopterus",
                         "Xenodermichthys_copei") ~ "Alepocephalidae",
          species == "Serrivomer_beanii" ~ "Serrivomeridae"
        )
      )%>% 
      mutate(
        order = case_when(
          family =="Myctophidae" ~ "Myctophiformes",
          family %in% c("Stomiidae","Gonostomatidae", "Sternoptychidae") ~  "Stomiiformes",
          family %in% c("Platytroctidae","Alepocephalidae") ~ "Alepocephaliformes",
          family == "Anoplogastridae" ~ "Trachichthyiformes",
          family %in% c("Paralepididae","Evermannellidae","Lestidiidae") ~ "Aulopiformes",
          family ==  "Bathylagidae" ~ "Argentiniformes",
          family %in% c("Derichthyidae","Serrivomeridae") ~ "Anguilliformes",
          family ==  "Eurypharyngidae" ~"Saccopharyngiformes",
          family == "Zoarcidae" ~ "Perciformes",
        )
      )
  ),
  
  tar_target(functional_rarity, calcul_functional_ri(data_biomass_2002_2019, data_biomass_2021_2022, 
                                                     depth_fish_biomass, sp_dist_fish, taxonomic_families))
  )
