#' functional space plot PC1-PC2
#'
#' @param sp_coord : sp_faxes_coord_fish, calculated with mFD::alpha.fd.multidim
#' @param depth_fish_biomass : biomass species matrix 
#'
#' @returns : a plot of the total functional space + each depth layer functional space
#'
#' @examples functional_space_plot (sp_faxes_coord_fish, depth_fish_biomass)

functional_space_plot <- function(sp_coord, depth_fish_biomass) {
  
# Convert sp_faxes_coord_fish to a data frame
sp_coord_community <- as.data.frame(sp_coord[, c("PC1", "PC2")]) %>%
  tibble::rownames_to_column(var = "species")

## Summary of the assemblages * species data.frame
asb_sp_fish_summ <- mFD::asb.sp.summary(asb_sp_w = depth_fish_biomass)
asb_sp_fish_occ  <- asb_sp_fish_summ$"asb_sp_occ"

# Depth_layer ----
# Transform biomass data
sp_all_layers <- depth_fish_biomass %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "depth_layer") %>%
  tidyr::pivot_longer(!depth_layer, values_to = "biomass", names_to = "species") %>%
  filter(biomass > 0)

# Create a list to store data frames for each depth layer
all_data_layers <- lapply(unique(sp_all_layers$depth_layer), function(layer) {
  sp_layer <- sp_all_layers %>%
    filter(depth_layer == layer)
  
  # Mark presence or absence
  sp_coord_layer <- sp_coord_community %>%
    mutate(layer_presence = ifelse(species %in% sp_layer$species, "yes", "no")) %>%
    mutate(depth_layer = layer)
  
  return(sp_coord_layer)
})

# Combine all layers into one data frame
sp_coord_community_all_layers <- bind_rows(all_data_layers)

# Function to calculate the convex hull for a given data frame
calculate_hull <- function(data) {
  if (nrow(data) < 3) return(data)  # Handle cases with fewer than 3 points
  data %>%
    slice(chull(PC1, PC2))
}

# Initialize lists to store hulls
hull_all_combined <- NULL
hull_layer_combined <- NULL

for(layer in unique(sp_coord_community_all_layers$depth_layer)) {
  sp_layer <- sp_coord_community_all_layers %>%
    filter(depth_layer == layer)
  
  hull_all <- calculate_hull(sp_layer) %>%
    mutate(depth_layer = layer, type = "all")
  
  hull_layer <- calculate_hull(sp_layer %>% filter(layer_presence == "yes")) %>%
    mutate(depth_layer = layer, type = "present")
  
  hull_all_combined <- bind_rows(hull_all_combined, hull_all)
  hull_layer_combined <- bind_rows(hull_layer_combined, hull_layer)
}

# Define depth layer levels in desired order
depth_levels <- c("Epipelagic", "Upper mesopelagic", "Lower mesopelagic", "Bathypelagic")

# Ensure depth_layer is a factor with specified levels
sp_coord_community_all_layers$depth_layer <- factor(sp_coord_community_all_layers$depth_layer, levels = depth_levels)
hull_all_combined$depth_layer <- factor(hull_all_combined$depth_layer, levels = depth_levels)
hull_layer_combined$depth_layer <- factor(hull_layer_combined$depth_layer, levels = depth_levels)

# Define colors for each depth layer
depth_colors <- c("Epipelagic" = "#FEA520", 
                  "Upper mesopelagic" = "#D62246", 
                  "Lower mesopelagic" = "#6255B4", 
                  "Bathypelagic" = "#3C685A")

# Plotting
plot_depth <- ggplot() +
  # Plot present species
  geom_point(data = sp_coord_community_all_layers %>% filter(layer_presence == "yes"), 
             aes(PC1, PC2, col = depth_layer), size = 2, shape=19) +
  # Plot absent species
  geom_point(data = sp_coord_community_all_layers %>% filter(layer_presence == "no"), 
             aes(PC1, PC2), col = "grey", shape = 8, size = 2) +
  # Plot hulls
  geom_polygon(data = hull_all_combined, aes(x = PC1, y = PC2), 
               fill =  "gray70", alpha = 0.2) +
  geom_polygon(data = hull_layer_combined, aes(x = PC1, y = PC2, group = depth_layer, fill = depth_layer), 
               alpha = 0.4) +
  theme_light() +
  scale_color_manual(values = depth_colors) +
  scale_fill_manual(values = depth_colors) +
  guides(col = "none", shape = "none", fill = "none") +
  facet_wrap(~ depth_layer, nrow=1 ) +  
  theme(
    strip.text.x = element_text(size = 14, color = "gray20"),
    strip.background = element_rect(fill = "white"),
    aspect.ratio = 1,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )


beta_fd_indices_fish <- mFD::beta.fd.multidim(
  sp_faxes_coord   = sp_coord[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_occ       = asb_sp_fish_occ,
  check_input      = TRUE,
  beta_family      = c("Jaccard"),
  details_returned = T)

vertices_community <-beta_fd_indices_fish$"details"$"asb_vertices"$Bathypelagic 

sp_coord_community <- as.data.frame(sp_coord[, 1:2]) %>% 
  tibble::rownames_to_column(var = "species") %>% 
  mutate(vertices=case_when(species%in%vertices_community~"vertice",
                            !species%in%vertices_community~"not_vertice")) 

hull <- sp_coord_community%>% 
  slice(chull(PC1, PC2))

plot_com <- ggplot(data = sp_coord_community,aes(PC1, PC2)) +
  scale_color_manual(values = c("grey", "black"))+
  geom_point(size = 2.5, alpha=0.8, aes( shape=vertices, col= vertices)) +
  geom_polygon(data=hull, aes(x = PC1, y = PC2),
               fill = "gray70", alpha = 0.2) +
  theme_light() +
  geom_hline(yintercept = 0, linetype = 2, col = "gray45") +
  geom_vline(xintercept = 0, linetype = 2, col = "gray45") +
  guides(col = "none", shape = "none", fill = "none") +
  theme(
    strip.text.x = element_text(size = 14, color = "gray20"),
    strip.background = element_rect(fill = "white"),
    aspect.ratio = 1,
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

ggpubr::ggarrange(
  labels = c("A","B"),
  plot_com,   
  plot_depth, 
  nrow = 2
) 

ggsave("functional_space.png", path = "figures", dpi = 800, height = 8, width = 10)
}