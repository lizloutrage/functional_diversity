#' Cacul of functional uniqueness and geographical restrictiveness of each species
#'
#' @param data_biomass_2002_2019 
#' @param data_biomass_2021_2022 
#'
#' @returns
#'
#' @examples

calcul_functional_ri <- function(data_biomass_2002_2019, data_biomass_2021_2022,
                                 depth_fish_biomass, sp_dist_fish, taxonomic_families){
  
  depth_distribution <- rbind(data_biomass_2002_2019, data_biomass_2021_2022)%>%
  as.data.frame()%>%
  select(species, biomass_sp, depth)%>%
  replace(is.na(.), 0)%>%
  group_by(species, depth)%>%
  mutate(biomass=sum(biomass_sp))%>%
  select(-c(biomass_sp))%>%
  filter(biomass>0) %>% 
  distinct()%>%
  tidyr::pivot_wider(names_from = species, values_from = biomass)%>%
  replace(is.na(.), 0)%>%
  tibble::column_to_rownames(var = "depth")%>%
  as.matrix()

ri <- funrar::restrictedness(depth_distribution)

sp_ui <- funrar::uniqueness(
  pres_matrix = depth_fish_biomass,
  as.matrix(sp_dist_fish)
)

sp_ri_ui <- sp_ui %>% 
  inner_join(ri) %>% 
  inner_join(taxonomic_families) %>% 
  mutate(species= gsub("_"," ",species))

stat <- sp_ri_ui %>%  
  summarise(med_ui = median(Ui),
            med_ri =median(Ri),
            mean_ui = mean(Ui),
            mean_ri = mean(Ri)) 

ggplot(sp_ri_ui, aes(Ri,Ui))+
  geom_point(size = 3, aes(shape=order))+
  ggrepel::geom_text_repel(aes(label = species), col="grey60", max.overlaps=30, size=5.2)+
  scale_shape_manual(values = c(8, 4, 7, 17,16,5, 15, 3, 25,22))+
  labs(y = "Functional Uniqueness", x = "Geographical Restrictedness", shape="Taxonomic order")+
  theme_bw()+
  geom_hline(yintercept = stat$mean_ui, linetype = "dashed", color = "black", linewidth = 0.8)+
  geom_vline(xintercept = stat$mean_ri, linetype = "dashed", color = "black", linewidth = 0.8)+
  theme(aspect.ratio = 1,
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))

ggsave("Uniqueness_Ri.png", path = "figures", dpi = 700, height = 9, width = 11)

}

