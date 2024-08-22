
ses <- picante::ses.mpd(samp = depth_fish_biomass, dis=sp_dist_fish,
                        null.model="frequency", runs = 1000, abundance.weighted = T)

ses_depth_layer <- ses %>% 
  tibble::rownames_to_column(var="depth_layer") 
ses_depth_layer$depth_layer <- factor(
  ses_depth_layer$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(ses_depth_layer, aes(x = depth_layer, y =mpd.obs.z, fill = depth_layer)) +
  geom_point(aes(col=depth_layer), size=3.5) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  theme_light() +
  guides(col="none", fill="none")


# by depth ----
ses <- picante::ses.mpd(samp = station_sp, dis=sp_dist_fish,
                        null.model="frequency", runs = 1000, abundance.weighted = T)

ses_depth <- ses %>% 
  tibble::rownames_to_column(var="station") %>% 
  inner_join(metadata) %>%
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  )

ses_depth$depth_layer <- factor(
  ses_depth$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(ses_depth, aes(x = depth_layer, y =mpd.obs.z, fill = depth_layer)) +
  geom_point(alpha = 0.5, size = 1, position = position_jitter(width = 0.2), aes(col=depth_layer)) +
  geom_boxplot(alpha = 0.1, outlier.shape = NA, width = 0.5, aes(col=depth_layer, fill=depth_layer))+
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
        axis.text = element_text(size = 13))+
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "gray50", linewidth = 0.8)+
  geom_hline(yintercept = -1.96, linetype = "dashed", color = "gray50", linewidth = 0.8)+
  guides(col="none", fill="none")


ggplot(ses_depth, aes(x = depth, y =mpd.obs.z)) +
  geom_point() +
  geom_smooth(method="lm", se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        vstep = -0.0005)+
  theme_light() +
  guides(col="none", fill="none")


