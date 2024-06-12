isotopes_data <- utils::read.csv(here::here("data", "isotopes_data.csv"), sep = ";", header = T, dec = ",")

data_sum_iso <- isotopes_data %>%
  group_by(species) %>%
  mutate(
    n = n(),
    mean_d15N = round(mean(d15N), 2),
    sd_d15N = round(sd(d15N), 2),
    mean_d13C = round(mean(d13C), 2),
    sd_d13C = round(sd(d13C), 2)
  ) %>%
  distinct()

ggplot(data = data_sum_iso, aes(x = mean_d13C, y = mean_d15N, col=species)) + 
  geom_point()+
  geom_errorbar(aes(ymin = mean_d15N - sd_d15N, ymax = mean_d15N + sd_d15N)) +
  geom_errorbarh(aes(xmin = mean_d13C - sd_d13C, xmax = mean_d13C + sd_d13C)) +
  theme_bw()+
  scale_x_continuous(expression({delta}^13*C~'\u2030')) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'))+
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15))+
  theme(aspect.ratio = 1)+
  geom_label(label=data_sum_iso$species)

d15N_sp <-data_sum_iso %>% 
  select(d15N) 

fish_trait_essai <- fish_traits %>%  
  tibble::rownames_to_column(var = "species") %>% 
  select(1:17) %>% 
  tidyr::pivot_longer(!species, names_to = "traits", values_to = "values") %>% 
  inner_join(d15N_sp) 

ggplot(fish_trait_essai, aes(x=values, y=d15N))+
  facet_wrap(~traits, scales = "free")+
  geom_smooth(method=lm, se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        label.x.npc = "right",
                        label.y.npc = "bottom",
                        vstep = -0.0005)+ 
  theme_minimal()

