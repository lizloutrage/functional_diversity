
# load melodic function 
source("R/melodic.R")

station_sp <-
  rbind(data_biomass_2002_2019, data_biomass_2021_2022) %>%
  as.data.frame() %>%
  left_join(metadata) %>%
  select(species, biomass_sp, volume_filtered, station) %>%
  # Divide biomass by the volume filtered at each trawl (g.m3)
  mutate(biomass_cpu = (biomass_sp / volume_filtered) * 1000) %>%
  select(species, biomass_cpu, station) %>%
  replace(is.na(.), 0) %>%
  group_by(species, station) %>%
  mutate(biomass = sum(biomass_cpu)) %>%
  select(-biomass_cpu) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = species, values_from = biomass) %>%
  replace(is.na(.), 0) %>%
  arrange(station) %>%
  tibble::column_to_rownames(var = "station") %>%
  select(order(colnames(.))) %>%
  as.matrix()

# Functional Mean Pairwise Distance ----
# observed values MPD: mean weighted distance between all species pairs
mpdObs <- melodic(samp = station_sp, dis = sp_dist_fish,
                  type = "abundance")$abundance$mpd

names(mpdObs) <- rownames(station_sp)

# with n randomization ----
numberReps <- 100

#Lets create a matrix to store results from each iteration (one column per iteration)
resultsRandom <- matrix(NA, nrow = nrow(station_sp), ncol = numberReps,
                        dimnames = list(rownames(station_sp),
                                        paste0("Sim.", 1:numberReps)))
for(rep in 1:numberReps){
  traitsRand <- fish_traits[sample(1:nrow(fish_traits)),]
  rownames(traitsRand) <- rownames(fish_traits)
  dissimRand <- mFD::funct.dist(
    sp_tr         = traitsRand,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE
  )
  mpdRand<-melodic(samp = station_sp, dis = dissimRand,
                   type = "abundance")$abundance$mpd
  resultsRandom[, rep] <- mpdRand
}
mpdObs <- melodic(samp = station_sp, dis = sp_dist_fish,
                  type = "abundance")$abundance$mpd

meanNull <- rowMeans(resultsRandom)

ES <- mpdObs - meanNull

sdNull <- apply(resultsRandom, 1, sd)

SES <- ES / sdNull
SES <- as.data.frame(SES)

# plot ----
SES_plot <- SES %>%
  tibble::rownames_to_column(var = "station") %>%
  inner_join(metadata %>% select(station, depth), by = "station") %>%
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  )

# along depth 
ggplot(SES_plot, aes(x = depth, y = SES)) +
  geom_point() +
  geom_smooth(method="lm", se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        vstep = -0.0005)+ 
  labs(
    y = "Standard Effect Size (SES) MPD") +
  theme_light() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))+
  guides(col="none", fill="none")


# by depth layer
SES_plot$depth_layer <- factor(
  SES_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)
  
ggplot(SES_plot, aes(x = depth_layer, y = SES, fill = depth_layer)) +
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
  guides(col="none", fill="none")

ggsave("SES_mpd_abundance.png", path = "figures", dpi = 700, height = 7, width = 9)

# Functional Rao index ----
# Rao index 
raoObs <- melodic(samp = station_sp, dis = sp_dist_fish,
                  type = "abundance")$abundance$rao

names(raoObs) <- rownames(station_sp)

# with n randomization ----
numberReps <- 100

#Lets create a matrix to store results from each iteration (one column per iteration)
resultsRandom <- matrix(NA, nrow = nrow(station_sp), ncol = numberReps,
                        dimnames = list(rownames(station_sp),
                                        paste0("Sim.", 1:numberReps)))
for(rep in 1:numberReps){
  traitsRand <- fish_traits[sample(1:nrow(fish_traits)),]
  rownames(traitsRand) <- rownames(fish_traits)
  dissimRand <- mFD::funct.dist(
    sp_tr         = traitsRand,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE
  )
  raoRand<-melodic(samp = station_sp, dis = dissimRand,
                   type = "abundance")$abundance$rao
  resultsRandom[, rep] <- raoRand
}
raoObs <- melodic(samp = station_sp, dis = sp_dist_fish,
                  type = "abundance")$abundance$rao

meanNull <- rowMeans(resultsRandom)

ES <- raoObs - meanNull

sdNull <- apply(resultsRandom, 1, sd)

SES <- ES / sdNull
SES <- as.data.frame(SES)

# plot ----
SES_plot <- SES %>%
  tibble::rownames_to_column(var = "station") %>%
  inner_join(metadata %>% select(station, depth), by = "station") %>%
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  )

# along depth 
ggplot(SES_plot, aes(x = depth, y = SES)) +
  geom_point() +
  geom_smooth(method="lm", se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        vstep = -0.0005)+ 
  labs(
    y = "Standard Effect Size (SES) rao") +
  theme_light() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))+
  guides(col="none", fill="none")


# by depth layer
SES_plot$depth_layer <- factor(
  SES_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(SES_plot, aes(x = depth_layer, y = SES, fill = depth_layer)) +
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
  guides(col="none", fill="none")

# test env filtering and biotic interactions
# MPD with independentswap randomization ----
dissTraits <- sp_dist_fish
plotxspPA <- replace(station_sp, station_sp > 0, 1)
SESMPD <-
  picante::ses.mpd(
    samp = plotxspPA,
    dissTraits,
    null.model = "independentswap",
    runs = 500,
    abundance.weighted = F
  )
SESMPD <- SESMPD$mpd.obs.z
names(SESMPD) <- rownames(station_sp)

plotxenv <- metadata %>%
  select(station, depth) %>%
  filter(station %in% c(names(SESMPD)))

model <- lm(SESMPD ~ depth, data = plotxenv)
summary(model)

SESMPD_plot <- SESMPD %>%
  as.data.frame() %>%
  rename("SES" = ".") %>%
  tibble::rownames_to_column(var = "station") %>%
  inner_join(plotxenv) %>%
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  )

ggplot(SESMPD_plot, aes(x = depth, y = SES)) +
  geom_point() +
  geom_smooth(method="lm", se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        vstep = -0.0005)+ 
  labs(
    y = "Standard Effect Size (SES) MPD") +
  theme_light() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))+
  guides(col="none", fill="none")

# by depth layer
SESMPD_plot$depth_layer <- factor(
  SESMPD_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(SESMPD_plot, aes(x = depth_layer, y = SES, fill = depth_layer)) +
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
  guides(col="none", fill="none")



# biotic interactions ----
numberReps <- 100
# 1. Estimate observed values of mpd using the melodic function:
observedMPD <- melodic(samp =station_sp, dis=sp_dist_fish ,
                       type="abundance")$abundance$mpd

# 2. Prepare a matrix to store the results of each iteration of the null model:
simMPD <- matrix(NA, nrow = numberReps, ncol = nrow(station_sp),
                 dimnames = list(paste0("Sim.", 1:numberReps), rownames(station_sp)))

# 3. Make the null model for each site.
for (i in 1:ncol(simMPD)){ #For each site
  # 3.1. create a matrix full of zeroes to store the 500 null communities for the site i:
  randomComms <- matrix(0, nrow = numberReps, ncol = ncol(station_sp),
                        dimnames = list(paste0("Sim.", 1:numberReps), colnames(station_sp)))
  
  # 3.2 check which species are present at site i. Those are the ones whose abundances we will shuffle:
  spInComm <- which(station_sp[i,] > 0)
  realisedAbund <- station_sp[i, spInComm]
  
  # 3.3. in each repetition of the null model, shuffle the abundances of species present in site
  for(j in 1:numberReps){
    randomisedAbund <- as.numeric(sample(realisedAbund))
    randomComms[j, spInComm] <- randomisedAbund
  }
  
  #3.4. Estimate mpd for each repetition, and store the results in simMPD
  simMPD[, i] <- melodic(samp=randomComms, dis=dissTraits ,
                         type="abundance")$abundance$mpd
}

#4. Estimate SES for each site
SESxSampleWithin <- (observedMPD - colMeans(simMPD)) / apply(simMPD, 2, sd, na.rm = T)

SESxSampleWithin_plot <- SESxSampleWithin%>%
  as.data.frame() %>%
  rename("SES" = ".") %>%
  tibble::rownames_to_column(var = "station") %>%
  inner_join(plotxenv) %>%
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  )

model2 <- lm(SESxSampleWithin ~ depth, data = plotxenv)
summary(model2)

ggplot(SESxSampleWithin_plot, aes(x = depth, y = SES)) +
  geom_point() +
  geom_smooth(method="lm", se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        vstep = -0.0005)+ 
  labs(
    y = "Standard Effect Size (SES) MPD") +
  theme_light() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))+
  guides(col="none", fill="none")

# by depth layer
SESxSampleWithin_plot$depth_layer <- factor(
  SESxSampleWithin_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(SESxSampleWithin_plot, aes(x = depth_layer, y = SES, fill = depth_layer)) +
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
  guides(col="none", fill="none")


# by depth layer ----

# load melodic function 
source("R/melodic.R")

sp_dist_fish <- mFD::funct.dist(
  sp_tr         = fish_traits,
  tr_cat        = fish_traits_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# Functional Mean Pairwise Distance ----
# observed values MPD: mean weighted distance between all species pairs
mpdObs <- melodic(samp = depth_fish_biomass, dis = sp_dist_fish,
                  type = "abundance")$abundance$mpd

names(mpdObs) <- rownames(depth_fish_biomass)

# with n randomization ----
numberReps <- 100

#Lets create a matrix to store results from each iteration (one column per iteration)
resultsRandom <- matrix(NA, nrow = nrow(depth_fish_biomass), ncol = numberReps,
                        dimnames = list(rownames(depth_fish_biomass),
                                        paste0("Sim.", 1:numberReps)))
for(rep in 1:numberReps){
  traitsRand <- fish_traits[sample(1:nrow(fish_traits)),]
  rownames(traitsRand) <- rownames(fish_traits)
  dissimRand <- mFD::funct.dist(
    sp_tr         = traitsRand,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE
  )
  mpdRand<-melodic(samp = depth_fish_biomass, dis = dissimRand,
                   type = "abundance")$abundance$mpd
  resultsRandom[, rep] <- mpdRand
}
mpdObs <- melodic(samp = depth_fish_biomass, dis = sp_dist_fish,
                  type = "abundance")$abundance$mpd

meanNull <- rowMeans(resultsRandom)

ES <- mpdObs - meanNull

sdNull <- apply(resultsRandom, 1, sd)

SES <- ES / sdNull
SES <- as.data.frame(SES) %>% 
  tibble::rownames_to_column(var = "depth_layer")
SES$depth_layer <- factor(
  SES$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(SES, aes(x = depth_layer, y = SES, fill = depth_layer)) +
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
  guides(col="none", fill="none")

