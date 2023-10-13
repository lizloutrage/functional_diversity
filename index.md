---
title: "index"
author: "Liz Loutrage"
format: 
  html:
    self-contained: false
    code-fold: true
editor: source
keep-md: true
execute:
  warning: false
  message : false
toc: true
toc-title: Sections
toc-location: left
page-layout: full
theme: yeti
---



# Questions 

* <span style="color:#2596be;">__β-functional diversity__</span>: calcul des indices de dissimilarité entre les assemblages (profondeurs) : profondeur = filtre environnemental  ?

  + _Turnover_: élevé s’il n’y a pas de combinaison de traits partagés entre les assemblages
  
  + _Nestedness component_: élevé si un assemblage abrite un petit sous-ensemble des stratégies fonctionnelles présentes dans l’autre

* <span style="color:#2596be;">__Functional rarity__</span>: 
  + Différencier échelle locale et régionale ?
  + inclure abondance/biomasse ? 
  + y a t-il beaucoup d'espèces avec des combinaisons de traits uniques ? 
  + y a en t-il plus en profondeur ? 
  + les espèces fonctionnellement rares ont t-elles une distribution plus restreinte ? 


# 1. Data preparation

::: {.cell}

```{.r .cell-code}
library(dplyr)

morphometric_data <- utils::read.csv(here::here("data", "morphometrics_data.csv"), sep = ";", header = T, dec = ".")

morpho_data <- morphometric_data %>%
  select(-c(variable_abbreviation, variable_unit)) %>%
  t() %>%
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1) %>%
  `rownames<-`(NULL)%>%
  # delete for now (n=1)
  filter(species!= "Diaphus_sp")

# replace empty value by NA 
morpho_data[morpho_data ==""] <- NA

# Numeric variables
morpho_data[, 4:23] <- sapply(morpho_data[, 4:23], as.numeric)
```
:::


### summary

- total measure = 359
-  total of species measured = 32

::: {.cell}

```{.r .cell-code}
morpho_data_summary <-morpho_data %>%
  group_by(species) %>%
  count(species)

htmltools::tagList(DT::datatable(morpho_data_summary))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-7d1d75907b0ca3621ce6" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-7d1d75907b0ca3621ce6">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"],["Anoplogaster_cornuta","Argyropelecus_hemigymnus","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metoclampus","Eurypharynx_pelecanoides","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Notoscopelus_bolini","Paralepis_coregonoides ","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Sigmops_bathyphilus"],[3,17,19,20,11,14,12,12,7,5,6,4,3,3,9,22,21,7,22,5,5,11,4,11,20,9,20,19,8,9,20]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::



## Species * traits
### data imputation  

::: {.cell}

```{.r .cell-code}
#select numeric variables for imputation 
morpho_num <- morpho_data %>%
  select(1:23)

imputed_data <-
  mice::mice(morpho_num,
             m = 5,
             maxit = 50,
             method = "pmm",
             printFlag=F)
             
completed_data <- mice::complete(imputed_data)
```
:::


### calctulate functional traits 

::: {.cell}

```{.r .cell-code}
# calculate functional numeric traits
morpho_numeric <- completed_data %>%
  na.omit() %>%
  select(-individual_code) %>%
  mutate(
    eye_size = eye_diameter / head_depth,
    orbital_length = eye_diameter / standard_length,
    oral_gap_surface = mouth_width * mouth_depth / body_width * body_depth,
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
    operculum_volume = operculum_depth / operculum_width
  ) %>%
  select(
    species,
    eye_size,
    orbital_length,
    oral_gap_surface,
    oral_gape_shape,
    oral_gape_position,
    lower_jaw_length,
    operculum_width,
    head_length,
    body_depth,
    pectoral_fin_position,
    pectoral_fin_insertion,
    transversal_shape,
    caudal_peduncle_min_depth,
    dorsal_fin_insertion,
    eye_position,
    operculum_volume
  ) %>%
  group_by(species) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  arrange(species)

# categorial traits for species without NA
cat_morpho <- morpho_data %>%
  na.omit() %>%
  select(
    species,
    photophores_ventral_position ,
    gland_head,
    chin_barbel,
    front_barbel,
    small_teeth,
    large_teeth,
    fang_teeth,
    retractile_teeth,
    internal_teeth,
    gill_raker_types,
    oral_gape_axis
  ) %>%
  distinct() %>%
  arrange(species)

# combined the two data frames
fish_traits <- morpho_numeric %>%
  inner_join(cat_morpho, by = "species") %>%
  tibble::column_to_rownames("species")%>%
  # assign trait type 
  # as.factor for qualitative traits
  mutate_if(is.character, as.factor)%>%
  # as.ordered for odered variables
  mutate_at(c("gill_raker_types", "oral_gape_axis"), as.ordered)
```
:::

::: {.cell}

```{.r .cell-code}
## Display the table ----
htmltools::tagList(DT::datatable(fish_traits))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-e0a1729bad976b0040ef" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-e0a1729bad976b0040ef">{"x":{"filter":"none","vertical":false,"data":[["Anoplogaster_cornuta","Bathylagus_euryops","Bolinichthys_supralateralis","Borostomias_antarcticus","Chauliodus_sloani","Cyclothone_sp","Diaphus_metoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Lampanyctus_ater","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Maulisia_mauli","Maurolicus_muelleri","Melanostomias_bartonbeani","Notoscopelus_bolini","Photostylus_pycnopterus","Sagamichthys_schnakenbecki"],[0.1870635427569378,0.8073379156515913,0.6943542959568116,0.3918502902611463,0.3236216278441371,0.2896732995527421,0.3965409748630725,0.4619095739058715,0.2627730950311468,0.7001620679316158,0.5876906171451259,0.4635401519874494,0.6379293967644859,0.4655204987772433,0.5436602586272192,0.5366700102175092,0.3404604909527202,0.58721677071155,0.3160340663331095,0.6531117613886723],[0.06513101563336203,0.09773731823878731,0.1002020784107539,0.0454502028499367,0.03195944404217845,0.01820101324427989,0.08617787828321036,0.06111392066224541,0.02365859567134613,0.107319044568802,0.04953860514578357,0.05324648475404745,0.03316666671153076,0.06137020103787766,0.0843550572708857,0.0978961923388425,0.02667037338846572,0.06117253100231441,0.03048020374468671,0.08748603143506498],[5842.317456564917,215.6744532820656,857.141772877457,2308.803855788343,1427.556961259949,479.1419350759956,509.9151356625916,746.3340129817943,6250.383835528853,1018.142068749161,1516.361659863597,2223.938773657847,191.888446067941,1226.901459954854,2055.911596674011,186.8514118522354,2537.245636289088,486.4451319733718,80.09634693327251,858.57580241159],[0.6488731902095241,1.208429987743106,1.145308631944953,0.9549389452177165,2.198671222745045,2.168647464401632,0.944195077723388,1.283292920158411,0.6068674018540213,1.051271164630917,1.144873224338048,1.162473804130551,2.185143165036392,1.100038150697565,1.163701414828532,1.285170782096033,1.398821013856888,1.278747111079255,1.080061200669548,1.203470421502817],[0.3951426840833095,0.3502518371432042,0.3380289556040331,0.4826993703447641,0.7068627312794583,0.7937932689673789,0.3551554524927789,0.5829657156202231,0.4721898773185077,0.4712003973315194,0.5769849659973523,0.5744746730161431,0.5520998821851169,0.5212153959103951,0.3738738564459647,0.5855853143839959,0.6176663797525285,0.4896021576606608,0.5624538104272562,0.361697362365493],[0.2861375317012913,0.07506640738457124,0.19638076001865,0.198414842989372,0.1144114643871464,0.2040581908463124,0.191587307998245,0.1634947940484265,0.1777783246347302,0.2092942077891755,0.1973450981018806,0.2048825282290511,0.1027761390567869,0.2223712374577787,0.2120154019923103,0.166670163223075,0.1234687560304594,0.2008815666871479,0.08475918421735412,0.1571232489521016],[20.92,8.536315789473685,12.47272727272727,15.79642857142857,9.851666666666667,7.613333333333333,6.976,9.282500000000001,27.92,9.563333333333333,18.38136363636364,25.00333333333333,4.928571428571429,15.45818181818182,15.73,3.591818181818182,18.11777777777778,9.629,6.5525,11.76],[0.3471179569492232,0.2214198161904544,0.2949444282485461,0.2219516612342632,0.1353122370036637,0.2203047019193976,0.2626215465872482,0.2169720130174164,0.208131053502031,0.3736914296278676,0.2268293066821225,0.2544000525765622,0.179483203158696,0.2690716828129346,0.3386367195769682,0.2861583530888247,0.1414616221329245,0.2484099358389354,0.1610794295053864,0.3035521067765809],[0.496765317595603,0.1776310863316782,0.23575289155312,0.1471579276088384,0.1139788839951372,0.1385564389886054,0.2794661893586785,0.1854842390181648,0.1439966534398835,0.1995718556257012,0.1634270563148341,0.1791292477891186,0.07016698379208147,0.2389029851391781,0.2006259493110997,0.2284769602774343,0.1281404978381019,0.1977431521215698,0.148949033138653,0.1648207240422557],[0.327552178795381,0.1987584730572414,0.4605590043104663,0.1754328409583324,0.289631998528789,0.1992742880547905,0.2327118342280331,0.215888746321026,0.2073022595505449,0.2511962839831692,0.2900486043297842,0.1868404720220886,0.3583607039010895,0.2710821884027898,0.1907795219830297,0.2251067492779084,0.2233648323381441,0.2851868407331372,0.4749769701497314,0.22306401155431],[0.4003208996368177,0.2584814374106652,0.3283279074114032,0.2369008386437003,0.1525109091904405,0.2296622405613443,0.3359303818851376,0.234931461549684,0.2185105436064756,0.3836208813775061,0.255430600290693,0.3991766831105948,0.1873220233715019,0.2905759860794116,0.3636636301971131,0.2720666504252247,0.1596798610655336,0.2755915950773311,0.1830307523791412,0.3649543206763802],[0.0371234059465991,0.03242347415555417,0.01988857958867888,0.03631916674726406,0.01525793056447875,0.07135857193615111,0.05850109312007883,0.0330170205426,0.01553375842430409,0.06429066383512382,0.02463758734428551,0.01871587454424042,0.01805418174713717,0.03025781637891173,0.03196526524002068,0.1787182752269798,0.01959986641736065,0.02763011114739326,0.02971955680387246,0.04254465702825964],[10.45666666666667,6.301578947368421,10.79727272727273,5.527142857142857,6.194166666666667,2.8375,5.86,6.265,12.76,4.013333333333334,8.561818181818182,13.30904761904762,2.92,8.897727272727273,7.612727272727272,2.204545454545455,4.792222222222223,6.7935,3.58375,5.828888888888889],[0.4441498752678317,0.4637086703778667,0.4469769590756064,0.555896646135866,0.2551650456937953,0.5434318108133503,0.4068008790692222,0.4314272231538535,0.5671939001428983,0.6326973503856025,0.4410062335674381,0.4509057936924641,0.5980112926718268,0.4076621012576387,0.6344647156330613,0.5480099853470315,0.8479658564847968,0.3681530667791175,0.7766759118958205,0.6195601138458109],[0.7176935425650939,0.6759418666886342,0.5912844468134186,0.6406218602628342,0.7608914348764781,0.9524117126142014,0.5025961273814956,0.8025189634953906,0.6875363082898668,0.6816922245429579,0.6434772867086079,0.6741368657991837,0.6136384769387926,0.6326776614466724,0.6352660702821236,0.6295496228498633,0.7023241150122855,0.5509132572485328,0.7832793790368987,0.5912461332246149],[1.91753487412749,1.124312726235241,1.266547465086961,0.9001426533263226,2.261636205418431,1.039677210085933,1.6964688060974,1.342729773708559,1.125109185048073,1.005414520892331,0.745538622186577,0.6972474697559381,1.425927736117199,0.9216188402082974,1.1351500186845,1.403601622773818,1.484009375370477,1.031327676466461,1.436677037589144,0.9152335515764092],["A","A","P","P","P","P","P","A","P","A","P","P","A","P","P","P","P","P","P","P"],["A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A"],["A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A"],["A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A"],["A","P","A","P","A","A","P","A","P","P","P","P","P","P","P","P","A","P","P","P"],["P","A","A","P","P","P","A","P","P","A","A","P","A","P","A","A","P","A","A","A"],["P","A","A","P","P","A","A","P","A","A","A","A","A","A","A","A","P","A","A","A"],["A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A"],["P","P","P","P","A","P","A","A","P","A","P","P","P","P","P","A","P","P","A","A"],["C","C","C","C","A","C","C","A","C","C","C","C","B","C","C","C","A","C","C","C"],["1","2","2","2","1","2","1","1","1","3","2","2","3","2","2","2","3","3","1","2"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eye_size<\/th>\n      <th>orbital_length<\/th>\n      <th>oral_gap_surface<\/th>\n      <th>oral_gape_shape<\/th>\n      <th>oral_gape_position<\/th>\n      <th>lower_jaw_length<\/th>\n      <th>operculum_width<\/th>\n      <th>head_length<\/th>\n      <th>body_depth<\/th>\n      <th>pectoral_fin_position<\/th>\n      <th>pectoral_fin_insertion<\/th>\n      <th>transversal_shape<\/th>\n      <th>caudal_peduncle_min_depth<\/th>\n      <th>dorsal_fin_insertion<\/th>\n      <th>eye_position<\/th>\n      <th>operculum_volume<\/th>\n      <th>photophores_ventral_position<\/th>\n      <th>gland_head<\/th>\n      <th>chin_barbel<\/th>\n      <th>front_barbel<\/th>\n      <th>small_teeth<\/th>\n      <th>large_teeth<\/th>\n      <th>fang_teeth<\/th>\n      <th>retractile_teeth<\/th>\n      <th>internal_teeth<\/th>\n      <th>gill_raker_types<\/th>\n      <th>oral_gape_axis<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## Species * assemblages matrix

::: {.cell}

```{.r .cell-code}
# species x assemblages (depth) matrix

# list of species 
sp_names <- c(rownames(fish_traits), "Nannobrachium_atrum", "Cyclothone")

# species biomass x depth  matrix 2002-2019
data_biomass_2002_2019 <- utils::read.csv(here::here("data", "data_biomass_2002_2019.csv"), sep = ";", header = T, dec = ".")%>%
  replace(is.na(.), 0)%>%
  as.data.frame()%>%
  rename("species"="Code_Station")%>%
  mutate(species= gsub(" ","_", species))%>%
  filter(species%in%sp_names)%>%
  t()%>%
  as.data.frame()%>%
  janitor::row_to_names(row_number = 1)%>%
  mutate_if(is.character, as.numeric)%>%
  tibble::rownames_to_column("Code_Station")%>%
  tidyr::pivot_longer(!Code_Station, names_to = "species", values_to = "Tot_V_HV")%>%
  rename("Nom_Scientifique"="species")

# species biomass x depth  matrix 2021
data_biomass_2021 <- utils::read.csv(here::here("data", "data_evhoe_catch_2021.csv"), sep = ";", header = T, dec = ".")%>%
  select(Nom_Scientifique, Tot_V_HV, Code_Station)%>%
  distinct()%>%
  mutate(Nom_Scientifique= gsub(" ","_",Nom_Scientifique))%>%
  filter(Nom_Scientifique%in%sp_names)

# species biomass x depth  matrix 2022
data_biomass_2022 <- utils::read.csv(here::here("data", "data_evhoe_catch_2022.csv"), sep = ";", header = T, dec = ".")%>%
  select(Nom_Scientifique, Tot_V_HV, Code_Station)%>%
  distinct()%>%
  mutate(Nom_Scientifique= gsub(" ","_",Nom_Scientifique))%>%
  filter(Nom_Scientifique%in%sp_names)

# merge all matrix 
# depht of all stations 
depth_station <- utils::read.csv(here::here("data", "depth.csv"), sep = ";", header = T, dec = ".")%>%
  rename("Code_Station"="Station")

depth_fish_biomass <- rbind(data_biomass_2002_2019, data_biomass_2021, data_biomass_2022)%>%
  as.data.frame()%>%
  left_join(depth_station)%>%
  filter(Code_Station != "H0472")%>%
  rename("species"="Nom_Scientifique")%>%  
  # add column with depth layer
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"))%>%
  replace(is.na(.), 0)%>%
  select(-depth)%>%
  group_by(species, depth_layer)%>%
  mutate(biomass=sum(Tot_V_HV))%>%
  select(-c(Tot_V_HV, Code_Station))%>%
  distinct()%>%
  tidyr::pivot_wider(names_from = species, values_from = biomass)%>%
  replace(is.na(.), 0)%>%
  tibble::column_to_rownames(var = "depth_layer")%>% 
  mutate(across(where(is.numeric), round, 2))%>%
  #change species name
  rename("Lampanyctus_ater"="Nannobrachium_atrum")%>%
  rename("Cyclothone_sp"="Cyclothone")%>%
  as.matrix()
```
:::


- __assemblages__ = depth layers 
- __biomass data__ = all EVHOE data 2002-2022

::: {.cell}

```{.r .cell-code}
htmltools::tagList(DT::datatable(depth_fish_biomass))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-703e42b8747a8a03b523" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-703e42b8747a8a03b523">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[0,0.1,0,0],[0,2.71,0,0],[0,0.06,0,0.16],[0.02,0.57,0,0.04],[0.04,0.97,0,0.24],[0.11,194.62,0.02,1.44],[0.02,0.05,0,0.37],[0,0.14,0,0.28],[0,0.21,0,0],[0,2.23,0,0],[0.01,0.03,0.06,0.11],[0.14,0.49,0.03,0.09],[0.01,1.38,0,0.05],[0.18,0.5,0.5600000000000001,2.4],[0.28,0.27,0.01,0.16],[0.07000000000000001,1.69,0.01,0.5600000000000001],[0,0.06,0,0],[0.04,0.04,0,0.14],[0,0.08,0.43,0.05]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Anoplogaster_cornuta<\/th>\n      <th>Bathylagus_euryops<\/th>\n      <th>Bolinichthys_supralateralis<\/th>\n      <th>Borostomias_antarcticus<\/th>\n      <th>Chauliodus_sloani<\/th>\n      <th>Cyclothone_sp<\/th>\n      <th>Evermannella_balbo<\/th>\n      <th>Gonostoma_elongatum<\/th>\n      <th>Holtbyrnia_anomala<\/th>\n      <th>Lampanyctus_macdonaldi<\/th>\n      <th>Lestidiops_sphyrenoides<\/th>\n      <th>Lobianchia_gemellarii<\/th>\n      <th>Maulisia_mauli<\/th>\n      <th>Maurolicus_muelleri<\/th>\n      <th>Melanostomias_bartonbeani<\/th>\n      <th>Lampanyctus_ater<\/th>\n      <th>Photostylus_pycnopterus<\/th>\n      <th>Sagamichthys_schnakenbecki<\/th>\n      <th>Notoscopelus_bolini<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::



## Traits types

The **first column** contains **traits name**. The **second column** contains
**traits type** following this code:

* **N**: nominal trait (factor variable)
* **O**: ordinal traits (ordered variable)
* **Q**: quantitative traits (numeric values)


::: {.cell}

```{.r .cell-code}
fish_traits_cat <- utils::read.csv(here::here("data", "fish_traits_cat.csv"), sep = ";", header = T, dec = ".")
htmltools::tagList(DT::datatable(fish_traits_cat))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-1638ee134afe9a59be04" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1638ee134afe9a59be04">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"],["eye_size","orbital_length","oral_gap_surface","oral_gape_shape","oral_gape_position","lower_jaw_length","operculum_width","head_length","body_depth","pectoral_fin_position","pectoral_fin_insertion","transversal_shape","caudal_peduncle_min_depth","dorsal_fin_insertion","eye_position","operculum_volume","photophores_ventral_position","gland_head","chin_barbel","front_barbel","small_teeth","large_teeth","fang_teeth","retractile_teeth","internal_teeth","gill_raker_types","oral_gape_axis"],["Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","N","N","N","N","N","N","N","N","N","O","O"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>trait_name<\/th>\n      <th>trait_type<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


# 2. Build a functional space using the mFD package

## 2.1 Compute data summaries  


::: {.cell}

```{.r .cell-code}
## Summary of the assemblages * species data.frame ----
asb_sp_fish_summ <- mFD::asb.sp.summary(asb_sp_w = depth_fish_biomass)
asb_sp_fish_occ  <- asb_sp_fish_summ$"asb_sp_occ"

htmltools::tagList(DT::datatable(asb_sp_fish_occ))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-d63d226bef40672a6e32" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-d63d226bef40672a6e32">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[0,1,0,0],[0,1,0,0],[0,1,0,1],[1,1,0,1],[1,1,0,1],[1,1,1,1],[1,1,0,1],[0,1,0,1],[0,1,0,0],[0,1,0,0],[1,1,1,1],[1,1,1,1],[1,1,0,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[0,1,0,0],[1,1,0,1],[0,1,1,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Anoplogaster_cornuta<\/th>\n      <th>Bathylagus_euryops<\/th>\n      <th>Bolinichthys_supralateralis<\/th>\n      <th>Borostomias_antarcticus<\/th>\n      <th>Chauliodus_sloani<\/th>\n      <th>Cyclothone_sp<\/th>\n      <th>Evermannella_balbo<\/th>\n      <th>Gonostoma_elongatum<\/th>\n      <th>Holtbyrnia_anomala<\/th>\n      <th>Lampanyctus_macdonaldi<\/th>\n      <th>Lestidiops_sphyrenoides<\/th>\n      <th>Lobianchia_gemellarii<\/th>\n      <th>Maulisia_mauli<\/th>\n      <th>Maurolicus_muelleri<\/th>\n      <th>Melanostomias_bartonbeani<\/th>\n      <th>Lampanyctus_ater<\/th>\n      <th>Photostylus_pycnopterus<\/th>\n      <th>Sagamichthys_schnakenbecki<\/th>\n      <th>Notoscopelus_bolini<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## 2.2 Computing distances between species based on functional traits
- We have non-continuous traits so we use the __Gower distance__ _(metric = "gower")_ as this method allows traits weighting.

::: {.cell}

```{.r .cell-code}
sp_dist_fish <- mFD::funct.dist(
  sp_tr         = fish_traits,
  tr_cat        = fish_traits_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

## Output of the function mFD::funct.dist() ----
round(sp_dist_fish, 3)
```

::: {.cell-output .cell-output-stdout}
```
                            Anoplogaster_cornuta Bathylagus_euryops
Bathylagus_euryops                         0.409                   
Bolinichthys_supralateralis                0.338              0.204
Borostomias_antarcticus                    0.356              0.283
Chauliodus_sloani                          0.456              0.499
Cyclothone_sp                              0.424              0.317
Diaphus_metoclampus                        0.385              0.223
Evermannella_balbo                         0.351              0.342
Gonostoma_elongatum                        0.308              0.308
Holtbyrnia_anomala                         0.414              0.189
Lampanyctus_ater                           0.386              0.171
Lampanyctus_macdonaldi                     0.326              0.258
Lestidiops_sphyrenoides                    0.504              0.205
Lobianchia_gemellarii                      0.313              0.216
Maulisia_mauli                             0.351              0.163
Maurolicus_muelleri                        0.500              0.217
Melanostomias_bartonbeani                  0.471              0.476
Notoscopelus_bolini                        0.416              0.163
Photostylus_pycnopterus                    0.495              0.278
Sagamichthys_schnakenbecki                 0.438              0.170
                            Bolinichthys_supralateralis Borostomias_antarcticus
Bathylagus_euryops                                                             
Bolinichthys_supralateralis                                                    
Borostomias_antarcticus                           0.328                        
Chauliodus_sloani                                 0.479                   0.401
Cyclothone_sp                                     0.314                   0.258
Diaphus_metoclampus                               0.217                   0.305
Evermannella_balbo                                0.404                   0.337
Gonostoma_elongatum                               0.328                   0.212
Holtbyrnia_anomala                                0.261                   0.352
Lampanyctus_ater                                  0.171                   0.188
Lampanyctus_macdonaldi                            0.241                   0.187
Lestidiops_sphyrenoides                           0.324                   0.359
Lobianchia_gemellarii                             0.187                   0.161
Maulisia_mauli                                    0.151                   0.217
Maurolicus_muelleri                               0.247                   0.317
Melanostomias_bartonbeani                         0.463                   0.255
Notoscopelus_bolini                               0.170                   0.233
Photostylus_pycnopterus                           0.316                   0.335
Sagamichthys_schnakenbecki                        0.176                   0.269
                            Chauliodus_sloani Cyclothone_sp Diaphus_metoclampus
Bathylagus_euryops                                                             
Bolinichthys_supralateralis                                                    
Borostomias_antarcticus                                                        
Chauliodus_sloani                                                              
Cyclothone_sp                           0.332                                  
Diaphus_metoclampus                     0.417         0.345                    
Evermannella_balbo                      0.257         0.335               0.347
Gonostoma_elongatum                     0.424         0.268               0.307
Holtbyrnia_anomala                      0.540         0.380               0.209
Lampanyctus_ater                        0.428         0.257               0.217
Lampanyctus_macdonaldi                  0.454         0.260               0.273
Lestidiops_sphyrenoides                 0.416         0.310               0.347
Lobianchia_gemellarii                   0.413         0.234               0.208
Maulisia_mauli                          0.509         0.301               0.176
Maurolicus_muelleri                     0.460         0.300               0.154
Melanostomias_bartonbeani               0.238         0.343               0.518
Notoscopelus_bolini                     0.449         0.272               0.177
Photostylus_pycnopterus                 0.343         0.306               0.216
Sagamichthys_schnakenbecki              0.458         0.328               0.121
                            Evermannella_balbo Gonostoma_elongatum
Bathylagus_euryops                                                
Bolinichthys_supralateralis                                       
Borostomias_antarcticus                                           
Chauliodus_sloani                                                 
Cyclothone_sp                                                     
Diaphus_metoclampus                                               
Evermannella_balbo                                                
Gonostoma_elongatum                      0.396                    
Holtbyrnia_anomala                       0.377               0.393
Lampanyctus_ater                         0.371               0.210
Lampanyctus_macdonaldi                   0.376               0.158
Lestidiops_sphyrenoides                  0.390               0.366
Lobianchia_gemellarii                    0.336               0.191
Maulisia_mauli                           0.417               0.247
Maurolicus_muelleri                      0.364               0.355
Melanostomias_bartonbeani                0.355               0.382
Notoscopelus_bolini                      0.376               0.270
Photostylus_pycnopterus                  0.359               0.293
Sagamichthys_schnakenbecki               0.367               0.312
                            Holtbyrnia_anomala Lampanyctus_ater
Bathylagus_euryops                                             
Bolinichthys_supralateralis                                    
Borostomias_antarcticus                                        
Chauliodus_sloani                                              
Cyclothone_sp                                                  
Diaphus_metoclampus                                            
Evermannella_balbo                                             
Gonostoma_elongatum                                            
Holtbyrnia_anomala                                             
Lampanyctus_ater                         0.248                 
Lampanyctus_macdonaldi                   0.300            0.124
Lestidiops_sphyrenoides                  0.270            0.238
Lobianchia_gemellarii                    0.261            0.096
Maulisia_mauli                           0.180            0.120
Maurolicus_muelleri                      0.193            0.205
Melanostomias_bartonbeani                0.527            0.394
Notoscopelus_bolini                      0.197            0.089
Photostylus_pycnopterus                  0.301            0.241
Sagamichthys_schnakenbecki               0.134            0.166
                            Lampanyctus_macdonaldi Lestidiops_sphyrenoides
Bathylagus_euryops                                                        
Bolinichthys_supralateralis                                               
Borostomias_antarcticus                                                   
Chauliodus_sloani                                                         
Cyclothone_sp                                                             
Diaphus_metoclampus                                                       
Evermannella_balbo                                                        
Gonostoma_elongatum                                                       
Holtbyrnia_anomala                                                        
Lampanyctus_ater                                                          
Lampanyctus_macdonaldi                                                    
Lestidiops_sphyrenoides                      0.356                        
Lobianchia_gemellarii                        0.096                   0.307
Maulisia_mauli                               0.155                   0.299
Maurolicus_muelleri                          0.285                   0.284
Melanostomias_bartonbeani                    0.394                   0.385
Notoscopelus_bolini                          0.191                   0.211
Photostylus_pycnopterus                      0.343                   0.238
Sagamichthys_schnakenbecki                   0.226                   0.298
                            Lobianchia_gemellarii Maulisia_mauli
Bathylagus_euryops                                              
Bolinichthys_supralateralis                                     
Borostomias_antarcticus                                         
Chauliodus_sloani                                               
Cyclothone_sp                                                   
Diaphus_metoclampus                                             
Evermannella_balbo                                              
Gonostoma_elongatum                                             
Holtbyrnia_anomala                                              
Lampanyctus_ater                                                
Lampanyctus_macdonaldi                                          
Lestidiops_sphyrenoides                                         
Lobianchia_gemellarii                                           
Maulisia_mauli                              0.131               
Maurolicus_muelleri                         0.226          0.193
Melanostomias_bartonbeani                   0.387          0.435
Notoscopelus_bolini                         0.117          0.132
Photostylus_pycnopterus                     0.294          0.293
Sagamichthys_schnakenbecki                  0.189          0.101
                            Maurolicus_muelleri Melanostomias_bartonbeani
Bathylagus_euryops                                                       
Bolinichthys_supralateralis                                              
Borostomias_antarcticus                                                  
Chauliodus_sloani                                                        
Cyclothone_sp                                                            
Diaphus_metoclampus                                                      
Evermannella_balbo                                                       
Gonostoma_elongatum                                                      
Holtbyrnia_anomala                                                       
Lampanyctus_ater                                                         
Lampanyctus_macdonaldi                                                   
Lestidiops_sphyrenoides                                                  
Lobianchia_gemellarii                                                    
Maulisia_mauli                                                           
Maurolicus_muelleri                                                      
Melanostomias_bartonbeani                 0.488                          
Notoscopelus_bolini                       0.191                     0.413
Photostylus_pycnopterus                   0.223                     0.408
Sagamichthys_schnakenbecki                0.134                     0.477
                            Notoscopelus_bolini Photostylus_pycnopterus
Bathylagus_euryops                                                     
Bolinichthys_supralateralis                                            
Borostomias_antarcticus                                                
Chauliodus_sloani                                                      
Cyclothone_sp                                                          
Diaphus_metoclampus                                                    
Evermannella_balbo                                                     
Gonostoma_elongatum                                                    
Holtbyrnia_anomala                                                     
Lampanyctus_ater                                                       
Lampanyctus_macdonaldi                                                 
Lestidiops_sphyrenoides                                                
Lobianchia_gemellarii                                                  
Maulisia_mauli                                                         
Maurolicus_muelleri                                                    
Melanostomias_bartonbeani                                              
Notoscopelus_bolini                                                    
Photostylus_pycnopterus                   0.262                        
Sagamichthys_schnakenbecki                0.157                   0.237
```
:::
:::


## 2.3 Building functional spaces and chosing the best one
### 2.3.1 Computing several multimensional functional spaces and assessing their quality

-mFD evaluates the quality of PCoA-based multidimensional spaces according to the deviation between trait-based distances and distances in the functional space (extension of Maire et al. (2015) framework). 


::: {.cell}

```{.r .cell-code}
fspaces_quality_fish <- mFD::quality.fspaces(
  sp_dist             = sp_dist_fish,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

## Quality metrics of functional spaces ----
round(fspaces_quality_fish$"quality_fspaces", 3)
```

::: {.cell-output .cell-output-stdout}
```
               mad
pcoa_1d      0.141
pcoa_2d      0.079
pcoa_3d      0.048
pcoa_4d      0.031
pcoa_5d      0.022
pcoa_6d      0.014
pcoa_7d      0.011
pcoa_8d      0.012
pcoa_9d      0.013
pcoa_10d     0.015
tree_average 0.041
```
:::
:::

The space with the best quality has the lowest quality metric. 5-D space good ?

### 2.3.2 Illustrating the quality of the functional spaces


::: {.cell}

```{.r .cell-code}
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_fish,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")
```

::: {.cell-output-display}
![](index_files/figure-html/fspaces_quality_plot-1.png){width=1344}
:::
:::


This function generates a figure with three panels (in rows) for each selected functional space (in columns). Each column represents a functional space, the value of the quality metric is written on the top of each column. The x-axis of all panels represents trait-based distances. The y-axis is different for each row:

+ on the first (top) row, the y-axis represents species functional distances in the multidimensional space. Thus, the closer species are to the 1:1 line, the better distances in the functional space fit trait-based ones.
+ on the second row, the y-axis shows the raw deviation of species distances in the functional space compared to trait-based distances. Thus, the raw deviation reflects the distance to the horizontal line.
+ on the third row (bottom), the y-axis shows the absolute or squared deviation of the (“scaled”) distance in the functional space. It is the deviation that is taken into account for computing the quality metric.

### 2.3.3 Testing the correlation between functional axes and traits

::: {.cell}

```{.r .cell-code}
sp_faxes_coord_fish <- fspaces_quality_fish$"details_fspaces"$"sp_pc_coord"

# fish_tr_faxes <- mFD::traits.faxes.cor(
#   sp_tr          = fish_traits, 
#   sp_faxes_coord = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")])
```
:::


## 2.4 Plotting the selected functional space and position of species

::: {.cell}

```{.r .cell-code}
sp_faxes_coord_fish <- fspaces_quality_fish$"details_fspaces"$"sp_pc_coord"

big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  plot_ch         = TRUE,
  plot_vertices   = TRUE,
  plot_sp_nm      = NULL,
  check_input     = TRUE)

big_plot$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_functional_space-1.png){width=672}
:::
:::


# 3. Computing and plotting beta FD indices

- The function returns a list containing:
+ a dist object with beta indices values for each pair of assemblages:

::: {.cell}

```{.r .cell-code}
beta_fd_indices_fish <- mFD::beta.fd.multidim(
      sp_faxes_coord   = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")],
      asb_sp_occ       = asb_sp_fish_occ,
      check_input      = TRUE,
      beta_family      = c("Jaccard"),
      details_returned = TRUE)
```

::: {.cell-output .cell-output-stdout}
```
Serial computing of convex hulls shaping assemblages with conv1

  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |==================                                                    |  25%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |====================================================                  |  75%
  |                                                                            
  |======================================================================| 100%
Serial computing of intersections between pairs of assemblages with inter_geom_coord

  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |============                                                          |  17%
  |                                                                            
  |=======================                                               |  33%
  |                                                                            
  |===================================                                   |  50%
  |                                                                            
  |===============================================                       |  67%
  |                                                                            
  |==========================================================            |  83%
  |                                                                            
  |======================================================================| 100%
```
:::

```{.r .cell-code}
head(beta_fd_indices_fish$"pairasb_fbd_indices", 10)
```

::: {.cell-output .cell-output-stdout}
```
$jac_diss
                  Upper mesopelagic Bathypelagic Epipelagic
Bathypelagic              0.6482290                        
Epipelagic                0.9295111    0.9750274           
Lower mesopelagic         0.2136774    0.5526378  0.9441781

$jac_turn
                  Upper mesopelagic Bathypelagic   Epipelagic
Bathypelagic           0.000000e+00                          
Epipelagic             1.313043e-02 8.954645e-16             
Lower mesopelagic      6.780801e-16 0.000000e+00 2.387905e-15

$jac_nest
                  Upper mesopelagic Bathypelagic Epipelagic
Bathypelagic              0.6482290                        
Epipelagic                0.9163807    0.9750274           
Lower mesopelagic         0.2136774    0.5526378  0.9441781
```
:::
:::

+ a vector containing the FRic value for each assemblage retrieved through the details_beta list:


::: {.cell}

```{.r .cell-code}
beta_fd_indices_fish$"details"$"asb_FRic"
```

::: {.cell-output .cell-output-stdout}
```
Upper mesopelagic      Bathypelagic        Epipelagic Lower mesopelagic 
       0.35177104        1.00000000        0.02497261        0.44736223 
```
:::
:::

+ a list of vectors containing names of species being vertices of the convex hull for each assemblage retrieved through the details_beta list:

::: {.cell}

```{.r .cell-code}
beta_fd_indices_fish$"details"$"asb_vertices"
```

::: {.cell-output .cell-output-stdout}
```
$`Upper mesopelagic`
 [1] "Lobianchia_gemellarii"      "Maurolicus_muelleri"       
 [3] "Melanostomias_bartonbeani"  "Lampanyctus_ater"          
 [5] "Maulisia_mauli"             "Evermannella_balbo"        
 [7] "Cyclothone_sp"              "Chauliodus_sloani"         
 [9] "Borostomias_antarcticus"    "Lestidiops_sphyrenoides"   
[11] "Sagamichthys_schnakenbecki"

$Bathypelagic
 [1] "Gonostoma_elongatum"         "Evermannella_balbo"         
 [3] "Maurolicus_muelleri"         "Lestidiops_sphyrenoides"    
 [5] "Photostylus_pycnopterus"     "Borostomias_antarcticus"    
 [7] "Sagamichthys_schnakenbecki"  "Lampanyctus_ater"           
 [9] "Cyclothone_sp"               "Bathylagus_euryops"         
[11] "Maulisia_mauli"              "Bolinichthys_supralateralis"
[13] "Notoscopelus_bolini"         "Melanostomias_bartonbeani"  
[15] "Lampanyctus_macdonaldi"      "Anoplogaster_cornuta"       
[17] "Chauliodus_sloani"           "Holtbyrnia_anomala"         

$Epipelagic
[1] "Lestidiops_sphyrenoides"   "Cyclothone_sp"            
[3] "Notoscopelus_bolini"       "Lobianchia_gemellarii"    
[5] "Lampanyctus_ater"          "Melanostomias_bartonbeani"
[7] "Maurolicus_muelleri"      

$`Lower mesopelagic`
 [1] "Evermannella_balbo"          "Melanostomias_bartonbeani"  
 [3] "Maurolicus_muelleri"         "Maulisia_mauli"             
 [5] "Cyclothone_sp"               "Bolinichthys_supralateralis"
 [7] "Lampanyctus_ater"            "Borostomias_antarcticus"    
 [9] "Notoscopelus_bolini"         "Gonostoma_elongatum"        
[11] "Chauliodus_sloani"           "Lestidiops_sphyrenoides"    
[13] "Sagamichthys_schnakenbecki" 
```
:::
:::


+ overlap between convex hulls shaping each of the two species assemblages.

::: {.cell}

```{.r .cell-code}
beta_plot_fish <- mFD::beta.multidim.plot(
  output_beta_fd_multidim = beta_fd_indices_fish,
  plot_asb_nm             = c("Epipelagic", "Upper mesopelagic"),
  beta_family             = c("Jaccard"),
  plot_sp_nm              = c("Maurolicus_muelleri", "Notoscopelus_bolini", "Lampanyctus_ater"),
  faxes                   = paste0("PC", 1:4),
  name_file               = NULL,
  faxes_nm                = NULL,
  range_faxes             = c(NA, NA),
  check_input             = TRUE)

beta_plot_fish$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_beta_FD_epi_up-1.png){width=1152}
:::
:::

::: {.cell}

```{.r .cell-code}
beta_plot_fish <- mFD::beta.multidim.plot(
  output_beta_fd_multidim = beta_fd_indices_fish,
  plot_asb_nm             = c("Upper mesopelagic", "Lower mesopelagic"),
  beta_family             = c("Jaccard"),
  plot_sp_nm              = c("Maulisia_mauli", "Evermannella_balbo", "Borostomias_antarcticus"),
  faxes                   = paste0("PC", 1:4),
  name_file               = NULL,
  faxes_nm                = NULL,
  range_faxes             = c(NA, NA),
  check_input             = TRUE)

beta_plot_fish$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_beta_FD_up_lw-1.png){width=1152}
:::
:::

::: {.cell}

```{.r .cell-code}
beta_plot_fish <- mFD::beta.multidim.plot(
  output_beta_fd_multidim = beta_fd_indices_fish,
  plot_asb_nm             = c("Lower mesopelagic", "Bathypelagic"),
  beta_family             = c("Jaccard"),
  plot_sp_nm              = c("Anoplogaster_cornuta", "Holtbyrnia_anomala", "Photostylus_pycnopterus"),
  faxes                   = paste0("PC", 1:4),
  name_file               = NULL,
  faxes_nm                = NULL,
  range_faxes             = c(NA, NA),
  check_input             = TRUE)

beta_plot_fish$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_beta_FD_lw_b-1.png){width=1152}
:::
:::


# 4. Functional rarity

## 4.1 Different indices of functional rarity

<span style="color:#2596be;">__Functional originality indices__</span>: 

+ __Functional distinctiveness__ is the mean of dissimilarity of the focal species to all the other species of the set of interest. It can be abundance-weighted if needed.

+ __Functional uniqueness__ is the smallest dissimilarity that exists between the focal species and the all other species in the set. It does not consider the abundance of any species.

<span style="color:#2596be;">__Rarity indices__</span>: 

+ __Scarcity__ is proportional to the relative abundance of the species. It gets close to one when the species is (relatively) rare and close to 0 when its dominant

+ __Restrictedness__ is 1 minus the ratio of sites a species occupy over the total number of sites.

## 4.2.Computing functional rarity
### 4.2.1 Functional originality at regional scale

 - For the choice or dissimilarity matrix we can use the raw dissimilarity matrix computed directly on raw traits values among species: 

::: {.cell}

```{.r .cell-code}
sp_di <- funrar::distinctiveness_global(sp_dist_fish, di_name = "distinctiveness")

htmltools::tagList(DT::datatable(sp_di))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-e62fe6a02ac467b3ed96" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-e62fe6a02ac467b3ed96">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],["Anoplogaster_cornuta","Bathylagus_euryops","Bolinichthys_supralateralis","Borostomias_antarcticus","Chauliodus_sloani","Cyclothone_sp","Diaphus_metoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Lampanyctus_ater","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Maulisia_mauli","Maurolicus_muelleri","Melanostomias_bartonbeani","Notoscopelus_bolini","Photostylus_pycnopterus","Sagamichthys_schnakenbecki"],[0.4022049984770705,0.2679220083116797,0.2799673432246437,0.2816544525973352,0.4196632661357991,0.3096739887363402,0.2717385635802042,0.3608751082379827,0.3014436407141283,0.3013744181206165,0.2273206217669616,0.263227282239539,0.3213788496613441,0.2295482377050696,0.2405508071894689,0.2807828576555086,0.4110371900377688,0.2360484227225557,0.3042008095593997,0.2504720848163838]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>distinctiveness<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


- Another option would be to compute a new functional dissimilarity matrix based on the selected functional axes. One advantage of the latter is that it already takes into account the correlation between traits (recompute regional functional distinctiveness based on the n selected functional axes. Because the space comes from a PCA, we can directly use euclidean distance): 

::: {.cell}

```{.r .cell-code}
new_dissim <- dist(sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")])

sp_di_alt <- funrar::distinctiveness_global(new_dissim, di_name = "alt_di")

#We can now compare both distinctiveness values.

sp_all_di <- merge(sp_di, sp_di_alt, by = "species")

plot(sp_all_di$distinctiveness, sp_all_di$alt_di)
```

::: {.cell-output-display}
![](index_files/figure-html/distinctiveness_n_axis-1.png){width=672}
:::

```{.r .cell-code}
cor.test(sp_all_di$distinctiveness, sp_all_di$alt_di)
```

::: {.cell-output .cell-output-stdout}
```

	Pearson's product-moment correlation

data:  sp_all_di$distinctiveness and sp_all_di$alt_di
t = 16.068, df = 18, p-value = 4.058e-12
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9164514 0.9870620
sample estimates:
      cor 
0.9668623 
```
:::
:::

Both seems very correlated, so in our case using either one should be fine. However, it can be better to use dissimilarity based on a reduced number of well-defined axes because: (1) there are more interpretable thanks to the multivariate analysis, (2) the first one contain of the most information, (3) they explicitly take into account potentially strong correlations between provided traits. We’ll stick here with raw dissimilarity for the sake of simplicity

To compute uniqueness at regional scale we also need the regional level functional dissimilarity matrix with the uniqueness() function, and the site-species matrix:

::: {.cell}

```{.r .cell-code}
sp_ui <- funrar::uniqueness(
  pres_matrix = depth_fish_biomass,
  as.matrix(sp_dist_fish)
)

quantile(sp_ui$Ui, probs = seq(0, 1, by = 0.1))
```

::: {.cell-output .cell-output-stdout}
```
        0%        10%        20%        30%        40%        50%        60% 
0.08918683 0.09435914 0.09859580 0.11390689 0.13748593 0.15822912 0.16227449 
       70%        80%        90%       100% 
0.21551717 0.23547784 0.24215133 0.30799449 
```
:::

```{.r .cell-code}
htmltools::tagList(DT::datatable(sp_ui))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-dfde0c9c1eef5bb74873" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-dfde0c9c1eef5bb74873">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19"],["Anoplogaster_cornuta","Bathylagus_euryops","Bolinichthys_supralateralis","Borostomias_antarcticus","Chauliodus_sloani","Cyclothone_sp","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Maulisia_mauli","Maurolicus_muelleri","Melanostomias_bartonbeani","Lampanyctus_ater","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Notoscopelus_bolini"],[0.3079944943291218,0.1625975312247552,0.1511398234166086,0.1609823011002499,0.2383873849818951,0.2335381350466273,0.2572070949254052,0.1582291231958843,0.1340724624335926,0.09573220402479143,0.2050247429360177,0.09565221629980204,0.1005048583664764,0.134009946682393,0.2383873849818951,0.08918683386983867,0.2225121171160887,0.1005048583664764,0.08918683386983867]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>Ui<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::

Based on these results we see that _Anoplogaster cornuta_, _Evermannella balbo_, and _Chauliodus sloani_ are the most isolated fish in the functional space. Meaning that they have the most distant nearest neighbors.

### 4.2.2 Functional originality at local scale


::: {.cell}

```{.r .cell-code}
sp_local_di <- funrar::distinctiveness(
  depth_fish_biomass, as.matrix(sp_dist_fish)
)
sp_local_di[1:4, 1:10]
```

::: {.cell-output .cell-output-stdout}
```
                  Anoplogaster_cornuta Bathylagus_euryops
Upper mesopelagic                   NA                 NA
Bathypelagic                 0.4222418          0.3144837
Epipelagic                          NA                 NA
Lower mesopelagic                   NA                 NA
                  Bolinichthys_supralateralis Borostomias_antarcticus
Upper mesopelagic                          NA               0.2575080
Bathypelagic                        0.3095523               0.2576447
Epipelagic                                 NA                      NA
Lower mesopelagic                   0.2830010               0.2850685
                  Chauliodus_sloani Cyclothone_sp Evermannella_balbo
Upper mesopelagic         0.3573761     0.3026306          0.3494591
Bathypelagic              0.3382178     0.2912187          0.3361591
Epipelagic                       NA     0.2878235                 NA
Lower mesopelagic         0.4036351     0.2985873          0.3558329
                  Gonostoma_elongatum Holtbyrnia_anomala Lampanyctus_macdonaldi
Upper mesopelagic                  NA                 NA                     NA
Bathypelagic                0.2678535          0.3744546              0.2583958
Epipelagic                         NA                 NA                     NA
Lower mesopelagic           0.3187773                 NA                     NA
```
:::

```{.r .cell-code}
identical(dim(sp_local_di), dim(depth_fish_biomass))
```

::: {.cell-output .cell-output-stdout}
```
[1] TRUE
```
:::
:::


To compute uniqueness at the site scale, we must use a more complex expression as it was not envisioned for local computation:

::: {.cell}

```{.r .cell-code}
depth_ui <- apply(
  depth_fish_biomass, 1,
  function(single_site, dist_m) {
    single_site = single_site[single_site > 0 & !is.na(single_site)]
    funrar::uniqueness(t(as.matrix(single_site)), dist_m)
  }, dist_m = as.matrix(sp_dist_fish)
)

head(depth_ui[1])
```

::: {.cell-output .cell-output-stdout}
```
$`Upper mesopelagic`
                      species         Ui
1     Borostomias_antarcticus 0.16098230
2           Chauliodus_sloani 0.23838738
3               Cyclothone_sp 0.23353814
4          Evermannella_balbo 0.25720709
5     Lestidiops_sphyrenoides 0.23789144
6       Lobianchia_gemellarii 0.09565222
7              Maulisia_mauli 0.10050486
8         Maurolicus_muelleri 0.13400995
9   Melanostomias_bartonbeani 0.23838738
10           Lampanyctus_ater 0.09565222
11 Sagamichthys_schnakenbecki 0.10050486
```
:::
:::


As we had to manually build the function to compute the local uniqueness the results are strangely formatted.

We provide here a function that can help them to be more easily read:


::: {.cell}

```{.r .cell-code}
depth_ui <- lapply(names(depth_ui), function(x) {
  single_depth = depth_ui[[x]]
  single_depth$site = x
  
  return(single_depth)
})

depth_ui <- do.call(rbind, depth_ui)

#Then we can again look at the apple to see how its uniqueness varies across depths.

subset(depth_ui, species == "Melanostomias_bartonbeani")
```

::: {.cell-output .cell-output-stdout}
```
                     species        Ui              site
9  Melanostomias_bartonbeani 0.2383874 Upper mesopelagic
26 Melanostomias_bartonbeani 0.2383874      Bathypelagic
35 Melanostomias_bartonbeani 0.3429078        Epipelagic
48 Melanostomias_bartonbeani 0.2383874 Lower mesopelagic
```
:::
:::


4.3.3.Rarity indices

__scarcity__:

::: {.cell}

```{.r .cell-code}
rel_weights = funrar::make_relative(depth_fish_biomass)

si =  funrar::scarcity(rel_weights)
summary(si)
```

::: {.cell-output .cell-output-stdout}
```
 Anoplogaster_cornuta Bathylagus_euryops Bolinichthys_supralateralis
 Min.   :0.9936       Min.   :0.8411     Min.   :0.7750             
 1st Qu.:0.9936       1st Qu.:0.8411     1st Qu.:0.8303             
 Median :0.9936       Median :0.8411     Median :0.8856             
 Mean   :0.9936       Mean   :0.8411     Mean   :0.8856             
 3rd Qu.:0.9936       3rd Qu.:0.8411     3rd Qu.:0.9409             
 Max.   :0.9936       Max.   :0.8411     Max.   :0.9962             
 NA's   :3            NA's   :3          NA's   :2                  
 Borostomias_antarcticus Chauliodus_sloani Cyclothone_sp     
 Min.   :0.8473          Min.   :0.6822    Min.   :0.000004  
 1st Qu.:0.8928          1st Qu.:0.7000    1st Qu.:0.075606  
 Median :0.9383          Median :0.7178    Median :0.251335  
 Mean   :0.9166          Mean   :0.7800    Mean   :0.354920  
 3rd Qu.:0.9513          3rd Qu.:0.8289    3rd Qu.:0.530650  
 Max.   :0.9642          Max.   :0.9399    Max.   :0.917004  
 NA's   :1               NA's   :1                           
 Evermannella_balbo Gonostoma_elongatum Holtbyrnia_anomala
 Min.   :0.5546     Min.   :0.6401      Min.   :0.9867    
 1st Qu.:0.7009     1st Qu.:0.7278      1st Qu.:0.9867    
 Median :0.8473     Median :0.8156      Median :0.9867    
 Mean   :0.7995     Mean   :0.8156      Mean   :0.9867    
 3rd Qu.:0.9220     3rd Qu.:0.9033      3rd Qu.:0.9867    
 Max.   :0.9968     Max.   :0.9911      Max.   :0.9867    
 NA's   :1          NA's   :2           NA's   :3         
 Lampanyctus_macdonaldi Lestidiops_sphyrenoides Lobianchia_gemellarii
 Min.   :0.8672         Min.   :0.7711          Min.   :0.3134       
 1st Qu.:0.8672         1st Qu.:0.8222          1st Qu.:0.7281       
 Median :0.8672         Median :0.8798          Median :0.8723       
 Mean   :0.8672         Mean   :0.8822          Mean   :0.7568       
 3rd Qu.:0.8672         3rd Qu.:0.9399          3rd Qu.:0.9009       
 Max.   :0.8672         Max.   :0.9981          Max.   :0.9692       
 NA's   :3                                                           
 Maulisia_mauli   Maurolicus_muelleri Melanostomias_bartonbeani
 Min.   :0.9156   Min.   :0.02183     Min.   :0.09822          
 1st Qu.:0.9180   1st Qu.:0.07175     1st Qu.:0.60577          
 Median :0.9205   Median :0.15668     Median :0.86628          
 Mean   :0.9198   Mean   :0.32594     Mean   :0.70342          
 3rd Qu.:0.9219   3rd Qu.:0.41087     3rd Qu.:0.96393          
 Max.   :0.9234   Max.   :0.96857     Max.   :0.98290          
 NA's   :1                                                     
 Lampanyctus_ater Photostylus_pycnopterus Sagamichthys_schnakenbecki
 Min.   :0.4097   Min.   :0.9962          Min.   :0.7178            
 1st Qu.:0.5223   1st Qu.:0.9962          1st Qu.:0.7589            
 Median :0.7288   Median :0.9962          Median :0.8000            
 Mean   :0.7062   Mean   :0.9962          Mean   :0.8384            
 3rd Qu.:0.9127   3rd Qu.:0.9962          3rd Qu.:0.8987            
 Max.   :0.9576   Max.   :0.9962          Max.   :0.9974            
                  NA's   :3               NA's   :1                 
 Notoscopelus_bolini
 Min.   :0.1552     
 1st Qu.:0.5393     
 Median :0.9234     
 Mean   :0.6912     
 3rd Qu.:0.9592     
 Max.   :0.9949     
 NA's   :1          
```
:::
:::


__ restrictiveness__: 

::: {.cell}

```{.r .cell-code}
ri = funrar::restrictedness(depth_fish_biomass)
summary(ri)
```

::: {.cell-output .cell-output-stdout}
```
   species                Ri        
 Length:19          Min.   :0.0000  
 Class :character   1st Qu.:0.0000  
 Mode  :character   Median :0.2500  
                    Mean   :0.3289  
                    3rd Qu.:0.6250  
                    Max.   :0.7500  
```
:::
:::


## 4.3 Plotting functional rarity
### 4.3.1 Plotting functional originality

option to be able to colour species according to their functional originality (and not use the ready-made functions in the mfd package)

::: {.cell}

```{.r .cell-code}
library("ggplot2")

# Make a summary data.frame
sp_coord_di_ui <- as.data.frame(sp_faxes_coord_fish[, 1:2])
sp_coord_di_ui$species <- rownames(sp_coord_di_ui)
rownames(sp_coord_di_ui) <- NULL
sp_coord_di_ui <- sp_coord_di_ui[, c(3, 1, 2)]
sp_coord_di_ui <- merge(sp_coord_di_ui, sp_di, by = "species")
sp_coord_di_ui <- merge(sp_coord_di_ui, sp_ui, by = "species")


plot_reg_distinctiveness <- ggplot(sp_coord_di_ui, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(aes(color = distinctiveness), size=4) +
  ggrepel::geom_text_repel(aes(label = species), size=4) +
  scale_color_gradient(high = "#914568", low = "#6BA1B9", "Functional\nDistinctiveness")+
  theme_bw() +
  theme(aspect.ratio = 1)

plot_reg_uniqueness <- ggplot(sp_coord_di_ui, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(aes(color = Ui), size=4) +
  ggrepel::geom_text_repel(aes(label = species), size=4) +
  scale_color_gradient(high = "#914568", low = "#6BA1B9","Functional\nUniqueness") +
  theme_bw() +
  theme(aspect.ratio = 1)

patchwork::wrap_plots(plot_reg_distinctiveness, plot_reg_uniqueness)
```

::: {.cell-output-display}
![](index_files/figure-html/plot_si_ri-1.png){width=1248}
:::
:::


- As was done with mFD to correlate the functional axes with species’ traits we can correlate functional distinctiveness to specific traits in order to see which traits are mainly driving distinctiveness.

- Regarding local level functional originality indices, the visualization can be more difficult to grasp and depends highly on the question. Would you rather focus on visualizing the functional distinctiveness of one species across communities? Compare the distribution of functional distinctiveness values across communities?

- One idea to keep in mind is that averaging functional distinctiveness per community is exactly equal to computing functional dispersion. Functional originality is computed on a species basis, so we should be aware that if we are rather interested by community properties than we can compute functional diversity metrics which are much more appropriate.


::: {.cell}

```{.r .cell-code}
local_di_ap <- as.data.frame(sp_local_di[, c(1, 11)])
local_di_ap$depth <- rownames(local_di_ap)
```
:::

### 4.3.2. Plotting rarity
 - __Functional Distinctiveness__

::: {.cell}

```{.r .cell-code}
sp_di_ri <- merge(sp_di, ri, by = "species")
sp_di_ri_ui <- merge(sp_di_ri, sp_ui, by = "species")

plot_dist_ri_reg <- ggplot(sp_di_ri, aes(distinctiveness, Ri)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = species)) +
  labs(x = "Functional Distinctiveness", y = "Geographical Restrictedness") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_dist_ri_reg
```

::: {.cell-output-display}
![](index_files/figure-html/plot_dist_ri_reg-1.png){width=672}
:::
:::

On this visualization we can clearly see that _Anoplogaster cornuta_ is overall the most distinct species while being quite restricted in terms of depth. On the other hand, the two Myctophidae _Lampanyctus ater_ and _Lobianchia_gemellarii_ are the most functionally common and regionally widespread species (low restrictedness).

- __Functional Uniqueness__

::: {.cell}

```{.r .cell-code}
plot_ui_ri_reg <- ggplot(sp_di_ri_ui, aes(Ui, Ri)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = species)) +
  labs(x = "Functional Uniqueness", y = "Geographical Restrictedness") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_ui_ri_reg
```

::: {.cell-output-display}
![](index_files/figure-html/plot_ui_ri_reg-1.png){width=672}
:::
:::



- plot local scale measurements:


::: {.cell}

```{.r .cell-code}
sp_local_di_df <- funrar::matrix_to_stack(
  sp_local_di, value_col = "local_di", row_to_col = "basket",
  col_to_col = "species"
)
sp_local_si_df <- funrar::matrix_to_stack(
  si, value_col = "local_si", row_to_col = "basket", col_to_col = "species"
)

sp_local_di_si <- merge(
  sp_local_di_df, sp_local_si_df, by = c("basket", "species")
)

head(sp_local_di_si)
```

::: {.cell-output .cell-output-stdout}
```
        basket                     species  local_di     local_si
1 Bathypelagic        Anoplogaster_cornuta 0.4222418 9.936334e-01
2 Bathypelagic          Bathylagus_euryops 0.3144837 8.410660e-01
3 Bathypelagic Bolinichthys_supralateralis 0.3095523 9.961752e-01
4 Bathypelagic     Borostomias_antarcticus 0.2576447 9.642494e-01
5 Bathypelagic           Chauliodus_sloani 0.3382178 9.399271e-01
6 Bathypelagic               Cyclothone_sp 0.2912187 3.996095e-06
```
:::
:::

::: {.cell}

```{.r .cell-code}
plot_local_di_si <- ggplot(sp_local_di_si, aes(local_di, local_si)) +
  geom_point(alpha = 1/3) +
  labs(x = "Functional Distinctiveness", y = "Scarcity") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_local_di_si
```

::: {.cell-output-display}
![](index_files/figure-html/plot_local_di_si-1.png){width=672}
:::
:::
