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
fontsize: medium
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

__Point méthodo__: 

+ exclure sp avec beaucoup NA pour certains traits ? 
+ pondérer le poids de certains triats dans les analyses? (beaucoup de traits catégoriels qui peuvent artificiellement augmenter la diversité fonctionnelle)
+ besoin de transformer les données de traits distribuées de façon non-normale ? 
+ Ajouter données profondeur 2022

# Data preparation

::: {.cell}

```{.r .cell-code}
library(dplyr)
library(ggplot2)

morphometric_data <- utils::read.csv(here::here("data", "morphometric_data.csv"), sep = ";", header = T, dec = ".")

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


__Data summary__

::: {.cell}

```{.r .cell-code}
morpho_data_summary <-morpho_data %>%
  group_by(species) %>%
  count(species)

htmltools::tagList(DT::datatable(morpho_data_summary))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f5f7983a26c83e46ab44" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-f5f7983a26c83e46ab44">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42"],["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metopoclampus","Eurypharynx_pelecanoides","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Normichthys_operosus","Notoscopelus_bolini","Notoscopelus_kroyeri","Paralepis_coregonoides","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei"],[3,30,17,37,19,20,11,14,30,12,12,7,4,6,4,3,3,9,22,39,21,7,22,5,5,11,4,11,20,9,25,38,20,36,19,8,9,36,30,20,26,38]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## Missing data 
The operculum width was the variable with the highest number of missing data (n = 30). This represents 4% of the data 

__2 species with only NAs for an entire trait:__

+ _Eurpharynx pelecanoides_ : 7 traits
+ _Melanostigma atlanticum_ : 2 traits


::: {.cell}

:::



### data imputation  
- mice algorithm: n imputation = 5, n iterations = 50
- plot comparaison for Operculum Width

::: {.cell}

```{.r .cell-code}
#select numeric variables for imputation 
original_data <- morpho_data %>%
  select(1:23)

imputation <-
  mice::mice(
    original_data,
    m = 5,
    maxit = 50,
    printFlag = F
  )

imputed_data <- mice::complete(imputation)

comparison <- tibble::tibble(species = original_data$species,
                             standard_length = original_data$standard_length,
                             Original = original_data$operculum_width,
                             Imputed = imputed_data$operculum_width) %>%
  arrange(species, standard_length)

ggplot(comparison, aes(standard_length, Imputed, col = is.na(Original))) +
  geom_point(size = 2, alpha=0.8) +
  facet_wrap(~species, scales = "free") +
  labs(col = "Imputed?") +
  scale_color_manual(values = c("grey60", "firebrick2")) +
  theme_bw()
```

::: {.cell-output-display}
![](index_files/figure-html/imputation_data-1.png){width=1248}
:::
:::

__Measures density distribution__

::: {.cell}

```{.r .cell-code}
density_plot <- imputed_data  %>%
  select(-c(individual_code, station, weight)) %>% 
  tidyr::pivot_longer(!species, names_to = "traits", values_to = "values")


ggplot(density_plot , aes(values)) +
  geom_histogram(bins = 10,
                 color = "darkgrey",
                 fill = "lightgray") +
  facet_wrap(~ traits, scales = "free") +
  theme_minimal()+
  theme(strip.text.x = element_text(size = 10, face = "bold"))
```

::: {.cell-output-display}
![](index_files/figure-html/density_plot_traits-1.png){width=1152}
:::
:::


## Species * traits
### calctulate functional traits 

::: {.cell}

```{.r .cell-code}
# calculate functional numeric traits
original_dataeric <- imputed_data %>%
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

# categorical traits for species without NA
cat_morpho <- morpho_data %>%
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
    na.omit() %>%
  distinct() %>%
  arrange(species)

# combined the two data frames
fish_traits <- original_dataeric %>%
  inner_join(cat_morpho, by = "species") %>%
  arrange(species) %>% 
  tibble::column_to_rownames("species")%>%
  # assign trait type 
  # as.factor for qualitative traits
  mutate_if(is.character, as.factor)%>%
  # as.ordered for ordered variables
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-597ddf1a30dba8cd2e1b" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-597ddf1a30dba8cd2e1b">{"x":{"filter":"none","vertical":false,"data":[["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metopoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Normichthys_operosus","Notoscopelus_bolini","Notoscopelus_kroyeri","Paralepis_coregonoides","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei"],[0.1870635427569378,0.6731215725922228,0.2528378924337218,0.2466185816292626,0.8073379156515913,0.553876225297698,0.6943542959568116,0.3918502902611463,0.61660846298933,0.3236216278441371,0.2896732995527421,0.7087200272405964,0.3806762185788406,0.4619095739058715,0.2627730950311468,0.7001620679316158,0.5861245029650884,0.5876906171451259,0.4605188017400862,0.4635401519874494,0.6379293967644859,0.4655204987772433,0.5686707446196964,0.5560047681071145,0.5436602586272192,0.4040027222573614,0.5366700102175092,0.5653194882201393,0.3404604909527202,0.5810276131287075,0.5877979401415817,0.58721677071155,0.5177971164068327,0.6557709453691855,0.3160340663331095,0.6531117613886723,0.5695641783784532,0.4097905992320632,0.3439623137409401,0.3246954087960091,0.653061224489796],[0.06513101563336203,0.03939043442921152,0.1103521526110715,0.1195337463229555,0.09733151619827526,0.09708002840620254,0.1002020784107539,0.0454502028499367,0.09456299480839496,0.03195944404217845,0.01820101324427989,0.01870641840179216,0.08672361809853502,0.06111392066224541,0.02365859567134613,0.107319044568802,0.09736638359612411,0.04953860514578357,0.04783772722762789,0.05324648475404745,0.03316666671153076,0.06137020103787766,0.0589832944995062,0.08848446584142593,0.0843550572708857,0.05808421042835739,0.0978961923388425,0.04280718124696028,0.02667037338846572,0.09235056814949912,0.105830132758109,0.06117253100231441,0.06286434018346203,0.05113309818745389,0.03048020374468671,0.08748603143506498,0.07967153949171366,0.007796575563762054,0.03401701434877021,0.02147373395945686,0.08897126969416125],[5083.202557222812,136.2152258622217,531.9137931071798,3042.322434320248,215.6744532820656,288.0952119082589,857.141772877457,2308.803855788343,281.2684201772519,1427.556961259949,442.8885203133374,102.2063575036608,496.3447641966349,746.3340129817943,6250.383835528853,854.060712338905,870.8481183231795,1516.361659863597,1550.595807548067,2223.938773657847,188.2579254995042,1226.901459954854,4216.396258458542,3465.852896151474,2055.911596674011,1384.771960858673,186.8514118522354,78.39976364627458,2537.245636289088,234.4223020823153,487.8793839205218,486.4451319733718,256.9243753109185,214.935335971926,80.09634693327251,822.5028358194945,550.0183144490258,121.0450061746473,4544.83974684605,733.3108496619434,250.3885057471265],[0.5910077137393964,2.701565369207982,1.382073522952367,0.9925153727538065,1.208429987743106,1.157723276407389,1.145308631944953,0.9549389452177165,1.995029714652799,2.198671222745045,2.449443009756685,1.689339435970036,0.9219855888959767,1.283292920158411,0.6068674018540213,1.283755231731155,1.246595443731965,1.144873224338048,1.276737860165135,1.162473804130551,2.223446422230094,1.100038150697565,0.8484726822993502,1.230101510957001,1.163701414828532,1.248535562855231,1.285170782096033,1.345688706965169,1.398821013856888,1.335620964108464,1.17547985904566,1.278747111079255,1.596452820078978,1.142638330612098,1.080061200669548,1.268934248354,1.096024427654899,0.5320584499681953,1.023028473395861,0.7082732997529833,1.752941176470588],[0.3951426840833095,0.6956161275567915,0.5404276289037069,0.7385898286347672,0.3502518371432042,0.3324025605583356,0.3380289556040331,0.4826993703447641,0.5595395778352831,0.7068627312794583,0.7937932689673789,0.492756633741221,0.3452486634420606,0.5829657156202231,0.4721898773185077,0.4712003973315194,0.3956514300993943,0.5680439070562933,0.7716613657494861,0.5744746730161431,0.5520998821851169,0.5212153959103951,0.3530711561087431,0.4631356481458706,0.3738738564459647,0.2036324254692755,0.5855853143839959,0.4857260963559027,0.6176663797525285,0.5678597742789305,0.556466751857582,0.4896021576606608,0.6177490372101652,0.4245107644843942,0.5624538104272562,0.361697362365493,0.5212244711908143,0.586629915236263,0.6490690083932986,0.7690740917957182,0.6394557823129252],[0.2861375317012913,0.1199066289068907,0.2514373364077059,0.266959472951216,0.07467819295892282,0.1884618356794901,0.19638076001865,0.198414842989372,0.2124063307567828,0.1144114643871464,0.2040581908463124,0.04491240788019163,0.1867292921905659,0.1634947940484265,0.1777783246347302,0.2092942077891755,0.2069838979262741,0.1973450981018806,0.210637486787245,0.2048825282290511,0.1027761390567869,0.2223712374577787,0.2544654948963843,0.2078438651875588,0.2120154019923103,0.1744403226406842,0.166670163223075,0.03814429068386168,0.1234687560304594,0.1566457249526299,0.1804848574435944,0.2008815666871479,0.197832221769231,0.1330213734139315,0.08475918421735412,0.1571232489521016,0.1366886549021795,0.08568386712504511,0.2267422887328622,0.09993380308826154,0.1139944392956441],[20.92,5.520333333333333,5.364705882352941,17.14486486486486,8.536315789473685,4.7555,12.47272727272727,15.79642857142857,5.523,9.454166666666666,7.475833333333333,4.758571428571429,6.3725,9.282500000000001,27.92,9.563333333333333,9.283333333333333,18.38136363636364,20.32410256410256,25.00333333333333,4.607142857142858,15.45818181818182,19.028,15.524,15.73,21.49,3.591818181818182,4.033,18.11777777777778,5.6196,9.430263157894737,9.629,6.779722222222222,4.898421052631579,6.5525,11.76,9.186666666666667,6.952666666666667,18.1015,6.292692307692308,19.9],[0.3471179569492232,0.2282622445820382,0.3034423890185704,0.308301242626706,0.2204700849878184,0.2577329098405489,0.2949444282485461,0.2219516612342632,0.3106831287972612,0.1353122370036637,0.2203047019193976,0.09229259278317947,0.2573417157046858,0.2169720130174164,0.208131053502031,0.3736914296278676,0.3084118074275706,0.2268293066821225,0.2617421786031248,0.2544000525765622,0.179483203158696,0.2690716828129346,0.2291135730880142,0.3581617707887102,0.3386367195769682,0.2602094930995654,0.2861583530888247,0.1289222320266107,0.1414616221329245,0.2434873975369473,0.3036775056329284,0.2484099358389354,0.2638957745998514,0.2491930232735534,0.1610794295053864,0.3035521067765809,0.2480447612095185,0.153233449096655,0.2463809400095261,0.1067771817341839,0.2344763670064875],[0.496765317595603,0.07231486748096279,0.5465676366743593,0.6527711514828205,0.1769713362911839,0.227186906305348,0.23575289155312,0.1471579276088384,0.1957057378562352,0.1139788839951372,0.1385564389886054,0.06475552609724118,0.2868922794103201,0.1854842390181648,0.1439966534398835,0.1995718556257012,0.1884279714218087,0.1634270563148341,0.174943368578538,0.1791292477891186,0.07016698379208147,0.2389029851391781,0.1846729123532705,0.2190923414047283,0.2006259493110997,0.2145214178694734,0.2284769602774343,0.08730489814688717,0.1281404978381019,0.2159343939861771,0.2184135875848383,0.1977431521215698,0.1921466687105275,0.09127720302297426,0.148949033138653,0.1648207240422557,0.2093720644298593,0.02676997296275347,0.1460846094753337,0.08181017986974991,0.1594068582020389],[0.327552178795381,0.2785452832292826,0.2307451284370966,0.2471710436456569,0.1987584730572414,0.4427266302964014,0.4605590043104663,0.1754328409583324,0.4771988618173915,0.289631998528789,0.1992742880547905,0.6600484532653362,0.2369353691925481,0.215888746321026,0.2073022595505449,0.2511962839831692,0.2467697101111054,0.3008188712564941,0.3780301606138066,0.1934391114778709,0.3583607039010895,0.2710821884027898,0.2070338218789051,0.1986434877697646,0.1907795219830297,0.1746990027397655,0.2251067492779084,0.3277490877358783,0.2233648323381441,0.4727531669301591,0.1841559512741846,0.2851868407331372,0.254490059280657,0.2229812001799436,0.4749769701497314,0.2331359005083858,0.2012247420149917,1,0.1973392223033529,0.1824120299750198,0.2203389830508475],[0.4003208996368177,0.2358202027404649,0.2779352032788916,0.2589592489665474,0.2573592617891224,0.2999602281286219,0.3283279074114032,0.2369008386437003,0.3413655289922946,0.1525109091904405,0.2296622405613443,0.1305046806357046,0.3475487347397182,0.234931461549684,0.2185105436064756,0.3836208813775061,0.3557612665211258,0.2575050471048759,0.2912274335906986,0.3991766831105948,0.1873220233715019,0.2905759860794116,0.1740874348298388,0.3692829665519732,0.3636636301971131,0.3343165173045963,0.2720666504252247,0.1519023270802228,0.1596798610655336,0.278532769448776,0.3277484293204377,0.2755915950773311,0.2868597770959138,0.2654937591570808,0.1830307523791412,0.3646360696731977,0.2602312413080092,1.912272743928823e-05,0.2852113862556448,0.1154570911731925,0.2391102873030584],[0.0371234059465991,0.01435870746028057,0.2549015223177563,0.1236266109740932,0.03231957651138185,0.09109791643347125,0.01988857958867888,0.03631916674726406,0.0314179790498654,0.01525793056447875,0.07135857193615111,0.01046142821823197,0.06090709882905589,0.0330170205426,0.01553375842430409,0.06429066383512382,0.04055065658457224,0.02463758734428551,0.01985425410804219,0.01871587454424042,0.01805418174713717,0.03025781637891173,0.02411855326084547,0.07493646258320825,0.03196526524002068,0.01480309413827949,0.1787182752269798,0.04130241912511247,0.01959986641736065,0.03254080223555961,0.01754916162030041,0.02763011114739326,0.02667208329968006,0.06703510034705321,0.02971955680387246,0.04254465702825964,0.01793523328064347,0.003317401467047132,0.08468757530738309,0.01107452305916778,0.0183226273795447],[10.45666666666667,3.639666666666667,3.425882352941176,5.975405405405406,6.301578947368421,3.3485,10.79727272727273,5.527142857142857,5.455333333333334,6.194166666666667,2.8375,2.242857142857143,5.945,6.265,12.76,4.013333333333334,6.078888888888889,8.561818181818182,11.33205128205128,13.30904761904762,2.92,8.897727272727273,8.033999999999999,5.81,7.612727272727272,18.175,2.204545454545455,1.3475,4.792222222222223,4.2856,8.535,6.7935,7.134722222222222,2.601052631578948,3.58375,5.828888888888889,10.18444444444444,1.211666666666667,3.299,3.472692307692308,5],[0.4441498752678317,0.6624057564359084,0.3558469543404688,0.3318952397764631,0.4615595496347744,0.4676150398984861,0.4469769590756064,0.5798280726541963,0.4979362376932329,0.2551650456937953,0.5434318108133503,0.3116459625644058,0.4041028771788586,0.4314272231538535,0.5671939001428983,0.6326973503856025,0.6361420651390463,0.4410062335674381,0.4528052962800324,0.4509057936924641,0.5980112926718268,0.4076621012576387,0.8268335886150313,0.6409601116629159,0.6344647156330613,0.6389295252984915,0.5480099853470315,0.4712126574456847,0.8479658564847968,0.4275739736694765,0.6823773209990294,0.3681530667791175,0.4118070492790029,0.6313155638870939,0.7766759118958205,0.6195601138458109,0.6397333531929763,0.3299632556271487,0.5465635247269044,0.8863608137478909,0.5708989805375347],[0.7176935425650939,0.6073283805170969,0.8378677998173621,0.7931300949045529,0.6759418666886342,0.52169659214019,0.5912844468134186,0.6406218602628342,0.5968153143538497,0.7608914348764781,0.9524117126142014,0.6792564664135844,0.5041147244442608,0.8025189634953906,0.6875363082898668,0.6816922245429579,0.6757727860416617,0.6434772867086079,0.6323528989520742,0.6741368657991837,0.6136384769387926,0.6326776614466724,0.6113076825121152,0.6801791224383178,0.6352660702821236,0.7404149553188488,0.6295496228498633,0.6523412642442598,0.7023241150122855,0.5915476455975354,0.5986729794261195,0.5509132572485328,0.5914664043406563,0.6770228372535668,0.7832793790368987,0.5912461332246149,0.5965030393450671,0.7257564943563971,0.6995258484505198,0.7053026635160292,0.7959183673469388],[1.91753487412749,1.29175272037924,2.067163041443901,1.200654781647563,1.124312726235241,1.615591789989959,1.266547465086961,0.9001426533263226,1.863584481393095,2.370446773449222,1.007904741768296,1.381482836039404,1.828253739251144,1.342729773708559,1.125109185048073,1.005414520892331,1.256182599309575,0.745538622186577,0.781843543150224,0.6972474697559381,1.513782115016354,0.9216188402082974,1.682642483233108,1.051571563080083,1.1351500186845,1.563131800106226,1.403601622773818,1.394534509216571,1.484009375370477,1.89822831578992,1.558071293993435,1.031327676466461,1.925545070392015,0.957153868401671,1.436677037589144,0.9152335515764092,1.685007419668435,1.7975179826019,0.8037016931661495,2.769898395886286,0.6834170854271358],["A","A","P","P","A","P","P","P","P","P","P","P","P","A","P","A","P","P","P","P","A","P","A","P","P","A","P","A","P","P","P","P","P","A","P","P","P","A","P","P","A"],["A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","P","A"],["A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","P","A"],["A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A"],["A","P","P","P","P","P","A","P","P","A","A","P","P","A","P","P","P","P","P","P","P","P","A","P","P","P","P","A","A","P","P","P","P","A","P","P","P","P","P","P","P"],["P","A","A","A","A","A","A","P","A","P","P","A","A","P","P","A","A","A","A","P","A","P","P","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","P","A","A"],["P","A","A","A","A","A","A","P","A","P","A","A","A","P","A","A","A","A","A","A","A","A","P","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","P","A"],["A","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A"],["P","P","A","A","P","P","P","P","P","A","P","A","A","A","P","A","A","P","P","P","P","P","P","P","P","P","A","A","P","P","P","P","P","P","A","A","P","P","P","P","A"],["C","B","C","C","C","B","C","C","C","A","C","A","C","A","C","C","C","C","C","C","B","C","A","C","C","C","C","B","A","C","C","C","C","A","C","C","C","A","C","A","C"],["1","3","1","1","2","3","2","2","3","1","2","3","1","1","1","3","3","2","3","2","3","2","1","2","2","2","2","3","3","3","2","3","3","3","1","2","2","3","2","2","2"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eye_size<\/th>\n      <th>orbital_length<\/th>\n      <th>oral_gap_surface<\/th>\n      <th>oral_gape_shape<\/th>\n      <th>oral_gape_position<\/th>\n      <th>lower_jaw_length<\/th>\n      <th>operculum_width<\/th>\n      <th>head_length<\/th>\n      <th>body_depth<\/th>\n      <th>pectoral_fin_position<\/th>\n      <th>pectoral_fin_insertion<\/th>\n      <th>transversal_shape<\/th>\n      <th>caudal_peduncle_min_depth<\/th>\n      <th>dorsal_fin_insertion<\/th>\n      <th>eye_position<\/th>\n      <th>operculum_volume<\/th>\n      <th>photophores_ventral_position<\/th>\n      <th>gland_head<\/th>\n      <th>chin_barbel<\/th>\n      <th>front_barbel<\/th>\n      <th>small_teeth<\/th>\n      <th>large_teeth<\/th>\n      <th>fang_teeth<\/th>\n      <th>retractile_teeth<\/th>\n      <th>internal_teeth<\/th>\n      <th>gill_raker_types<\/th>\n      <th>oral_gape_axis<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## Species * assemblages matrix

::: {.cell}

```{.r .cell-code}
# list of species 
sp_names <- c(rownames(fish_traits), "Nannobrachium_atrum", "Cyclothone", "Stomias_boa_boa")

# Metadata
metadata <-  utils::read.csv(here::here("data", "metadata.csv"), sep = ";", header = T, dec = ".")%>%
  # calculation of standardized biomass values (vertical  trawl opening * horizontal trawl opening * distance traveled)  
  mutate(volume_filtered = 24*58*distance)

# species biomass x depth  matrix 2002-2019 ----
data_biomass_2002_2019 <- utils::read.csv(here::here("data", "data_evhoe_catch_2002_2019.csv"), sep = ";", header = T, dec = ".")%>%
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
  filter(!Code_Station=="H0472")%>%
  tidyr::pivot_longer(!Code_Station, names_to = "species", values_to = "Tot_V_HV")%>%
  rename("Nom_Scientifique"="species")

# species biomass x depth  matrix 2021 ----
data_biomass_2021 <- utils::read.csv(here::here("data", "data_evhoe_catch_2021.csv"), sep = ";", header = T, dec = ".")%>%
  select(Nom_Scientifique, Tot_V_HV, Code_Station)%>%
  distinct()%>%
  mutate(Nom_Scientifique= gsub(" ","_",Nom_Scientifique))%>%
  filter(Nom_Scientifique%in%sp_names)

# species biomass x depth  matrix 2022 ----
data_biomass_2022 <- utils::read.csv(here::here("data", "data_evhoe_catch_2022.csv"), sep = ";", header = T, dec = ".")%>%
  select(Nom_Scientifique, Tot_V_HV, Code_Station)%>%
  distinct()%>%
  mutate(Nom_Scientifique= gsub(" ","_",Nom_Scientifique))%>%
  filter(Nom_Scientifique%in%sp_names)

# merge all matrix ----
depth_fish_biomass <- rbind(data_biomass_2002_2019, data_biomass_2021, data_biomass_2022)%>%
  as.data.frame()%>%
   rename("species"="Nom_Scientifique",
         "station"="Code_Station") %>%  
  left_join(metadata) %>% 
  select(species, Tot_V_HV, depth, volume_filtered)%>%
  # divise biomass by the volume filtered at each trawl (g.m3)
  mutate(biomass_cpu=(Tot_V_HV/volume_filtered)*1000)%>%
  select(species, depth, biomass_cpu)%>%  
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
  mutate(biomass=sum(biomass_cpu))%>%
  select(-c(biomass_cpu))%>%
  distinct()%>%
  tidyr::pivot_wider(names_from = species, values_from = biomass)%>%
  replace(is.na(.), 0)%>%
  tibble::column_to_rownames(var = "depth_layer")%>% 
  #change species name
  rename("Lampanyctus_ater"="Nannobrachium_atrum")%>%
  rename("Cyclothone_sp"="Cyclothone")%>%
  rename("Stomias_boa"="Stomias_boa_boa") %>% 
  as.matrix()
```
:::


- __assemblages__ = depth layers 
- __biomass data__ = all EVHOE data 2002-2022 (in m^3)

::: {.cell}

```{.r .cell-code}
htmltools::tagList(DT::datatable(depth_fish_biomass))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-788e571b179bb8ef6c97" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-788e571b179bb8ef6c97">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[0,1.46979030819833e-05,0,0],[0.0003406467398861954,0.0001830509681171748,2.247997235116746e-05,0.0003200058914129239],[9.227241317699017e-06,1.74621026731269e-06,3.073702565095805e-07,1.350435600850066e-06],[0.0004384521811043648,0.0001584056765425404,8.294544471760621e-06,0.0001106243426396359],[0,0.0003787778721707042,0,0],[2.358034623529346e-05,0.0005287378514412932,6.563685598332661e-05,0.0001055074390119732],[0,6.387171618704474e-06,0,1.843488085160593e-05],[2.013956910292874e-06,6.994079564419096e-05,2.19422970249756e-07,4.761470024474112e-06],[0.0002505891214706563,9.271012727896899e-05,4.602250525025226e-05,6.849662314381425e-05],[3.397291188001234e-06,0.0001325848282254463,2.556247673814617e-07,2.563671675378479e-05],[1.126342196284479e-05,9.960012576538391e-05,2.567248751922145e-06,0.0001747199582490278],[2.691171361888271e-06,6.750422967770746e-06,0,1.009996741507595e-05],[0,2.28785367484745e-06,0,9.384757187990091e-07],[1.870062508390188e-06,5.878217370546951e-06,0,4.022361141703668e-05],[0,1.60261481517401e-05,0,3.242315186476398e-05],[0,2.798938102147332e-05,0,0],[5.69846685013904e-06,4.644978163458014e-06,0,2.661892487971513e-06],[0.0007297286548823404,0.001658709295750286,5.200324394919217e-05,0.001188357747672526],[0,0.0003042711061727375,0,0],[1.4638098214541e-06,3.306115556613948e-06,4.771662324453952e-06,1.404006574068237e-05],[1.764407093688444e-05,7.249054994238721e-05,2.962210098371706e-06,1.000613525491782e-05],[7.55181708337206e-06,6.886475881291627e-05,0,0],[0,2.595768484792304e-05,0,1.445797067587536e-05],[8.998632207904398e-07,0.0001779188482664291,0,6.469723997890821e-06],[0,0.0001220139226306193,0,0],[2.459678973321686e-05,5.735472266543518e-05,4.784878324444082e-05,0.0003456096185494774],[3.516796308811765e-06,2.815171303870814e-05,6.58268910749268e-07,1.469189176696493e-05],[2.839296619579706e-05,3.343758464157721e-05,1.192915581113488e-06,2.048984646223213e-05],[0.0001159430378285174,0.0001850958355876571,0.0001062516187806056,0.0001508825508098425],[8.308549484029906e-06,0.0002322379997396959,6.58268910749268e-07,6.370667648837288e-05],[0,0.0007834072022762728,0,0],[0.0005374538886043501,0.0004120020185634977,2.846278619695132e-05,0.0002418615151627449],[3.294686463826039e-07,5.484714652404532e-06,4.38845940499512e-07,2.308553444175256e-06],[0,9.052632253719798e-06,0,0],[3.737319094047666e-06,5.963964403942441e-06,0,1.606357653595781e-05],[3.165241984961546e-05,9.885353512621318e-05,1.601787682823219e-05,0.0001298938334924048],[9.009649424629528e-05,0.0007415390905688788,2.358796930184877e-05,0.0001955665485722926],[0,3.314879065408746e-05,4.38845940499512e-07,0],[0.0002606241551788656,0.0004312611671966216,5.810413668274208e-05,0.0003255567176324167],[0.001597834021419992,0.0004313707701624349,2.073547068860194e-05,0.0009341500970614152],[0,9.053856407257921e-06,3.655434173554902e-05,5.952250133696695e-06]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Anoplogaster_cornuta<\/th>\n      <th>Arctozenus_risso<\/th>\n      <th>Argyropelecus_hemigymnus<\/th>\n      <th>Argyropelecus_olfersii<\/th>\n      <th>Bathylagus_euryops<\/th>\n      <th>Benthosema_glaciale<\/th>\n      <th>Bolinichthys_supralateralis<\/th>\n      <th>Borostomias_antarcticus<\/th>\n      <th>Ceratoscopelus_maderensis<\/th>\n      <th>Chauliodus_sloani<\/th>\n      <th>Cyclothone_sp<\/th>\n      <th>Derichthys_serpentinus<\/th>\n      <th>Diaphus_metopoclampus<\/th>\n      <th>Evermannella_balbo<\/th>\n      <th>Gonostoma_elongatum<\/th>\n      <th>Holtbyrnia_anomala<\/th>\n      <th>Holtbyrnia_macrops<\/th>\n      <th>Lampanyctus_crocodilus<\/th>\n      <th>Lampanyctus_macdonaldi<\/th>\n      <th>Lestidiops_sphyrenoides<\/th>\n      <th>Lobianchia_gemellarii<\/th>\n      <th>Malacosteus_niger<\/th>\n      <th>Maulisia_argipalla<\/th>\n      <th>Maulisia_mauli<\/th>\n      <th>Maulisia_microlepis<\/th>\n      <th>Maurolicus_muelleri<\/th>\n      <th>Melanostigma_atlanticum<\/th>\n      <th>Melanostomias_bartonbeani<\/th>\n      <th>Myctophum_punctatum<\/th>\n      <th>Lampanyctus_ater<\/th>\n      <th>Normichthys_operosus<\/th>\n      <th>Notoscopelus_kroyeri<\/th>\n      <th>Paralepis_coregonoides<\/th>\n      <th>Photostylus_pycnopterus<\/th>\n      <th>Sagamichthys_schnakenbecki<\/th>\n      <th>Searsia_koefoedi<\/th>\n      <th>Serrivomer_beanii<\/th>\n      <th>Sigmops_bathyphilus<\/th>\n      <th>Stomias_boa<\/th>\n      <th>Xenodermichthys_copei<\/th>\n      <th>Notoscopelus_bolini<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


### Species depth distribution 

::: {.cell}

```{.r .cell-code}
depth_distribution <- rbind(data_biomass_2002_2019, data_biomass_2021, data_biomass_2022)%>%
  as.data.frame()%>%  
  rename("station"="Code_Station")%>%
  left_join(metadata)%>%
  rename("species"="Nom_Scientifique")%>% 
  select(-c(station))%>%
  distinct()%>%
  filter(Tot_V_HV>0)%>%
  group_by(species)%>%
  mutate(biomass_tot = sum(Tot_V_HV))%>%
  ungroup()%>%
  group_by(species, depth)%>%
  mutate(biomass_depth = sum(Tot_V_HV))%>%
  select(species, biomass_depth, biomass_tot, depth)%>%
  distinct()%>%
  group_by(species, depth)%>%
  mutate(biomass_rel=biomass_depth/biomass_tot*100)%>%
  select(species, depth, biomass_rel)%>%
  mutate(biomass = as.integer(biomass_rel))%>%
  select(-biomass_rel)%>%
  tidyr::uncount(biomass)

# Order in function of median depth
depth_distribution$species = with(depth_distribution, reorder(species, depth, median))  

ggplot(depth_distribution,
       aes(x = depth, y = species, group = species))+ 
  ggridges::stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5 , alpha=0.4, size=0.7,
                                rel_min_height = 0.001, scale=1.2, col="gray60")+
  theme_bw()+
  scale_y_discrete(position = "left")+
  scale_x_reverse(limits = c(2000,0))+
  coord_flip()+
  ylab(label = "")+ xlab("Depth (m)")+
  theme(axis.text.y = element_text(size=11),
        axis.text.x = element_text(face="italic", size=9, angle=80, vjust = 0.5, hjust=0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11))+
  guides(fill="none", col="none", alpha="none")
```

::: {.cell-output-display}
![](index_files/figure-html/density_depth_distribution-1.png){width=1056}
:::
:::



## Traits types

The **first column** contains **traits name**. The **second column** contains
**traits type** following this code:

* **N**: nominal trait (factor variable)
* **O**: ordinal traits (ordered variable)
* **Q**: quantitative traits (numeric values)

* 1/3 of the traits are nominal (traits related to teeth and photophores), need to give them different weights so as not to overestimate functional diversity? 

::: {.cell}

```{.r .cell-code}
fish_traits_cat <- utils::read.csv(here::here("data", "fish_traits_cat.csv"), sep = ";", header = T, dec = ".")
htmltools::tagList(DT::datatable(fish_traits_cat))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-0ec39266b64f79a410b9" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-0ec39266b64f79a410b9">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"],["eye_size","orbital_length","oral_gap_surface","oral_gape_shape","oral_gape_position","lower_jaw_length","operculum_width","head_length","body_depth","pectoral_fin_position","pectoral_fin_insertion","transversal_shape","caudal_peduncle_min_depth","dorsal_fin_insertion","eye_position","operculum_volume","photophores_ventral_position","gland_head","chin_barbel","front_barbel","small_teeth","large_teeth","fang_teeth","retractile_teeth","internal_teeth","gill_raker_types","oral_gape_axis"],["Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","N","N","N","N","N","N","N","N","N","O","O"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>trait_name<\/th>\n      <th>trait_type<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::

# 1. CWM 
__Community Weighted Metric__ : somme de l'abondance relative d'une espèce x valeur du trait

+ trait quantittatif : valeur moyenne du trait si on prend un individu au hasard dans l'assemblage 

+ trait catégoriel : proportion des espèces possédant ce trait, une valeur élevée peut indiquer soit qu'un grand nombre possèdent se trait ou que l'espèce avec la plus forte abondance relative possède ce trait 


::: {.cell}

```{.r .cell-code}
# spxcom.matrix ----
spxcom.matrix <-  depth_fish_biomass %>% 
  t() %>% 
  as.data.frame() %>% 
  relocate("Epipelagic", "Upper mesopelagic", "Lower mesopelagic","Bathypelagic" ) %>% 
  tibble::rownames_to_column("species") %>% 
  arrange(species) %>% 
  tibble::column_to_rownames("species") %>% 
  as.matrix()

# spxtraits.matrix ----
spxtraits.matrix <- fish_traits %>%
  mutate(across(17:25, ~ ifelse(. == "P", 1, ifelse(. == "A", 0, .))),
         across(26, ~ ifelse(. == "A", 1, ifelse(. == "B", 2, ifelse(. == "C", 3, .))))) %>% 
  select(-c(gill_raker_types, oral_gape_axis)) 

#check rownames
#rownames(spxtraits.matrix) == rownames(spxcom.matrix)

result_CWM <- FD::functcomp(spxtraits.matrix, t(spxcom.matrix)) 
#FD::functcomp(spxtraits.matrix, t(spxcom.matrix), CWM.type = "all")

htmltools::tagList(DT::datatable(result_CWM))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-5b3dd04833d1d4513662" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-5b3dd04833d1d4513662">{"x":{"filter":"none","vertical":false,"data":[["Epipelagic","Upper mesopelagic","Lower mesopelagic","Bathypelagic"],[0.5264502630476001,0.5340917292069929,0.5140116131672948,0.5071153226163653],[0.07087703794886653,0.07255448226100607,0.06167097014872081,0.0621819100670699],[497.8593608534,779.9564958142092,788.7313938189892,958.3656802712215],[1.339452782109091,1.556306306290292,1.462287203184484,1.238940286255584],[0.5892356730738959,0.6674084311606796,0.6660852135095413,0.600072516137201],[0.1651799844422884,0.1620517304436526,0.161014328692584,0.1673610589839888],[8.113567187739578,14.48773022269152,13.32449112000713,12.86427286870236],[0.2385121677555892,0.2445089607289577,0.2365572684005304,0.2399268395005708],[0.1843159049908707,0.205007249611302,0.169558608825106,0.1731414577870444],[0.3727802198337863,0.292274127012442,0.3185208866838657,0.3535860351581783],[0.2523502691482608,0.2499818462269511,0.2435588712276255,0.2505763724636959],[0.0467648766895262,0.03178547208922426,0.03736512513707586,0.0290234128492122],[5.063773139544572,6.132516300967942,6.348204274912248,7.143403138722079],[0.5140025858199347,0.5228335578328848,0.5306864796471685,0.5131145150895369],[0.6204681801610573,0.702651322309507,0.6842532901756222,0.6560097892439939],[1.638251170258219,1.209797621080455,1.234232874258506,1.350116835943927],["1","1","1","1"],["0","0","0","0"],["0","0","0","0"],["0","0","0","0"],["1","1","1","1"],["0","0","0","0"],["0","0","0","0"],["0","0","0","0"],["1","1","1","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eye_size<\/th>\n      <th>orbital_length<\/th>\n      <th>oral_gap_surface<\/th>\n      <th>oral_gape_shape<\/th>\n      <th>oral_gape_position<\/th>\n      <th>lower_jaw_length<\/th>\n      <th>operculum_width<\/th>\n      <th>head_length<\/th>\n      <th>body_depth<\/th>\n      <th>pectoral_fin_position<\/th>\n      <th>pectoral_fin_insertion<\/th>\n      <th>transversal_shape<\/th>\n      <th>caudal_peduncle_min_depth<\/th>\n      <th>dorsal_fin_insertion<\/th>\n      <th>eye_position<\/th>\n      <th>operculum_volume<\/th>\n      <th>photophores_ventral_position<\/th>\n      <th>gland_head<\/th>\n      <th>chin_barbel<\/th>\n      <th>front_barbel<\/th>\n      <th>small_teeth<\/th>\n      <th>large_teeth<\/th>\n      <th>fang_teeth<\/th>\n      <th>retractile_teeth<\/th>\n      <th>internal_teeth<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-99fbeae400ef0d9ae51b" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-99fbeae400ef0d9ae51b">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[0,1,0,0],[1,1,1,1],[1,1,1,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[0,1,0,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,0,1],[0,1,0,1],[1,1,0,1],[0,1,0,1],[0,1,0,0],[1,1,0,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[1,1,1,1],[1,1,0,0],[0,1,0,1],[1,1,0,1],[0,1,0,0],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[1,1,1,1],[0,1,0,0],[1,1,0,1],[1,1,1,1],[1,1,1,1],[0,1,1,0],[1,1,1,1],[1,1,1,1],[0,1,1,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Anoplogaster_cornuta<\/th>\n      <th>Arctozenus_risso<\/th>\n      <th>Argyropelecus_hemigymnus<\/th>\n      <th>Argyropelecus_olfersii<\/th>\n      <th>Bathylagus_euryops<\/th>\n      <th>Benthosema_glaciale<\/th>\n      <th>Bolinichthys_supralateralis<\/th>\n      <th>Borostomias_antarcticus<\/th>\n      <th>Ceratoscopelus_maderensis<\/th>\n      <th>Chauliodus_sloani<\/th>\n      <th>Cyclothone_sp<\/th>\n      <th>Derichthys_serpentinus<\/th>\n      <th>Diaphus_metopoclampus<\/th>\n      <th>Evermannella_balbo<\/th>\n      <th>Gonostoma_elongatum<\/th>\n      <th>Holtbyrnia_anomala<\/th>\n      <th>Holtbyrnia_macrops<\/th>\n      <th>Lampanyctus_crocodilus<\/th>\n      <th>Lampanyctus_macdonaldi<\/th>\n      <th>Lestidiops_sphyrenoides<\/th>\n      <th>Lobianchia_gemellarii<\/th>\n      <th>Malacosteus_niger<\/th>\n      <th>Maulisia_argipalla<\/th>\n      <th>Maulisia_mauli<\/th>\n      <th>Maulisia_microlepis<\/th>\n      <th>Maurolicus_muelleri<\/th>\n      <th>Melanostigma_atlanticum<\/th>\n      <th>Melanostomias_bartonbeani<\/th>\n      <th>Myctophum_punctatum<\/th>\n      <th>Lampanyctus_ater<\/th>\n      <th>Normichthys_operosus<\/th>\n      <th>Notoscopelus_kroyeri<\/th>\n      <th>Paralepis_coregonoides<\/th>\n      <th>Photostylus_pycnopterus<\/th>\n      <th>Sagamichthys_schnakenbecki<\/th>\n      <th>Searsia_koefoedi<\/th>\n      <th>Serrivomer_beanii<\/th>\n      <th>Sigmops_bathyphilus<\/th>\n      <th>Stomias_boa<\/th>\n      <th>Xenodermichthys_copei<\/th>\n      <th>Notoscopelus_bolini<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
#round(sp_dist_fish, 3)
```
:::


## 2.3 Building functional spaces and chosing the best one
### 2.3.1 Computing several multimensional functional spaces and assessing their quality

- mFD evaluates the quality of PCoA-based multidimensional spaces according to the deviation between trait-based distances and distances in the functional space (extension of Maire et al. (2015) framework). 


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
pcoa_1d      0.140
pcoa_2d      0.078
pcoa_3d      0.049
pcoa_4d      0.030
pcoa_5d      0.023
pcoa_6d      0.016
pcoa_7d      0.014
pcoa_8d      0.014
pcoa_9d      0.016
pcoa_10d     0.018
tree_average 0.039
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
![](index_files/figure-html/fspaces_quality_plot-1.png){width=1248}
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

# As we have 27 traits we have to split the df to see correlation between functional axes and traits 
# first set ----
fish_traits_1 <- fish_traits%>%
  select(1:9)

fish_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = fish_traits_1, 
  sp_faxes_coord = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = T)

## Print traits with significant effect ----
fish_tr_faxes$"tr_faxes_stat"[which(fish_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]
```

::: {.cell-output .cell-output-stdout}
```
                trait axis         test stat value p.value
1            eye_size  PC1 Linear Model   r2 0.167  0.0080
2            eye_size  PC2 Linear Model   r2 0.196  0.0037
3            eye_size  PC3 Linear Model   r2 0.138  0.0167
4            eye_size  PC4 Linear Model   r2 0.101  0.0432
5      orbital_length  PC1 Linear Model   r2 0.404  0.0000
6      orbital_length  PC2 Linear Model   r2 0.099  0.0447
9    oral_gap_surface  PC1 Linear Model   r2 0.126  0.0225
10   oral_gap_surface  PC2 Linear Model   r2 0.518  0.0000
14    oral_gape_shape  PC2 Linear Model   r2 0.139  0.0165
20 oral_gape_position  PC4 Linear Model   r2 0.197  0.0037
22   lower_jaw_length  PC2 Linear Model   r2 0.678  0.0000
26    operculum_width  PC2 Linear Model   r2 0.519  0.0000
29        head_length  PC1 Linear Model   r2 0.336  0.0001
30        head_length  PC2 Linear Model   r2 0.364  0.0000
34         body_depth  PC2 Linear Model   r2 0.366  0.0000
35         body_depth  PC3 Linear Model   r2 0.178  0.0060
```
:::

```{.r .cell-code}
## Plot ----
fish_tr_faxes$"tr_faxes_plot"
```

::: {.cell-output-display}
![](index_files/figure-html/test_correlation-1.png){width=1344}
:::
:::

::: {.cell}

```{.r .cell-code}
# second set ----
fish_traits_2 <- fish_traits%>%
  select(10:18)

fish_tr_faxes_2 <- mFD::traits.faxes.cor(
  sp_tr          = fish_traits_2, 
  sp_faxes_coord = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = T)

## Print traits with significant effect ----
fish_tr_faxes_2$"tr_faxes_stat"[which(fish_tr_faxes_2$"tr_faxes_stat"$"p.value" < 0.05), ]
```

::: {.cell-output .cell-output-stdout}
```
                          trait axis           test stat value p.value
2         pectoral_fin_position  PC2   Linear Model   r2 0.264  0.0006
5        pectoral_fin_insertion  PC1   Linear Model   r2 0.294  0.0002
6        pectoral_fin_insertion  PC2   Linear Model   r2 0.379  0.0000
9             transversal_shape  PC1   Linear Model   r2 0.114  0.0310
11            transversal_shape  PC3   Linear Model   r2 0.221  0.0019
14    caudal_peduncle_min_depth  PC2   Linear Model   r2 0.400  0.0000
19         dorsal_fin_insertion  PC3   Linear Model   r2 0.168  0.0078
23                 eye_position  PC3   Linear Model   r2 0.190  0.0043
25             operculum_volume  PC1   Linear Model   r2 0.097  0.0472
32 photophores_ventral_position  PC4 Kruskal-Wallis eta2 0.590  0.0000
33                   gland_head  PC1 Kruskal-Wallis eta2 0.245  0.0011
```
:::

```{.r .cell-code}
## Plot ----
fish_tr_faxes_2$"tr_faxes_plot"
```

::: {.cell-output-display}
![](index_files/figure-html/test_correlation_2-1.png){width=1344}
:::
:::

::: {.cell}

```{.r .cell-code}
# third set ----
fish_traits_3 <- fish_traits%>%
  select(19:27)

fish_tr_faxes_3 <- mFD::traits.faxes.cor(
  sp_tr          = fish_traits_3, 
  sp_faxes_coord = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = T)

## Print traits with significant effect ----
fish_tr_faxes_3$"tr_faxes_stat"[which(fish_tr_faxes_3$"tr_faxes_stat"$"p.value" < 0.05), ]
```

::: {.cell-output .cell-output-stdout}
```
              trait axis           test stat value p.value
1       chin_barbel  PC1 Kruskal-Wallis eta2 0.142  0.0107
4       chin_barbel  PC4 Kruskal-Wallis eta2 0.183  0.0043
9       small_teeth  PC1 Kruskal-Wallis eta2 0.317  0.0003
12      small_teeth  PC4 Kruskal-Wallis eta2 0.095  0.0298
13      large_teeth  PC1 Kruskal-Wallis eta2 0.448  0.0000
14      large_teeth  PC2 Kruskal-Wallis eta2 0.214  0.0022
17       fang_teeth  PC1 Kruskal-Wallis eta2 0.410  0.0000
25   internal_teeth  PC1 Kruskal-Wallis eta2 0.079  0.0437
27   internal_teeth  PC3 Kruskal-Wallis eta2 0.634  0.0000
29 gill_raker_types  PC1 Kruskal-Wallis eta2 0.333  0.0007
30 gill_raker_types  PC2 Kruskal-Wallis eta2 0.368  0.0003
34   oral_gape_axis  PC2 Kruskal-Wallis eta2 0.378  0.0003
35   oral_gape_axis  PC3 Kruskal-Wallis eta2 0.198  0.0086
```
:::

```{.r .cell-code}
## Plot ----
fish_tr_faxes_3$"tr_faxes_plot"
```

::: {.cell-output-display}
![](index_files/figure-html/test_correlation_3-1.png){width=1344}
:::
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
![](index_files/figure-html/plot_functional_space-1.png){width=768}
:::
:::


# 3. Computing and plotting FD indices using the mFD package

## 3.1 Computing and plotting alpha FD indices


::: {.cell}

```{.r .cell-code}
alpha_fd_indices_fish <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = depth_fish_biomass,
  ind_vect         = c("fdis", "fric", "fdiv", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)
```
:::


The function has two main outputs:

+ a data.frame gathering indices values in each assemblage (for FIde values, there are as many columns as there are axes to the studied functional space).


::: {.cell}

```{.r .cell-code}
fd_ind_values_fish <- alpha_fd_indices_fish$"functional_diversity_indices"
htmltools::tagList(DT::datatable(round(fd_ind_values_fish, 3)))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-20b0bc87005b9e36217c" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-20b0bc87005b9e36217c">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[28,41,24,32],[0.433,0.455,0.386,0.456],[0.718,1,0.355,0.641],[0.589,0.652,0.612,0.612],[0.37,0.386,0.377,0.374],[-0.04,-0.018,-0.036,-0.024],[-0.022,-0.017,-0.047,-0.028],[-0.008,0.03,0.02,0.008999999999999999],[0.005,0.024,0.038,0.02]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sp_richn<\/th>\n      <th>fdis<\/th>\n      <th>fric<\/th>\n      <th>fdiv<\/th>\n      <th>fspe<\/th>\n      <th>fide_PC1<\/th>\n      <th>fide_PC2<\/th>\n      <th>fide_PC3<\/th>\n      <th>fide_PC4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


+ a details list of data.frames and lists gathering information such as coordinates of centroids, distances and identity of the nearest neighbour, distances to the centroid, etc. The user does not have to directly use it but it will be useful if FD indices are then plotted. It can be retrieved through:


::: {.cell}

```{.r .cell-code}
details_list_fish <- alpha_fd_indices_fish$"details"
```
:::

::: {.cell}

```{.r .cell-code}
plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fish,
  plot_asb_nm              = c("Epipelagic", "Bathypelagic"),
  ind_nm                   = c("fdis", "fric", "fdiv", 
                              "fspe", "fide"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  plot_sp_nm               = NULL,
  save_file                = FALSE,
  check_input              = TRUE) 
```
:::


### __FRic Functional Richness__

- the proportion of functional space filled by species of the studied assemblage, i.e. the volume inside the convex-hull shaping species. To compute FRic the number of species must be at least higher than the number of functional axis + 1.

::: {.cell}

```{.r .cell-code}
plots_alpha$"fric"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FRic-1.png){width=1152}
:::
:::


### __FDiv Functional Divergence__

- the proportion of the biomass supported by the species with the most extreme functional traits i.e. the ones located close to the edge of the convex-hull filled by the assemblage

::: {.cell}

```{.r .cell-code}
plots_alpha$"fdiv"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FDiv-1.png){width=1152}
:::
:::


### __FSpe Functional Specialization__

- the biomass weighted mean distance to the mean position of species from the global pool (present in all assemblages).

::: {.cell}

```{.r .cell-code}
plots_alpha$"fspe"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FSpe-1.png){width=1152}
:::
:::


### __FDis Functional Dispersion__

- the biomass weighted deviation of species traits values from the center of the functional space filled by the assemblage i.e. the biomass-weighted mean distance to the biomass-weighted mean trait values of the assemblage.


::: {.cell}

```{.r .cell-code}
plots_alpha$"fdis"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FDis-1.png){width=1152}
:::
:::


### __FIde Functional Identity__
- the mean traits values for the assemblage. FIde is always computed when FDis is computed.


::: {.cell}

```{.r .cell-code}
plots_alpha$"fide"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FIde-1.png){width=1152}
:::
:::



## 3.2.Computing and plotting beta FD indices

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
Bathypelagic              0.2817243                        
Epipelagic                0.5059558    0.6450816           
Lower mesopelagic         0.1479024    0.3593021  0.4460600

$jac_turn
                  Upper mesopelagic Bathypelagic   Epipelagic
Bathypelagic           9.963967e-16                          
Epipelagic             2.203684e-04 0.000000e+00             
Lower mesopelagic      4.716059e-02 0.000000e+00 3.706431e-05

$jac_nest
                  Upper mesopelagic Bathypelagic Epipelagic
Bathypelagic              0.2817243                        
Epipelagic                0.5057354    0.6450816           
Lower mesopelagic         0.1007419    0.3593021  0.4460230
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
        0.7182757         1.0000000         0.3549184         0.6406979 
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
 [1] "Stomias_boa"                "Melanostigma_atlanticum"   
 [3] "Argyropelecus_olfersii"     "Evermannella_balbo"        
 [5] "Serrivomer_beanii"          "Xenodermichthys_copei"     
 [7] "Borostomias_antarcticus"    "Sagamichthys_schnakenbecki"
 [9] "Maurolicus_muelleri"        "Lampanyctus_crocodilus"    
[11] "Holtbyrnia_macrops"         "Arctozenus_risso"          
[13] "Maulisia_mauli"             "Lestidiops_sphyrenoides"   
[15] "Lobianchia_gemellarii"      "Benthosema_glaciale"       
[17] "Chauliodus_sloani"          "Malacosteus_niger"         
[19] "Ceratoscopelus_maderensis"  "Derichthys_serpentinus"    
[21] "Paralepis_coregonoides"     "Argyropelecus_hemigymnus"  
[23] "Melanostomias_bartonbeani" 

$Bathypelagic
 [1] "Evermannella_balbo"        "Borostomias_antarcticus"  
 [3] "Malacosteus_niger"         "Gonostoma_elongatum"      
 [5] "Lampanyctus_macdonaldi"    "Melanostigma_atlanticum"  
 [7] "Derichthys_serpentinus"    "Maulisia_microlepis"      
 [9] "Sigmops_bathyphilus"       "Lestidiops_sphyrenoides"  
[11] "Argyropelecus_hemigymnus"  "Lampanyctus_crocodilus"   
[13] "Ceratoscopelus_maderensis" "Maulisia_argipalla"       
[15] "Benthosema_glaciale"       "Maurolicus_muelleri"      
[17] "Maulisia_mauli"            "Stomias_boa"              
[19] "Paralepis_coregonoides"    "Argyropelecus_olfersii"   
[21] "Chauliodus_sloani"         "Serrivomer_beanii"        
[23] "Arctozenus_risso"          "Bathylagus_euryops"       
[25] "Anoplogaster_cornuta"      "Holtbyrnia_anomala"       
[27] "Melanostomias_bartonbeani"

$Epipelagic
 [1] "Stomias_boa"               "Melanostigma_atlanticum"  
 [3] "Lobianchia_gemellarii"     "Arctozenus_risso"         
 [5] "Borostomias_antarcticus"   "Argyropelecus_olfersii"   
 [7] "Maurolicus_muelleri"       "Lestidiops_sphyrenoides"  
 [9] "Xenodermichthys_copei"     "Sigmops_bathyphilus"      
[11] "Myctophum_punctatum"       "Ceratoscopelus_maderensis"
[13] "Chauliodus_sloani"         "Paralepis_coregonoides"   
[15] "Benthosema_glaciale"       "Lampanyctus_crocodilus"   
[17] "Serrivomer_beanii"         "Melanostomias_bartonbeani"
[19] "Argyropelecus_hemigymnus" 

$`Lower mesopelagic`
 [1] "Stomias_boa"                 "Ceratoscopelus_maderensis"  
 [3] "Argyropelecus_olfersii"      "Melanostigma_atlanticum"    
 [5] "Arctozenus_risso"            "Maulisia_mauli"             
 [7] "Bolinichthys_supralateralis" "Borostomias_antarcticus"    
 [9] "Maurolicus_muelleri"         "Benthosema_glaciale"        
[11] "Serrivomer_beanii"           "Xenodermichthys_copei"      
[13] "Maulisia_argipalla"          "Lestidiops_sphyrenoides"    
[15] "Holtbyrnia_macrops"          "Gonostoma_elongatum"        
[17] "Sagamichthys_schnakenbecki"  "Chauliodus_sloani"          
[19] "Evermannella_balbo"          "Lampanyctus_crocodilus"     
[21] "Derichthys_serpentinus"      "Paralepis_coregonoides"     
[23] "Argyropelecus_hemigymnus"    "Melanostomias_bartonbeani"  
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
  plot_sp_nm              = c("Maurolicus_muelleri", "Lampanyctus_crocodilus", "Argyropelecus_olfersii"),
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-aa372a93a0ac350e105b" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-aa372a93a0ac350e105b">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41"],["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metopoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Normichthys_operosus","Notoscopelus_bolini","Notoscopelus_kroyeri","Paralepis_coregonoides","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei"],[0.3668772626168548,0.2529676099854551,0.2946777813742018,0.2943802300138824,0.2245779581869246,0.2246599240484081,0.2333277385144243,0.2714679057964132,0.2158933188549138,0.3868479665814772,0.2840939938238449,0.2981434226609408,0.2373125800145302,0.3488074925386992,0.2828995795133522,0.2475420309894603,0.2128350987726451,0.1947354631039703,0.2174191764724976,0.2381432773516548,0.2491495060171916,0.2111727024550822,0.3700755567907904,0.2125914632007417,0.204009308275686,0.2488000724526063,0.2247798088260556,0.2870297249986952,0.3792172282546871,0.2034832430677826,0.1986325786972981,0.1948561210059207,0.199054250102451,0.2974681046303838,0.2503363471916696,0.2145326484492943,0.1924533697781431,0.3246815676155657,0.2390844389464392,0.3762515007733525,0.2434193348189186]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>distinctiveness<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
t = 25.003, df = 39, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9444435 0.9841082
sample estimates:
      cor 
0.9701954 
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
0.03861127 0.04734512 0.05180288 0.06772377 0.07558509 0.10822527 0.12548328 
       70%        80%        90%       100% 
0.14133964 0.16519174 0.19158608 0.22482167 
```
:::

```{.r .cell-code}
htmltools::tagList(DT::datatable(sp_ui))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-d6ab47753c8bc3ee2138" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-d6ab47753c8bc3ee2138">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41"],["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metopoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Lampanyctus_ater","Normichthys_operosus","Notoscopelus_kroyeri","Paralepis_coregonoides","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei","Notoscopelus_bolini"],[0.2216999532825106,0.05056694527832013,0.1126089790952527,0.1126089790952527,0.1254832848934597,0.07558508715979755,0.1082252738506514,0.1395090868782561,0.04734512251457217,0.2186657964261447,0.1599092853533166,0.1590709547722683,0.1106351643187424,0.2248216724205994,0.1280197844326792,0.07856781840615111,0.05812991482354539,0.0677237699161883,0.07062176809492998,0.05056694527832013,0.07062176809492998,0.2216999532825106,0.03861126714074527,0.03861126714074527,0.1413396442089121,0.1013793052923886,0.1438711573044715,0.1915860755088155,0.04734512251457217,0.0677237699161883,0.04407512470334325,0.0518028800247474,0.1771860011042057,0.1651917420855135,0.05812991482354539,0.04407512470334325,0.1661931491752429,0.09853782491490212,0.1915860755088155,0.1344418026160116,0.0518028800247474]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>Ui<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::

Based on these results we see that _Serrivomer beanii_, _Anoplogaster cornuta_, and _Malacosteus niger_ are the most isolated fish in the functional space. Meaning that they have the most distant nearest neighbors.

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
                  Anoplogaster_cornuta Arctozenus_risso
Upper mesopelagic                   NA        0.2161728
Bathypelagic                 0.3767824        0.2229648
Epipelagic                          NA        0.2079065
Lower mesopelagic                   NA        0.2194040
                  Argyropelecus_hemigymnus Argyropelecus_olfersii
Upper mesopelagic                0.2660142              0.2756554
Bathypelagic                     0.2938755              0.2923411
Epipelagic                       0.2665793              0.2925497
Lower mesopelagic                0.2834153              0.2809862
                  Bathylagus_euryops Benthosema_glaciale
Upper mesopelagic                 NA           0.2173321
Bathypelagic               0.2092679           0.2040058
Epipelagic                        NA           0.1667439
Lower mesopelagic                 NA           0.2115402
                  Bolinichthys_supralateralis Borostomias_antarcticus
Upper mesopelagic                          NA               0.2787882
Bathypelagic                        0.2121591               0.2608313
Epipelagic                                 NA               0.2699629
Lower mesopelagic                   0.2291705               0.2684266
                  Ceratoscopelus_maderensis Chauliodus_sloani
Upper mesopelagic                 0.1948874         0.3953637
Bathypelagic                      0.1814621         0.4071009
Epipelagic                        0.1516539         0.4016180
Lower mesopelagic                 0.1897701         0.3932241
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
1            Arctozenus_risso 0.05056695
2    Argyropelecus_hemigymnus 0.11260898
3      Argyropelecus_olfersii 0.11260898
4         Benthosema_glaciale 0.07558509
5     Borostomias_antarcticus 0.13950909
6   Ceratoscopelus_maderensis 0.04734512
7           Chauliodus_sloani 0.21866580
8               Cyclothone_sp 0.19799107
9      Derichthys_serpentinus 0.15907095
10         Evermannella_balbo 0.22482167
11         Holtbyrnia_macrops 0.05812991
12     Lampanyctus_crocodilus 0.06772377
13    Lestidiops_sphyrenoides 0.05056695
14      Lobianchia_gemellarii 0.08383755
15          Malacosteus_niger 0.22945080
16             Maulisia_mauli 0.08718195
17        Maurolicus_muelleri 0.10137931
18    Melanostigma_atlanticum 0.14387116
19  Melanostomias_bartonbeani 0.19158608
20        Myctophum_punctatum 0.04734512
21           Lampanyctus_ater 0.06772377
22       Notoscopelus_kroyeri 0.05234555
23     Paralepis_coregonoides 0.17718600
24 Sagamichthys_schnakenbecki 0.05812991
25           Searsia_koefoedi 0.08718195
26          Serrivomer_beanii 0.16619315
27                Stomias_boa 0.19158608
28      Xenodermichthys_copei 0.13634869
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
19  Melanostomias_bartonbeani 0.1915861 Upper mesopelagic
56  Melanostomias_bartonbeani 0.1915861      Bathypelagic
83  Melanostomias_bartonbeani 0.1915861        Epipelagic
115 Melanostomias_bartonbeani 0.1915861 Lower mesopelagic
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
 Anoplogaster_cornuta Arctozenus_risso Argyropelecus_hemigymnus
 Min.   :0.9469       Min.   :0.2134   Min.   :0.9614          
 1st Qu.:0.9469       1st Qu.:0.2287   1st Qu.:0.9834          
 Median :0.9469       Median :0.3691   Median :0.9921          
 Mean   :0.9469       Mean   :0.3647   Mean   :0.9848          
 3rd Qu.:0.9469       3rd Qu.:0.5051   3rd Qu.:0.9935          
 Max.   :0.9469       Max.   :0.5071   Max.   :0.9935          
 NA's   :3                                                     
 Argyropelecus_olfersii Bathylagus_euryops Benthosema_glaciale
 Min.   :0.1540         Min.   :0.2453     Min.   :0.1356     
 1st Qu.:0.4553         1st Qu.:0.2453     1st Qu.:0.1394     
 Median :0.5710         Median :0.2453     Median :0.3708     
 Mean   :0.5182         Mean   :0.2453     Mean   :0.4454     
 3rd Qu.:0.6340         3rd Qu.:0.2453     3rd Qu.:0.6768     
 Max.   :0.7769         Max.   :0.2453     Max.   :0.9043     
                        NA's   :3                             
 Bolinichthys_supralateralis Borostomias_antarcticus Ceratoscopelus_maderensis
 Min.   :0.9149              Min.   :0.7715          Min.   :0.2463           
 1st Qu.:0.9303              1st Qu.:0.9258          1st Qu.:0.3191           
 Median :0.9457              Median :0.9844          Median :0.5262           
 Mean   :0.9457              Mean   :0.9334          Mean   :0.5043           
 3rd Qu.:0.9612              3rd Qu.:0.9919          3rd Qu.:0.7114           
 Max.   :0.9766              Max.   :0.9933          Max.   :0.7185           
 NA's   :2                                                                    
 Chauliodus_sloani Cyclothone_sp    Derichthys_serpentinus
 Min.   :0.6115    Min.   :0.4303   Min.   :0.9524        
 1st Qu.:0.8156    1st Qu.:0.6259   1st Qu.:0.9638        
 Median :0.9346    Median :0.8080   Median :0.9753        
 Mean   :0.8682    Mean   :0.7498   Mean   :0.9721        
 3rd Qu.:0.9873    3rd Qu.:0.9319   3rd Qu.:0.9819        
 Max.   :0.9922    Max.   :0.9531   Max.   :0.9886        
                                    NA's   :1             
 Diaphus_metopoclampus Evermannella_balbo Gonostoma_elongatum
 Min.   :0.9915        Min.   :0.8236     Min.   :0.8551     
 1st Qu.:0.9925        1st Qu.:0.9010     1st Qu.:0.8769     
 Median :0.9935        Median :0.9784     Median :0.8987     
 Mean   :0.9935        Mean   :0.9313     Mean   :0.8987     
 3rd Qu.:0.9945        3rd Qu.:0.9852     3rd Qu.:0.9205     
 Max.   :0.9955        Max.   :0.9921     Max.   :0.9423     
 NA's   :2             NA's   :1          NA's   :2          
 Holtbyrnia_anomala Holtbyrnia_macrops Lampanyctus_crocodilus
 Min.   :0.9014     Min.   :0.9760     Min.   :0.002127      
 1st Qu.:0.9014     1st Qu.:0.9794     1st Qu.:0.002955      
 Median :0.9014     Median :0.9829     Median :0.023844      
 Mean   :0.9014     Mean   :0.9820     Mean   :0.063790      
 3rd Qu.:0.9014     3rd Qu.:0.9851     3rd Qu.:0.084679      
 Max.   :0.9014     Max.   :0.9872     Max.   :0.205344      
 NA's   :3          NA's   :1                                
 Lampanyctus_macdonaldi Lestidiops_sphyrenoides Lobianchia_gemellarii
 Min.   :0.3235         Min.   :0.8648          Min.   :0.7642       
 1st Qu.:0.3235         1st Qu.:0.9171          1st Qu.:0.8764       
 Median :0.3235         Median :0.9611          Median :0.9206       
 Mean   :0.3235         Mean   :0.9452          Mean   :0.8896       
 3rd Qu.:0.3235         3rd Qu.:0.9893          3rd Qu.:0.9338       
 Max.   :0.3235         Max.   :0.9938          Max.   :0.9529       
 NA's   :3                                                           
 Malacosteus_niger Maulisia_argipalla Maulisia_mauli   Maulisia_microlepis
 Min.   :0.7746    Min.   :0.9082     Min.   :0.5169   Min.   :0.636      
 1st Qu.:0.8230    1st Qu.:0.9143     1st Qu.:0.7431   1st Qu.:0.636      
 Median :0.8714    Median :0.9204     Median :0.9693   Median :0.636      
 Mean   :0.8714    Mean   :0.9204     Mean   :0.8274   Mean   :0.636      
 3rd Qu.:0.9199    3rd Qu.:0.9265     3rd Qu.:0.9827   3rd Qu.:0.636      
 Max.   :0.9683    Max.   :0.9326     Max.   :0.9962   Max.   :0.636      
 NA's   :2         NA's   :2          NA's   :1        NA's   :3          
 Maurolicus_muelleri Melanostigma_atlanticum Melanostomias_bartonbeani
 Min.   :0.1886      Min.   :0.9008          Min.   :0.8833           
 1st Qu.:0.2219      1st Qu.:0.9239          1st Qu.:0.8853           
 Median :0.5207      Median :0.9559          Median :0.8959           
 Mean   :0.5326      Mean   :0.9494          Mean   :0.9099           
 3rd Qu.:0.8314      3rd Qu.:0.9814          3rd Qu.:0.9205           
 Max.   :0.9004      Max.   :0.9851          Max.   :0.9643           
                                                                      
 Myctophum_punctatum Lampanyctus_ater Normichthys_operosus Notoscopelus_kroyeri
 Min.   :0.03938     Min.   :0.4225   Min.   :0.05469      Min.   :0.1010      
 1st Qu.:0.37194     1st Qu.:0.6571   1st Qu.:0.05469      1st Qu.:0.1879      
 Median :0.49303     Median :0.8502   Median :0.05469      Median :0.2641      
 Mean   :0.40881     Mean   :0.7758   Mean   :0.05469      Mean   :0.2624      
 3rd Qu.:0.52991     3rd Qu.:0.9689   3rd Qu.:0.05469      3rd Qu.:0.3385      
 Max.   :0.60979     Max.   :0.9802   Max.   :0.05469      Max.   :0.4204      
                                      NA's   :3                                
 Paralepis_coregonoides Photostylus_pycnopterus Sagamichthys_schnakenbecki
 Min.   :0.9799         Min.   :0.967           Min.   :0.9254            
 1st Qu.:0.9850         1st Qu.:0.967           1st Qu.:0.9518            
 Median :0.9878         Median :0.967           Median :0.9781            
 Mean   :0.9885         Mean   :0.967           Mean   :0.9626            
 3rd Qu.:0.9913         3rd Qu.:0.967           3rd Qu.:0.9812            
 Max.   :0.9986         Max.   :0.967           Max.   :0.9842            
                        NA's   :3               NA's   :1                 
 Searsia_koefoedi Serrivomer_beanii Sigmops_bathyphilus  Stomias_boa    
 Min.   :0.5343   Min.   :0.06388   Min.   :0.8843      Min.   :0.1705  
 1st Qu.:0.5941   1st Qu.:0.30782   1st Qu.:0.9099      1st Qu.:0.1941  
 Median :0.6536   Median :0.43842   Median :0.9355      Median :0.2049  
 Mean   :0.6788   Mean   :0.40540   Mean   :0.9355      Mean   :0.2273  
 3rd Qu.:0.7382   3rd Qu.:0.53599   3rd Qu.:0.9611      3rd Qu.:0.2381  
 Max.   :0.8737   Max.   :0.68088   Max.   :0.9867      Max.   :0.3289  
                                    NA's   :2                           
 Xenodermichthys_copei Notoscopelus_bolini
 Min.   :0.001095      Min.   :0.3286     
 1st Qu.:0.008536      1st Qu.:0.6478     
 Median :0.106440      Median :0.9670     
 Mean   :0.186479      Mean   :0.7558     
 3rd Qu.:0.284382      3rd Qu.:0.9693     
 Max.   :0.531940      Max.   :0.9717     
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
 Length:41          Min.   :0.0000  
 Class :character   1st Qu.:0.0000  
 Mode  :character   Median :0.0000  
                    Mean   :0.2378  
                    3rd Qu.:0.5000  
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
  ggrepel::geom_text_repel(aes(label = species), size=3) +
  scale_color_gradient(high = "#914568", low = "#6BA1B9", "Functional\nDistinctiveness")+
  theme_bw() +
  theme(aspect.ratio = 1)

plot_reg_uniqueness <- ggplot(sp_coord_di_ui, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(aes(color = Ui), size=4) +
  ggrepel::geom_text_repel(aes(label = species), size=3) +
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
local_di_ap <- as.data.frame(sp_local_di[, c(1: 11)])
local_di_ap$depth <- rownames(local_di_ap)
```
:::


### 4.3.2. Plotting rarity
 - __Functional Distinctiveness__

::: {.cell}

```{.r .cell-code}
sp_di_ri <- merge(sp_di, ri, by = "species")
sp_di_ri_ui <- merge(sp_di_ri, sp_ui, by = "species")

plot_dist_ri_reg <-ggplot(sp_di_ri, aes(distinctiveness, Ri)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps=22, size=3) +
  labs(x = "Functional Distinctiveness", y = "Geographical Restrictedness") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_dist_ri_reg
```

::: {.cell-output-display}
![](index_files/figure-html/plot_dist_ri_reg-1.png){width=576}
:::
:::

On this visualization we can clearly see that _Anoplogaster cornuta_ is overall the most distinct species while being quite restricted in terms of depth. On the other hand, the two Myctophidae _Lampanyctus ater_ and _Lobianchia_gemellarii_ are the most functionally common and regionally widespread species (low restrictedness).




::: {.cell}

```{.r .cell-code}
# median depth of species 
median_depth_fish <- depth_distribution%>%
  group_by(species)%>%
  summarise(median_depth= median(depth))

sp_di_ri_depth <- sp_di_ri%>%
  left_join(median_depth_fish)

sp_di_ri_depth_plot <- ggplot(sp_di_ri_depth, aes(distinctiveness, Ri)) +
  geom_point(aes(col=median_depth), size=3) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps=22, size=3) +
  labs(x = "Functional Distinctiveness", y = "Geographical Restrictedness") +
  scale_color_gradient(high = "#00263A", low = "#C5F8FF","Median depth (m)") +
  theme_bw() +
  theme(aspect.ratio = 1)

sp_di_ri_depth_plot
```

::: {.cell-output-display}
![](index_files/figure-html/plot_dist_ri_reg_depth-1.png){width=672}
:::
:::



- __Functional Uniqueness__

::: {.cell}

```{.r .cell-code}
plot_ui_ri_reg <- ggplot(sp_di_ri_ui, aes(Ui, Ri)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps=22, size=3) +
  labs(x = "Functional Uniqueness", y = "Geographical Restrictedness") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_ui_ri_reg
```

::: {.cell-output-display}
![](index_files/figure-html/plot_ui_ri_reg-1.png){width=576}
:::
:::

::: {.cell}

```{.r .cell-code}
sp_di_ri_ui_depth <- sp_di_ri_ui%>%
  left_join(median_depth_fish)

plot_ui_ri_reg_depth <- ggplot(sp_di_ri_ui_depth, aes(Ui, Ri)) +
  geom_point(aes(col=median_depth), size=3) +
  ggrepel::geom_text_repel(aes(label = species), max.overlaps=22, size=3) +
  labs(x = "Functional Uniqueness", y = "Geographical Restrictedness") +
  scale_color_gradient(high = "#00263A", low = "#C5F8FF","Median depth (m)") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_ui_ri_reg_depth
```

::: {.cell-output-display}
![](index_files/figure-html/plot_ui_ri_reg_depth-1.png){width=672}
:::
:::



- plot local scale measurements:

__bathypelagic layer__

::: {.cell}

```{.r .cell-code}
sp_local_di_df <- funrar::matrix_to_stack(
  sp_local_di, value_col = "local_di", row_to_col = "depth",
  col_to_col = "species"
)
sp_local_si_df <- funrar::matrix_to_stack(
  si, value_col = "local_si", row_to_col = "depth", col_to_col = "species"
)

sp_local_di_si <- merge(
  sp_local_di_df, sp_local_si_df, by = c("depth", "species")
)

head(sp_local_di_si)
```

::: {.cell-output .cell-output-stdout}
```
         depth                  species  local_di  local_si
1 Bathypelagic     Anoplogaster_cornuta 0.3767824 0.9469379
2 Bathypelagic         Arctozenus_risso 0.2229648 0.5071105
3 Bathypelagic Argyropelecus_hemigymnus 0.2938755 0.9935434
4 Bathypelagic   Argyropelecus_olfersii 0.2923411 0.5556566
5 Bathypelagic       Bathylagus_euryops 0.2092679 0.2453489
6 Bathypelagic      Benthosema_glaciale 0.2040058 0.1406685
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
