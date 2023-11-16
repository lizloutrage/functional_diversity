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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-b61aa818ad60beb20269" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-b61aa818ad60beb20269">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42"],["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metoclampus","Eurypharynx_pelecanoides","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Normichthys_operosus","Notoscopelus_bolini","Notoscopelus_kroyeri","Paralepis_coregonoides ","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei"],[3,30,17,37,19,20,11,14,30,12,12,7,5,6,4,3,3,9,22,39,21,7,22,5,5,11,4,11,20,9,25,38,20,36,19,8,9,36,30,20,26,38]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-2864be0c0094fbcea6b8" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-2864be0c0094fbcea6b8">{"x":{"filter":"none","vertical":false,"data":[["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Normichthys_operosus","Notoscopelus_bolini","Notoscopelus_kroyeri","Paralepis_coregonoides ","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei"],[0.1870635427569378,0.5846309826745519,0.2528378924337218,0.4592456778322155,0.8073379156515913,0.553876225297698,0.6943542959568116,0.3918502902611463,0.6283997564870827,0.3236216278441371,0.2896732995527421,0.7087200272405964,0.3965409748630725,0.4619095739058715,0.2627730950311468,0.7001620679316158,0.5861245029650884,0.5876906171451259,0.5870177305121238,0.4635401519874494,0.6379293967644859,0.4655204987772433,0.5686707446196964,0.5560047681071145,0.5436602586272192,0.4040027222573614,0.5366700102175092,0.5653194882201393,0.3404604909527202,0.5253244611262143,0.5446404207622028,0.58721677071155,0.5113840280260891,0.6557709453691855,0.3160340663331095,0.6531117613886723,0.5879149224883636,0.5765207995552576,0.3439623137409401,0.7090121921743234,0.4296875],[0.06513101563336203,0.04373517018184282,0.1103521526110715,0.1502130097325588,0.09938039796259482,0.09708002840620254,0.1002020784107539,0.0454502028499367,0.08169052017109087,0.03195944404217845,0.01820101324427989,0.01870641840179216,0.08617787828321036,0.06111392066224541,0.02365859567134613,0.107319044568802,0.09736638359612411,0.04953860514578357,0.07212330160492497,0.05324648475404745,0.03316666671153076,0.06137020103787766,0.0589832944995062,0.08848446584142593,0.0843550572708857,0.05808421042835739,0.0978961923388425,0.04280718124696028,0.02667037338846572,0.102088606810114,0.09612954738570681,0.06117253100231441,0.0829147173874412,0.05113309818745389,0.03048020374468671,0.08748603143506498,0.07579150239724425,0.005392035446476243,0.03401701434877021,0.03544892518496002,0.03],[5842.317456564917,2111.722684686202,529.649932788448,1577.147761649424,215.6744532820656,288.0952119082589,857.141772877457,2308.803855788343,1150.885005261976,1427.556961259949,472.0229634409745,102.2063575036608,509.9151356625916,746.3340129817943,6250.383835528853,1069.928509005572,870.8481183231795,1516.361659863597,1697.767213931643,2223.938773657847,178.4598940858916,1226.901459954854,4216.396258458542,3465.852896151474,2055.911596674011,1384.771960858673,186.8514118522354,78.39976364627458,2537.245636289088,1190.246320535136,1490.110812657505,486.4451319733718,1884.787897717559,208.4731695208822,80.09634693327251,848.5770168784057,1121.886976386014,13856.24507766957,4544.83974684605,660.3541125369107,3128.224808219178],[0.6488731902095241,1.338469099476293,1.419691902006563,1.03165709575236,1.208429987743106,1.157723276407389,1.145308631944953,0.9549389452177165,1.143504719001661,2.198671222745045,2.196516066163295,1.689339435970036,0.944195077723388,1.283292920158411,0.6068674018540213,1.078672179961666,1.246595443731965,1.144873224338048,1.21245598924451,1.162473804130551,2.422986953900589,1.100038150697565,0.8484726822993502,1.230101510957001,1.163701414828532,1.248535562855231,1.285170782096033,1.345688706965169,1.398821013856888,1.230126420256282,1.168410973543925,1.278747111079255,1.23328408850495,1.257275949159544,1.080061200669548,1.215185926388624,1.051275601333645,1.390674506837714,1.023028473395861,2.073736388290534,1.209851822186624],[0.3951426840833095,0.5936920275250842,0.5404276289037069,0.6267609459347112,0.3502518371432042,0.3324025605583356,0.3380289556040331,0.4826993703447641,0.499666832134522,0.7068627312794583,0.7937932689673789,0.492756633741221,0.3551554524927789,0.5829657156202231,0.4721898773185077,0.4712003973315194,0.3956514300993943,0.5670948561072423,0.5763546408927493,0.5744746730161431,0.5520998821851169,0.5212153959103951,0.3530711561087431,0.4631356481458706,0.3738738564459647,0.2036324254692755,0.5855853143839959,0.4857260963559027,0.6176663797525285,0.4937836384577197,0.5383848074193586,0.4896021576606608,0.6766829935960927,0.4245107644843942,0.5624538104272562,0.361697362365493,0.6725516967007241,1.301709739258645,0.6490690083932986,0.4893190215677281,0.4674479166666667],[0.2861375317012913,0.1419299685239746,0.2514373364077059,0.2936861124939084,0.07663827540787548,0.1884618356794901,0.19638076001865,0.198414842989372,0.1723957271341177,0.1144114643871464,0.2040581908463124,0.04491240788019163,0.191587307998245,0.1634947940484265,0.1777783246347302,0.2092942077891755,0.2069838979262741,0.1973450981018806,0.2160878749273041,0.2048825282290511,0.1027761390567869,0.2223712374577787,0.2544654948963843,0.2078438651875588,0.2120154019923103,0.1744403226406842,0.166670163223075,0.03814429068386168,0.1234687560304594,0.1709386276379063,0.2124814802283585,0.2008815666871479,0.1944790792579468,0.1330213734139315,0.08475918421735412,0.1571232489521016,0.1796959703213518,0.1051773540657428,0.2267422887328622,0.08012031757545401,0.1059090909090909],[20.92,9.996666666666666,5.214117647058823,15.0927027027027,8.536315789473685,4.7555,12.47272727272727,15.79642857142857,10.359,10.1375,7.638333333333334,2.771428571428571,6.976,9.282500000000001,27.92,9.563333333333333,9.283333333333333,18.38136363636364,16.44564102564103,25.00333333333333,4.607142857142858,15.45818181818182,22.672,15.524,15.73,21.49,3.591818181818182,4.033,18.11777777777778,11.6064,12.00684210526316,9.629,11.93888888888889,4.898421052631579,6.5525,11.76,10.92333333333333,11.41733333333333,18.1015,9.640000000000001,14],[0.3471179569492232,0.2009929585768647,0.3034423890185704,0.4430016839620673,0.2252652480176319,0.2577329098405489,0.2949444282485461,0.2219516612342632,0.2810060482912339,0.1353122370036637,0.2203047019193976,0.09229259278317947,0.2626215465872482,0.2169720130174164,0.208131053502031,0.3736914296278676,0.3084118074275706,0.2268293066821225,0.2952402466992932,0.2544000525765622,0.179483203158696,0.2690716828129346,0.2291135730880142,0.3581617707887102,0.3386367195769682,0.2602094930995654,0.2861583530888247,0.1289222320266107,0.1414616221329245,0.3106499916582697,0.3367281575818815,0.2484099358389354,0.2934608456754343,0.2491930232735534,0.1610794295053864,0.3035521067765809,0.2693301083783761,0.09565857463839177,0.2463809400095261,0.1408472718663085,0.1672727272727273],[0.496765317595603,0.1206694146969274,0.5465676366743593,0.4638582588424479,0.1803023935359234,0.227186906305348,0.23575289155312,0.1471579276088384,0.2129271582075423,0.1139788839951372,0.1385564389886054,0.06475552609724118,0.2794661893586785,0.1854842390181648,0.1439966534398835,0.1995718556257012,0.1884279714218087,0.1634270563148341,0.2262550151064295,0.1791292477891186,0.07016698379208147,0.2389029851391781,0.1846729123532705,0.2190923414047283,0.2006259493110997,0.2145214178694734,0.2284769602774343,0.08730489814688717,0.1281404978381019,0.2657415518634131,0.2667515421825118,0.1977431521215698,0.2287584626732439,0.09127720302297426,0.148949033138653,0.1648207240422557,0.2089522151774925,0.05817864030760073,0.1460846094753337,0.08296986687063404,0.1513636363636363],[0.327552178795381,0.3886495663795672,0.2307451284370966,0.2772889216093987,0.1987584730572414,0.4427266302964014,0.4605590043104663,0.1754328409583324,0.2744389419969662,0.289631998528789,0.1992742880547905,0.6600484532653362,0.2327118342280331,0.215888746321026,0.2073022595505449,0.2511962839831692,0.2467697101111054,0.2892874845482781,0.320647684200895,0.1887452339268505,0.3583607039010895,0.2710821884027898,0.2070338218789051,0.1986434877697646,0.1907795219830297,0.1746990027397655,0.2251067492779084,0.3125584835447972,0.2233648323381441,0.2811404615012355,0.2636255375035926,0.2851868407331372,0.3015650260524716,0.2229812001799436,0.4749769701497314,0.2183612406444402,0.2594134992787389,0.5005157102849597,0.1973392223033529,0.1543301939549473,0.5674978503869302],[0.4003208996368177,0.236057925483874,0.2779352032788916,0.5401390627099971,0.2630250912769236,0.2999602281286219,0.3283279074114032,0.2369008386437003,0.3197499208694726,0.1525109091904405,0.2296622405613443,0.1305046806357046,0.3359303818851376,0.234931461549684,0.2185105436064756,0.3836208813775061,0.3557612665211258,0.2542559135404931,0.3578735448733034,0.3991766831105948,0.1873220233715019,0.2905759860794116,0.1740874348298388,0.3692829665519732,0.3636636301971131,0.3343165173045963,0.2720666504252247,0.1370431628331831,0.1596798610655336,0.3422495725959262,0.3771845995309003,0.2755915950773311,0.3528001856126906,0.2654937591570808,0.1830307523791412,0.365134201678179,0.3105924160886941,0.1023626617132965,0.2852113862556448,0.1296918739730582,0.1409090909090909],[0.0371234059465991,0.0322419350331208,0.2549015223177563,0.07619150444435517,0.0328441524554353,0.09109791643347125,0.01988857958867888,0.03631916674726406,0.04678411498293825,0.01525793056447875,0.07135857193615111,0.01046142821823197,0.05850109312007883,0.0330170205426,0.01553375842430409,0.06429066383512382,0.04055065658457224,0.02463758734428551,0.03192293778820045,0.01871587454424042,0.01805418174713717,0.03025781637891173,0.02411855326084547,0.07493646258320825,0.03196526524002068,0.01480309413827949,0.1787182752269798,0.04130241912511247,0.01959986641736065,0.05942358687160085,0.05969822531543297,0.02763011114739326,0.04960846559295785,0.06703510034705321,0.02971955680387246,0.04254465702825964,0.04035864302866191,0.01340758519582868,0.08468757530738309,0.02174715733754718,0.0188497679157704],[10.45666666666667,5.042333333333334,3.425882352941176,6.817837837837838,6.301578947368421,3.3485,10.79727272727273,5.527142857142857,5.363,6.194166666666667,2.8375,2.242857142857143,5.86,6.265,12.76,4.013333333333334,6.078888888888889,8.561818181818182,8.955128205128204,13.30904761904762,2.92,8.897727272727273,8.033999999999999,5.81,7.612727272727272,18.175,2.204545454545455,1.3475,4.792222222222223,5.504,5.44,6.7935,6.163888888888889,2.601052631578948,3.58375,5.828888888888889,7.266388888888889,6.264333333333333,3.299,2.037307692307692,5],[0.4441498752678317,0.4511748320752814,0.3558469543404688,0.7818238531938475,0.4724103928052195,0.4676150398984861,0.4469769590756064,0.5484990764909237,0.6234586368613509,0.2551650456937953,0.5434318108133503,0.3116459625644058,0.4068008790692222,0.4314272231538535,0.5671939001428983,0.6326973503856025,0.6361420651390463,0.4410062335674381,0.4759239546486726,0.4509057936924641,0.5980112926718268,0.4076621012576387,0.8268335886150313,0.6409601116629159,0.6344647156330613,0.6389295252984915,0.5480099853470315,0.4902538416315447,0.8479658564847968,0.7034331504327653,0.6754663550372079,0.3681530667791175,0.5244857983331518,0.6313155638870939,0.7766759118958205,0.6195601138458109,0.506573238418478,0.3299632556271487,0.5465635247269044,0.8863608137478909,0.3959090909090909],[0.7176935425650939,0.6214972793973041,0.8378677998173621,0.7271622676008382,0.6759418666886342,0.52169659214019,0.5912844468134186,0.6406218602628342,0.7013777002628838,0.7608914348764781,0.9524117126142014,0.6792564664135844,0.5025961273814956,0.8025189634953906,0.6875363082898668,0.6816922245429579,0.6757727860416617,0.6434772867086079,0.6214480626877658,0.6741368657991837,0.6136384769387926,0.6326776614466724,0.6113076825121152,0.6801791224383178,0.6352660702821236,0.7404149553188488,0.6295496228498633,0.6523412642442598,0.7023241150122855,0.6002872769321257,0.6912700288852882,0.5509132572485328,0.6778850177784417,0.6770228372535668,0.7832793790368987,0.5912461332246149,0.6977929602446417,0.8624986131376594,0.6995258484505198,0.8310676664827777,0.380859375],[1.91753487412749,1.431454025467558,2.139885271329579,1.220534807560166,1.124312726235241,1.615591789989959,1.266547465086961,0.9001426533263226,1.293273756758089,2.160667691433549,1.010216217178132,2.02597438517849,1.6964688060974,1.342729773708559,1.125109185048073,1.005414520892331,1.256182599309575,0.745538622186577,0.915004846827665,0.6972474697559381,1.513782115016354,0.9216188402082974,1.314990184872054,1.051571563080083,1.1351500186845,1.563131800106226,1.403601622773818,1.394534509216571,1.484009375370477,1.155465122079889,1.15501147000044,1.031327676466461,1.378982647187706,0.957153868401671,1.436677037589144,0.9152335515764092,1.4386979519368,3.24426452933186,0.8037016931661495,1.51291183767707,1.185714285714286],["A","A","P","P","A","P","P","P","P","P","P","P","P","A","P","A","P","P","P","P","A","P","A","P","P","A","P","A","P","P","P","P","P","A","P","P","P","A","P","P","A"],["A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","P","A"],["A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","P","A"],["A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A"],["A","P","P","P","P","P","A","P","P","A","A","P","P","A","P","P","P","P","P","P","P","P","A","P","P","P","P","A","A","P","P","P","P","A","P","P","P","P","P","P","P"],["P","A","A","A","A","A","A","P","A","P","P","A","A","P","P","A","A","A","A","P","A","P","P","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","P","A","A"],["P","A","A","A","A","A","A","P","A","P","A","A","A","P","A","A","A","A","A","A","A","A","P","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","P","A"],["A","A","A","A","A","A","A","A","A","A","A","A","A","P","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A"],["P","P","A","A","P","P","P","P","P","A","P","A","A","A","P","A","A","P","P","P","P","P","P","P","P","P","A","A","P","P","P","P","P","P","A","A","P","P","P","P","A"],["C","B","C","C","C","C","C","C","C","A","C","A","C","A","C","C","C","C","C","C","B","C","A","C","C","C","C","B","A","C","C","C","C","A","C","C","C","A","C","A","C"],["1","3","1","1","2","3","2","2","3","1","2","3","1","1","1","3","3","2","3","2","3","2","1","2","2","2","2","3","3","3","2","3","3","3","1","2","2","3","2","2","2"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>eye_size<\/th>\n      <th>orbital_length<\/th>\n      <th>oral_gap_surface<\/th>\n      <th>oral_gape_shape<\/th>\n      <th>oral_gape_position<\/th>\n      <th>lower_jaw_length<\/th>\n      <th>operculum_width<\/th>\n      <th>head_length<\/th>\n      <th>body_depth<\/th>\n      <th>pectoral_fin_position<\/th>\n      <th>pectoral_fin_insertion<\/th>\n      <th>transversal_shape<\/th>\n      <th>caudal_peduncle_min_depth<\/th>\n      <th>dorsal_fin_insertion<\/th>\n      <th>eye_position<\/th>\n      <th>operculum_volume<\/th>\n      <th>photophores_ventral_position<\/th>\n      <th>gland_head<\/th>\n      <th>chin_barbel<\/th>\n      <th>front_barbel<\/th>\n      <th>small_teeth<\/th>\n      <th>large_teeth<\/th>\n      <th>fang_teeth<\/th>\n      <th>retractile_teeth<\/th>\n      <th>internal_teeth<\/th>\n      <th>gill_raker_types<\/th>\n      <th>oral_gape_axis<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## Species * assemblages matrix

::: {.cell}

```{.r .cell-code}
# list of species 
sp_names <- c(rownames(fish_traits), "Nannobrachium_atrum", "Cyclothone")

# species biomass x depth  matrix 2002-2019 ----
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
# depth of all stations 
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-140a2748bae0dce6e466" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-140a2748bae0dce6e466">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[0,0.1,0,0],[3.26,1.41,0.2,2.7],[0.1,0.01,0,0.01],[4.3,1.14,0.08,0.97],[0,2.71,0,0],[0.22,4.15,0.6899999999999999,0.9],[0,0.06,0,0.16],[0.02,0.57,0,0.04],[2.13,0.63,0.51,0.62],[0.04,0.97,0,0.24],[0.11,194.62,0.02,1.44],[0.03,0.05,0,0.09],[0.02,0.05,0,0.37],[0,0.14,0,0.28],[0,0.21,0,0],[0.06,0.03,0,0.03],[6.76,12.8,0.47,10.23],[0,2.23,0,0],[0.01,0.03,0.06,0.11],[0.14,0.49,0.03,0.09],[0.08,0.5,0,0],[0,0.22,0,0.13],[0.01,1.38,0,0.05],[0,0.99,0,0],[0.18,0.5,0.5600000000000001,2.4],[0.03,0.24,0.01,0.12],[0.28,0.27,0.01,0.16],[1.07,1.37,1.21,1.27],[0.07000000000000001,1.69,0.01,0.5600000000000001],[0,5.36,0,0],[4.92,3.12,0.27,2.05],[0,0.06,0,0],[0.04,0.04,0,0.14],[0.31,0.83,0.15,1.15],[0.9,5.64,0.22,1.8],[0,0.24,0,0],[14.19,3.27,0.19,8.199999999999999],[0,0.08,0.43,0.05]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Anoplogaster_cornuta<\/th>\n      <th>Arctozenus_risso<\/th>\n      <th>Argyropelecus_hemigymnus<\/th>\n      <th>Argyropelecus_olfersii<\/th>\n      <th>Bathylagus_euryops<\/th>\n      <th>Benthosema_glaciale<\/th>\n      <th>Bolinichthys_supralateralis<\/th>\n      <th>Borostomias_antarcticus<\/th>\n      <th>Ceratoscopelus_maderensis<\/th>\n      <th>Chauliodus_sloani<\/th>\n      <th>Cyclothone_sp<\/th>\n      <th>Derichthys_serpentinus<\/th>\n      <th>Evermannella_balbo<\/th>\n      <th>Gonostoma_elongatum<\/th>\n      <th>Holtbyrnia_anomala<\/th>\n      <th>Holtbyrnia_macrops<\/th>\n      <th>Lampanyctus_crocodilus<\/th>\n      <th>Lampanyctus_macdonaldi<\/th>\n      <th>Lestidiops_sphyrenoides<\/th>\n      <th>Lobianchia_gemellarii<\/th>\n      <th>Malacosteus_niger<\/th>\n      <th>Maulisia_argipalla<\/th>\n      <th>Maulisia_mauli<\/th>\n      <th>Maulisia_microlepis<\/th>\n      <th>Maurolicus_muelleri<\/th>\n      <th>Melanostigma_atlanticum<\/th>\n      <th>Melanostomias_bartonbeani<\/th>\n      <th>Myctophum_punctatum<\/th>\n      <th>Lampanyctus_ater<\/th>\n      <th>Normichthys_operosus<\/th>\n      <th>Notoscopelus_kroyeri<\/th>\n      <th>Photostylus_pycnopterus<\/th>\n      <th>Sagamichthys_schnakenbecki<\/th>\n      <th>Searsia_koefoedi<\/th>\n      <th>Serrivomer_beanii<\/th>\n      <th>Sigmops_bathyphilus<\/th>\n      <th>Xenodermichthys_copei<\/th>\n      <th>Notoscopelus_bolini<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-5d7c9f424724fc988244" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-5d7c9f424724fc988244">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"],["eye_size","orbital_length","oral_gap_surface","oral_gape_shape","oral_gape_position","lower_jaw_length","operculum_width","head_length","body_depth","pectoral_fin_position","pectoral_fin_insertion","transversal_shape","caudal_peduncle_min_depth","dorsal_fin_insertion","eye_position","operculum_volume","photophores_ventral_position","gland_head","chin_barbel","front_barbel","small_teeth","large_teeth","fang_teeth","retractile_teeth","internal_teeth","gill_raker_types","oral_gape_axis"],["Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","Q","N","N","N","N","N","N","N","N","N","O","O"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>trait_name<\/th>\n      <th>trait_type<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-7f64fa3a64537c7624b1" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-7f64fa3a64537c7624b1">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[0,1,0,0],[1,1,1,1],[1,1,0,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[0,1,0,1],[1,1,0,1],[1,1,1,1],[1,1,0,1],[1,1,1,1],[1,1,0,1],[1,1,0,1],[0,1,0,1],[0,1,0,0],[1,1,0,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[1,1,1,1],[1,1,0,0],[0,1,0,1],[1,1,0,1],[0,1,0,0],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[0,1,0,0],[1,1,0,1],[1,1,1,1],[1,1,1,1],[0,1,0,0],[1,1,1,1],[0,1,1,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Anoplogaster_cornuta<\/th>\n      <th>Arctozenus_risso<\/th>\n      <th>Argyropelecus_hemigymnus<\/th>\n      <th>Argyropelecus_olfersii<\/th>\n      <th>Bathylagus_euryops<\/th>\n      <th>Benthosema_glaciale<\/th>\n      <th>Bolinichthys_supralateralis<\/th>\n      <th>Borostomias_antarcticus<\/th>\n      <th>Ceratoscopelus_maderensis<\/th>\n      <th>Chauliodus_sloani<\/th>\n      <th>Cyclothone_sp<\/th>\n      <th>Derichthys_serpentinus<\/th>\n      <th>Evermannella_balbo<\/th>\n      <th>Gonostoma_elongatum<\/th>\n      <th>Holtbyrnia_anomala<\/th>\n      <th>Holtbyrnia_macrops<\/th>\n      <th>Lampanyctus_crocodilus<\/th>\n      <th>Lampanyctus_macdonaldi<\/th>\n      <th>Lestidiops_sphyrenoides<\/th>\n      <th>Lobianchia_gemellarii<\/th>\n      <th>Malacosteus_niger<\/th>\n      <th>Maulisia_argipalla<\/th>\n      <th>Maulisia_mauli<\/th>\n      <th>Maulisia_microlepis<\/th>\n      <th>Maurolicus_muelleri<\/th>\n      <th>Melanostigma_atlanticum<\/th>\n      <th>Melanostomias_bartonbeani<\/th>\n      <th>Myctophum_punctatum<\/th>\n      <th>Lampanyctus_ater<\/th>\n      <th>Normichthys_operosus<\/th>\n      <th>Notoscopelus_kroyeri<\/th>\n      <th>Photostylus_pycnopterus<\/th>\n      <th>Sagamichthys_schnakenbecki<\/th>\n      <th>Searsia_koefoedi<\/th>\n      <th>Serrivomer_beanii<\/th>\n      <th>Sigmops_bathyphilus<\/th>\n      <th>Xenodermichthys_copei<\/th>\n      <th>Notoscopelus_bolini<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
                            Anoplogaster_cornuta Arctozenus_risso
Arctozenus_risso                           0.352                 
Argyropelecus_hemigymnus                   0.354            0.323
Argyropelecus_olfersii                     0.333            0.328
Bathylagus_euryops                         0.336            0.128
Benthosema_glaciale                        0.370            0.149
Bolinichthys_supralateralis                0.282            0.210
Borostomias_antarcticus                    0.287            0.262
Ceratoscopelus_maderensis                  0.347            0.134
Chauliodus_sloani                          0.365            0.382
Cyclothone_sp                              0.322            0.275
Derichthys_serpentinus                     0.545            0.222
Diaphus_metoclampus                        0.330            0.242
Evermannella_balbo                         0.280            0.298
Gonostoma_elongatum                        0.251            0.268
Holtbyrnia_anomala                         0.356            0.176
Holtbyrnia_macrops                         0.379            0.186
Lampanyctus_ater                           0.320            0.138
Lampanyctus_crocodilus                     0.313            0.138
Lampanyctus_macdonaldi                     0.267            0.229
Lestidiops_sphyrenoides                    0.421            0.078
Lobianchia_gemellarii                      0.258            0.201
Malacosteus_niger                          0.225            0.326
Maulisia_argipalla                         0.311            0.193
Maulisia_mauli                             0.304            0.184
Maulisia_microlepis                        0.278            0.180
Maurolicus_muelleri                        0.414            0.215
Melanostigma_atlanticum                    0.401            0.147
Melanostomias_bartonbeani                  0.387            0.335
Myctophum_punctatum                        0.351            0.153
Normichthys_operosus                       0.313            0.183
Notoscopelus_bolini                        0.354            0.120
Notoscopelus_kroyeri                       0.326            0.134
Paralepis_coregonoides                     0.401            0.174
Photostylus_pycnopterus                    0.404            0.225
Sagamichthys_schnakenbecki                 0.380            0.205
Searsia_koefoedi                           0.316            0.145
Serrivomer_beanii                          0.477            0.183
Sigmops_bathyphilus                        0.282            0.223
Stomias_boa                                0.521            0.311
Xenodermichthys_copei                      0.382            0.159
                            Argyropelecus_hemigymnus Argyropelecus_olfersii
Arctozenus_risso                                                           
Argyropelecus_hemigymnus                                                   
Argyropelecus_olfersii                         0.184                       
Bathylagus_euryops                             0.282                  0.288
Benthosema_glaciale                            0.224                  0.257
Bolinichthys_supralateralis                    0.285                  0.262
Borostomias_antarcticus                        0.357                  0.334
Ceratoscopelus_maderensis                      0.239                  0.209
Chauliodus_sloani                              0.380                  0.434
Cyclothone_sp                                  0.300                  0.351
Derichthys_serpentinus                         0.323                  0.391
Diaphus_metoclampus                            0.138                  0.163
Evermannella_balbo                             0.370                  0.392
Gonostoma_elongatum                            0.316                  0.301
Holtbyrnia_anomala                             0.242                  0.193
Holtbyrnia_macrops                             0.196                  0.166
Lampanyctus_ater                               0.255                  0.227
Lampanyctus_crocodilus                         0.257                  0.213
Lampanyctus_macdonaldi                         0.308                  0.265
Lestidiops_sphyrenoides                        0.358                  0.381
Lobianchia_gemellarii                          0.257                  0.233
Malacosteus_niger                              0.491                  0.428
Maulisia_argipalla                             0.231                  0.169
Maulisia_mauli                                 0.243                  0.179
Maulisia_microlepis                            0.306                  0.277
Maurolicus_muelleri                            0.135                  0.191
Melanostigma_atlanticum                        0.342                  0.372
Melanostomias_bartonbeani                      0.514                  0.490
Myctophum_punctatum                            0.236                  0.188
Normichthys_operosus                           0.212                  0.153
Notoscopelus_bolini                            0.239                  0.248
Notoscopelus_kroyeri                           0.232                  0.197
Paralepis_coregonoides                         0.403                  0.415
Photostylus_pycnopterus                        0.203                  0.209
Sagamichthys_schnakenbecki                     0.207                  0.172
Searsia_koefoedi                               0.220                  0.194
Serrivomer_beanii                              0.442                  0.484
Sigmops_bathyphilus                            0.268                  0.255
Stomias_boa                                    0.446                  0.456
Xenodermichthys_copei                          0.300                  0.296
                            Bathylagus_euryops Benthosema_glaciale
Arctozenus_risso                                                  
Argyropelecus_hemigymnus                                          
Argyropelecus_olfersii                                            
Bathylagus_euryops                                                
Benthosema_glaciale                      0.156                    
Bolinichthys_supralateralis              0.165               0.125
Borostomias_antarcticus                  0.249               0.256
Ceratoscopelus_maderensis                0.132               0.081
Chauliodus_sloani                        0.434               0.444
Cyclothone_sp                            0.262               0.252
Derichthys_serpentinus                   0.287               0.244
Diaphus_metoclampus                      0.194               0.135
Evermannella_balbo                       0.314               0.403
Gonostoma_elongatum                      0.252               0.269
Holtbyrnia_anomala                       0.146               0.174
Holtbyrnia_macrops                       0.164               0.120
Lampanyctus_ater                         0.131               0.132
Lampanyctus_crocodilus                   0.160               0.099
Lampanyctus_macdonaldi                   0.204               0.206
Lestidiops_sphyrenoides                  0.163               0.186
Lobianchia_gemellarii                    0.179               0.158
Malacosteus_niger                        0.335               0.409
Maulisia_argipalla                       0.142               0.125
Maulisia_mauli                           0.131               0.125
Maulisia_microlepis                      0.140               0.203
Maurolicus_muelleri                      0.176               0.125
Melanostigma_atlanticum                  0.215               0.249
Melanostomias_bartonbeani                0.422               0.400
Myctophum_punctatum                      0.152               0.084
Normichthys_operosus                     0.141               0.114
Notoscopelus_bolini                      0.134               0.079
Notoscopelus_kroyeri                     0.151               0.083
Paralepis_coregonoides                   0.204               0.244
Photostylus_pycnopterus                  0.226               0.209
Sagamichthys_schnakenbecki               0.144               0.150
Searsia_koefoedi                         0.118               0.102
Serrivomer_beanii                        0.287               0.318
Sigmops_bathyphilus                      0.212               0.190
Stomias_boa                              0.315               0.358
Xenodermichthys_copei                    0.177               0.228
                            Bolinichthys_supralateralis Borostomias_antarcticus
Arctozenus_risso                                                               
Argyropelecus_hemigymnus                                                       
Argyropelecus_olfersii                                                         
Bathylagus_euryops                                                             
Benthosema_glaciale                                                            
Bolinichthys_supralateralis                                                    
Borostomias_antarcticus                           0.270                        
Ceratoscopelus_maderensis                         0.129                   0.214
Chauliodus_sloani                                 0.394                   0.342
Cyclothone_sp                                     0.229                   0.214
Derichthys_serpentinus                            0.317                   0.400
Diaphus_metoclampus                               0.183                   0.261
Evermannella_balbo                                0.351                   0.310
Gonostoma_elongatum                               0.258                   0.177
Holtbyrnia_anomala                                0.218                   0.310
Holtbyrnia_macrops                                0.163                   0.257
Lampanyctus_ater                                  0.123                   0.165
Lampanyctus_crocodilus                            0.118                   0.210
Lampanyctus_macdonaldi                            0.186                   0.154
Lestidiops_sphyrenoides                           0.268                   0.321
Lobianchia_gemellarii                             0.150                   0.135
Malacosteus_niger                                 0.334                   0.291
Maulisia_argipalla                                0.138                   0.192
Maulisia_mauli                                    0.117                   0.183
Maulisia_microlepis                               0.196                   0.252
Maurolicus_muelleri                               0.191                   0.265
Melanostigma_atlanticum                           0.268                   0.379
Melanostomias_bartonbeani                         0.385                   0.220
Myctophum_punctatum                               0.132                   0.225
Normichthys_operosus                              0.124                   0.207
Notoscopelus_bolini                               0.131                   0.207
Notoscopelus_kroyeri                              0.129                   0.213
Paralepis_coregonoides                            0.256                   0.366
Photostylus_pycnopterus                           0.255                   0.285
Sagamichthys_schnakenbecki                        0.146                   0.233
Searsia_koefoedi                                  0.113                   0.193
Serrivomer_beanii                                 0.369                   0.434
Sigmops_bathyphilus                               0.228                   0.126
Stomias_boa                                       0.380                   0.261
Xenodermichthys_copei                             0.246                   0.292
                            Ceratoscopelus_maderensis Chauliodus_sloani
Arctozenus_risso                                                       
Argyropelecus_hemigymnus                                               
Argyropelecus_olfersii                                                 
Bathylagus_euryops                                                     
Benthosema_glaciale                                                    
Bolinichthys_supralateralis                                            
Borostomias_antarcticus                                                
Ceratoscopelus_maderensis                                              
Chauliodus_sloani                               0.411                  
Cyclothone_sp                                   0.230             0.280
Derichthys_serpentinus                          0.257             0.311
Diaphus_metoclampus                             0.153             0.345
Evermannella_balbo                              0.364             0.214
Gonostoma_elongatum                             0.217             0.354
Holtbyrnia_anomala                              0.123             0.458
Holtbyrnia_macrops                              0.072             0.390
Lampanyctus_ater                                0.097             0.379
Lampanyctus_crocodilus                          0.064             0.428
Lampanyctus_macdonaldi                          0.165             0.381
Lestidiops_sphyrenoides                         0.177             0.382
Lobianchia_gemellarii                           0.126             0.353
Malacosteus_niger                               0.370             0.265
Maulisia_argipalla                              0.076             0.434
Maulisia_mauli                                  0.076             0.428
Maulisia_microlepis                             0.156             0.455
Maurolicus_muelleri                             0.131             0.386
Melanostigma_atlanticum                         0.246             0.322
Melanostomias_bartonbeani                       0.345             0.208
Myctophum_punctatum                             0.040             0.433
Normichthys_operosus                            0.062             0.420
Notoscopelus_bolini                             0.062             0.393
Notoscopelus_kroyeri                            0.041             0.401
Paralepis_coregonoides                          0.216             0.415
Photostylus_pycnopterus                         0.197             0.299
Sagamichthys_schnakenbecki                      0.098             0.380
Searsia_koefoedi                                0.050             0.376
Serrivomer_beanii                               0.304             0.410
Sigmops_bathyphilus                             0.160             0.358
Stomias_boa                                     0.306             0.284
Xenodermichthys_copei                           0.234             0.365
                            Cyclothone_sp Derichthys_serpentinus
Arctozenus_risso                                                
Argyropelecus_hemigymnus                                        
Argyropelecus_olfersii                                          
Bathylagus_euryops                                              
Benthosema_glaciale                                             
Bolinichthys_supralateralis                                     
Borostomias_antarcticus                                         
Ceratoscopelus_maderensis                                       
Chauliodus_sloani                                               
Cyclothone_sp                                                   
Derichthys_serpentinus              0.361                       
Diaphus_metoclampus                 0.279                  0.278
Evermannella_balbo                  0.300                  0.389
Gonostoma_elongatum                 0.205                  0.383
Holtbyrnia_anomala                  0.317                  0.284
Holtbyrnia_macrops                  0.270                  0.237
Lampanyctus_ater                    0.209                  0.274
Lampanyctus_crocodilus              0.250                  0.282
Lampanyctus_macdonaldi              0.196                  0.365
Lestidiops_sphyrenoides             0.271                  0.196
Lobianchia_gemellarii               0.179                  0.332
Malacosteus_niger                   0.347                  0.491
Maulisia_argipalla                  0.223                  0.326
Maulisia_mauli                      0.232                  0.323
Maulisia_microlepis                 0.285                  0.374
Maurolicus_muelleri                 0.248                  0.245
Melanostigma_atlanticum             0.295                  0.177
Melanostomias_bartonbeani           0.297                  0.389
Myctophum_punctatum                 0.247                  0.289
Normichthys_operosus                0.223                  0.316
Notoscopelus_bolini                 0.223                  0.239
Notoscopelus_kroyeri                0.225                  0.273
Paralepis_coregonoides              0.280                  0.293
Photostylus_pycnopterus             0.255                  0.207
Sagamichthys_schnakenbecki          0.263                  0.257
Searsia_koefoedi                    0.205                  0.279
Serrivomer_beanii                   0.396                  0.232
Sigmops_bathyphilus                 0.134                  0.351
Stomias_boa                         0.355                  0.292
Xenodermichthys_copei               0.323                  0.222
                            Diaphus_metoclampus Evermannella_balbo
Arctozenus_risso                                                  
Argyropelecus_hemigymnus                                          
Argyropelecus_olfersii                                            
Bathylagus_euryops                                                
Benthosema_glaciale                                               
Bolinichthys_supralateralis                                       
Borostomias_antarcticus                                           
Ceratoscopelus_maderensis                                         
Chauliodus_sloani                                                 
Cyclothone_sp                                                     
Derichthys_serpentinus                                            
Diaphus_metoclampus                                               
Evermannella_balbo                        0.308                   
Gonostoma_elongatum                       0.236              0.347
Holtbyrnia_anomala                        0.175              0.331
Holtbyrnia_macrops                        0.114              0.335
Lampanyctus_ater                          0.168              0.348
Lampanyctus_crocodilus                    0.172              0.385
Lampanyctus_macdonaldi                    0.218              0.336
Lestidiops_sphyrenoides                   0.292              0.361
Lobianchia_gemellarii                     0.172              0.309
Malacosteus_niger                         0.394              0.225
Maulisia_argipalla                        0.155              0.380
Maulisia_mauli                            0.147              0.374
Maulisia_microlepis                       0.212              0.346
Maurolicus_muelleri                       0.113              0.329
Melanostigma_atlanticum                   0.274              0.268
Melanostomias_bartonbeani                 0.455              0.323
Myctophum_punctatum                       0.151              0.388
Normichthys_operosus                      0.142              0.373
Notoscopelus_bolini                       0.147              0.353
Notoscopelus_kroyeri                      0.148              0.365
Paralepis_coregonoides                    0.334              0.307
Photostylus_pycnopterus                   0.150              0.322
Sagamichthys_schnakenbecki                0.101              0.328
Searsia_koefoedi                          0.130              0.335
Serrivomer_beanii                         0.407              0.412
Sigmops_bathyphilus                       0.215              0.342
Stomias_boa                               0.400              0.402
Xenodermichthys_copei                     0.196              0.319
                            Gonostoma_elongatum Holtbyrnia_anomala
Arctozenus_risso                                                  
Argyropelecus_hemigymnus                                          
Argyropelecus_olfersii                                            
Bathylagus_euryops                                                
Benthosema_glaciale                                               
Bolinichthys_supralateralis                                       
Borostomias_antarcticus                                           
Ceratoscopelus_maderensis                                         
Chauliodus_sloani                                                 
Cyclothone_sp                                                     
Derichthys_serpentinus                                            
Diaphus_metoclampus                                               
Evermannella_balbo                                                
Gonostoma_elongatum                                               
Holtbyrnia_anomala                        0.322                   
Holtbyrnia_macrops                        0.265              0.076
Lampanyctus_ater                          0.164              0.200
Lampanyctus_crocodilus                    0.215              0.157
Lampanyctus_macdonaldi                    0.112              0.252
Lestidiops_sphyrenoides                   0.312              0.212
Lobianchia_gemellarii                     0.141              0.225
Malacosteus_niger                         0.289              0.395
Maulisia_argipalla                        0.198              0.140
Maulisia_mauli                            0.190              0.153
Maulisia_microlepis                       0.204              0.196
Maurolicus_muelleri                       0.277              0.151
Melanostigma_atlanticum                   0.381              0.207
Melanostomias_bartonbeani                 0.336              0.459
Myctophum_punctatum                       0.235              0.132
Normichthys_operosus                      0.216              0.134
Notoscopelus_bolini                       0.219              0.161
Notoscopelus_kroyeri                      0.222              0.145
Paralepis_coregonoides                    0.375              0.229
Photostylus_pycnopterus                   0.227              0.232
Sagamichthys_schnakenbecki                0.246              0.111
Searsia_koefoedi                          0.191              0.165
Serrivomer_beanii                         0.391              0.351
Sigmops_bathyphilus                       0.115              0.245
Stomias_boa                               0.401              0.405
Xenodermichthys_copei                     0.279              0.199
                            Holtbyrnia_macrops Lampanyctus_ater
Arctozenus_risso                                               
Argyropelecus_hemigymnus                                       
Argyropelecus_olfersii                                         
Bathylagus_euryops                                             
Benthosema_glaciale                                            
Bolinichthys_supralateralis                                    
Borostomias_antarcticus                                        
Ceratoscopelus_maderensis                                      
Chauliodus_sloani                                              
Cyclothone_sp                                                  
Derichthys_serpentinus                                         
Diaphus_metoclampus                                            
Evermannella_balbo                                             
Gonostoma_elongatum                                            
Holtbyrnia_anomala                                             
Holtbyrnia_macrops                                             
Lampanyctus_ater                         0.143                 
Lampanyctus_crocodilus                   0.100            0.063
Lampanyctus_macdonaldi                   0.199            0.097
Lestidiops_sphyrenoides                  0.231            0.204
Lobianchia_gemellarii                    0.173            0.080
Malacosteus_niger                        0.402            0.328
Maulisia_argipalla                       0.100            0.095
Maulisia_mauli                           0.095            0.078
Maulisia_microlepis                      0.198            0.148
Maurolicus_muelleri                      0.099            0.157
Melanostigma_atlanticum                  0.220            0.265
Melanostomias_bartonbeani                0.396            0.354
Myctophum_punctatum                      0.079            0.115
Normichthys_operosus                     0.092            0.094
Notoscopelus_bolini                      0.100            0.069
Notoscopelus_kroyeri                     0.082            0.095
Paralepis_coregonoides                   0.261            0.261
Photostylus_pycnopterus                  0.178            0.194
Sagamichthys_schnakenbecki               0.055            0.124
Searsia_koefoedi                         0.106            0.064
Serrivomer_beanii                        0.357            0.305
Sigmops_bathyphilus                      0.208            0.118
Stomias_boa                              0.358            0.313
Xenodermichthys_copei                    0.212            0.189
                            Lampanyctus_crocodilus Lampanyctus_macdonaldi
Arctozenus_risso                                                         
Argyropelecus_hemigymnus                                                 
Argyropelecus_olfersii                                                   
Bathylagus_euryops                                                       
Benthosema_glaciale                                                      
Bolinichthys_supralateralis                                              
Borostomias_antarcticus                                                  
Ceratoscopelus_maderensis                                                
Chauliodus_sloani                                                        
Cyclothone_sp                                                            
Derichthys_serpentinus                                                   
Diaphus_metoclampus                                                      
Evermannella_balbo                                                       
Gonostoma_elongatum                                                      
Holtbyrnia_anomala                                                       
Holtbyrnia_macrops                                                       
Lampanyctus_ater                                                         
Lampanyctus_crocodilus                                                   
Lampanyctus_macdonaldi                       0.125                       
Lestidiops_sphyrenoides                      0.205                  0.297
Lobianchia_gemellarii                        0.092                  0.066
Malacosteus_niger                            0.364                  0.312
Maulisia_argipalla                           0.082                  0.133
Maulisia_mauli                               0.068                  0.120
Maulisia_microlepis                          0.165                  0.154
Maurolicus_muelleri                          0.161                  0.223
Melanostigma_atlanticum                      0.268                  0.357
Melanostomias_bartonbeani                    0.366                  0.341
Myctophum_punctatum                          0.068                  0.177
Normichthys_operosus                         0.082                  0.144
Notoscopelus_bolini                          0.057                  0.151
Notoscopelus_kroyeri                         0.046                  0.148
Paralepis_coregonoides                       0.259                  0.335
Photostylus_pycnopterus                      0.236                  0.271
Sagamichthys_schnakenbecki                   0.121                  0.182
Searsia_koefoedi                             0.073                  0.132
Serrivomer_beanii                            0.311                  0.390
Sigmops_bathyphilus                          0.155                  0.091
Stomias_boa                                  0.358                  0.388
Xenodermichthys_copei                        0.230                  0.265
                            Lestidiops_sphyrenoides Lobianchia_gemellarii
Arctozenus_risso                                                         
Argyropelecus_hemigymnus                                                 
Argyropelecus_olfersii                                                   
Bathylagus_euryops                                                       
Benthosema_glaciale                                                      
Bolinichthys_supralateralis                                              
Borostomias_antarcticus                                                  
Ceratoscopelus_maderensis                                                
Chauliodus_sloani                                                        
Cyclothone_sp                                                            
Derichthys_serpentinus                                                   
Diaphus_metoclampus                                                      
Evermannella_balbo                                                       
Gonostoma_elongatum                                                      
Holtbyrnia_anomala                                                       
Holtbyrnia_macrops                                                       
Lampanyctus_ater                                                         
Lampanyctus_crocodilus                                                   
Lampanyctus_macdonaldi                                                   
Lestidiops_sphyrenoides                                                  
Lobianchia_gemellarii                         0.266                      
Malacosteus_niger                             0.367                 0.302
Maulisia_argipalla                            0.246                 0.117
Maulisia_mauli                                0.237                 0.100
Maulisia_microlepis                           0.222                 0.175
Maurolicus_muelleri                           0.241                 0.182
Melanostigma_atlanticum                       0.147                 0.322
Melanostomias_bartonbeani                     0.358                 0.337
Myctophum_punctatum                           0.201                 0.133
Normichthys_operosus                          0.233                 0.112
Notoscopelus_bolini                           0.178                 0.103
Notoscopelus_kroyeri                          0.198                 0.119
Paralepis_coregonoides                        0.179                 0.310
Photostylus_pycnopterus                       0.226                 0.240
Sagamichthys_schnakenbecki                    0.244                 0.159
Searsia_koefoedi                              0.206                 0.095
Serrivomer_beanii                             0.215                 0.366
Sigmops_bathyphilus                           0.273                 0.084
Stomias_boa                                   0.270                 0.371
Xenodermichthys_copei                         0.200                 0.232
                            Malacosteus_niger Maulisia_argipalla Maulisia_mauli
Arctozenus_risso                                                               
Argyropelecus_hemigymnus                                                       
Argyropelecus_olfersii                                                         
Bathylagus_euryops                                                             
Benthosema_glaciale                                                            
Bolinichthys_supralateralis                                                    
Borostomias_antarcticus                                                        
Ceratoscopelus_maderensis                                                      
Chauliodus_sloani                                                              
Cyclothone_sp                                                                  
Derichthys_serpentinus                                                         
Diaphus_metoclampus                                                            
Evermannella_balbo                                                             
Gonostoma_elongatum                                                            
Holtbyrnia_anomala                                                             
Holtbyrnia_macrops                                                             
Lampanyctus_ater                                                               
Lampanyctus_crocodilus                                                         
Lampanyctus_macdonaldi                                                         
Lestidiops_sphyrenoides                                                        
Lobianchia_gemellarii                                                          
Malacosteus_niger                                                              
Maulisia_argipalla                      0.345                                  
Maulisia_mauli                          0.326              0.030               
Maulisia_microlepis                     0.316              0.145          0.127
Maurolicus_muelleri                     0.432              0.134          0.141
Melanostigma_atlanticum                 0.362              0.307          0.303
Melanostomias_bartonbeani               0.217              0.376          0.374
Myctophum_punctatum                     0.378              0.072          0.076
Normichthys_operosus                    0.360              0.036          0.043
Notoscopelus_bolini                     0.367              0.108          0.097
Notoscopelus_kroyeri                    0.381              0.074          0.076
Paralepis_coregonoides                  0.315              0.261          0.268
Photostylus_pycnopterus                 0.410              0.225          0.219
Sagamichthys_schnakenbecki              0.390              0.092          0.084
Searsia_koefoedi                        0.349              0.076          0.067
Serrivomer_beanii                       0.437              0.352          0.355
Sigmops_bathyphilus                     0.313              0.126          0.139
Stomias_boa                             0.323              0.328          0.324
Xenodermichthys_copei                   0.393              0.233          0.227
                            Maulisia_microlepis Maurolicus_muelleri
Arctozenus_risso                                                   
Argyropelecus_hemigymnus                                           
Argyropelecus_olfersii                                             
Bathylagus_euryops                                                 
Benthosema_glaciale                                                
Bolinichthys_supralateralis                                        
Borostomias_antarcticus                                            
Ceratoscopelus_maderensis                                          
Chauliodus_sloani                                                  
Cyclothone_sp                                                      
Derichthys_serpentinus                                             
Diaphus_metoclampus                                                
Evermannella_balbo                                                 
Gonostoma_elongatum                                                
Holtbyrnia_anomala                                                 
Holtbyrnia_macrops                                                 
Lampanyctus_ater                                                   
Lampanyctus_crocodilus                                             
Lampanyctus_macdonaldi                                             
Lestidiops_sphyrenoides                                            
Lobianchia_gemellarii                                              
Malacosteus_niger                                                  
Maulisia_argipalla                                                 
Maulisia_mauli                                                     
Maulisia_microlepis                                                
Maurolicus_muelleri                       0.224                    
Melanostigma_atlanticum                   0.293               0.224
Melanostomias_bartonbeani                 0.406               0.431
Myctophum_punctatum                       0.171               0.132
Normichthys_operosus                      0.156               0.125
Notoscopelus_bolini                       0.180               0.151
Notoscopelus_kroyeri                      0.163               0.132
Paralepis_coregonoides                    0.263               0.283
Photostylus_pycnopterus                   0.256               0.165
Sagamichthys_schnakenbecki                0.187               0.091
Searsia_koefoedi                          0.139               0.117
Serrivomer_beanii                         0.327               0.395
Sigmops_bathyphilus                       0.199               0.195
Stomias_boa                               0.368               0.356
Xenodermichthys_copei                     0.213               0.212
                            Melanostigma_atlanticum Melanostomias_bartonbeani
Arctozenus_risso                                                             
Argyropelecus_hemigymnus                                                     
Argyropelecus_olfersii                                                       
Bathylagus_euryops                                                           
Benthosema_glaciale                                                          
Bolinichthys_supralateralis                                                  
Borostomias_antarcticus                                                      
Ceratoscopelus_maderensis                                                    
Chauliodus_sloani                                                            
Cyclothone_sp                                                                
Derichthys_serpentinus                                                       
Diaphus_metoclampus                                                          
Evermannella_balbo                                                           
Gonostoma_elongatum                                                          
Holtbyrnia_anomala                                                           
Holtbyrnia_macrops                                                           
Lampanyctus_ater                                                             
Lampanyctus_crocodilus                                                       
Lampanyctus_macdonaldi                                                       
Lestidiops_sphyrenoides                                                      
Lobianchia_gemellarii                                                        
Malacosteus_niger                                                            
Maulisia_argipalla                                                           
Maulisia_mauli                                                               
Maulisia_microlepis                                                          
Maurolicus_muelleri                                                          
Melanostigma_atlanticum                                                      
Melanostomias_bartonbeani                     0.353                          
Myctophum_punctatum                           0.267                     0.356
Normichthys_operosus                          0.297                     0.378
Notoscopelus_bolini                           0.238                     0.363
Notoscopelus_kroyeri                          0.254                     0.349
Paralepis_coregonoides                        0.173                     0.341
Photostylus_pycnopterus                       0.223                     0.375
Sagamichthys_schnakenbecki                    0.244                     0.415
Searsia_koefoedi                              0.264                     0.364
Serrivomer_beanii                             0.276                     0.414
Sigmops_bathyphilus                           0.336                     0.301
Stomias_boa                                   0.355                     0.187
Xenodermichthys_copei                         0.187                     0.424
                            Myctophum_punctatum Normichthys_operosus
Arctozenus_risso                                                    
Argyropelecus_hemigymnus                                            
Argyropelecus_olfersii                                              
Bathylagus_euryops                                                  
Benthosema_glaciale                                                 
Bolinichthys_supralateralis                                         
Borostomias_antarcticus                                             
Ceratoscopelus_maderensis                                           
Chauliodus_sloani                                                   
Cyclothone_sp                                                       
Derichthys_serpentinus                                              
Diaphus_metoclampus                                                 
Evermannella_balbo                                                  
Gonostoma_elongatum                                                 
Holtbyrnia_anomala                                                  
Holtbyrnia_macrops                                                  
Lampanyctus_ater                                                    
Lampanyctus_crocodilus                                              
Lampanyctus_macdonaldi                                              
Lestidiops_sphyrenoides                                             
Lobianchia_gemellarii                                               
Malacosteus_niger                                                   
Maulisia_argipalla                                                  
Maulisia_mauli                                                      
Maulisia_microlepis                                                 
Maurolicus_muelleri                                                 
Melanostigma_atlanticum                                             
Melanostomias_bartonbeani                                           
Myctophum_punctatum                                                 
Normichthys_operosus                      0.046                     
Notoscopelus_bolini                       0.074                0.103
Notoscopelus_kroyeri                      0.046                0.062
Paralepis_coregonoides                    0.240                0.265
Photostylus_pycnopterus                   0.214                0.203
Sagamichthys_schnakenbecki                0.102                0.095
Searsia_koefoedi                          0.083                0.061
Serrivomer_beanii                         0.329                0.350
Sigmops_bathyphilus                       0.176                0.144
Stomias_boa                               0.333                0.322
Xenodermichthys_copei                     0.238                0.237
                            Notoscopelus_bolini Notoscopelus_kroyeri
Arctozenus_risso                                                    
Argyropelecus_hemigymnus                                            
Argyropelecus_olfersii                                              
Bathylagus_euryops                                                  
Benthosema_glaciale                                                 
Bolinichthys_supralateralis                                         
Borostomias_antarcticus                                             
Ceratoscopelus_maderensis                                           
Chauliodus_sloani                                                   
Cyclothone_sp                                                       
Derichthys_serpentinus                                              
Diaphus_metoclampus                                                 
Evermannella_balbo                                                  
Gonostoma_elongatum                                                 
Holtbyrnia_anomala                                                  
Holtbyrnia_macrops                                                  
Lampanyctus_ater                                                    
Lampanyctus_crocodilus                                              
Lampanyctus_macdonaldi                                              
Lestidiops_sphyrenoides                                             
Lobianchia_gemellarii                                               
Malacosteus_niger                                                   
Maulisia_argipalla                                                  
Maulisia_mauli                                                      
Maulisia_microlepis                                                 
Maurolicus_muelleri                                                 
Melanostigma_atlanticum                                             
Melanostomias_bartonbeani                                           
Myctophum_punctatum                                                 
Normichthys_operosus                                                
Notoscopelus_bolini                                                 
Notoscopelus_kroyeri                      0.067                     
Paralepis_coregonoides                    0.228                0.249
Photostylus_pycnopterus                   0.214                0.213
Sagamichthys_schnakenbecki                0.127                0.117
Searsia_koefoedi                          0.074                0.052
Serrivomer_beanii                         0.286                0.294
Sigmops_bathyphilus                       0.166                0.151
Stomias_boa                               0.324                0.341
Xenodermichthys_copei                     0.204                0.231
                            Paralepis_coregonoides  Photostylus_pycnopterus
Arctozenus_risso                                                           
Argyropelecus_hemigymnus                                                   
Argyropelecus_olfersii                                                     
Bathylagus_euryops                                                         
Benthosema_glaciale                                                        
Bolinichthys_supralateralis                                                
Borostomias_antarcticus                                                    
Ceratoscopelus_maderensis                                                  
Chauliodus_sloani                                                          
Cyclothone_sp                                                              
Derichthys_serpentinus                                                     
Diaphus_metoclampus                                                        
Evermannella_balbo                                                         
Gonostoma_elongatum                                                        
Holtbyrnia_anomala                                                         
Holtbyrnia_macrops                                                         
Lampanyctus_ater                                                           
Lampanyctus_crocodilus                                                     
Lampanyctus_macdonaldi                                                     
Lestidiops_sphyrenoides                                                    
Lobianchia_gemellarii                                                      
Malacosteus_niger                                                          
Maulisia_argipalla                                                         
Maulisia_mauli                                                             
Maulisia_microlepis                                                        
Maurolicus_muelleri                                                        
Melanostigma_atlanticum                                                    
Melanostomias_bartonbeani                                                  
Myctophum_punctatum                                                        
Normichthys_operosus                                                       
Notoscopelus_bolini                                                        
Notoscopelus_kroyeri                                                       
Paralepis_coregonoides                                                     
Photostylus_pycnopterus                       0.335                        
Sagamichthys_schnakenbecki                    0.269                   0.170
Searsia_koefoedi                              0.255                   0.184
Serrivomer_beanii                             0.305                   0.337
Sigmops_bathyphilus                           0.305                   0.224
Stomias_boa                                   0.352                   0.308
Xenodermichthys_copei                         0.311                   0.159
                            Sagamichthys_schnakenbecki Searsia_koefoedi
Arctozenus_risso                                                       
Argyropelecus_hemigymnus                                               
Argyropelecus_olfersii                                                 
Bathylagus_euryops                                                     
Benthosema_glaciale                                                    
Bolinichthys_supralateralis                                            
Borostomias_antarcticus                                                
Ceratoscopelus_maderensis                                              
Chauliodus_sloani                                                      
Cyclothone_sp                                                          
Derichthys_serpentinus                                                 
Diaphus_metoclampus                                                    
Evermannella_balbo                                                     
Gonostoma_elongatum                                                    
Holtbyrnia_anomala                                                     
Holtbyrnia_macrops                                                     
Lampanyctus_ater                                                       
Lampanyctus_crocodilus                                                 
Lampanyctus_macdonaldi                                                 
Lestidiops_sphyrenoides                                                
Lobianchia_gemellarii                                                  
Malacosteus_niger                                                      
Maulisia_argipalla                                                     
Maulisia_mauli                                                         
Maulisia_microlepis                                                    
Maurolicus_muelleri                                                    
Melanostigma_atlanticum                                                
Melanostomias_bartonbeani                                              
Myctophum_punctatum                                                    
Normichthys_operosus                                                   
Notoscopelus_bolini                                                    
Notoscopelus_kroyeri                                                   
Paralepis_coregonoides                                                 
Photostylus_pycnopterus                                                
Sagamichthys_schnakenbecki                                             
Searsia_koefoedi                                 0.102                 
Serrivomer_beanii                                0.380            0.304
Sigmops_bathyphilus                              0.190            0.131
Stomias_boa                                      0.338            0.303
Xenodermichthys_copei                            0.182            0.217
                            Serrivomer_beanii Sigmops_bathyphilus Stomias_boa
Arctozenus_risso                                                             
Argyropelecus_hemigymnus                                                     
Argyropelecus_olfersii                                                       
Bathylagus_euryops                                                           
Benthosema_glaciale                                                          
Bolinichthys_supralateralis                                                  
Borostomias_antarcticus                                                      
Ceratoscopelus_maderensis                                                    
Chauliodus_sloani                                                            
Cyclothone_sp                                                                
Derichthys_serpentinus                                                       
Diaphus_metoclampus                                                          
Evermannella_balbo                                                           
Gonostoma_elongatum                                                          
Holtbyrnia_anomala                                                           
Holtbyrnia_macrops                                                           
Lampanyctus_ater                                                             
Lampanyctus_crocodilus                                                       
Lampanyctus_macdonaldi                                                       
Lestidiops_sphyrenoides                                                      
Lobianchia_gemellarii                                                        
Malacosteus_niger                                                            
Maulisia_argipalla                                                           
Maulisia_mauli                                                               
Maulisia_microlepis                                                          
Maurolicus_muelleri                                                          
Melanostigma_atlanticum                                                      
Melanostomias_bartonbeani                                                    
Myctophum_punctatum                                                          
Normichthys_operosus                                                         
Notoscopelus_bolini                                                          
Notoscopelus_kroyeri                                                         
Paralepis_coregonoides                                                       
Photostylus_pycnopterus                                                      
Sagamichthys_schnakenbecki                                                   
Searsia_koefoedi                                                             
Serrivomer_beanii                                                            
Sigmops_bathyphilus                     0.369                                
Stomias_boa                             0.370               0.362            
Xenodermichthys_copei                   0.264               0.251       0.384
```
:::
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
pcoa_1d      0.129
pcoa_2d      0.071
pcoa_3d      0.044
pcoa_4d      0.028
pcoa_5d      0.020
pcoa_6d      0.015
pcoa_7d      0.015
pcoa_8d      0.016
pcoa_9d      0.017
pcoa_10d     0.019
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

# fish_tr_faxes <- mFD::traits.faxes.cor(
#   sp_tr          = fish_traits, 
#   sp_faxes_coord = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")], 
#   plot           = F)
# 
# ## Print traits with significant effect ----
# fish_tr_faxes$"tr_faxes_stat"[which(fish_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]
```
:::


## 2.4 Plotting the selected functional space and position of species

::: {.cell}

```{.r .cell-code}
#sp_faxes_coord_fish <- fspaces_quality_fish$"details_fspaces"$"sp_pc_coord"
# 
# big_plot <- mFD::funct.space.plot(
#   sp_faxes_coord  = sp_faxes_coord_fish[ , c("PC1", "PC2", "PC3", "PC4")],
#   faxes           = c("PC1", "PC2", "PC3", "PC4"),
#   name_file       = NULL,
#   faxes_nm        = NULL,
#   range_faxes     = c(NA, NA),
#   plot_ch         = TRUE,
#   plot_vertices   = TRUE,
#   plot_sp_nm      = NULL,
#   check_input     = TRUE)
# 
# big_plot$"patchwork"
```
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
htmltools::tagList(DT::datatable(fd_ind_values_fish))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-7218e881fc4a8599bbea" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-7218e881fc4a8599bbea">{"x":{"filter":"none","vertical":false,"data":[["Upper mesopelagic","Bathypelagic","Epipelagic","Lower mesopelagic"],[26,38,18,29],[0.3819164720301633,0.2243253503720049,0.2446625873605787,0.3886067466899954],[0.6012988578872813,0.8789237490675231,0.0754571432815377,0.5050769961744795],[0.5790784216741277,0.477576253963031,0.6319059400530773,0.6149599312212776],[0.3500983697751718,0.31322282063433,0.3168508413427947,0.3450784782052695],[-0.04771379074690088,0.04770084145993005,-0.06521922096268024,-0.03773467190579252],[-0.02855512352540044,0.04358899021220848,-0.02642033128167787,-0.02863067280483439],[0.004495240490256234,0.00297892635947409,-0.02722190067401664,-0.01081275021599532],[-0.01966683219535953,0.03417990419712141,0.01266014702671997,-0.01078836195960942]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sp_richn<\/th>\n      <th>fdis<\/th>\n      <th>fric<\/th>\n      <th>fdiv<\/th>\n      <th>fspe<\/th>\n      <th>fide_PC1<\/th>\n      <th>fide_PC2<\/th>\n      <th>fide_PC3<\/th>\n      <th>fide_PC4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
![](index_files/figure-html/plot_FRic-1.png){width=1056}
:::
:::


### __FDiv Functional Divergence__

- the proportion of the biomass supported by the species with the most extreme functional traits i.e. the ones located close to the edge of the convex-hull filled by the assemblage

::: {.cell}

```{.r .cell-code}
plots_alpha$"fdiv"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FDiv-1.png){width=1056}
:::
:::


### __FSpe Functional Specialization__

- the biomass weighted mean distance to the mean position of species from the global pool (present in all assemblages).

::: {.cell}

```{.r .cell-code}
plots_alpha$"fspe"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FSpe-1.png){width=1056}
:::
:::


### __FDis Functional Dispersion__

- the biomass weighted deviation of species traits values from the center of the functional space filled by the assemblage i.e. the biomass-weighted mean distance to the biomass-weighted mean trait values of the assemblage.


::: {.cell}

```{.r .cell-code}
plots_alpha$"fdis"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FDis-1.png){width=1056}
:::
:::


### __FIde Functional Identity__
- the mean traits values for the assemblage. FIde is always computed when FDis is computed.


::: {.cell}

```{.r .cell-code}
plots_alpha$"fide"$"patchwork"
```

::: {.cell-output-display}
![](index_files/figure-html/plot_FIde-1.png){width=1056}
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
Bathypelagic              0.3158691                        
Epipelagic                0.8745098    0.9141482           
Lower mesopelagic         0.1820053    0.4253461  0.8506027

$jac_turn
                  Upper mesopelagic Bathypelagic   Epipelagic
Bathypelagic           7.006867e-16                          
Epipelagic             0.000000e+00 0.000000e+00             
Lower mesopelagic      2.838112e-02 4.170870e-15 0.000000e+00

$jac_nest
                  Upper mesopelagic Bathypelagic Epipelagic
Bathypelagic              0.3158691                        
Epipelagic                0.8745098    0.9141482           
Lower mesopelagic         0.1536242    0.4253461  0.8506027
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
       0.68413086        1.00000000        0.08585175        0.57465394 
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
 [1] "Serrivomer_beanii"         "Borostomias_antarcticus"  
 [3] "Melanostigma_atlanticum"   "Argyropelecus_hemigymnus" 
 [5] "Myctophum_punctatum"       "Malacosteus_niger"        
 [7] "Lestidiops_sphyrenoides"   "Lampanyctus_crocodilus"   
 [9] "Benthosema_glaciale"       "Maulisia_mauli"           
[11] "Arctozenus_risso"          "Maurolicus_muelleri"      
[13] "Lobianchia_gemellarii"     "Xenodermichthys_copei"    
[15] "Chauliodus_sloani"         "Evermannella_balbo"       
[17] "Derichthys_serpentinus"    "Melanostomias_bartonbeani"
[19] "Argyropelecus_olfersii"   

$Bathypelagic
 [1] "Borostomias_antarcticus"   "Argyropelecus_hemigymnus" 
 [3] "Myctophum_punctatum"       "Lampanyctus_macdonaldi"   
 [5] "Serrivomer_beanii"         "Photostylus_pycnopterus"  
 [7] "Maurolicus_muelleri"       "Maulisia_argipalla"       
 [9] "Benthosema_glaciale"       "Holtbyrnia_anomala"       
[11] "Malacosteus_niger"         "Lampanyctus_crocodilus"   
[13] "Lestidiops_sphyrenoides"   "Melanostigma_atlanticum"  
[15] "Chauliodus_sloani"         "Anoplogaster_cornuta"     
[17] "Evermannella_balbo"        "Maulisia_microlepis"      
[19] "Derichthys_serpentinus"    "Melanostomias_bartonbeani"
[21] "Argyropelecus_olfersii"   

$Epipelagic
 [1] "Maurolicus_muelleri"       "Xenodermichthys_copei"    
 [3] "Lestidiops_sphyrenoides"   "Cyclothone_sp"            
 [5] "Arctozenus_risso"          "Benthosema_glaciale"      
 [7] "Lampanyctus_crocodilus"    "Myctophum_punctatum"      
 [9] "Melanostigma_atlanticum"   "Lobianchia_gemellarii"    
[11] "Melanostomias_bartonbeani" "Serrivomer_beanii"        
[13] "Argyropelecus_olfersii"   

$`Lower mesopelagic`
 [1] "Serrivomer_beanii"           "Borostomias_antarcticus"    
 [3] "Melanostigma_atlanticum"     "Argyropelecus_hemigymnus"   
 [5] "Myctophum_punctatum"         "Gonostoma_elongatum"        
 [7] "Xenodermichthys_copei"       "Lestidiops_sphyrenoides"    
 [9] "Lampanyctus_crocodilus"      "Benthosema_glaciale"        
[11] "Arctozenus_risso"            "Maulisia_argipalla"         
[13] "Bolinichthys_supralateralis" "Maurolicus_muelleri"        
[15] "Maulisia_mauli"              "Chauliodus_sloani"          
[17] "Evermannella_balbo"          "Derichthys_serpentinus"     
[19] "Melanostomias_bartonbeani"   "Argyropelecus_olfersii"     
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
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f571907a19eeef4c6313" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-f571907a19eeef4c6313">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41"],["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Diaphus_metoclampus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Normichthys_operosus","Notoscopelus_bolini","Notoscopelus_kroyeri","Paralepis_coregonoides ","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Stomias_boa","Xenodermichthys_copei"],[0.3474235353638858,0.2128276334028571,0.2904536539272046,0.281954274600248,0.2112061681400249,0.2059299479907813,0.2205297119552489,0.2577768477533011,0.1777316495178109,0.3723049947093757,0.2652278878309365,0.3063502371549318,0.2207148838457942,0.3384078595429094,0.2589582420556513,0.2288919354769615,0.1958822069078748,0.1821230118425658,0.1879982872912118,0.2235021270463685,0.2505910986598637,0.1985837149946914,0.3526657787248053,0.1882707105862882,0.1840244217418455,0.2313766860693964,0.2149543116317324,0.2769516524079481,0.3635239957135539,0.1885879713453348,0.1843343836386956,0.1834697008233927,0.1806486740725788,0.2883896716641033,0.2420676130650323,0.1970618319096552,0.1737487211100869,0.3496492039658237,0.2177715131682252,0.3482488978131095,0.2528305395603939]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>distinctiveness<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
t = 23.765, df = 39, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9388754 0.9824797
sample estimates:
      cor 
0.9671647 
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
0.03035540 0.04049332 0.05202209 0.06331655 0.07752888 0.08742419 0.11940348 
       70%        80%        90%       100% 
0.13493191 0.15855312 0.20818342 0.22497248 
```
:::

```{.r .cell-code}
htmltools::tagList(DT::datatable(sp_ui))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-d043e4b12bff5d0f7aac" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-d043e4b12bff5d0f7aac">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38"],["Anoplogaster_cornuta","Arctozenus_risso","Argyropelecus_hemigymnus","Argyropelecus_olfersii","Bathylagus_euryops","Benthosema_glaciale","Bolinichthys_supralateralis","Borostomias_antarcticus","Ceratoscopelus_maderensis","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Evermannella_balbo","Gonostoma_elongatum","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_crocodilus","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Maurolicus_muelleri","Melanostigma_atlanticum","Melanostomias_bartonbeani","Myctophum_punctatum","Lampanyctus_ater","Normichthys_operosus","Notoscopelus_kroyeri","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Searsia_koefoedi","Serrivomer_beanii","Sigmops_bathyphilus","Xenodermichthys_copei","Notoscopelus_bolini"],[0.2249724789440367,0.07780048982062067,0.1350846448701017,0.1528126418836971,0.1176825226682185,0.07869585498402229,0.1129921829454625,0.1262873252148768,0.04049332163604825,0.2081834209388946,0.1335572841772924,0.1770975056328812,0.2140763266869283,0.1117001739383256,0.07644244905429304,0.0552024912524502,0.04563773832466766,0.06578292242973674,0.07780048982062067,0.06578292242973674,0.2167307310652254,0.03035540037384281,0.03035540037384281,0.1274434182392546,0.09120983712936397,0.1469688850096205,0.2081834209388946,0.04049332163604825,0.06304250442540542,0.03626914731113035,0.04066738670835385,0.1585531237947687,0.0552024912524502,0.04990182369327653,0.1827444371373168,0.08363853683493312,0.1585531237947687,0.05724982792289757]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>Ui<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
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
                  Anoplogaster_cornuta Arctozenus_risso
Upper mesopelagic                   NA        0.1741345
Bathypelagic                 0.3265944        0.2531392
Epipelagic                          NA        0.1565857
Lower mesopelagic                   NA        0.1700968
                  Argyropelecus_hemigymnus Argyropelecus_olfersii
Upper mesopelagic                0.2712219              0.2651073
Bathypelagic                     0.2959860              0.3325026
Epipelagic                              NA              0.2349658
Lower mesopelagic                0.2722708              0.2648721
                  Bathylagus_euryops Benthosema_glaciale
Upper mesopelagic                 NA           0.1740355
Bathypelagic                0.245879           0.2348102
Epipelagic                        NA           0.1182136
Lower mesopelagic                 NA           0.1650842
                  Bolinichthys_supralateralis Borostomias_antarcticus
Upper mesopelagic                          NA               0.2649623
Bathypelagic                        0.2186404               0.2217742
Epipelagic                                 NA                      NA
Lower mesopelagic                   0.1893132               0.2545473
                  Ceratoscopelus_maderensis Chauliodus_sloani
Upper mesopelagic                 0.1593837         0.3937305
Bathypelagic                      0.2092825         0.3079344
Epipelagic                        0.0942359                NA
Lower mesopelagic                 0.1441832         0.3920527
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
1            Arctozenus_risso 0.07780049
2    Argyropelecus_hemigymnus 0.13508464
3      Argyropelecus_olfersii 0.16568529
4         Benthosema_glaciale 0.08137893
5     Borostomias_antarcticus 0.13458900
6   Ceratoscopelus_maderensis 0.04049332
7           Chauliodus_sloani 0.20818342
8               Cyclothone_sp 0.17904425
9      Derichthys_serpentinus 0.17709751
10         Evermannella_balbo 0.21407633
11         Holtbyrnia_macrops 0.05520249
12     Lampanyctus_crocodilus 0.04563774
13    Lestidiops_sphyrenoides 0.07780049
14      Lobianchia_gemellarii 0.07992847
15          Malacosteus_niger 0.21673073
16             Maulisia_mauli 0.06694465
17        Maurolicus_muelleri 0.09120984
18    Melanostigma_atlanticum 0.14696889
19  Melanostomias_bartonbeani 0.20818342
20        Myctophum_punctatum 0.04049332
21           Lampanyctus_ater 0.06304250
22       Notoscopelus_kroyeri 0.04066739
23 Sagamichthys_schnakenbecki 0.05520249
24           Searsia_koefoedi 0.04990182
25          Serrivomer_beanii 0.18274444
26      Xenodermichthys_copei 0.15913755
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
19  Melanostomias_bartonbeani 0.2081834 Upper mesopelagic
53  Melanostomias_bartonbeani 0.2081834      Bathypelagic
75  Melanostomias_bartonbeani 0.2966495        Epipelagic
103 Melanostomias_bartonbeani 0.2081834 Lower mesopelagic
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
 Min.   :0.9894       Min.   :0.2241   Min.   :0.9552          
 1st Qu.:0.9894       1st Qu.:0.2246   1st Qu.:0.9748          
 Median :0.9894       Median :0.4195   Median :0.9945          
 Mean   :0.9894       Mean   :0.4810   Mean   :0.9829          
 3rd Qu.:0.9894       3rd Qu.:0.6759   3rd Qu.:0.9967          
 Max.   :0.9894       Max.   :0.8610   Max.   :0.9989          
 NA's   :3                             NA's   :1               
 Argyropelecus_olfersii Bathylagus_euryops Benthosema_glaciale
 Min.   :0.1391         Min.   :0.7501     Min.   :0.1861     
 1st Qu.:0.4735         1st Qu.:0.7501     1st Qu.:0.5025     
 Median :0.7039         Median :0.7501     Median :0.6259     
 Mean   :0.6082         Mean   :0.7501     Mean   :0.5855     
 3rd Qu.:0.8387         3rd Qu.:0.7501     3rd Qu.:0.7088     
 Max.   :0.8861         Max.   :0.7501     Max.   :0.9040     
                        NA's   :3                             
 Bolinichthys_supralateralis Borostomias_antarcticus Ceratoscopelus_maderensis
 Min.   :0.9153              Min.   :0.9413          Min.   :0.2886           
 1st Qu.:0.9349              1st Qu.:0.9597          1st Qu.:0.3544           
 Median :0.9545              Median :0.9781          Median :0.5431           
 Mean   :0.9545              Mean   :0.9701          Mean   :0.5775           
 3rd Qu.:0.9741              3rd Qu.:0.9845          3rd Qu.:0.7662           
 Max.   :0.9937              Max.   :0.9909          Max.   :0.9353           
 NA's   :2                   NA's   :1                                        
 Chauliodus_sloani Cyclothone_sp    Derichthys_serpentinus Evermannella_balbo
 Min.   :0.8757    Min.   :0.0000   Min.   :0.9515         Min.   :0.8150    
 1st Qu.:0.8890    1st Qu.:0.3383   1st Qu.:0.9689         1st Qu.:0.9029    
 Median :0.9022    Median :0.7009   Median :0.9863         Median :0.9909    
 Mean   :0.9199    Mean   :0.5886   Mean   :0.9775         Mean   :0.9335    
 3rd Qu.:0.9420    3rd Qu.:0.9512   3rd Qu.:0.9905         3rd Qu.:0.9928    
 Max.   :0.9818    Max.   :0.9524   Max.   :0.9947         Max.   :0.9947    
 NA's   :1                          NA's   :1              NA's   :1         
 Gonostoma_elongatum Holtbyrnia_anomala Holtbyrnia_macrops
 Min.   :0.8566      Min.   :0.978      Min.   :0.9728    
 1st Qu.:0.8888      1st Qu.:0.978      1st Qu.:0.9782    
 Median :0.9209      Median :0.978      Median :0.9836    
 Mean   :0.9209      Mean   :0.978      Mean   :0.9844    
 3rd Qu.:0.9531      3rd Qu.:0.978      3rd Qu.:0.9902    
 Max.   :0.9853      Max.   :0.978      Max.   :0.9968    
 NA's   :2           NA's   :3          NA's   :1         
 Lampanyctus_crocodilus Lampanyctus_macdonaldi Lestidiops_sphyrenoides
 Min.   :0.003498       Min.   :0.7893         Min.   :0.8640         
 1st Qu.:0.034610       1st Qu.:0.7893         1st Qu.:0.9217         
 Median :0.151031       Median :0.7893         Median :0.9682         
 Mean   :0.155920       Mean   :0.7893         Mean   :0.9493         
 3rd Qu.:0.272341       3rd Qu.:0.7893         3rd Qu.:0.9958         
 Max.   :0.318123       Max.   :0.7893         Max.   :0.9968         
                        NA's   :3                                     
 Lobianchia_gemellarii Malacosteus_niger Maulisia_argipalla Maulisia_mauli  
 Min.   :0.9295        Min.   :0.9483    Min.   :0.9307     Min.   :0.8638  
 1st Qu.:0.9357        1st Qu.:0.9522    1st Qu.:0.9422     1st Qu.:0.9183  
 Median :0.9436        Median :0.9561    Median :0.9538     Median :0.9727  
 Mean   :0.9420        Mean   :0.9561    Mean   :0.9538     Mean   :0.9440  
 3rd Qu.:0.9499        3rd Qu.:0.9601    3rd Qu.:0.9654     3rd Qu.:0.9841  
 Max.   :0.9515        Max.   :0.9640    Max.   :0.9769     Max.   :0.9954  
                       NA's   :2         NA's   :2          NA's   :1       
 Maulisia_microlepis Maurolicus_muelleri Melanostigma_atlanticum
 Min.   :0.9003      Min.   :0.2555      Min.   :0.9358         
 1st Qu.:0.9003      1st Qu.:0.2629      1st Qu.:0.9651         
 Median :0.9003      Median :0.5930      Median :0.9754         
 Mean   :0.9003      Mean   :0.5975      Mean   :0.9682         
 3rd Qu.:0.9003      3rd Qu.:0.9276      3rd Qu.:0.9785         
 Max.   :0.9003      Max.   :0.9483      Max.   :0.9863         
 NA's   :3                                                      
 Melanostomias_bartonbeani Myctophum_punctatum Lampanyctus_ater
 Min.   :0.8794            Min.   :0.05241     Min.   :0.7337  
 1st Qu.:0.9064            1st Qu.:0.38476     1st Qu.:0.8103  
 Median :0.9435            Median :0.55380     Median :0.9021  
 Mean   :0.9356            Mean   :0.50618     Mean   :0.8785  
 3rd Qu.:0.9728            3rd Qu.:0.67522     3rd Qu.:0.9703  
 Max.   :0.9759            Max.   :0.86469     Max.   :0.9759  
                                                               
 Normichthys_operosus Notoscopelus_kroyeri Photostylus_pycnopterus
 Min.   :0.5662       Min.   :0.1046       Min.   :0.9937         
 1st Qu.:0.5662       1st Qu.:0.2676       1st Qu.:0.9937         
 Median :0.5662       Median :0.4199       Median :0.9937         
 Mean   :0.5662       Mean   :0.4157       Mean   :0.9937         
 3rd Qu.:0.5662       3rd Qu.:0.5680       3rd Qu.:0.9937         
 Max.   :0.5662       Max.   :0.7181       Max.   :0.9937         
 NA's   :3                                 NA's   :3              
 Sagamichthys_schnakenbecki Searsia_koefoedi Serrivomer_beanii
 Min.   :0.9255             Min.   :0.5295   Min.   :0.3697   
 1st Qu.:0.9537             1st Qu.:0.6528   1st Qu.:0.5046   
 Median :0.9818             Median :0.7806   Median :0.5673   
 Mean   :0.9677             Mean   :0.7516   Mean   :0.5415   
 3rd Qu.:0.9888             3rd Qu.:0.8795   3rd Qu.:0.6042   
 Max.   :0.9958             Max.   :0.9157   Max.   :0.6617   
 NA's   :1                                                    
 Sigmops_bathyphilus Xenodermichthys_copei Notoscopelus_bolini
 Min.   :0.9749      Min.   :0.001488      Min.   :0.3507     
 1st Qu.:0.9749      1st Qu.:0.008431      1st Qu.:0.6617     
 Median :0.9749      Median :0.320069      Median :0.9727     
 Mean   :0.9749      Mean   :0.337104      Mean   :0.7717     
 3rd Qu.:0.9749      3rd Qu.:0.648742      3rd Qu.:0.9821     
 Max.   :0.9749      Max.   :0.706791      Max.   :0.9915     
 NA's   :3                                 NA's   :1          
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
 Length:38          Min.   :0.0000  
 Class :character   1st Qu.:0.0000  
 Mode  :character   Median :0.2500  
                    Mean   :0.2697  
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
1 Bathypelagic     Anoplogaster_cornuta 0.3265944 0.9894439
2 Bathypelagic         Arctozenus_risso 0.2531392 0.8610242
3 Bathypelagic Argyropelecus_hemigymnus 0.2959860 0.9989393
4 Bathypelagic   Argyropelecus_olfersii 0.3325026 0.8860520
5 Bathypelagic       Bathylagus_euryops 0.2458790 0.7500677
6 Bathypelagic      Benthosema_glaciale 0.2348102 0.6437736
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
