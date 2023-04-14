---
title: "functional_diversity"
format: 
  html:
    self-contained: false
    code-fold: true
editor: source
theme: minty
keep-md: true
execute:
  warning: false
  message : false
toc: true
toc-title: Sections
toc-location: left
---


::: {.cell}

:::


## Species selection

- Select species with at least 5 individuals in data set between 2002 and 2022

- Selection of species with  highest biomass 

::: {.cell}
::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-224f5e70175dc4e82bfa" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-224f5e70175dc4e82bfa">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49"],["Cyclothone","Lampanyctus crocodilus","Xenodermichthys copei","Notoscopelus kroyeri","Stomias boa boa","Serrivomer beanii","Arctozenus risso","Argyropelecus olfersii","Benthosema glaciale","Normichthys operosus","Myctophum punctatum","Ceratoscopelus maderensis","Maurolicus muelleri","Searsia koefoedi","Nannobrachium atrum","Lampanyctus macdonaldi","Maulisia mauli","Chauliodus sloani","Maulisia microlepis","Lobianchia gemellarii","Melanostomias bartonbeani","Cyclothone microdon","Borostomias antarcticus","Malacosteus niger","Notoscopelus bolini","Evermannella balbo","Gonostoma elongatum","Melanostigma atlanticum","Eurypharynx pelecanoides","Maulisia argipalla","Sigmops bathyphilus","Bolinichthys supralateralis","Lestidiops sphyrenoides","Sagamichthys schnakenbecki","Holtbyrnia anomala","Bathylagus euryops","Derichthys serpentinus","Dolicholagus longirostris","Argyropelecus hemigymnus","Holtbyrnia macrops","Paralepis coregonoides","Photostylus pycnopterus","Macroparalepis affinis","Taaningichthys bathyphilus","Cyclothone braueri","Diaphus metopoclampus","Entelurus aequoreus","Mentodus rostratus","Bathylagichthys greyae"],[4063,3391,2998,1988,404,212,1370,857,6961,402,1326,1616,14331,128,252,114,21,28,10,119,20,470,35,15,96,13,7,305,24,34,73,13,49,32,12,182,13,7,179,38,43,16,38,11,101,6,5,9,7],[195.607,30.258,25.853,10.362,9.107,8.552,7.576,6.442,5.97,5.367,4.927,3.88,3.647,2.436,2.325,2.231,1.448,1.322,0.99,0.746,0.73,0.667,0.629,0.573,0.56,0.444,0.42,0.4,0.363,0.356,0.241,0.219,0.217,0.214,0.21,0.176,0.169,0.14,0.126,0.11,0.07,0.06,0.06,0.04,0.026,0.02,0.01,0.009,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Nom_Scientifique<\/th>\n      <th>abundance_all<\/th>\n      <th>biomasse_all<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::

::: {.cell}

:::


## Morphometric data

::: {.cell}

:::


### number of individuals measured per species

::: {.cell}
::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-76d878c82e8cfa1bd463" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-76d878c82e8cfa1bd463">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26"],["Argyropelecus_hemigymnus","Bathylagus_euryops","Bolinichthys_supralateralis","Borostomias_antarcticus","Chauliodus_sloani","Cyclothone_sp","Derichthys_serpentinus","Eurypharynx_pelecanoides","Evermannella_balbo","Holtbyrnia_anomala","Holtbyrnia_macrops","Lampanyctus_ater","Lampanyctus_macdonaldi","Lestidiops_sphyrenoides","Lobianchia_gemellarii","Malacosteus_niger","Maulisia_argipalla","Maulisia_mauli","Maulisia_microlepis","Melanostigma_atlanticum","Melanostomias_bartonbeani","Notoscopelus_bolini","Paralepis_coregonoides ","Photostylus_pycnopterus","Sagamichthys_schnakenbecki","Sigmops_bathyphilus"],[17,19,11,14,12,5,1,3,1,3,9,22,21,7,22,5,5,10,4,3,9,3,19,4,9,20]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::
