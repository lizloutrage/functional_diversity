
# Functional diversity of deep-pelagic fish in the Bay of Biscay, Northeast Atlantic 🐟📏

<!-- badges: start -->

[![License:
GPL-2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

The aim of the `functional_diversity` project is to study the functional
diversity of the deep pelagic fish community (between 20 and 2000m
depth) and reproduce the analysis performed by Loutrage et al. 2025 (DOI: )

## Data (any use must include the reference and DOI):
- DOI of the raw morphological data set:  
- DOI of the 2002-2019 trawling data set: <https://doi.pangaea.de/10.1594/PANGAEA.967132> 
- DOI of the 2021-2022 trawling data set:  <https://doi.org/10.48579/PRO/AIKOEB> 
 
## Overview

Here is an overview of `functional_diversity` content:

- [`data/`](https://github.com/lizloutrage/functional_diversity/tree/main/data):
  contains all raw data required to perform analyses
  
- [`R/`](https://github.com/lizloutrage/functional_diversity/tree/main/R):
  contains the functions to run the analysis

- [`_targets`](https://github.com/lizloutrage/functional_diversity/tree/main/index.qmd):
  contains the workflow of the analyses

- [`index.qmd`](https://github.com/lizloutrage/functional_diversity/tree/main/index.qmd):
  contains the final report to be knitted with the code to carry out the
  analysis

- [`figures/`](https://github.com/lizloutrage/functional_diversity/tree/main/figures):
  contains the figures in high resolution

## Comments

- We have used the **targets workflow** to run the analysis [`_targets`]. To run the analysis run “tar_make()” and to view the
  workflow run “tar_visnetwork()”.

- The 'morphometric_measurements' PDF illustrates how morphological traits have been measured

## Code of Conduct

Please note that the `functional_diversity` project is released with a
[Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
