#!/usr/bin/Rscript
# ##############################################################################
# Title         : make paper figures
#
# Description   : Script to reproduce all figures of the following paper
#
#     Duveiller G., Filipponi F., Walther S., Köhler P., Frankenberg C., Guanter
#     L., and Cescatti A. 2020. A spatially downscaled sun-induced fluorescence
#     global product for enhanced monitoring of vegetation productivity. Earth
#     System Science Data. https://doi.org/10.5194/essd-2019-121
#
# Date          : Apr 2020
# Licence       : GPL v3
# Authors       : Gregory Duveiller
# ##############################################################################


require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)
require(here)

# set path and format of target figures
fig.path <- 'figures/final_figures'
dir.create(fig.path, showWarnings = F)
fig.fmt <- 'png'

# set path of where the necessary data is
# [check zenodo repository DOI: 10.5281/zenodo.3753024]
dat.path <- 'data/final_data/support2paper'


# Fig 1
source('fig___SIFval-summarized-Improvement.R')

# Fig 2
source('fig___SIFval-specific-Improvement.R')

# Fig 3
source('fig___SIFval-overall-Improvement.R')

# Fig 4
source('fig___SIF-zonal-time-series.R') 

# Fig 5
source('fig___max-SIF-map.R')

# Fig 6
source('fig___GlobalAgreementMaps-TROPOMI-GOME2.R')

# Fig 7
source('fig___latProfiles-TROPOMI-GOME2.R')