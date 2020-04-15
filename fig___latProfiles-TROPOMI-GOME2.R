#!/usr/bin/Rscript
# 
# Script to make latitudinal cross-sections of the comparison with TROPOMI
# G. Duveiller


library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(here)


if(exists('fig.fmt') == F) {figfmt <- 'pdf'}

load(paste0(dat.path, '/latProfiles-TROPOMI-GOME2.Rda')) # df.lat.med

col1 <- 'lightgreen'
col2 <- 'cornflowerblue'
col3 <- 'orchid'

selTimes <- c(13,23,28,33,43)

gLat <-  ggplot(df.lat.med %>%
                  dplyr::filter(time_ID %in% selTimes)) +
  geom_line(aes(y = SIF, x = lat, colour = Source)) + 
  facet_wrap(~time_label, ncol = length(selTimes)) +
  scale_y_continuous(bquote("SIF [" ~ mW/m^2/sr/nm ~ "]")) +
  scale_x_continuous('Latitude') +
  scale_color_manual('', values = c(col1,col2,col3), labels = c('tropomi'='TROPOMI','gomedsc'='Downscaled GOME2','gomedsc_bc'='Corr. downscaled GOME2')) + 
  coord_flip(ylim = c(-0.22, 0.82),expand = F) +
  theme(legend.position = 'bottom') +
  ggtitle(paste('Latitudinal medians for different time slices')) 

ggsave(filename = paste0('/fig___LatProfiles-TROPOMI-GOME2.', fig.fmt), 
       plot = gLat, width = 9, height = 7, path = fig.path, dpi = 150)
