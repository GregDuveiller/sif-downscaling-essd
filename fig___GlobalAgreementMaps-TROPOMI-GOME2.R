#!/usr/bin/Rscript
# 
# Script to make plots of the agreement maps
# G. Duveiller


require(dplyr)
require(tidyr)
require(ggplot2)
require(scales)
require(here)
require(rgdal)

if(exists('fig.fmt') == F) {fig.fmt <- 'png'}

# load general vectors
# source: from Natural Earth at http://www.naturalearthdata.com/
dpathVctr <- '~/Work/AncillaryDatasets/WorldVector'
wmap <- readOGR(dsn = dpathVctr, layer = "ne_110m_land")
wmap_df <- fortify(wmap)

# load df with the metrics per pixel
load(paste0(dat.path, '/df-TROPOMIvsGOME2-metrics.Rda'))

pxsz <- 0.05

# prepare data frame
df <- df.metrics %>% 
  dplyr::mutate(lat = 90 - row * pxsz - pxsz/2,
         lon = col * pxsz - pxsz/2 - 180) %>%
  dplyr::select(L,L.unsys,f.sys,lat,lon) %>%
  tidyr::gather(key = metric_type, value = metric_value, c('L','f.sys','L.unsys'),
                factor_key = T)

# define labels
levels(df$metric_type) <- c("Total agreement", 
                            "Fraction of the systematic deviations",
                            "Agreement of the unsystematic component")

# set-up themes 
theme_opts <- list(theme(plot.title= element_text(size = rel(1.3),hjust = 0.5),
                         axis.title = element_text(size = rel(1.2)),
                         axis.text = element_text(size = rel(1.1)),
                         strip.text = element_text(size = rel(1.3)),
                         panel.background = element_rect(colour = NA, fill = "grey20"),
                         panel.grid.major = element_line(colour = 'grey30'),
                         panel.grid.minor = element_line(colour = 'grey30')))

latRange <- c(-60,85)


g <- ggplot(df, aes(x = lon, y = lat)) + 
  geom_polygon(data = wmap_df, aes(long, lat, group=group), fill = 'grey45') + 
  geom_raster(aes(fill = metric_value)) + 
  scale_fill_viridis_c('', limits = c(0,1), oob = squish) +
  scale_y_continuous('Latitude', expand = c(0,0))+
  scale_x_continuous('Longitude', expand = c(0,0))+
  facet_wrap(~metric_type, nc = 1) +
  coord_cartesian(ylim = latRange)+
  theme_opts + 
  theme(legend.position = 'right',
        legend.key.height = unit(3.4, "cm"), 
        legend.text = element_text(size = rel(1.1)))+
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

ggsave(filename = paste0(fig.path, '/fig___GlobalAgreementMaps-TROPOMI-GOME2.', fig.fmt), 
       plot = g, width = 10, height = 12, dpi = 150)
