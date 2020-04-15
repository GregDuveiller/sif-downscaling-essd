#!/usr/bin/Rscript
# 
# Script to map the maximum SIF 
# G. Duveiller


require(ggplot)
require(here)
require(raster)
require(ncdf4)
require(rgdal)

if(exists('fig.fmt') == F) {fig.fmt <- 'png'}

# load general vectors
# source: from Natural Earth at http://www.naturalearthdata.com/
dpathVctr <- '~/Work/AncillaryDatasets/WorldVector'
wmap <- readOGR(dsn = dpathVctr, layer = "ne_110m_land")
wmap_df <- fortify(wmap)

# load summarized raster
r <- raster(paste0(dat.path, '/SIF-max-2007-2018.nc'))
r_LR <- aggregate(r, fact = 5, fun =mean)

# transform from raster to data.frame
df <- as.data.frame(r_LR,xy=T)
colnames(df) <- c('lon','lat','sif')
df <- df %>% filter(!is.na(sif))

# set-up themes 
theme_opts <- list(theme(plot.title= element_text(size = rel(1.3),hjust = 0.5),
                         axis.title = element_text(size = rel(1.2)),
                         axis.text = element_text(size = rel(1.1)),
                         strip.text = element_text(size = rel(1.3)),
                         panel.background = element_rect(colour = NA, fill = "grey20"),
                         panel.grid.major = element_line(colour = 'grey30'),
                         panel.grid.minor = element_line(colour = 'grey30')))

latRange <- c(-60,85)

# need to load data.frame with subzone labels and extents
load(file = 'data/final_data/ancillary4paper/harvested-SIF-zonal-extents.Rda') # 'df.zone.labels'

g <- ggplot(df, aes(x=lon, y=lat)) + 
  geom_polygon(data=wmap_df, aes(long,lat, group=group),fill='grey45') + 
  geom_raster(aes(fill=sif)) + 
  geom_polygon(data = df.zone.labels, aes(x = x, y = y, group = zone), fill = NA, colour = 'white') + 
  geom_text(data = df.zone.labels %>% filter(pt == 'LR'), aes(x = x, y = y, label = zone), fontface = 'bold', colour = 'white', nudge_x = 3) + 
  scale_fill_viridis_c(bquote("Maximum SIF over the period 2007 to 2018 [" ~ mW/m^2/sr/nm ~ "]"), option = 'magma') +
  scale_y_continuous('Latitude',expand=c(0,0))+
  scale_x_continuous('Longitude',expand=c(0,0))+
  coord_cartesian(ylim=latRange)+
  theme_opts + 
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm"))+
  guides(fill = guide_colourbar(title.position="top",title.hjust = 0.5))

ggsave(filename = paste0(fig.path, '/fig___MaxSIFMap.', fig.fmt), 
       plot = g, width = 11, height = 6, dpi = 300)

