#!/usr/bin/Rscript
# 
# Script to make sp-time examples of SIF dataset
# G. Duveiller


require(ggplot2)
require(viridis)
require(dplyr)

if(exists('fig.fmt') == F) {fig.fmt <- 'png'}


load(paste0(dat.path, '/SIF-zonal-time-series.Rda')) # df.sif.zts

df.sif.zts <- df.sif.zts %>% mutate(lbl=paste(zone, date, sep = ': '))

sif.colour.scl <- c('#000000','#000066',
                    viridis(11)[3:11],
                    plasma(11)[10:3])


g <- ggplot(df.sif.zts) + 
  geom_raster(aes(x=lon,y=lat,fill=sif))+
  facet_wrap(~lbl, scales = "free",nrow=7, dir="v")+
  scale_fill_gradientn(bquote(atop('SIF ',bgroup("[",frac('mW','m'^2 %.% 'sr'%.%'nm'),"]"))), 
                       limits=c(0,2.2),expand = c(0,0),
                       colours=sif.colour.scl)+
  coord_cartesian(expand = F)+
  theme(
    legend.position = 'right',
    legend.key.height = unit(4,'cm'),
    legend.title = element_text(size = rel(1.4)),
    legend.text = element_text(size = rel(1.3)),
    strip.text = element_text(size = rel(1.1)),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )+
 guides(fill = guide_colourbar(title.position="top",title.hjust = 0.5))

ggsave(filename = paste0(fig.path, '/fig___SIF-zonal-time-series.', fig.fmt), 
       plot = g, width = 10, height = 14, dpi = 150)
