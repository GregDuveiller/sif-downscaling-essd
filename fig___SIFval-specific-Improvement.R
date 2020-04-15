#!/usr/bin/Rscript
# 
# Script to make plots showing the specific improvement with the downscaling
# G. Duveiller


require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)
require(here)
require(gridExtra)

if(exists('fig.fmt') == F) {fig.fmt <- 'pdf'}

load(paste0(dat.path, '/metrics-generic.Rda')) #dfAll_generic
load(paste0(dat.path, '/metrics-specific.Rda')) #dfAll_specific

metric <- 'IoAg'

dfAll_specific$climateZone <- factor(dfAll_specific$climateZone, 
                                     levels = c("Tropical", "Temperate", "Dry", "Continental", "Polar"))   

dfAll_specific$PFT.dom <- factor(dfAll_specific$PFT.dom, 
                                 levels = c("EBF", "DBF", "ENF", "DNF", "MF", "SAV","SHR", "GRA", "CRO", "WET"))   

dfAll_specific <- dfAll_specific %>%
  mutate(lbl = paste(DSname, V1name, V2name, V3name, sep='_'))

dfAll_generic <- dfAll_generic %>%
  mutate(lbl = paste(DSname, V1name, V2name, V3name, sep='_'))

PFTcols <- c("EBF"="forestgreen", "DBF"="chartreuse3", "ENF"="darkslategray", "DNF"='darkseagreen', 
             "MF"="cadetblue", "SAV"='darkgoldenrod3',"SHR"='goldenrod4', "GRA"='cornflowerblue', 
             "CRO"="wheat", "WET"="steelblue4")
CZshapes2 <- c("Tropical"=15, "Temperate"=16, "Dry"=17, "Continental"=18, "Polar"=3)
CZshapes <- c("Tropical"=21, "Temperate"=22, "Dry"=23, "Continental"=24, "Polar"=25)
minN <- 1000
markerSize <- 4


# function to prepare the metrics for display on all subplots
prepareText <- function(x,y){
  dftxt <- dfAll_generic %>%
    filter(lbl %in% c(x,y)) %>%
    dplyr::select(lbl, Bias, Corr, IoAg) %>%
    mutate(ffBias = if_else(lbl[base::which.min(Bias)]==lbl,'bold','plain'),
           ffCorr = if_else(lbl[base::which.max(Corr)]==lbl,'bold','plain'),
           ffIoAg = if_else(lbl[base::which.max(IoAg)]==lbl,'bold','plain'),
           BiasTxt = paste0('B = ',round(Bias, digits = 4)),
           CorrTxt = paste0('r = ',round(Corr, digits = 4)),
           IoAgTxt = paste0('L = ',round(IoAg, digits = 4))) %>%
    gather(key = TxtType, value = label, c('BiasTxt','CorrTxt','IoAgTxt')) %>%
    gather(key = dd, value = ff, c('ffBias','ffCorr','ffIoAg')) %>%
    filter(dd == paste0('ff',substr(TxtType,1,4))) %>%
    left_join(data.frame(lbl=c(rep(x,3),rep(y,3)), 
                         TxtType=rep(c('BiasTxt','CorrTxt','IoAgTxt'),2), 
                         v.pos = c(0.15,0.075,0,1,0.925,0.85), 
                         h.pos = c(1,1,1,0,0,0),
                         h.just = c(1,1,1,0,0,0),
                         v.just = c(0,0,0,1,1,1),
                         stringsAsFactors=F), 
              by = c('TxtType','lbl'))
  
  return(dftxt)
}

# function to prepare the subplot for each combo
preparePlot <- function(x, y, metric){
  
g0 <- ggplot(dfAll_specific %>% 
               filter(lbl %in% c(x,y), N > minN) %>%
               dplyr::select(climateZone, PFT.dom, !!metric, lbl) %>%
               spread(lbl, !!metric),
             aes_string(x = x, y = y)) + 
  geom_abline(colour = 'grey30') +
  geom_point(aes(shape = factor(climateZone), fill = PFT.dom),
             size = markerSize, colour = 'grey30')+
  geom_text(data = prepareText(x,y), 
            aes(x = h.pos, y = v.pos, label = label, group=lbl, 
                hjust = h.just, vjust = v.just, fontface = ff), 
            family = 'mono') +
  scale_shape_manual(values = CZshapes) +
  scale_fill_manual(values = PFTcols) +
  # coord_fixed(xlim = c(0.3,1),ylim = c(0.3,1))+
  coord_fixed(xlim = c(0,1),ylim = c(0,1))+
  theme(legend.position = 'none')

return(g0)
}

# dummy plot to strip the legend
x <- 'JJ_NDVI_ET_MYD'; y <- 'PK_NIRv_NDWI_MYD'
g0 <- ggplot(dfAll_specific %>% 
               filter(lbl %in% c(x,y), N>minN) %>%
               dplyr::select(climateZone, PFT.dom, !!metric, lbl) %>%
               spread(lbl, !!metric),
             aes_string(x = x,y = y)) + 
  geom_abline(colour = 'grey30')+
  geom_point(aes(shape = factor(climateZone),colour = PFT.dom),size = markerSize)+
  scale_shape_manual('Type of climate',values = CZshapes) +
  scale_colour_manual('Dominant vegetation type',values = PFTcols) +
  coord_fixed(xlim = c(0,1),ylim = c(0,1))+
  theme(legend.position = 'bottom',
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.1))) +
  guides(colour=guide_legend(title.position = 'top', title.hjust = 0.5),
         shape=guide_legend(title.position = 'top', title.hjust = 0.5, nrow = 2))
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
g0 <- g_legend(g0)



# Make plot for only best and worst JJ and PK scenarios 

# JJ old method vs PK old method
g1 <- preparePlot(x = 'JJ_NDVI_ET_MYD', y = 'PK_NDVI_ET_MYD', metric = metric)

# JJ old method vs JJ new method
g2 <- preparePlot(x = 'JJ_NDVI_ET_MYD', y = 'JJ_NIRv_NDWI_MYD', metric = metric)

# JJ old method vs PK new method
g3 <- preparePlot(x = 'JJ_NDVI_ET_MYD', y = 'PK_NIRv_NDWI_MYD', metric = metric)

# JJ new method vs PK new method
g4 <- preparePlot(x = 'JJ_NIRv_NDWI_MYD', y = 'PK_NIRv_NDWI_MYD', metric = metric)


# combine subplots in a single figure
width <- 8; height <- width + 1
fig.name <- paste0(fig.path, '/fig___Downscale_Improvement_perCZandPFT_', 
                   metric, '_old-vs-new.', fig.fmt)
if(fig.fmt == 'pdf'){ pdf(fig.name, width = width, height = height)}
if(fig.fmt == 'png'){ png(fig.name, width = width, height = height, units = "in", res = 150) }

grid.arrange(g0, vp = viewport(width = 1, height = 0.10, x = 0.0, y = 0.05, just = c(0,0.5)))

print(g1, vp = viewport(width = 0.50, height = 0.45, x = 0.00, y = 0.55, just = c(0,0)))
print(g2, vp = viewport(width = 0.50, height = 0.45, x = 0.50, y = 0.55, just = c(0,0)))
print(g3, vp = viewport(width = 0.50, height = 0.45, x = 0.00, y = 0.10, just = c(0,0)))
print(g4, vp = viewport(width = 0.50, height = 0.45, x = 0.50, y = 0.10, just = c(0,0)))

grid.text(expression(bold("a")), x = unit(0.03, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.53, "npc"), y = unit(0.94, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("c")), x = unit(0.03, "npc"), y = unit(0.49, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("d")), x = unit(0.53, "npc"), y = unit(0.49, "npc"), gp = gpar(fontsize = 18))

dev.off()




