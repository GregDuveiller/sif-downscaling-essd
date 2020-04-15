#!/usr/bin/Rscript
# 
# Script to make plots summarising the improvement with the downscaling approach
# G. Duveiller


require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)
require(viridis)
require(here)

if(exists('fig.fmt') == F) {fig.fmt <- 'pdf'}

load(paste0(dat.path, '/metrics-generic.Rda')) #dfAll_generic

lbls <- c('JJ_NDVI', 'JJ_EVI', 'JJ_NIRv','PK_NDVI', 'PK_EVI', 'PK_NIRv')

df <- dfAll_generic %>%
  mutate(lbl = paste(DSname, V1name, sep='_')) %>%
  mutate(lbl_ord = factor(lbl, levels = lbls)) %>%
  tidyr::gather(key = Metric, value = Value, c('Bias','IoAg','Corr')) %>%
  left_join(data.frame(Metric = c('Bias','IoAg','Corr'),
                       MetricFullNameLabel = c('Bias (absolute)','Agreement','Correlation'),
                       stringsAsFactors = F), by = "Metric")

pt.size <- 4

gsum <- ggplot(df %>% 
         filter(V2name %in% c('NDWI', 'ET'),
                V3name == 'MYD')) +
  geom_point(aes(y = Value, x = lbl_ord, shape = V2name,
                 fill = V2name), size = pt.size) +
  geom_point(data = df %>% filter(V3name == 'MOD', V2name == 'NDWI', DSname == 'PK'),
             aes(y = Value, x = lbl_ord), shape = 2, size = pt.size) +
  geom_point(aes(y = Value, x = lbl_ord, shape = V2name,
                 fill = V2name), size = pt.size) + 
  geom_point(data = df %>% filter(DSname == 'JJ',
                                  V2name %in% c('NDWI', 'ET'),
                                  V3name == 'MYD'),
             aes(y = Value, x = lbl_ord, shape = V2name),
             alpha = 0.6, size = pt.size, fill = 'white') +
  geom_vline(xintercept = 2.5, colour = 'Grey80') + 
  scale_shape_manual(values = c('NDWI' = 24, 'ET' = 21)) + 
  scale_colour_manual(values = c('PK' = 'grey50', 'JJ' = 'grey10')) +
  scale_fill_manual(values = c('ET' = viridis::plasma(9)[8], 
                               'NDWI' = viridis::viridis(9)[4] )) +
  facet_wrap(~MetricFullNameLabel,  nrow = 1, scales = 'free') + 
  theme(legend.position = 'none',
        strip.text = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.1)),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank())

ggsave(filename = paste0(fig.path,'/fig___Summary_Improvement','.',fig.fmt),
       plot = gsum, width = 9, height = 4)

