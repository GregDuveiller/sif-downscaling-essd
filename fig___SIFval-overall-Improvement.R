#!/usr/bin/Rscript
# 
# Script to make plots summarising the overall improvement with the downscaling
# G. Duveiller


require(ggplot2)
require(dplyr)
require(tidyr)
require(grid)
require(here)

if(exists('fig.fmt') == F) {fig.fmt <- 'pdf'}

load(paste0(dat.path, '/metrics-generic.Rda')) # dfAll_generic

dfAll_generic <- dfAll_generic %>%
  mutate(lbl=paste(DSname,V1name,V2name,V3name,sep='_'))

lbls <- c(' PK at coarse resolution  ',' JJ at coarse resolution',
          ' PK downscaled with old method  ', ' JJ downscaled with old method',
          ' PK downscaled with new method', ' JJ downscaled with new method')
rTypes <- c('1000','2000','1121','2121','1311','2311')

dflabs <- data.frame(rType=rTypes, label=factor(lbls,levels=lbls,ordered = T))

# Fig general improvement
df <- dfAll_generic %>% 
  filter(rType %in% rTypes) %>%
  tidyr::gather(key = Metric, value = Value, c('Bias','IoAg','Corr')) %>%
  left_join(data.frame(Metric=c('Bias','IoAg','Corr'),
                       MetricFullNameLabel = c('Bias (absolute)','Agreement','Correlation'),
                       stringsAsFactors = F), by = "Metric") %>%
  left_join(dflabs, by = "rType")

gbars <- ggplot(df) + 
  geom_bar(aes(y = Value, x = factor(DSname), fill = label), colour = 'grey50', stat = 'identity',position = 'dodge')+
  geom_hline(yintercept = 0, size = 2, colour = 'grey50') + 
  coord_cartesian()+
  facet_grid(.~MetricFullNameLabel)+
  scale_y_continuous('Metric value')+
  scale_fill_manual(values = c('lemonchiffon2','lemonchiffon3','lightskyblue1','lightskyblue3','darkolivegreen3','darkolivegreen4')) + 
  theme(legend.position = 'bottom',
        axis.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())+
  guides(fill=guide_legend(title.position = 'top',title.hjust=0.5,nrow=2))

ggsave(filename = paste0(fig.path, '/fig___Overall_Downscaling_Improvement', '.', fig.fmt), 
       plot = gbars, width = 8.5, height = 5)

