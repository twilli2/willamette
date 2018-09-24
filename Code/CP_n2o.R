library(tidyverse)
CP_n2o <- filter(flux_data, compound == "n2o", plot == "C" | plot == "P") %>% 
  group_by(date,field,plot) %>% 
  summarize(flux_mean = mean (flux,na.rm = T))    
ggplot(CP_n2o, aes(x = date, y = flux_mean, color = plot)) +
  geom_point() +
  geom_line() +
  facet_wrap(~field)

library(RColorBrewer)
ggplot(CP_n2o,aes(x = plot, y = flux_mean, fill = plot)) +
  geom_boxplot(color = "black", size = .05, alpha = 0.7) +
  stat_boxplot(geom = "errorbar") +
  facet_grid(.~field)
