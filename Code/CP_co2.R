library(tidyverse)
CP_co2 <- filter(flux_data, compound == "co2", plot == "C" | plot == "P") %>% 
  group_by(date,field,plot) %>% 
  summarize(flux_mean = mean (flux,na.rm = T))    
ggplot(CP_co2, aes(x = date, y = flux_mean, color = plot)) +
  geom_point(position = "jitter") +
  geom_line() +
  facet_wrap(~field)

library(RColorBrewer)
ggplot(CP_co2,aes(x = plot, y = flux_mean, fill = plot)) +
  geom_boxplot(color = "black", size = .05, alpha = 0.7) +
  stat_boxplot(geom = "errorbar") +
  facet_grid(.~field)

ggplot(data = filter(flux_data, compound == "co2", plot == "C" | "P"))+
  geom_smooth(mapping = (aes(x = soiltemp, y = flux)))
  
C_co2 <- filter(flux_data, compound == "co2", plot == "C")
P_co2 <- filter(flux_data, plot == "P")    

ggplot(data = C_co2)+
  geom_smooth(mapping = (aes(x = soiltemp, y = flux)))
                         +
  geom_smooth(data = P_co2, mapping = (aes(x = soiltemp, y = flux)))

              