library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)

stars %>% ggplot()+
  geom_density(aes(magnitude))

ggplot(stars)+
  geom_density(aes(temp))

stars %>% filter(temp >= 5000) %>% 
  ggplot(aes(temp, magnitude, label = star))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_text_repel()

stars %>% 
  ggplot(aes(temp, magnitude, label = star, col = (star == "Sun")))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_text_repel()

stars %>% 
  ggplot(aes(log10(temp), magnitude, label = type, col = type == "G"))+
  geom_point()+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_text_repel()
