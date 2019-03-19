library(tidyverse)
library(dplyr)
library(ggplot2)

Cytokinesis <- read.csv("Spastin_deconv_midbody_all.csv")

# Calculate midbodies per cell_count

Cytokinesis$midbody_norm_cell<- (Cytokinesis$midbody/Cytokinesis$cell_count)
Cytokinesis$midbody_norm_nuc<- (Cytokinesis$midbody/Cytokinesis$nuclei)

# Calculate Mean for each experiment

Cytokinesis_gw <- Cytokinesis %>% 
  group_by(siRNA, experiment) %>% 
  summarize(mean_midbodycount = mean(midbody_norm_cell)

# plot graph with mean

ggplot(data = Cytokinesis_gw, mapping = aes(x = siRNA, y = mean_midbodycount,))+
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill = "black") 


# Anova test

lm_Cytokinesis_gw <- lm(mean_midbodycount~siRNA, data = Cytokinesis_gw)
anova(lm_Cytokinesis_gw)

# Tukeys post-hoc analysis

aov.tukey<-aov(mean_midbodycount~siRNA, data = Cytokinesis_gw)
TukeyHSD(aov.tukey)

