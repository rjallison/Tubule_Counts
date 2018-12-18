library(tidyverse)
library(dplyr)
library(ggplot2)

SNX1 <- read.csv("WWP1_SNX1_deconv_all.csv")


# Calculate Mean for each experiment and total n

SNX1_gw <- SNX1 %>% 
  group_by(siRNA, Experiment) %>% 
  summarize(mean_length = mean(tubule_length_um),
            total_count = n())

# Filter out results where tubs less than 2um

SNX1_filter <- SNX1 %>% 
  filter(tubule_length_um >= 2)

# Group table and calculate n for tub > 2

SNX1_filter_gw <- SNX1_filter %>% 
  group_by(siRNA, Experiment) %>% 
  summarize(count = n())

# Join tables

SNX1_join <- merge(SNX1_gw, SNX1_filter_gw)

# Calculate percentage tubules

SNX1_join$percentage <- (SNX1_join$count/SNX1_join$total_count)*100

# plot graph with mean

ggplot(data = SNX1_join, mapping = aes(x = siRNA, y = percentage,))+
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill = "black")+
  labs(y = "Percentage cells with tubules > 2um")


# Calculates mean, sd, se and IC

my_sum <- SNX1_join %>%
  group_by(siRNA) %>%
  summarise(
    n=n(),
    mean=mean(percentage),
    sd=sd(percentage)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# Plot histogram with SEM as error bars
ggplot(my_sum) +
  geom_bar( aes(x=siRNA, y=mean), stat="identity", fill="skyblue", alpha=0.5) +
  geom_errorbar( aes(x=siRNA, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1)+
  ylab("Percentage cells with tubule > 2um")

# Anova test

lm_SNX1_join <- lm(percentage~siRNA, data = SNX1_join)
anova(lm_SNX1_join)

# Tukeys post-hoc analysis

aov.tukey<-aov(percentage~siRNA , data=SNX1_join)
TukeyHSD(aov.tukey)




