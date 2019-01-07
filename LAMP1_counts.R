library(tidyverse)
library(dplyr)
library(ggplot2)

LAMP1 <- read.csv("ITCH_LAMP1_all.csv")


# Calculate Mean for each experiment and total n

LAMP1_gw <- LAMP1 %>% 
  group_by(siRNA, Experiment) %>% 
  summarize(mean_diameter = mean(diameter),
            total_count = n())

# Filter out results where tubs less than 1.8um

LAMP1_filter <- LAMP1 %>% 
  filter(diameter >= 1.8)

# Group table and calculate n for diameter > 1.8

LAMP1_filter_gw <- LAMP1_filter %>% 
  group_by(siRNA, Experiment) %>% 
  summarize(count = n())

# Join tables

LAMP1_join <- merge(LAMP1_gw, LAMP1_filter_gw)

# Calculate percentage tubules

LAMP1_join$percentage <- (LAMP1_join$count/LAMP1_join$total_count)*100

# plot graph with mean

ggplot(data = LAMP1_join, mapping = aes(x = siRNA, y = percentage,))+
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill = "black")+
  labs(y = "Percentage cells with lysosome diameter > 1.8um")


# Calculates mean, sd, se and IC

my_sum <- LAMP1_join %>%
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
  ylab("Percentage lysosomes with diameter > =1.8um")

# Anova test

lm_LAMP1_join <- lm(percentage~siRNA, data = LAMP1_join)
anova(lm_LAMP1_join)

# Tukeys post-hoc analysis

aov.tukey<-aov(percentage~siRNA , data=LAMP1_join)
TukeyHSD(aov.tukey)




