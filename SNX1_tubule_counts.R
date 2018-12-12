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


# Calculate SEM
SNX1_agg <- aggregate(percentage~siRNA, data = SNX1_join,
                      FUN = function(x) c(mean = mean(x), sd = sd(x),
                                          n = length(x)))
SNX1_agg$se <- SNX1_agg$x.sd / sqrt(SNX1_agg$x.n)
SNX1_agg_2 <- do.call(data.frame, SNX1_agg)
colnames(SNX1_agg_2) <- c("siRNA", "mean", "sd", "n", "se")


SNX1_agg_2$se <- SNX1_agg_2$percentage.sd / sqrt(SNX1_agg_2$percentage.n)


# Anova test

lm_SNX1_join <- lm(percentage~siRNA, data = SNX1_join)
anova(lm_SNX1_join)

# Tukeys post-hoc analysis

aov.tukey<-aov(percentage~siRNA , data=SNX1_join)
TukeyHSD(aov.tukey)

# Check normality of data


