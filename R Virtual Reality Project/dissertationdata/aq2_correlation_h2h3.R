library(tidyverse)
library(corrr)
library(emmeans)
library(patchwork)
library(ggplot2)
library(ggthemes)
library(prettydoc)

#This script will focus on the correlations for this study
 
#Import data
data <- read_csv("vr_condition.csv")

#TIDY THE DATA
corr_data <- data %>%
  select(-c(reaction_time, TAscore)) %>% # removing some variables unneeded for this
  pivot_wider(names_from = "type",
              values_from = "accuracy")
head(corr_data)

#Summary 

#Creating a table to show SD and Mean for Accuracy x Cond
data %>%
  group_by(type) %>%
  summarise(mean = mean(accuracy), sd = sd(accuracy)) 

#MERGE
corr_data2 <- aggregate( .~participant_ID , data=corr_data, max, na.rm=TRUE, na.action = NULL)

#COEFFICIENTS 
corr_data3 <- corr_data2 %>%
  select(GAMBLER, CRT, CONJUNCTION) %>%
  correlate()

corr_data3

corr_data3$GAMBLER <- as.numeric(corr_data3$GAMBLER)
corr_data3$CRT <- as.numeric(corr_data3$CRT)
corr_data3$CONJUNCTION <- as.numeric(corr_data3$CONJUNCTION)

#PEARSONS
cor.test(corr_data2$GAMBLER, corr_data2$CRT) 
cor.test(corr_data2$GAMBLER, corr_data2$CONJUNCTION) 
cor.test(corr_data2$CRT, corr_data2$CONJUNCTION) 

#Graphs
#Reation Time Gambler's Fallacy x CRT
plot_1 <- corr_data2 %>%
  ggplot(aes(x = GAMBLER, y = CRT)) + 
  geom_point(size = 1.5) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              alpha = 0.05) +
  theme_gdocs() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = 
  ) +
  xlab("Gambler's Fallacy Score") +
  ylab("CRT Score") +
  ggtitle("Accuracy Correlation: Gambler's Fallacy x CRT")
plot_1

#Reaction Time: Gambler's x Conjunction

plot_2 <- corr_data2 %>%
  ggplot(aes(x = GAMBLER, y = CONJUNCTION)) + 
  geom_point(size = 1.5) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              alpha = 0.05) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = 
  ) +
  xlab("Gambler's Fallacy Score") +
  ylab("Conjunction Fallacy Score") +
  ggtitle("Accuracy Correlation: Gambler's x Conjunction") 
plot_2

#Reaction Time: CRT x Conjunction

plot_3 <- corr_data2 %>%
  ggplot(aes(x = CRT, y = CONJUNCTION)) + 
  geom_point(size = 1.5) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              alpha = 0.05) +
  theme_gdocs() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = 
  ) +
  xlab("CRT Score") +
  ylab("Conjunction Fallacy Score") +
  ggtitle("Accuracy Correlation: CRT x Conjunction") + 
  guides(color = FALSE, alpha = FALSE) 
plot_3

cogbias_visual <- plot_1 + plot_2 + plot_3 
cogbias_visual
ggsave("cogimage.svg", plot = cogbias_visual, height = 5, width = 5, dpi = 300 )
(plot_1 + plot_2 + plot_3)

# coefficient (r), then p-value 
#--------------------------------------------------
#HypotHesis 3: Correlation for Reaction Time 
#The reaction time across tasks will be relatively similar.

#TIDY
corr_reaction <- data %>%
  select(-c(accuracy, TAscore)) %>% # removing some variables unneeded for this
  pivot_wider(names_from = "type",
              values_from = "reaction_time")
head(corr_reaction)

#Summary
data %>%
  group_by(type) %>%
  summarise(mean = mean(reaction_time), sd = sd(reaction_time)) 

#MERGE
corr_reaction2 <- aggregate( .~participant_ID , data=corr_reaction, max, na.rm=TRUE, na.action = NULL)

#COEFFICIENT
corr_reaction3 <- corr_reaction2 %>%
  select(GAMBLER, CRT, CONJUNCTION) %>%
  correlate()

corr_reaction3

corr_data3$GAMBLER <- as.numeric(corr_data3$GAMBLER)
corr_data3$CRT <- as.numeric(corr_data3$CRT)
corr_data3$CONJUNCTION <- as.numeric(corr_data3$CONJUNCTION)

#PEARSONS
cor.test(corr_reaction2$GAMBLER, corr_data2$CRT) 
cor.test(corr_reaction2$GAMBLER, corr_data2$CONJUNCTION) 
cor.test(corr_reaction2$CRT, corr_data2$CONJUNCTION) 

#scatterplots
#Reaction Time: CRT x Gambler
vis_1 <- corr_reaction2 %>%
  ggplot(aes(x = GAMBLER, y = CRT)) + 
  geom_point(size = 1.5) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              alpha = 0.05) +
  theme_gdocs() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = 
  ) +
  xlab("Gambler's Fallacy Score") +
  ylab("CRT Score") +
  ggtitle("RT Correlation (m.s): Gambler's Fallacy x CRT")
vis_1

##Reaction Time: Gambler x Conjunction

vis_2 <- corr_reaction2 %>%
  ggplot(aes(x = GAMBLER, y = CONJUNCTION)) + 
  geom_point(size = 1.5) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              alpha = 0.05) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = 
  ) +
  xlab("Gambler's Fallacy Score") +
  ylab("Conjunction Fallacy Score") +
  ggtitle("RT Correlation (m.s): Gambler's x Conjunction") 
vis_2

#Reaction Time: CRT x Conjunction
vis_3 <- corr_reaction2 %>%
  ggplot(aes(x = CRT, y = CONJUNCTION)) + 
  geom_point(size = 1.5) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              alpha = 0.05) +
  theme_gdocs() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = 
  ) +
  xlab("CRT Score") +
  ylab("Conjunction Fallacy Score") +
  ggtitle("RT Correlation (m.s): CRT x Conjunction") + 
  guides(color = FALSE, alpha = FALSE) 
vis_3

#Make one plot
cogbias_visual2 <- vis_1 + vis_2 + vis_3 
cogbias_visual2
ggsave("cogimage.svg", plot = cogbias_visual2, height = 5, width = 5, dpi = 300 )
(plot_1 + plot_2 + plot_3)