library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(afex)
library(performance) 
library(pwr)
library(car)
library(rstatix)
library(patchwork)
library(easystats)

#This script will concentrate on the ANOVA's for this study. 

#Import NonVr Condition and VR COndition Data 
vr_data <- read_csv("vr_condition.csv")
nonvr_data <- read_csv("nonvr_condition.csv")

#Tidy

head(nonvr_data)

nonvr_data <- nonvr_data %>%
  rename( "type" = "tasktype",
          "reaction_time" = "reaction_time(s)",
          "participant_ID" = "participant.id")
nonvr_data

nvr_data <- as.character(nonvr_data$participant_ID)

#Prepare to merge data by column "group"

nonvr_data <- nonvr_data %>%
  add_column(group = "Non-VR")
nonvr_data

nonvr_data2 <- nonvr_data %>%
  select(-participant_ID)
nonvr_data3 <- nonvr_data2 %>%
  add_column(nvr_data)
nonvr_data3$reaction_time <- nonvr_data3$reaction_time*1000
head(nonvr_data3)


nonvr_data3 <- nonvr_data3 %>%
  rename("participant_ID" = "nvr_data")
nonvr_data3

vr_data <- vr_data %>%
  add_column(group = "In-VR")
vr_data 

#remove TAscore, Non-VR does not have this variable (may help conflation)
vr_data <- vr_data %>%
  select(-c(TAscore)) 
head(vr_data)

#Merge
data <- bind_rows(nonvr_data3, vr_data)

#filter out catch and sample size

data_1 <- data %>%
  filter(type == "GAMBLER" | type == "CRT" | type == "CONJUNCTION") %>%
  mutate(type = factor(type), 
         group = factor(group))
head(data_1)

#Summarize the data
data_1 %>%
  group_by(type, group) %>%
  summarise(mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy))

#Level check
unique(data_1$type)
unique(data_1$group)

#Visualizations 

#Non- VR Visualizations 

#NVR Accuracy  
plot1 <- data_1 %>%
  ggplot(aes(x = type, y = accuracy, col = "Non-VR")) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  scale_y_continuous(breaks = seq(from = 0.0, to = 1.0, by = 0.1)) +
  guides(colour = FALSE) +
  labs(title= "Accuracy Levels Across Task Types, Non-VR",
       x = "Task Type",
       y = "Accuracy") +
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none")
plot1

#For Reaction Time
plot2 <- data_1 %>%
  ggplot(aes(x = type, y = reaction_time, col = "Non-VR")) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs(title= "Reaction Time Across Task Types, Non VR",
       x = "Task Type",
       y = "Reaction Time/ m.s") +
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none")
plot2

nvr_visual <- plot1 + plot2 
nvr_visual
ggsave("nvr_visual.svg", plot = nvr_visual, height = 5, width = 5, dpi = 300 )

#Comparison Visualizations, VR v Non VR

#For Accuracy 
data_1 %>%
  ggplot(aes(x = type, y = accuracy, col = group)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  scale_y_continuous(breaks = seq(from = 0.0, to = 1.0, by = 0.1)) +
  guides(colour = FALSE) +
  labs(title= "Accuracy Levels Across Task Types in VR vs Non-VR",
       x = "Group and Task Type",
       y = "Accuracy") +
  theme_minimal()+
  facet_wrap(~group) +
  scale_fill_manual(values = c( "darkgoldenrod1", "navyblue")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none")

#For Reaction Time
data_1 %>%
  ggplot(aes(x = type, y = reaction_time, col = group)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs(title= "Reaction Time Across Task Types in VR vs Non-VR",
       x = "Group and Task Type",
       y = "Reaction Time/ m.s") +
  theme_minimal()+
  facet_wrap(~group) +
  scale_fill_manual(values = c( "darkgoldenrod1", "navyblue")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none")

#--------ANOVA

#Accuracy
b<-aov(accuracy~type*group+Error(participant_ID/type), data = data_1)
summary(b)
emmeans(b, pairwise ~ type, adjust = "Bonferroni")

#Effect Size
eta_squared(b,partial = FALSE)

eta2_to_f(1.50e-03) #0.0388
#poweranalysis: 0.0505240

#Reaction time, group, 0.14 and sig is 0.05, borderline is 0.06, so it's not close
c <-aov(reaction_time~type*group+Error(participant_ID/type), data = data_1)
summary(c)
emmeans(c, pairwise ~ type, adjust = 'Bonferroni') 

eta_squared(c,partial = FALSE)
eta2_to_f(7.24e-03) #0.08539789
#poweranalysis using G Power: 0.0615356
#at the end of each anova, say the poweranalysis was found and if there
#was an effect it was unlikely to find it due to the analysis being
#severely under powered

#---- Running T-Test for Reaction Time in Tasks

#Conjunction T Test
dataset_conjunction <- subset(data, type == 'CONJUNCTION')

t.test( accuracy ~ group, data = dataset_conjunction)

#Gamblers T Test 
dataset_gamblers <- subset(data, type == 'GAMBLER')

t.test( accuracy ~ group, data = dataset_gamblers)

#CRT T Test

dataset_crt <- subset(data, type == 'CRT')

t.test( accuracy ~ group, data = dataset_crt)

# CATCH Trials (Used to double check that participants pay attention) 
#Running for VR + Accuracy non-significance 
dataset_catch <- subset(data, type == 'catch')

t.test( reaction_time ~ group, data = dataset_catch)

t.test( accuracy ~ group, data = dataset_catch)
#----------------
wilcox_anova <- data_1 %>%
wilcox_test(accuracy ~ group, paired = TRUE, p.adjust.method = "Bonferroni")

#---------------
#Testing Normality 
check_model(b)
shapiro_test(residuals(b$participant_ID))
shapiro_test(residuals(b$`participant_ID:type`))
shapiro_test(residuals(b$Within))

#Reference Andy Field 