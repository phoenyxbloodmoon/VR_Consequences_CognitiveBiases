library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(pwr)
library(dplyr)
library(ggpubr)
library(rstatix)

#This script will focus on Hypotheses 4 and 5. This is about Trait Anxiety.

#T-Test Anxiety: low Trait Anxiety vs high Trait Anxiety in VR

#Import 
data <- read_csv("vr_condition.csv")

#Prepare for TA
median(data$TAscore)

data$TA_cat <- cut(data$TAscore,
                   breaks = c(0, 2.25, 4),
                   labels = c("low", "high"))
view(data)
head(data)

data_anxiety <- data %>%
  select(participant_ID,TA_cat, TAscore, type, accuracy, reaction_time)%>%
  filter(TA_cat %in% c("low", "high"))
head(data_anxiety)

#Summary 
data_anxiety %>%
  group_by(TA_cat) %>%
  summarise(mean = mean(TAscore), sd = sd(TAscore)) 

t.test( TAscore ~ TA_cat, data = data_anxiety)

#Accuracy and Reaction Time
anxiety_1 <- data_anxiety %>%
  filter(type == "GAMBLER" | type == "CRT" | type == "CONJUNCTION") %>%
  mutate(type = factor(type))
head(anxiety_1)

#t.test(accuracy ~ TA_cat, data = anxiety_1) #p-value = 0.4123
#t.test(reaction_time ~ TA_cat, data = anxiety_1) #p-value = 0.0667 (almost)
#Welch's are non-parametric?

#Testing Normality 
shapiro.test(data_anxiety$TAscore)
#Not Normal 

#Add Diagram 

#Nonparametric Testing 
wilcox.test(accuracy ~ TA_cat, data = anxiety_1) #0.3388
wilcox.test(reaction_time ~ TA_cat, data = anxiety_1) #0.08036

#Include effect size for this 

#Visualization 

# Calculate mean and standard error for each group
group_summary <- data_anxiety %>%
  group_by(TA_cat) %>%
  summarise(mean = mean(TAscore),
            se = sd(TAscore) / sqrt(n()))

#Accuracy Levels Across Anxiety Levels 
anxiety_accuracy <- anxiety_1 %>%
  ggplot(aes(x = TA_cat, y = accuracy, colour = TA_cat)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = .25, alpha = .5) +
  guides(colour = "none") +
  geom_boxplot(width= 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", shape = 16, size = 2, colour = "black") + 
  theme_gdocs() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  xlab("Anxiety Level") +
  ylab("Accuracy") +
  ggtitle("Comparison of Accuracy Levels") +
  scale_color_manual(values = c("deepskyblue", "royalblue"))
anxiety_accuracy

#Reaction Time Across TA Levels
anxiety_rt <- anxiety_1 %>%
  ggplot(aes(x = TA_cat, y = reaction_time, colour = TA_cat)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = .25, alpha = .5) +
  guides(colour = "none") +
  geom_boxplot(width= 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", shape = 16, size = 2, colour = "black") + 
  theme_gdocs() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  xlab("Anxiety Level") +
  ylab("Accuracy") +
  ggtitle("Comparison of RT Levels") +
  scale_color_manual(values = c("deepskyblue", "royalblue"))
anxiety_rt

#poweranalysis 

#--------
wilcox_effsize(accuracy ~ TA_cat, data = anxiety_1)#0.0791, v.small
wilcox_effsize(reaction_time ~ TA_cat, data = anxiety_1) #0.144, small; 0.1-0.3 is small
#italicized r 
#0.3 - 0.5 = medium > 
# no effect for correlation 
#POWER ANALYSIS = 0.1542722 (OUT OF 100/ 85% CHANCE OF A FALSE RESULT OR FALSE NEG)
#WHEN THERE MIGHT BE AN EFFECT I DETECT NO EFFECT 
#IN DISCUSSION: POST HOC POWER ANALYSES WERE CONDUCTED IN GPOWER (REF) AND SHOWED X
#NUMBER OF T-TEST WERE UNDER POWERED 
#FUTURE RESEARCH IN THIS FIELD WOULD REQUIRE A LARGER # OF PARTS
# AND SHOULD CONDUCT A PRIORI TO EXAMINE HOW MANY PARTS NEEDED
#ALL POWER ANALYSES WERE LOWER THAN 0.8 SUGGESTING WE HAD A SEVERELY SMALL SAMPLE SIZE

#----
#I ran a normality test (shapiro) and it was significant which means
#not normal, so a wilcox t test was used to calculate the difference across
#means

#effect sizes were found to be small for accuracy and reaction time, 0.0791, 0.144 respestively
#a post hoc poweranalysis was run calculate the achieved power, the power analysis showed
# that the test lacked power (< 0.8) suggesting a need for larger samples. 