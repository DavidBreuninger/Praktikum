library(tidyverse)
library(ggplot2)
library(data.table)
source("functions.R")

# read 
umzug_ohneBezirke <- read.csv("Clean_Data/umzug_ohneBezirke.csv")
umzug_stadt <- read.csv("Clean_Data/umzug_innen_außen.csv")
umzug_Bezirksgruppen <-  read.csv("Clean_Data/umzug_Bezirksgruppen.csv")

# pivot longer for plots
umzug_stadt_long <- umzug_stadt %>%
  select(!insgesamt) %>%
  pivot_longer(cols = c("innerstaedtisch", "außerstaedtisch"),
               names_to = "Umzug",
               values_to = "Anzahl")

umzug_Bezirksgruppen_long <- umzug_Bezirksgruppen %>%
  select(!insgesamt) %>%
  rename("selber Bezirk" = selber_Bezirk) %>%
  pivot_longer(cols = c("selber Bezirk", "Nachbarbezirke", "Restbezirke", "außerstaedtisch"),
               names_to = "Umzug",
               values_to = "Anzahl")

# stacked barplot absolute values
ggplot(umzug_stadt_long, aes(x = Jahr, y = Anzahl, 
                             fill = Umzug)) + 
  geom_bar(stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk, scales = "free_y") +
  theme_bw() +
  labs(y = "Anzahl Umzüge", fill = "Umzug") +
  scale_fill_manual(values = c("außerstaedtisch" = "#FFFF6D", "innerstaedtisch" = "#8491B4"))

ggplot(umzug_Bezirksgruppen_long, aes(x = Jahr, y = Anzahl, 
                                      fill = factor(Umzug, levels = c("außerstaedtisch", "Restbezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk, scales = "free_y") +
  theme_bw() +
  labs(y = "Anzahl Umzüge", fill = "Umzug") + 
  scale_fill_manual(values = c("außerstaedtisch" = "#FFFF6D", "Restbezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF"))

# stacked percentage barplot 
plot_Umzug_Stadt <- ggplot(umzug_stadt_long, aes(x = Jahr, y = Anzahl, fill = Umzug)) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  ylab("Anteil Umzüge") +
  scale_fill_manual(values = c("außerstaedtisch" = "#FFFF6D", "innerstaedtisch" = "#8491B4")) + 
  theme(axis.text.x = element_text(size = 7))

plot_Umzug_Bezirke <- ggplot(umzug_Bezirksgruppen_long, 
                             aes(x = Jahr, y = Anzahl, 
                                 fill = factor(Umzug, levels = c("außerstaedtisch", "Restbezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  labs(fill = "Umzug", y = "Anteil Umzüge") +
  scale_fill_manual(values = c("außerstaedtisch" = "#FFFF6D", "Restbezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF")) +
  theme(axis.text.x = element_text(size = 7))

# save plots
ggsave("Results/plot_Umzug_Stadt.jpg", plot = plot_Umzug_Stadt, width = 12, height = 8)
ggsave("Results/plot_Umzug_Bezirke.jpg", plot = plot_Umzug_Bezirke, width = 12, height = 8)
