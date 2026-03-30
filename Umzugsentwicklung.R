library(tidyverse)
library(ggplot2)
library(data.table)
library(scales)
source("functions.R")

# read 
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")
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
  rename("sonstige Bezirke" = Restbezirke) %>%
  pivot_longer(cols = c("selber Bezirk", "Nachbarbezirke", "sonstige Bezirke", "außerstaedtisch"),
               names_to = "Umzug",
               values_to = "Anzahl")

# stacked barplot absolute values
ggplot(umzug_stadt_long, aes(x = Jahr, y = Anzahl, 
                             fill = Umzug)) + 
  geom_bar(stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk, scales = "free_y") +
  theme_bw() +
  labs(y = "Anzahl Umzüge", fill = "Umzug") +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "innerstaedtisch" = "#8491B4"))

ggplot(umzug_Bezirksgruppen_long, aes(x = Jahr, y = Anzahl, 
                                      fill = factor(Umzug, levels = c("außerstaedtisch", "sonstige Bezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk, scales = "free_y") +
  theme_bw() +
  labs(y = "Anzahl Umzüge", fill = "Umzug") + 
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "sonstige Bezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF"))

# stacked percentage barplot 
plot_Umzug_Stadt <- ggplot(umzug_stadt_long, aes(x = Jahr, y = Anzahl, fill = Umzug)) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  ylab("Anteil Umzüge") +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "innerstaedtisch" = "#8491B4")) + 
  theme(axis.text.x = element_text(size = 7))

plot_Umzug_Bezirke <- ggplot(umzug_Bezirksgruppen_long, 
                             aes(x = Jahr, y = Anzahl, 
                                 fill = factor(Umzug, levels = c("außerstaedtisch", "Restbezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  labs(fill = "Umzug", y = "Anteil Umzüge") +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "sonstige Bezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF")) +
  theme(axis.text.x = element_text(size = 7))

# save plots
ggsave("Results/plot_Umzug_Stadt.jpg", plot = plot_Umzug_Stadt, width = 12, height = 8)
ggsave("Results/plot_Umzug_Bezirke.jpg", plot = plot_Umzug_Bezirke, width = 12, height = 8)

# Entwicklung Umzüge Münchens inner-/außerstaedtisch
# pivot_to_lonh for plotting

Mobilitaet_long <- Mobilitaet_thin %>%
  mutate(Raumbezug = case_when(
    BezirksID == 1 ~ "Altstadt",
    BezirksID == 2 ~ "Ludwigsvorstadt",
    BezirksID == 3 ~ "Maxvorstadt",
    BezirksID == 4 ~ "Schwabing-West",
    BezirksID == 5 ~ "Haidhausen",
    BezirksID == 6 ~ "Sendling",
    BezirksID == 7 ~ "Sendling-Westpark",
    BezirksID == 8 ~ "Schwanthalerhöhe",
    BezirksID == 9 ~ "Neuhausen",
    BezirksID == 10 ~ "Moosach",
    BezirksID == 11 ~ "Milbertshofen",
    BezirksID == 12 ~ "Schwabing",
    BezirksID == 13 ~ "Bogenhausen",
    BezirksID == 14 ~ "Berg am Laim",
    BezirksID == 15 ~ "Trudering",
    BezirksID == 16 ~ "Ramersdorf",
    BezirksID == 17 ~ "Obergiesing",
    BezirksID == 18 ~ "Untergiesing",
    BezirksID == 19 ~ "Thalkirchen",
    BezirksID == 20 ~ "Hadern",
    BezirksID == 21 ~ "Pasing",
    BezirksID == 22 ~ "Aubing",
    BezirksID == 23 ~ "Allach",
    BezirksID == 24 ~ "Feldmoching",
    BezirksID == 25 ~ "Laim",
    TRUE ~ Raumbezug)) %>%
  pivot_longer(cols = c("innerstädtisch.Weggezogene..", "außerstädtisch.Weggezogene."), 
               names_to = "Wegzug",
               values_to = "Anzahl_Wegzug") 
  
# plot for Zuzug
Mobilitaet_muenchen_weg <- Mobilitaet_long %>%
  filter(Ausprägung == "insgesamt" & Raumbezug == "Stadt München") %>%
  select(all_of(c("Jahr", "Wegzug", "Anzahl_Wegzug"))) %>%
  mutate(Wegzug = case_when(
    Wegzug == "innerstädtisch.Weggezogene.." ~ "innerstaedtisch",
    Wegzug == "außerstädtisch.Weggezogene." ~ "außerstaedtisch"))

# lineplot for Umzug Muenchen
ggplot(Mobilitaet_muenchen_weg,
       aes(x = Jahr, y = Anzahl_Wegzug, color = Wegzug)) +
  geom_point() + geom_line() +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  labs(y = "Anzahl in Tsd.", title = "Umzüge München") + 
  theme_bw() +
  scale_color_manual(values = c("#F0D852", "#8491B4"))

# bar plot abs
ggplot(Mobilitaet_muenchen_weg,
       aes(x = Jahr, y = Anzahl_Wegzug, fill = Wegzug)) +
  geom_bar(position =  "stack", stat = "identity", color = "black") + 
  scale_fill_manual(values = c("außerstaedtisch" =  "#F0D852", "innerstaedtisch" = "#8491B4")) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  labs(y = "Anzahl in Tsd.", title = "Umzüge München") + 
  theme_bw() 

# bar blot percentage

ggplot(Mobilitaet_muenchen_weg,
       aes(x = Jahr, y = Anzahl_Wegzug, fill = Wegzug)) +
  geom_bar(position = "fill", stat = "identity", color = "white") + 
  scale_fill_manual(values = c("außerstaedtisch" =  "#F0D852", "innerstaedtisch" = "#8491B4")) +
  labs(y = "Anteil", title = "Umzüge München") + 
  theme_bw() 

