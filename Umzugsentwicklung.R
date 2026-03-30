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
  labs(fill = "Umzug", y = "Anteil", title = "Umzüge je nach Bezirk")  +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "innerstaedtisch" = "#8491B4")) + 
  theme(axis.text.x = element_text(size = 7))

plot_Umzug_Bezirke <- ggplot(umzug_Bezirksgruppen_long, 
                             aes(x = Jahr, y = Anzahl, 
                                 fill = factor(Umzug, levels = c("außerstaedtisch", "sonstige Bezirke", "Nachbarbezirke", "selber Bezirk")))) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw() + 
  labs(fill = "Umzug", y = "Anteil", title = "Umzüge je nach Bezirk") +
  scale_fill_manual(values = c("außerstaedtisch" = "#F0D852", "sonstige Bezirke" = "#009292", "Nachbarbezirke" = "#DB6D00", "selber Bezirk" = "#B66DFF")) +
  theme(axis.text.x = element_text(size = 7))

# save plots
ggsave("Results/plot_Umzug_Stadt.jpg", plot = plot_Umzug_Stadt, width = 12, height = 8)
ggsave("Results/plot_Umzug_Bezirke.jpg", plot = plot_Umzug_Bezirke, width = 12, height = 8)

# Entwicklung Umzüge Münchens inner-/außerstaedtisch
# pivot_to_lonh for plotting

Mobilitaet_long <- Mobilitaet_thin %>%
  pivot_longer(cols = c("innerstädtisch.Weggezogene..", "außerstädtisch.Weggezogene."), 
               names_to = "Wegzug",
               values_to = "Anzahl_Wegzug") %>%
  rename("mittlere_Bevölkerung" = mittlere.Hauptwohnsitzbevölkerung.)
  
# plot for Zuzug
Mobilitaet_muenchen_weg <- Mobilitaet_long %>%
  filter(Ausprägung == "insgesamt" & Raumbezug == "Stadt München") %>%
  select(all_of(c("Jahr", "Wegzug", "Anzahl_Wegzug", "mittlere_Bevölkerung"))) %>%
  mutate(Wegzug = case_when(
    Wegzug == "innerstädtisch.Weggezogene.." ~ "innerstaedtisch",
    Wegzug == "außerstädtisch.Weggezogene." ~ "außerstaedtisch")) %>% 
  mutate(Prozent = Anzahl_Wegzug / mittlere_Bevölkerung * 100)

# lineplot for Umzug Muenchen
plot_stadt_prozent <- ggplot(Mobilitaet_muenchen_weg,
       aes(x = Jahr, y = Prozent, color = Wegzug)) +
  geom_point() + geom_line() +
  labs(y = "Anteil in %", title = "Anteil Umzüge an mittlerer Bevölkerung", color = "Umzug") + 
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

Mobilitaet_allg <- Mobilitaet_muenchen_weg %>%
  group_by(Jahr, mittlere_Bevölkerung) %>%
  summarise(Gesamtwegzug = sum(Anzahl_Wegzug)) %>%
  ungroup() %>%
  mutate(Prozent = Gesamtwegzug / mittlere_Bevölkerung * 100)

plot_muenchen_prozent <- ggplot(Mobilitaet_allg, aes(x = Jahr,  y = Prozent)) +
  geom_point(color = "black") + geom_line(color = "black") +
  labs(y = "Anteil in %", title = "Anteil Umzüge an mittlerer Bevölkerung") +
  theme_bw()

# save plots
ggsave("Results/plot_muenchen_prozent.jpg", plot = plot_muenchen_prozent, width = 12, height = 8)
ggsave("Results/plot_stadt_prozent.jpg", plot = plot_stadt_prozent, width = 12, height = 8)
