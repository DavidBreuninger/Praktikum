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
ggplot(umzug_stadt_long, aes(x = Jahr, y = Anzahl, fill = Umzug)) + 
  geom_bar(stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk, scales = "free") +
  theme_bw()

ggplot(umzug_Bezirksgruppen_long, aes(x = Jahr, y = Anzahl, fill = Umzug)) + 
  geom_bar(stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk, scales = "free") +
  theme_bw()

# stacked percentage barplot 
ggplot(umzug_stadt_long, aes(x = Jahr, y = Anzahl, fill = Umzug)) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw()

ggplot(umzug_Bezirksgruppen_long, aes(x = Jahr, y = Anzahl, fill = Umzug)) + 
  geom_bar(position = "fill", stat = "identity",  color = "white") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw()
