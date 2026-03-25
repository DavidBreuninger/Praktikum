library(readxl)
library(tidyverse)
library(ggplot2)
library(data.table)
source("functions.R")

# read umzug_all
umzug_all <- read.csv("Clean_Data/umzug_all.csv")
# bar plot tests

ggplot(umzug_zsmhang_04_05, aes(x = Jahr, y = Anzahl, fill = Umzugsbezirk)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~Anfangsbezirk)

umzugMaxvorstadt <- umzug_all %>%
  filter(Anfangsbezirk_Nr == 3)
# stacked barplot 
ggplot(umzug_all, aes(x = Jahr, y = Anzahl, fill = Umzugsbezirk)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw()

# grouped barplot
ggplot(umzug_all, aes(x = Jahr, y = Anzahl, fill = Umzugsbezirk)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ Anfangsbezirk)

ggplot(umzugMaxvorstadt, aes(x = Jahr, y = Anzahl, fill = Umzugsbezirk)) + 
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ Anfangsbezirk)
