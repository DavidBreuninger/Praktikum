library(readxl)
library(tidyverse)
library(ggplot2)
library(data.table)
source("functions.R")

# read umzug_all
umzug_all <- read.csv("Clean_Data/umzug_all.csv")

# stacked barplot 
ggplot(umzug_all, aes(x = Jahr, y = Anzahl, fill = Umzugsbezirk)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Anfangsbezirk) +
  theme_bw()

