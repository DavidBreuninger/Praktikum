library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
library(lubridate)
source("functions.R")

# read csv files
Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")

# remove columns or rows consisting of only NAs
Mobilitaet <- remove_NAs(Mobilitaet)
Bevoelkerungsdichte <- remove_NAs(Bevoelkerungsdichte)

# overview of data
str(Mobilitaet)
summary(Mobilitaet)
str(Bevoelkerungsdichte)
summary(Bevoelkerungsdichte)

# remove text with brackets
Mobilitaet_thin <- remove_Auspraegung(Mobilitaet)
Bevoelkerungsdichte_thin <- remove_Auspraegung(Bevoelkerungsdichte)

sapply(Mobilitaet, function(x) n_distinct(x))

# rename columns
Mobilitaet_thin <- Mobilitaet_thin %>%
  pivot_wider(
    names_from = Indikator,
    values_from = Indikatorwert) %>%
  pivot_wider(
    names_from = Name.Basiswert.1,
    values_from = Basiswert.1) %>%
  pivot_wider(
    names_from = Name.Basiswert.2,
    values_from = Basiswert.2) %>%
  pivot_wider(
    names_from = Name.Basiswert.3,
    values_from = Basiswert.3) %>%
  pivot_wider(
    names_from = Name.Basiswert.4,
    values_from = Basiswert.4) %>%
  pivot_wider(
    names_from = Name.Basiswert.5,
    values_from = Basiswert.5)

Bevoelkerungsdichte_thin <- Bevoelkerungsdichte_thin %>%
  pivot_wider(
    names_from = Indikator,
    values_from = Indikatorwert) %>%
  pivot_wider(
    names_from = Name.Basiswert.1,
    values_from = Basiswert.1) %>%
  pivot_wider(
    names_from = Name.Basiswert.2,
    values_from = Basiswert.2)
