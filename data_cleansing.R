library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
library(lubridate)
library(readxl)
library(data.table)
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

# create column with numbers in "Raumbezug" to ease identification
Bevoelkerungsdichte_thin <- Bevoelkerungsdichte_thin %>%
  mutate(
    BezirksID = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))

Mobilitaet_thin <- Mobilitaet_thin %>%
  mutate(
    BezirksID = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))

# save Mobilitaet_thin and Bevoelkerungsdichte_thin
write.csv(Mobilitaet_thin, "Clean_Data/Mobilitaet_thin.csv", row.names = FALSE)
write.csv(Bevoelkerungsdichte_thin, "Clean_Data/Bevoelkerungsdichte_thin.csv", row.names = FALSE)

# read Excel files and pivot longer

umzug2005_long <- read_xls("Data/Exceldateien Jahrbuch/jt060124.xls") %>%
  matrix_to_long(2005)
umzug2006_long <- read_xls("Data/Exceldateien Jahrbuch/jt070124.xls") %>%
  matrix_to_long(2006)
umzug2007_long <- read_xls("Data/Exceldateien Jahrbuch/jt080125.xls") %>%
  matrix_to_long(2007)
umzug2008_long <- read_xls("Data/Exceldateien Jahrbuch/jt090130.xls") %>%
  matrix_to_long(2008)
umzug2009_long <- read_xls("Data/Exceldateien Jahrbuch/jt100138.xls") %>%
  matrix_to_long(2009)
umzug2010_long <- read_xls("Data/Exceldateien Jahrbuch/jt110142.xls") %>%
  matrix_to_long(2010)
umzug2011_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt120145.xlsx") %>%
  matrix_to_long(2011)
umzug2012_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt130145.xlsx") %>%
  matrix_to_long(2012)
umzug2013_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt140145.xlsx") %>%
  matrix_to_long(2013)
umzug2014_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt150146.xlsx") %>%
  matrix_to_long(2014)
umzug2015_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt160140.xlsx") %>%
  matrix_to_long(2015)
umzug2016_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt170142.xlsx") %>%
  matrix_to_long(2016)
umzug2017_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt180142.xlsx") %>%
  matrix_to_long(2017)
umzug2018_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt190142.xlsx") %>%
  matrix_to_long(2018)
umzug2019_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt200142.xlsx") %>%
  select(!colnames(.)[2]) %>%
  matrix_to_long(2019)
umzug2020_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt210142.xlsx") %>%
  select(!colnames(.)[2]) %>%
  matrix_to_long(2020)
umzug2021_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt220142.xlsx") %>%
  select(!colnames(.)[2]) %>%
  matrix_to_long(2021)
umzug2022_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt230142.xlsx") %>%
  select(!colnames(.)[2]) %>%
  matrix_to_long(2022)
umzug2023_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt240142.xlsx") %>%
  select(!colnames(.)[2]) %>%
  matrix_to_long(2023)
umzug2024_long <- read_xlsx("Data/Exceldateien Jahrbuch/jt250142.xlsx") %>%
  select(!colnames(.)[2]) %>%
  matrix_to_long(2024)

# combine all long Data Frames to one
umzug_all <- bind_rows(umzug2005_long, umzug2006_long, umzug2007_long, umzug2008_long,
                       umzug2009_long, umzug2010_long, umzug2011_long, umzug2012_long,
                       umzug2013_long, umzug2014_long, umzug2015_long, umzug2016_long,
                       umzug2017_long, umzug2018_long, umzug2019_long, umzug2020_long, 
                       umzug2021_long, umzug2022_long, umzug2023_long, umzug2024_long)
# save umzug_all
write.csv(umzug_all, "Clean_Data/umzug_all.csv", row.names = FALSE)