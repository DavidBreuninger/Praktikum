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

# read Excel files
umzug2005 <- read_xls("Data/Exceldateien Jahrbuch/jt060124.xls") 
umzug2006 <- read_xls("Data/Exceldateien Jahrbuch/jt070124.xls") 
umzug2007 <- read_xls("Data/Exceldateien Jahrbuch/jt080125.xls") 
umzug2008 <- read_xls("Data/Exceldateien Jahrbuch/jt090130.xls") 
umzug2009 <- read_xls("Data/Exceldateien Jahrbuch/jt100138.xls") 
umzug2010 <- read_xls("Data/Exceldateien Jahrbuch/jt110142.xls") 
umzug2011 <- read_xlsx("Data/Exceldateien Jahrbuch/jt120145.xlsx") 
umzug2012 <- read_xlsx("Data/Exceldateien Jahrbuch/jt130145.xlsx") 
umzug2013 <- read_xlsx("Data/Exceldateien Jahrbuch/jt140145.xlsx") 
umzug2014 <- read_xlsx("Data/Exceldateien Jahrbuch/jt150146.xlsx") 
umzug2015 <- read_xlsx("Data/Exceldateien Jahrbuch/jt160140.xlsx") 
umzug2016 <- read_xlsx("Data/Exceldateien Jahrbuch/jt170142.xlsx") 
umzug2017 <- read_xlsx("Data/Exceldateien Jahrbuch/jt180142.xlsx") 
umzug2018 <- read_xlsx("Data/Exceldateien Jahrbuch/jt190142.xlsx") 
# removing second column to ease transformation
umzug2019 <- read_xlsx("Data/Exceldateien Jahrbuch/jt200142.xlsx") %>%
  select(!colnames(.)[2]) 
umzug2020 <- read_xlsx("Data/Exceldateien Jahrbuch/jt210142.xlsx") %>%
  select(!colnames(.)[2]) 
umzug2021 <- read_xlsx("Data/Exceldateien Jahrbuch/jt220142.xlsx") %>%
  select(!colnames(.)[2])
umzug2022 <- read_xlsx("Data/Exceldateien Jahrbuch/jt230142.xlsx") %>%
  select(!colnames(.)[2])
umzug2023 <- read_xlsx("Data/Exceldateien Jahrbuch/jt240142.xlsx") %>%
  select(!colnames(.)[2]) 
umzug2024 <- read_xlsx("Data/Exceldateien Jahrbuch/jt250142.xlsx") %>%
  select(!colnames(.)[2])

# transform matrix to grouped sums
umzug2005_grouped <- umzug2005 %>%
  colnames_to_ID() %>%
  add_umzug_info(2005) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2006_grouped <- umzug2006 %>%
  colnames_to_ID() %>%
  add_umzug_info(2006) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2007_grouped <- umzug2007 %>%
  colnames_to_ID() %>%
  add_umzug_info(2007) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2008_grouped <- umzug2008 %>%
  colnames_to_ID() %>%
  add_umzug_info(2008) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2009_grouped <- umzug2009 %>%
  colnames_to_ID() %>%
  add_umzug_info(2009) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2010_grouped <- umzug2010 %>%
  colnames_to_ID() %>%
  add_umzug_info(2010) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2011_grouped <- umzug2011 %>%
  colnames_to_ID() %>%
  add_umzug_info(2011) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2012_grouped <- umzug2012 %>%
  colnames_to_ID() %>%
  add_umzug_info(2012) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2013_grouped <- umzug2013 %>%
  colnames_to_ID() %>%
  add_umzug_info(2013) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2014_grouped <- umzug2014 %>%
  colnames_to_ID() %>%
  add_umzug_info(2014) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2015_grouped <- umzug2015 %>%
  colnames_to_ID() %>%
  add_umzug_info(2015) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2016_grouped <- umzug2016 %>%
  colnames_to_ID() %>%
  add_umzug_info(2016) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2017_grouped <- umzug2017 %>%
  colnames_to_ID() %>%
  add_umzug_info(2017) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2018_grouped <- umzug2018 %>%
  colnames_to_ID() %>%
  add_umzug_info(2018) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2019_grouped <- umzug2019 %>%
  colnames_to_ID() %>%
  add_umzug_info(2019) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2020_grouped <- umzug2020 %>%
  colnames_to_ID() %>%
  add_umzug_info(2020) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2021_grouped <- umzug2021 %>%
  colnames_to_ID() %>%
  add_umzug_info(2021) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2022_grouped <- umzug2022 %>%
  colnames_to_ID() %>%
  add_umzug_info(2022) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2023_grouped <- umzug2023 %>%
  colnames_to_ID() %>%
  add_umzug_info(2023) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()
umzug2024_grouped <- umzug2024 %>%
  colnames_to_ID() %>%
  add_umzug_info(2024) %>%
  sum_im_bezirk() %>%
  sum_Nachbarbezirke() %>%
  sum_Restbezirke()

# combine all grouped Data Frames to one
umzug_all <- bind_rows(umzug2005_grouped, umzug2006_grouped, umzug2007_grouped, umzug2008_grouped,
                       umzug2009_grouped, umzug2010_grouped, umzug2011_grouped, umzug2012_grouped,
                       umzug2013_grouped, umzug2014_grouped, umzug2015_grouped, umzug2016_grouped,
                       umzug2017_grouped, umzug2018_grouped, umzug2019_grouped, umzug2020_grouped, 
                       umzug2021_grouped, umzug2022_grouped, umzug2023_grouped, umzug2024_grouped)

# select different constellation of columns for plotting
umzug_ohneBezirke <- umzug_all %>%
  select(!all_of(as.character(c(1:25))))
umzug_innen_außen <-  umzug_all %>%
  select(!all_of(as.character(c(1:25, "selber_Bezirk", "Nachbarbezirke", "Restbezirke"))))
umzug_Bezirksgruppen <- umzug_all %>%
  select(!all_of(as.character(c(1:25, "innerstaedtisch"))))

# save 
write.csv(umzug_ohneBezirke, "Clean_Data/umzug_ohneBezirke.csv", row.names = FALSE)
write.csv(umzug_innen_außen, "Clean_Data/umzug_innen_außen.csv", row.names = FALSE)
write.csv(umzug_Bezirksgruppen, "Clean_Data/umzug_Bezirksgruppen.csv", row.names = FALSE)


