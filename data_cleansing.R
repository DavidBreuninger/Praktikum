library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
library(lubridate)
source("functions.R")

# read csv files
Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")

# remove columns or lines consisting of only NAs
Mobilitaet <- remove_NAs(Mobilitaet)
Bevoelkerungsdichte <- remove_NAs(Bevoelkerungsdichte)
