#Data erhalten
Mobilitätsziffer<- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")

#library
library(dplyr)
library(tidyr)

#Daten schauen
head(Mobilitätsziffer)
colnames(Mobilitätsziffer)

#von Wide Format nach Long Format
mob_long <- Mobilitätsziffer %>%
  pivot_longer(
    cols = matches("Basiswert|Name.Basiswert"),
    names_to = c(".value", "id"),
    names_pattern = "(.*)\\.(\\d+)"
  )

# Zu- und Umzuege 
zuzug <- mob_long %>%
  filter(grepl("Zugezogene", Name.Basiswert))

#Entferne Stadt Muenchen
unique(mob_long$Raumbezug)
zuzug <- zuzug %>% 
  filter(Raumbezug!= "Stadt München")

#Nur Deutsch und Nicht-Deutsch
unique(mob_long$Ausprägung)
zuzug <- zuzug %>% 
  filter(Ausprägung%in%c("deutsch","nichtdeutsch")
  )

#Grafik
library(ggplot2)
Zuzug <-ggplot(zuzug,aes(x=Jahr,y=Basiswert,color=Ausprägung))+
  geom_line()+facet_wrap(~Raumbezug)+
  labs(title = "Entwicklung der Zuzüge in die Münchner Stadtbezirke",
       color="Nationalitaet",y="Anzahl der Zuzuege")

Zuzug 
#save Grafik
ggsave("Result/zuzug_plot.jpg", plot = Zuzug)


