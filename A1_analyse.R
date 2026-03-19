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

#Zuzuege
zuzug<- zuzug %>%
  group_by(Jahr, Raumbezug, Ausprägung) %>%
  summarise(Zuzuege = sum(Basiswert), .groups = "drop")

#Grafik
library(ggplot2)
Zuzug <-ggplot(zuzug,
               aes(x=Jahr,
                   y=Zuzuege,
                   color=Ausprägung))+
  geom_line()+
  facet_wrap(~Raumbezug,
             labeller = label_wrap_gen(width = 30))+
  labs(title = "Entwicklung der Zu- und Umzüge in die Münchner Stadtbezirke (2000–2024)",
       color="Nationalität",y="Anzahl der Zu- und Umzüge")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))
      
Zuzug 

#save Grafik
ggsave("Result/zuzug_plot.jpg", plot = Zuzug,width = 10, height = 6)

#save Datensaetzen
write.csv(mob_long, "Data/A1_mob_long.csv", row.names = FALSE)
write.csv(zuzug, "Data/A1_zuzug.csv", row.names = FALSE)










