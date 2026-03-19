#Entwicklung der Zu- und Umzüge in München (2000–2024)

#Unterscheidung zwischen Zuzug und Umzug
unique(mob_long$Name.Basiswert)

zuzug <- mob_long %>%
  filter(grepl("außerstädtisch Zugezogene", Name.Basiswert)) %>%
  filter(Ausprägung %in% c("deutsch", "nichtdeutsch"))

umzug <- mob_long %>%
  filter(grepl("innerstädtisch Zugezogene", Name.Basiswert)) %>%
  filter(Ausprägung %in% c("deutsch", "nichtdeutsch"))

#add Typ
zuzug$Typ <- "Zuzug"
umzug$Typ <- "Umzug"

#Setze Ober- und Unterteil zusammen
data_combined <- bind_rows(zuzug, umzug)

#Data clean
data<- data_combined %>%
  filter(Raumbezug == "Stadt München")

#Grafik
Gesamt <- ggplot(data,
       aes(x = Jahr,
           y = Basiswert,
           color = Ausprägung,
           linetype = Typ)) +
  geom_line(size = 1.2) +
  labs(title="Entwicklung der Zu- und Umzüge in München (2000–2024)",
       y="Anzahl der Personen",
       color = "Nationalität")
  theme_minimal()

Gesamt

#save Grafik
ggsave("Result/Gesamt.jpg", plot = Gesamt)

#save Datensaetzen
write.csv(data, "Data/A1_data.csv", row.names = FALSE)  
  
  
  
  
  
  
  
  
  
  
  
  
