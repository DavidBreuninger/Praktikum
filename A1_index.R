#Index Grafik

#NUr Zu- und Umzug
mob_index<- mob_long %>%
  filter(grepl("Zugezogene", Name.Basiswert)) %>%  
  filter(Ausprägung %in% c("deutsch", "nichtdeutsch"))

#add Typ
mob_index <- mob_index %>%
  mutate(Typ = ifelse(grepl("innerstädtisch", Name.Basiswert),
                 "Umzug", "Zuzug"))

#Entferne Stadt Muenchen
mob_index <- mob_index %>%
  filter(Raumbezug != "Stadt München")   

#Sortieren
mob_index <- mob_index %>%
  arrange(Raumbezug, Ausprägung, Typ, Jahr)

#Index
index <- mob_index %>%
  group_by(Raumbezug, Ausprägung, Typ) %>%
  mutate(Index = Basiswert / first(Basiswert) * 100) %>%
  ungroup()

#Grafik

#ZU und Umzug
data_sum <- index %>%
  group_by(Raumbezug, Jahr, Ausprägung) %>%
  summarise(Wert = sum(Basiswert), .groups = "drop")

data_sum <- data_sum %>%
  arrange(Raumbezug, Ausprägung, Jahr) %>%
  group_by(Raumbezug, Ausprägung) %>%
  mutate(Index = Wert / first(Wert) * 100) %>%
  ungroup()

IndexSum <- ggplot(data_sum,
       aes(x = Jahr,
           y = Index,
           color = Ausprägung)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Raumbezug,
             labeller = label_wrap_gen(width = 30)) +
  labs(
    title = "Indexierte Entwicklung der gesamten Zuzüge und Umzüge (2000 = 100)",
    x = "Jahr",
    y = "Index (2000 = 100)",
    color = "Nationalität"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 7, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

IndexSum


#Nur Zuzug 
IndexZuzug <- ggplot(index %>% filter(Typ == "Zuzug"),
       aes(x = Jahr,
           y = Index,
           color = Ausprägung,
           linetype = Typ)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Raumbezug,
             labeller = label_wrap_gen(width = 30)) +
  labs(
    title = "Indexierte Entwicklung der Zuzüge (2000 = 100)",
    x = "Jahr",
    y = "Index (2000 = 100)",
    color = "Nationalität",
    linetype = "Mobilitätstyp"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))

IndexZuzug


#Nur Umzug
IndexUmzug <- ggplot(index %>% filter(Typ == "Umzug"),
       aes(x = Jahr,
           y = Index,
           color = Ausprägung,
           linetype = Typ)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Raumbezug,
             labeller = label_wrap_gen(width = 30)) +
  labs(
    title = "Indexierte Entwicklung der Umzüge (2000 = 100)",
    x = "Jahr",
    y = "Index (2000 = 100)",
    color = "Nationalität",
    linetype = "Mobilitätstyp"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))

IndexUmzug

#save Grafik
ggsave("Result/IndexSum.jpg", plot = IndexSum,width = 10, height = 6)
ggsave("Result/IndexZuzug.jpg", plot = IndexZuzug,width = 10, height = 6)
ggsave("Result/IndexUmzug.jpg", plot = IndexUmzug,width = 10, height = 6)
#save Datensaetzen
write.csv(data, "Data/A1_data.csv", row.names = FALSE) 











 
