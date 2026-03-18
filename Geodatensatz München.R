#Tipp aus Word
#Bezirksdatensatz plotten
install.packages("sf")
library("sf")
read.csv("vablock_stadtbezirk")
bezirke <- vablock_stadtbezirk
bezirke <- bezirke %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)
ggplot(bezirke) + 
  geom_sf() + 
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Mobilitätsdatensatz für join vorbereiten
Mobilität_clean <- indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25 %>%
  mutate(bezirk_num = sub(" .*", "", Raumbezug)) %>%
  mutate(Zuzügegesamt = Basiswert.1 + Basiswert.2) %>%
  mutate(Wegzügegesamt = Basiswert.3 + Basiswert.4)
Mobilität_cleaner <- Mobilität_clean %>%
  filter(grepl("^[0-9]{2} ", Raumbezug)) 

#Join durchführen
plotdata <- bezirke %>%
  left_join(Mobilität_cleaner, by = c("sb_nummer" = "bezirk_num"))

#plotdata für Zuzüge 2024 nicht deutsch vorbereiten und dann plotten
plotdata2024Zuzügenichtdeutsch <- plotdata %>%
  filter(Jahr == 2024, Ausprägung == "nichtdeutsch") %>%
  mutate(Zuzügegesamt = Basiswert.1 + Basiswert.2)

ggplot(plotdata2024Zuzügenichtdeutsch) + 
  geom_sf(aes(fill = Zuzügegesamt)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Zuzüge 2024 von nicht deutschen") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


plotdata2024Zuzügeinsgesamt <- plotdata %>%
  filter(Jahr == 2024, Ausprägung == "insgesamt")

ggplot(plotdata2024Zuzügeinsgesamt) + 
  geom_sf(aes(fill = Zuzügegesamt)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Zuzüge 2024 insgesamt")

#plotdata für Wegzüge 2024 insgesamt
plotdata2024Wegzügeinsgesamt <- plotdata %>% filter(Jahr == 2024, Ausprägung == "insgesamt")
ggplot(plotdata2024Wegzügeinsgesamt) +
  geom_sf(aes(fill = Wegzügegesamt)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Wegzüge 2024 insgesamt")

#plotdata für Wegzüge 2000 insgesamt
plotdata2000Wegzügeinsgesamt <- plotdata %>% filter(Jahr == 2000, Ausprägung == "insgesamt")
ggplot(plotdata2000Wegzügeinsgesamt) +
  geom_sf(aes(fill = Wegzügegesamt)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Wegzüge 2000 insgesamt")

#Facet
plotdatafacet <- plotdata %>% filter(Ausprägung == "insgesamt")

ggplot(plotdatafacet) +
  geom_sf(aes(fill = Wegzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge insgesamt nach Stadtbezirk",
       fill = "Wegzüge") +
  theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())

ggplot(plotdatafacet) +
  geom_sf(aes(fill = Basiswert.3)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge aus München nach Stadtbezirk",
       fill = "Wegzüge"
       legend = "Wegzüge außerstädtisch") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())









