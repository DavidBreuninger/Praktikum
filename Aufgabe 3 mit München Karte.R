#Bezirksdatensatz plotten
install.packages("sf")
library("sf")
library("dplyr")
library("ggplot2")
library("readr")
read_csv("data/vablock_stadtbezirk.csv")
read_csv("data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
bezirke <- vablock_stadtbezirk
bezirke <- bezirke %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()
st_crs(bezirke) <- 25832
bezirke <- st_transform(bezirke, 4326)

#Bevölkerungsdatensatz für join vorbereiten
Bevölkerung_clean <- indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25 %>%
  mutate(bezirk_num = sub(" .*", "", Raumbezug)) %>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#Join durchführen
plotdatabevölkerung <- bezirke %>%
  left_join(Bevölkerung_clean, by = c("sb_nummer" = "bezirk_num"))

#Bevölkerungsentwicklung
plotdataBevölkerungsentwicklung <- plotdata %>%
  filter(Jahr == 2024, Ausprägung == "nichtdeutsch") %>%
  mutate(Zuzügegesamt = Basiswert.1 + Basiswert.2)

ggplot(plotdatabevölkerung) +
  geom_sf(aes(fill = Basiswert.1)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Bevölkerungsentwicklung nach Stadtbezirk",
       fill = "Anzahl Einwohner") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Entwicklung der Bevölkerungsdichte
plotdataBevölkerungsdichte <- plotdatabevölkerung %>%
  mutate(Bevölkerungsdichte = Basiswert.1 / Basiswert.2)

ggplot(plotdataBevölkerungsdichte) +
  geom_sf(aes(fill = Bevölkerungsdichte)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Bevölkerungsdichte nach Stadtbezirk",
       fill = "Dichte im Bezirk") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())








