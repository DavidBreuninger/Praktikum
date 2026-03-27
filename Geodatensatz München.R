#Tipp aus Word
#Pakete laden
install.packages("sf")
library("sf")
library("dplyr")
library("ggplot2")
library("readr")
library("stringr")

#Datensätze einlesen
#https://opendata.muenchen.de/dataset/vablock_stadtbezirke_opendata
read.csv("Data/vablock_stadtbezirk.csv")
Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")
read_csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
bezirke <- vablock_stadtbezirk

#Geodatensatz plotten
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

#Bezirksnamen kürzen
bezirkkurz <- bezirke %>%
  group_by(sb_nummer, sb_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(short_name = sb_name)

bezirkkurz$short_name <- bezirkkurz$short_name %>%
  str_replace_all("Ludwigsvorstadt-Isarvorstadt", "Ludwigsv.-Isarv.") %>%
  str_replace_all("Schwanthalerhöhe", "Schwanthalerh.") %>%
  str_replace_all("Au-Haidhausen", "Au-Haidh.") %>%
  str_replace_all("Altstadt-Lehel", "Altst.-Lehel") %>%
  str_replace_all("Thalkirchen-Obersendling-Forstenried-Fürstenried-Solln",
                  "Thalk.-Obersendl.-Forstenr.-Fürstenr.-Solln")


centroids <- st_centroid(bezirkkurz)
centroids <- st_point_on_surface(bezirkkurz)
ggplot(bezirkkurz) +
  geom_sf(fill = "lightgrey", color = "black") +
  geom_sf_text(data = centroids, aes(label = short_name), size = 3)

#Bestimmte Stadtbezirke hervorheben
highlight <- c(
  "Altstadt-Lehel",
  "Ludwigsvorstadt-Isarvorstadt",
  "Sendling",
  "Schwabing-Freimann",
  "Trudering-Riem",
  "Aubing-Lochhausen-Langwied"
)

bezirkkurz2 <- bezirkkurz %>%
  mutate(highlight = sb_name %in% highlight)

ggplot(bezirkkurz2) +
  geom_sf(aes(fill = highlight), color = "black") +
  scale_fill_manual(values = c("grey85", "green")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  theme(legend.position = "none")

ggplot(bezirkkurz2) +
  geom_sf(aes(fill = highlight), color = "black") +
  scale_fill_manual(values = c("grey", "orange")) +
  geom_sf_text(
    data = bezirkkurz2 %>% filter(highlight),
    aes(label = short_name),
    size = 3
  ) +
 theme(legend.position = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Mobilitätsdatensatz für join vorbereiten
Mobilität_clean <- Mobilitaet %>%
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

#Facet Wegzüge insgesamt
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

#Facet Wegzüge außerstädtisch
ggplot(plotdatafacet) +
  geom_sf(aes(fill = Basiswert.3)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge aus München nach Stadtbezirk",
       fill = "Wegzüge innerstädtisch") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Facet Wegzüge innerstädtisch
ggplot(plotdatafacet) +
  geom_sf(aes(fill = Basiswert.4)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge innerhalb Münchens nach Stadtbezirk",
       fill = "Wegzüge innerstädtisch") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Facet Zuzüge außerstädtisch
ggplot(plotdatafacet) +
  geom_sf(aes(fill = Basiswert.1)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzüge außerstädtisch nach Stadtbezirk",
       fill = "Zuzüge außerstädtisch") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Facet Zuzüge innerstädtisch
ggplot(plotdatafacet) +
  geom_sf(aes(fill = Basiswert.2)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzüge innerstädtisch nach Stadtbezirk",
       fill = "Zuzüge innerstädtisch") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Netto-Migration
plotdataNetto <- plotdata %>%
  filter(Ausprägung == "insgesamt") %>%
  mutate(netto = (Basiswert.1 + Basiswert.2) - (Basiswert.3 + Basiswert.4))

ggplot(plotdataNetto) +
  geom_sf(aes(fill = netto)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
  ) +
  facet_wrap(~ Jahr) +
  labs(title = "Netto Migration") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

#Wegzüge insgesamt alle fünf Jahre
plotdatand <- plotdata %>%
  filter(Ausprägung == "nichtdeutsch")

plotdatad <- plotdata %>%
  filter(Ausprägung == "deutsch")

ggplot(plotdatafacet) +
  geom_sf(aes(fill = Wegzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge von 2000 bis 2024",
       fill = "Anzahl Wegzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggplot(plotdatafacet) +
  geom_sf(aes(fill = Zuzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzüge von 2000 bis 2024",
       fill = "Anzahl Zuzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggplot(plotdatand) +
  geom_sf(aes(fill = Wegzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge von nicht-deutschen",
       fill = "Anzahl Wegzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggplot(plotdatand) +
  geom_sf(aes(fill = Zuzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzüge von nicht-deutschen",
       fill = "Anzahl Zuzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggplot(plotdatad) +
  geom_sf(aes(fill = Wegzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Wegzüge von deutschen",
       fill = "Anzahl Wegzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

ggplot(plotdatad) +
  geom_sf(aes(fill = Zuzügegesamt)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzüge von deutschen",
       fill = "Anzahl Zuzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

plotdatap <- plotdata %>%
  filter(Ausprägung == "nichtdeutsch") %>%
  mutate(Zuzugsrate = (Zuzügegesamt / Basiswert.5) * 100)

ggplot(plotdatap) +
  geom_sf(aes(fill = Zuzugsrate)) +
  scale_fill_viridis_c() +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Zuzüge von deutschen",
       fill = "Anzahl Zuzüge") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
#Plots mit Bevölkerungsdatensatz
#Bevölkerungsdatensatz für join vorbereiten
Bevölkerung_clean <- indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25 %>%
  mutate(bezirk_num = sub(" .*", "", Raumbezug)) %>%
  filter(grepl("^[0-9]{2} ", Raumbezug))

#Join durchführen
plotdatabevölkerung <- bezirke %>%
  left_join(Bevölkerung_clean, by = c("sb_nummer" = "bezirk_num"))

#Bevölkerung 2024
plotdataBevölkerungsentwicklung <- plotdatabevölkerung %>%
  filter(Jahr == 2024)

maxb <- max(abs(plotdataBevölkerungsentwicklung$Basiswert.1), na.rm = TRUE)
minb <- min(abs(plotdataBevölkerungsentwicklung$Basiswert.1), na.rm = TRUE)
ggplot(plotdataBevölkerungsentwicklung) +
  geom_sf(aes(fill = Basiswert.1)) +
  scale_fill_viridis_c(
    limits = c(minb, maxb),
    breaks = c(minb, 40000, 60000, 80000, 100000, maxb)
  ) +
  theme_minimal() +
  labs(title = "Bevölkerung 2024",
       fill = "Anzahl Einwohner") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Bevölkerungsdichte 2024
plotdataBevölkerungsdichte <- plotdatabevölkerung %>%
  mutate(Bevölkerungsdichte = Basiswert.1 / Basiswert.2) %>%
  filter(Jahr == 2024, Ausprägung == "insgesamt")

maxd <- max(abs(plotdataBevölkerungsdichte$Bevölkerungsdichte), na.rm = TRUE) %>% round(digits = 2)
mind <- min(abs(plotdataBevölkerungsdichte$Bevölkerungsdichte), na.rm = TRUE) %>%
  round(digits = 2)

ggplot(plotdataBevölkerungsdichte) +
  geom_sf(aes(fill = Bevölkerungsdichte)) +
  scale_fill_viridis_c(
    limits = c(1780, 15600),
    breaks = c(1780, 4000, 8000, 12000, 15600)
  ) +
  theme_minimal() +
  labs(title = "Bevölkerungsdichte nach Stadtbezirk 2024",
       fill = "Dichte pro km²") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Alle drei Datensätze joinen
Mobilität_suited <- Mobilität_cleaner %>%
  filter(Ausprägung == "insgesamt") %>%
  filter(Jahr >= 2002)

mundb <- left_join(Bevölkerung_clean, Mobilität_suited, by = c("Raumbezug", "Jahr"))

full <- bezirke %>%
  left_join(mundb, by = c("sb_nummer" = "bezirk_num.x")) 

#Neue Spalten
fullnewcolumns <- full %>%
  mutate(zuzugsrate = Zuzügegesamt / Basiswert.1.x * 100,
         wegzugsrate = Wegzügegesamt / Basiswert.1.x * 100,
         netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.1.x * 100)
plotdataNettorate <- plotdataNetto %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.5 * 100)

#Netto-Migrationsrate plotten
max_abs <- max(abs(plotdataNettorated$netto_rate), na.rm = TRUE)

ggplot(plotdataNettorate) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-7, 7),
    breaks = c(-7, -4, -2, 0, 2, 4, 7)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Netto-Migrationsrate nach Stadtbezirk",
       fill = "Netto-Migrationsrate") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Netto Migrationsrate für nicht-deutsche
plotdataNettoratend <- plotdata %>%
  filter(Ausprägung == "nichtdeutsch") %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.5 * 100)

ggplot(plotdataNettoratend) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-19, 19),
    breaks = c(-19, -15, -10, -5, 0, 5, 10, 15, 19)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Netto-Migrationsrate von nicht-deutschen",
       fill = "Netto-Migrationsrate in Prozent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

#Netto-Migrationsrate von deutschen
plotdataNettorated <- plotdata %>%
  filter(Ausprägung == "deutsch") %>%
  mutate(netto_rate = (Zuzügegesamt - Wegzügegesamt) / Basiswert.5 * 100)

ggplot(plotdataNettorated) +
  geom_sf(aes(fill = netto_rate)) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-4, 4),
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  facet_wrap(~ Jahr) +
  theme_minimal() +
  labs(title = "Netto-Migrationsrate von deutschen",
       fill = "Netto-Migrationsrate in Prozent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
