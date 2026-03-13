library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")


#12, 19 ausreißer nicht deutscher
#https://stadt.muenchen.de/dam/jcr:fcc81666-f6ef-4c83-84b3-8714c6c38f6f/mb220302_r.pdf
#mit import dataset Klick in R geholt
#view(bevoelkerungsdichte)
#b<- bevoelkerungsdichte
#m <- mobilitaetsziffer
m <-Mobilitaet #nache einlesung
b <-Bevoelkerungsdichte #nache einlesung

b1<- b%>%
  filter(Raumbezug == "Stadt München")
ggplot(b1, aes(x = Jahr, y = Basiswert.1)) + geom_point() + geom_line() + geom_smooth(method = "lm")
model <- lm(Jahr ~ Basiswert.1, data = b1)
summary(model)

#Geborene 2025: 15 399 https://de.statista.com/statistik/daten/studie/1228950/umfrage/geburten-todesfaelle-muenchen/
#Gestorbene 2025: 12 539

b %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Indikatorwert, Jahr ) )%>%
  arrange(korrelation)%>%
  print(n = Inf)



b %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Jahr, Indikatorwert ) )%>%
  arrange(korrelation) %>%
  head(10)
b %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Jahr, Indikatorwert ) )%>%
  arrange(korrelation) %>%
  tail(10)
# betrachten 7 13 Bogenhausen                                                          0.991
#8 19 Thalkirchen - Obersendling - Forstenried - Fürstenried - Solln       0.991
#9 23 Allach - Untermenzing                                                0.992
#10 21 Pasing - Obermenzing                                                 0.995
#1 08 Schwanthalerhöhe                     0.778
#2 02 Ludwigsvorstadt - Isarvorstadt       0.798
#3 03 Maxvorstadt                          0.845
#4 01 Altstadt - Lehel                     0.902






b2 <- b%>%
  filter(Raumbezug %in% c("Stadt München", "01 Altstadt - Lehel", "03 Maxvorstadt",
                          "02 Ludwigsvorstadt - Isarvorstadt", "08 Schwanthalerhöhe",
                          "21 Pasing - Obermenzing", "23 Allach - Untermenzing",
                          "19 Thalkirchen - Obersendling - Forstenried - Fürstenried - Solln"
                          , "13 Bogenhausen"))%>%
  group_by(Raumbezug)

ggplot(b2, aes(x = Jahr, y = Indikatorwert, color = Raumbezug)) +geom_point() +geom_line()
b3 <- b2 %>%
  filter(Raumbezug != "Stadt München")
ggplot(b3, aes(x = Jahr, y = Basiswert.1, color = Raumbezug)) +geom_point() +geom_line()
ggplot(b3, aes(x = Jahr, y = Indikatorwert, color = Raumbezug)) +geom_point() +geom_line()

bu <- b%>%
  filter(Jahr <= 2014)
bo<- b%>%
  filter(Jahr > 2014)

bu %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Indikatorwert, Jahr ) )%>%
  print(n = Inf) # alle sehr hoch und ähnlich

bo %>%
  group_by(Raumbezug) %>%
  summarise(
    korrelation = cor(Indikatorwert, Jahr ) )%>%
  print(n = Inf) # sb 17 02 01 08  auffälig

bo1<- bo%>%
  filter(Raumbezug %in% c("01 Altstadt - Lehel", "17 Obergiesing - Fasangarten",
                          "02 Ludwigsvorstadt - Isarvorstadt", "08 Schwanthalerhöhe"))

ggplot(bo1, aes(x = Jahr , y = Indikatorwert, color = Raumbezug)) + geom_point() +geom_line()



bnew <- b%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))

bnewp <- bnew%>%
  filter(Jahr == 2004 | Jahr == 2014 | Jahr == 2024 | Jahr == 2019 | Jahr == 2009)

bnewp1 <- bnewp%>%
  filter(sn < 14)
bnewp2 <- bnewp%>%
  filter(sn > 13)


ggplot(bnewp1, aes(x= Jahr, y = Indikatorwert, colour = Raumbezug)) +geom_point() + geom_line()
ggplot(bnewp1, aes(x= Jahr, y = Basiswert.1, colour = Raumbezug)) +geom_point() + geom_line()

ggplot(bnewp2, aes(x= Jahr, y = Indikatorwert, colour = Raumbezug)) +geom_point() + geom_line()



bnewp11 <-bnewp%>% #sb ohne Außengrenze
  filter(sn < 11 | sn == 14 | sn == 25)
ggplot(bnewp11, aes(x= Jahr, y = Indikatorwert, colour = Raumbezug)) +geom_point() + geom_line()
bnewp12 <-bnewp%>%
  filter(sn > 10 & sn != 14 & sn != 25)
ggplot(bnewp12, aes(x= Jahr, y = Indikatorwert, colour = Raumbezug)) +geom_point() + geom_line()


ggplot(bnewp11, aes(x= Jahr, y = Indikatorwert)) +geom_point() + geom_line() + facet_wrap(~ sn, scales ="free")
ggplot(bnewp12, aes(x= Jahr, y = Indikatorwert)) +geom_point() + geom_line() + facet_wrap(~ sn, scales ="free")

ggplot(bnewp11, aes(x= Jahr, y = Indikatorwert)) +geom_point() + geom_line() + facet_wrap(~ sn)
ggplot(bnewp12, aes(x= Jahr, y = Indikatorwert)) +geom_point() + geom_line() + facet_wrap(~ sn)











m3 <- m%>% #protzentuale Wert für Ausagekraft ähnlcih wie Indikatorwert nur um 10 Faktor anders
  mutate(bpn1 = 100 *Basiswert.1 / Basiswert.5,
         bpn2 = 100 *Basiswert.2 / Basiswert.5,
         bpn3 = 100 *Basiswert.3 / Basiswert.5,
         bpn4 = 100 *Basiswert.4 / Basiswert.5)



mnew <- m3%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))



mnew <- mnew%>%
  mutate(gsr = Basiswert.1 - Basiswert.3)
mnew <- mnew%>%
  mutate(rgsr = gsr / Basiswert.5)

mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)
mnew <- mnew%>%
  mutate(rar = ar / Basiswert.5)

m1 <- m%>%
  filter(Raumbezug == "Stadt München")%>%
  filter(Ausprägung == "insgesamt")
ggplot(m1, aes(x = Jahr, y = Indikatorwert)) + geom_point() + geom_line() + geom_smooth(method = "lm")

m2 <- m%>%
  filter(Raumbezug == "Stadt München")
ggplot(m2, aes(x = Jahr, y = Indikatorwert, color = Ausprägung)) + geom_point() +geom_line()




m%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, Indikatorwert ) )%>%
  arrange(korrelation) %>%
  print(n = Inf) #sehr interessant 22 12 23; andere seite 08 17 06 




m%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, Indikatorwert ) )%>%
  arrange(korrelation) %>%
  tail(5)

m%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, Indikatorwert) )%>%
  arrange(korrelation) %>%
  head(5)


m%>%
  filter(Raumbezug == "Stadt München")%>%
  group_by(Ausprägung)%>%
  summarise(
    korrelation = cor(Jahr, Indikatorwert) )%>%
  arrange(korrelation) # leicht interessant


m3 <- m%>% #protzentuale Wert für Ausagekraft ähnlcih wie Indikatorwert nur um 10 Faktor anders
  mutate(bpn1 = 100 *Basiswert.1 / Basiswert.5,
         bpn2 = 100 *Basiswert.2 / Basiswert.5,
         bpn3 = 100 *Basiswert.3 / Basiswert.5,
         bpn4 = 100 *Basiswert.4 / Basiswert.5)

m3%>%
  filter(Raumbezug == "Stadt München")%>%
  group_by(Ausprägung)%>%
  summarise(
    korrelation = cor(Jahr, bpn1) )%>%
  arrange(korrelation)

m3%>%
  filter(Raumbezug == "Stadt München")%>%
  group_by(Ausprägung)%>%
  summarise(
    korrelation = cor(Jahr, bpn2) )%>%
  arrange(korrelation)


m3%>%
  filter(Raumbezug == "Stadt München")%>%
  group_by(Ausprägung)%>%
  summarise(
    korrelation = cor(Jahr, bpn3) )%>%
  arrange(korrelation)

m3%>%
  filter(Raumbezug == "Stadt München")%>%
  group_by(Ausprägung)%>%
  summarise(
    korrelation = cor(Jahr, bpn4) )%>%
  arrange(korrelation)

m3%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, bpn4) )%>%
  arrange(korrelation)%>%
  print(n = Inf) #17, 12, 10, 02

m3%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, bpn3) )%>%
  arrange(korrelation)%>%
  print(n = Inf) #12, 22, 17, 07

mp301 <- mnew %>%
  filter(sn == 26)
ggplot(mp301, aes(x = Jahr, y = bpn3, color = Ausprägung)) +geom_point() + geom_line() + geom_smooth(method = "lm")
ggplot(mp301, aes(x = Jahr, y = Basiswert.3, color = Ausprägung)) +geom_point() + geom_line() + geom_smooth(method = "lm")
ggplot(mp301, aes(x = Jahr, y = bpn4, color = Ausprägung)) +geom_point() + geom_line()
ggplot(mp301, aes(x = Jahr, y = Basiswert.4, color = Ausprägung)) +geom_point() + geom_line()

mp302 <- mnew %>%
  filter(sn == 17 |sn == 22 |sn == 12 |sn == 07 |sn == 10 |sn == 02 )

ggplot(mp302, aes(x = Jahr, y = bpn3, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = bpn4, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)



ggplot(mp302, aes(x = Jahr, y = bpn3 + bpn4, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)


ggplot(mp301, aes(x = Jahr, y = bpn3 , color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp301, aes(x = Jahr, y = Basiswert.3 , color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)

ggplot(mp302, aes(x = Jahr, y = bpn2, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = bpn4 +bpn3, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = Basiswert.2, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)
ggplot(mp302, aes(x = Jahr, y = Basiswert.4 + Basiswert.3, color = Ausprägung)) +geom_point() + geom_line() + facet_wrap(~ Raumbezug)






ggplot(mp301, aes(x = Jahr, y = bpn1, color = Ausprägung)) +geom_point() + geom_line()

ggplot(mp301, aes(x = Jahr, y = Basiswert.1, color = Ausprägung)) +geom_point() + geom_line()




m3%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, bpn2) )%>%
  arrange(korrelation)%>%
  print(n = Inf)

m3%>%
  filter(Ausprägung == "insgesamt")%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, bpn1) )%>%
  arrange(korrelation)%>%
  print(n = Inf)

mplot1 <- m3%>%
  filter(Raumbezug == "Stadt München")
ggplot(mplot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + geom_line() #+geom_smooth(method = "lm")


ggplot(m3, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + geom_line() +facet_wrap(~ Raumbezug, scales = "free") 





ggplot(mplot1, aes(x = Jahr, y = bpn1, color = Ausprägung)) + geom_point() + geom_line()

ggplot(mplot1, aes(x = Jahr, y = bpn2, color = Ausprägung)) + geom_point() + geom_line()

ggplot(mplot1, aes(x = Jahr, y = bpn3, color = Ausprägung)) + geom_point() + geom_line()

ggplot(mplot1, aes(x = Jahr, y = bpn4, color = Ausprägung)) + geom_point() + geom_line()

mnew <- m3%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))


mnew <- mnew%>%
  mutate(gsr = Basiswert.1 - Basiswert.3)
mnew <- mnew%>%
  mutate(rgsr = gsr / Basiswert.5)

mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)
mnew <- mnew%>%
  mutate(rar = ar / Basiswert.5)

mplot009 <- mnew%>%
  filter(sn == 26)

ggplot(mplot009, aes(x = Jahr, y = gsr, color = Ausprägung)) + geom_point()+ geom_line() + geom_smooth(method = "lm")
ggplot(mplot009, aes(x = Jahr, y = rgsr, color = Ausprägung)) + geom_point()+ geom_line() + geom_smooth(method = "lm")

mplot09 <- mnew%>%
  filter(Ausprägung == "insgesamt")%>%
  filter(Jahr == 2000 | Jahr == 2005 | Jahr == 2010 | Jahr == 2015 | Jahr == 2020 | Jahr == 2024)%>%
  filter(sn != 26)

ggplot(mplot09, aes(x = Jahr, y = ar, color = Raumbezug)) + geom_point()+ geom_line() #ma könnte zunächst meinen corona Auswirkung


mplot0901 <- mnew%>%
  filter(sn == 26)

ggplot(mplot0901, aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ geom_line() # staatsbürgerschaft

ggplot(mplot0901, aes(x = Jahr, y = rar, color = Ausprägung)) + geom_point()+ geom_line()




mnew%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, rar) )%>%
  arrange(korrelation)%>%
  print(n = Inf)

mnew%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, ar) )%>%
  arrange(korrelation)%>%
  print(n = Inf)

mnew%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, gsr) )%>%
  arrange(korrelation)%>%
  print(n = Inf)

mnew%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, rgsr) )%>%
  arrange(korrelation)%>%
  print(n = Inf) #17, 22


mplot0902<- mnew%>%
  filter(sn == 17 | sn == 22)

ggplot(mplot0902, aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)

ggplot(mplot0902, aes(x = Jahr, y = rar, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)

ggplot(mplot0902, aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)

ggplot(mplot0902, aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug)


b009 <- bnew%>%
  filter(sn == 22 | sn == 17 | sn == 26)
  
ggplot(b009, aes(x = Jahr, y = Indikatorwert, color = Ausprägung)) + geom_point()+ geom_line() + facet_wrap(~Raumbezug, scales = "free")












mnew <- mnew %>%
  group_by(Raumbezug) %>%
  mutate(indexb1 = 100 * Basiswert.1 / Basiswert.1[Jahr == 2000])

mpi1 <- mnew%>%
  filter(sn == 26)%>%
  group_by(Ausprägung)
ggplot(mpi1, aes(x = Jahr , y = indexb1, color = Ausprägung)) +geom_point()+ geom_line()
ggplot(mnew, aes(x = Jahr , y = indexb1, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)

mnew%>%
  group_by(Raumbezug)%>%
  summarise(
    korrelation = cor(Jahr, indexb1) )%>%
  arrange(korrelation)%>%
  print(n = Inf)



bj <- b%>%
  rename(bi = Indikatorwert)%>%
  rename(b1 = Basiswert.1)%>%
  rename(b2 = Basiswert.2)


zusammen <- full_join(m, bj, by = c("Jahr", "Raumbezug"))

pz1 <- zusammen%>%
  filter(Ausprägung.x == "insgesamt")%>%
  filter(Jahr != 2000 & Jahr != 2001)
ggplot(pz1, aes(x = Jahr, y = Indikatorwert, color = bi)) + geom_point() + geom_line() +facet_wrap(~Raumbezug)
ggplot(pz1, aes(x = Jahr, y = bi, color = Indikatorwert)) + geom_point() + geom_line() +facet_wrap(~Raumbezug)
ggplot(pz1, aes(x = Jahr, y = bi, color = Indikatorwert)) + geom_point() + geom_line() +facet_wrap(~Raumbezug, scales = "free_y")


pz2 <- zusammen%>%
  filter(Ausprägung.x == "insgesamt")%>%
  filter(Jahr == 2024)
ggplot(pz2, aes(x = bi, y = Indikatorwert, color = Raumbezug)) + geom_point() 

zusammen <- zusammen %>%
  mutate(bqm1 = Basiswert.1 / b2,
         bqm2 = Basiswert.2 / b2,
         bqm3 = Basiswert.3.x / b2,
         bqm4 = Basiswert.4.x / b2)
pz3<- zusammen%>%
  filter(Ausprägung.x == "insgesamt")
ggplot(pz3, aes(x = Jahr, y = bqm1, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm1)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(pz3, aes(x = Jahr, y = bqm2, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm2)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(pz3, aes(x = Jahr, y = bqm3, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm3)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(pz3, aes(x = Jahr, y = bqm4, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm4)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(pz3, aes(x = Jahr, y = bqm2 +bqm1, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm2 + bqm1)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(pz3, aes(x = Jahr, y = bqm3 + bqm4, color = Raumbezug)) +geom_point() +geom_line()
ggplot(pz3, aes(x = Jahr, y = bqm3 + bqm4)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)




ggplot(zusammen, aes(x = Jahr, y = bqm1, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(zusammen, aes(x = Jahr, y = bqm2, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(zusammen, aes(x = Jahr, y = bqm3, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)

ggplot(zusammen, aes(x = Jahr, y = bqm4, color = Ausprägung.x)) +geom_point() +geom_line() +facet_wrap(~Raumbezug)










