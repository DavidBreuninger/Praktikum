
library("ggplot2")
library("tidyverse")
library("tidyr")
library("dplyr")

Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bevoelkerungsdichte <- read.csv("Data/indikat2510_bevoelkerung_bevoelkerungsdichte_28_10_25.csv")
m <-Mobilitaet #nache einlesung
b <-Bevoelkerungsdichte #nache einlesung


bnew<-b
mnew<-m
bnew <- bnew%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))
mnew <- mnew%>%
  mutate(
    sn = case_when(
      Raumbezug == "Stadt München" ~ 26,
      TRUE ~ as.numeric(str_extract(Raumbezug, "^\\d+"))))


bnew <-bnew%>% #damit auf plot nicht abgeschnitten wird
  mutate(Raumbezug = case_when(sn == 19 ~ "19 Thalkirchen",
                               TRUE ~ Raumbezug))
mnew <- mnew%>%
  mutate(Raumbezug = case_when(sn == 19 ~ "19 Thalkirchen",
                               TRUE ~ Raumbezug))


b1<- b%>%
  filter(Raumbezug == "Stadt München")
p1 <-ggplot(b1, aes(x = Jahr, y = Basiswert.1)) + geom_point() + geom_line() +
  labs(y = "Einwohnerzahl", title = "Stadt München")
p1
mplot1 <- mnew%>%
  filter(Raumbezug == "Stadt München")
p2<-ggplot(mplot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung", title = "Einwohnerzahl nach Staatsbürgerschaft") 
p2

bnew <- bnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb1 = 100 * Basiswert.1 / Basiswert.1[Jahr == 2002]
  )
p3 <-ggplot(bnew, aes(x = Jahr, y = indexb1)) +geom_point(color = "blue") +
  geom_line(color = "blue")+ facet_wrap(~ Raumbezug) + labs(y = "2002 als Indikatorwert",
                              title = "Protzentuale Bevölkerungsentwicklung")

p3
mnew <- mnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb5 = 100 * Basiswert.5 / Basiswert.5[Jahr == 2002]
  )
p4 <- mnew%>%
  filter(Jahr != 2000 & Jahr != 2001)%>%
ggplot(aes(x = Jahr, y = indexb5, color = Ausprägung)) +geom_point() +
  geom_line()+ facet_wrap(~ Raumbezug)+ labs(y = "2002 als Indikatorwert",
                             title = "Protzentuale Bevölkerungsentwicklung")
p4


mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)



p5 <- mnew%>%
  filter(sn == 26)%>%
 ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 
  geom_line() + labs(y = "Nettozuzug", title = "Entwicklung in der Stadt München")

p5
mnew%>%
  filter(sn != 26)%>%
  ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")

mnew%>%
  filter(sn != 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Zuzug", title = "Entwicklung in den Stadtbezirken")

mnew%>%
  filter(sn != 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Wegzug", title = "Entwicklung in den Stadtbezirken")

p7<- mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Zuzug", title = "Entwicklung in der Stadt München")
p7
p8<-mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Wegzug", title = "Entwicklung in der Stadt München")
p8


p9<-mnew%>%
  filter(sn == 22 | sn == 12 | sn == 15 | sn == 02 | sn == 01 | sn == 06 )%>%
  ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")
p9
p10<-mnew%>%
  filter(sn == 22 | sn == 12 | sn == 15 | sn == 02 | sn == 01 | sn == 06 )%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")
p10
p11<-mnew%>%
  filter(sn == 22 | sn == 12 | sn == 15 | sn == 02 | sn == 01 | sn == 06 )%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")
p11




#eventuell indexb12 und indexb34  wenn ja dann beschriftung fehlt noch
mnew <- mnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb1 = 100 * Basiswert.1 / Basiswert.1[Jahr == 2000],
         indexb2 = 100 * Basiswert.2 / Basiswert.2[Jahr == 2000],
         indexb3 = 100 * Basiswert.3 / Basiswert.3[Jahr == 2000],
         indexb4 = 100 * Basiswert.4 / Basiswert.4[Jahr == 2000],
         indexb12 = 100 * (Basiswert.1 + Basiswert.2)/ (Basiswert.1[Jahr == 2000] +Basiswert.2[Jahr == 2000]),
         indexb34 = 100 * (Basiswert.4 + Basiswert.3)/ (Basiswert.3[Jahr == 2000] +Basiswert.4[Jahr == 2000]))



ggplot(mnew, aes(x = Jahr , y = indexb12, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)


ggplot(mnew, aes(x = Jahr , y = indexb34, color = Ausprägung)) +geom_point()+ geom_line() +facet_wrap(~Raumbezug)

mnew%>%
  filter(Ausprägung == "insgesamt")%>%
ggplot(aes(x = Jahr , y = indexb12)) +geom_point(color = "blue")+ geom_line(color = "blue") +facet_wrap(~ Raumbezug)

mnew%>%
  filter(Ausprägung == "insgesamt")%>%
ggplot(aes(x = Jahr , y = indexb34)) +geom_point(color = "blue")+ geom_line(color = "blue") +facet_wrap(~ Raumbezug)

# ende von indexb12 und b34

mnew <- mnew %>%
  group_by(Jahr, Raumbezug) %>% 
  mutate(rb5 = Basiswert.5 / Basiswert.5[Ausprägung == "insgesamt"])

p12<-mnew%>%
  filter(Ausprägung != "insgesamt")%>%
ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point()+ geom_line() +facet_wrap(~Raumbezug) +
  labs(y= "Protzentrate", title = "Staatsbürgerschaft")
p12










