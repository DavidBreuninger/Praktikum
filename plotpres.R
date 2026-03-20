
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


#Stadt München
b1<- b%>%
  filter(Raumbezug == "Stadt München")
p1 <-ggplot(b1, aes(x = Jahr, y = Basiswert.1)) + geom_point() + geom_line() +
  labs(y = "Einwohnerzahl", title = "Stadt München")
p1
ggsave("Results/p1.jpg", plot = p1,width = 10, height = 6)


#Einwohnerzahl nach Staatsbürgerschaft
mplot1 <- mnew%>%
  filter(Raumbezug == "Stadt München")
p2<-ggplot(mplot1, aes(x = Jahr, y = Basiswert.5, color = Ausprägung)) + geom_point() + 
  geom_line() +labs(y = "mittlere Wohnbevölkerung", title = "Einwohnerzahl nach Staatsbürgerschaft") 
p2
ggsave("Results/p2.jpg", plot = p2,width = 10, height = 6)


#Protzentuale Bevölkerungsentwicklung
bnew <- bnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb1 = 100 * Basiswert.1 / Basiswert.1[Jahr == 2002]
  )
p3 <-ggplot(bnew, aes(x = Jahr, y = indexb1)) +geom_point(color = "blue",size=1) +
  geom_line(color = "blue")+ facet_wrap(~ Raumbezug,labeller = label_wrap_gen(width = 30)) + 
  labs(y = "2002 als Indikatorwert",title = "Prozentuale Bevölkerungsentwicklung")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))
p3
ggsave("Results/p3.jpg", plot = p3,width = 10, height = 6)


#Prozentuale Bevölkerungsentwicklung
mnew <- mnew %>%
  group_by(Raumbezug) %>%
  group_by(Ausprägung)%>%
  mutate(indexb5 = 100 * Basiswert.5 / Basiswert.5[Jahr == 2002]
  )
p4 <- mnew%>%
  filter(Jahr != 2000 & Jahr != 2001)%>%
ggplot(aes(x = Jahr, y = indexb5, color = Ausprägung)) +geom_point(size=0.6) +
  geom_line()+ facet_wrap(~ Raumbezug,labeller = label_wrap_gen(width = 30))+ 
  labs(y = "2002 als Indikatorwert",title = "Prozentuale Bevölkerungsentwicklung")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))
p4
ggsave("Results/p4.jpg", plot = p4,width = 10, height = 6)


mnew <- mnew%>%
  mutate(ar = Basiswert.1 - Basiswert.3 + Basiswert.2 -Basiswert.4)


#Entwicklung in der Stadt München
p5 <- mnew%>%
  filter(sn == 26)%>%
 ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 
  geom_line() + labs(y = "Nettozuzug", title = "Entwicklung in der Stadt München")

p5
ggsave("Results/p5.jpg", plot = p5,width = 10, height = 6)


#Nettozuzug
mnew%>%
  filter(sn != 26)%>%
  ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point(size=0.9)+ 
  geom_line() + facet_wrap(~Raumbezug,labeller = label_wrap_gen(width = 30)) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))


#Zuzug
mnew%>%
  filter(sn != 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + 
  geom_point(size=0.9)+ geom_line() + facet_wrap(~Raumbezug,labeller = label_wrap_gen(width = 30)) +
  labs(y = "Zuzug", title = "Entwicklung in den Stadtbezirken")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))

#Wegzug
mnew%>%
  filter(sn != 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + 
  geom_point(size=0.9)+ geom_line() + facet_wrap(~Raumbezug,labeller = label_wrap_gen(width = 30)) +
  labs(y = "Wegzug", title = "Entwicklung in den Stadtbezirken")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))


#Entwicklung in der Stadt München
p7<- mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Zuzug", title = "Entwicklung in der Stadt München")
p7
ggsave("Results/p7.jpg", plot = p7,width = 10, height = 6)


#Entwicklung in der Stadt München
p8<-mnew%>%
  filter(sn == 26)%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Wegzug", title = "Entwicklung in der Stadt München")
p8
ggsave("Results/p8.jpg", plot = p8,width = 10, height = 6)


#Entwicklung in den Stadtbezirken
p9<-mnew%>%
  filter(sn == 22 | sn == 12 | sn == 15 | sn == 02 | sn == 01 | sn == 06 )%>%
  ggplot( aes(x = Jahr, y = ar, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")+
  theme(panel.spacing.x = unit(1, "lines"))
p9
ggsave("Results/p9.jpg", plot = p9,width = 10, height = 6)


#Entwicklung in den Stadtbezirken
p10<-mnew%>%
  filter(sn == 22 | sn == 12 | sn == 15 | sn == 02 | sn == 01 | sn == 06 )%>%
  ggplot( aes(x = Jahr, y = Basiswert.1 + Basiswert.2, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")+
  theme(panel.spacing.x = unit(1, "lines"))
p10
ggsave("Results/p10.jpg", plot = p10,width = 10, height = 6)


#Entwicklung in den Stadtbezirken
p11<-mnew%>%
  filter(sn == 22 | sn == 12 | sn == 15 | sn == 02 | sn == 01 | sn == 06 )%>%
  ggplot( aes(x = Jahr, y = Basiswert.3 + Basiswert.4, color = Ausprägung)) + geom_point()+ 
  geom_line() + facet_wrap(~Raumbezug) +
  labs(y = "Nettozuzug", title = "Entwicklung in den Stadtbezirken")+
  theme(panel.spacing.x = unit(1, "lines"))
p11
ggsave("Results/p11.jpg", plot = p11,width = 10, height = 6)


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


#Staatsbürgerschaft
p12<-mnew%>%
  filter(Ausprägung != "insgesamt")%>%
ggplot(aes(x = Jahr, y = rb5, color = Ausprägung)) +
  geom_point(size=0.6)+ geom_line() +facet_wrap(~Raumbezug,labeller = label_wrap_gen(width = 30)) +
  labs(y= "Protzentrate", title = "Staatsbürgerschaft")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7,face="bold"),
        panel.spacing.x = unit(1.2, "lines"))
  
p12
ggsave("Results/p12.jpg", plot = p12,width = 10, height = 6)









