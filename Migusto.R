library(RSelenium)
library(robotstxt)
library(dplyr)
library(rvest)
library(stringr)
Sys.setenv("LANG"='en')

get_robotstxt('https://migusto.migros.ch/de/rezept-uebersicht/')
robo<-robotstxt(domain = 'https://migusto.migros.ch/de/rezept-uebersicht/')
robo$domain
robo$permissions
paths_allowed('https://migusto.migros.ch/de/rezept-uebersicht/')

get_robotstxt('https://migusto.migros.ch/de/rezepte/boerek-mit-spinat-und-feta')
robo<-robotstxt(domain = 'https://migusto.migros.ch/de/rezepte/boerek-mit-spinat-und-feta')
robo$domain
robo$permissions
paths_allowed('https://migusto.migros.ch/de/rezepte/boerek-mit-spinat-und-feta')

###Migusto 'scrapen' - Rezepte
Rezepte<-data_frame()

for (i in 1:506) {
  Sys.sleep(0.0001)
  
  Migusto <- paste0('https://migusto.migros.ch/de/rezept-uebersicht.html?page=',i) %>%
    read_html()%>%
    html_nodes('h3') %>%
    html_text()%>%
    as.data.frame()%>%
    slice(1:(n()-1))%>%
    rename_at(1,~"Rezept")
  
  Rezepte<-rbind(Migusto, Rezepte)
}
Rezepte_backup<-Rezepte

  Rezepte <- Rezepte %>%
    mutate(Rezept=tolower(Rezept))%>%
    mutate(Rezept=trimws(Rezept))%>%
    mutate(Rezept=str_replace_all(Rezept,' ','-'))%>%
    mutate(Rezept=str_replace_all(Rezept,'ä','ae'))%>%
    mutate(Rezept=str_replace_all(Rezept,'ü','ue'))%>%
    mutate(Rezept=str_replace_all(Rezept,'ö','oe'))%>%
    mutate(Rezept=str_replace_all(Rezept,'é','e'))%>%
    mutate(Rezept=str_replace_all(Rezept,'è','e'))%>%
    mutate(Rezept=str_replace_all(Rezept,'ê','e'))%>%
    mutate(Rezept=str_replace_all(Rezept,'î','i'))%>%
  mutate(Rezept=str_replace_all(Rezept,'ù','u'))%>%
  mutate(Rezept=str_replace_all(Rezept,',',''))%>%
  mutate(Rezept=str_replace_all(Rezept,'ô',''))%>%
  mutate(Rezept=str_replace_all(Rezept,'û',''))
  
  
### Mit den gescrapten Namen der Rezepte die einzelnen Rezept-Pages aufrufen und Eigenschaften der Rezepte scrapen
  final1<-data.frame()
  final2<-data_frame()
  final3<-data_frame()
  final4<-data_frame()
  
  rD<-rsDriver(browser = 'firefox', port = 461L)
  remDr<-rD$client
  
  for (i in Rezepte2$Rezept) {
    remDr$navigate(paste0('https://migusto.migros.ch/de/rezepte/',i))
    
    Sys.sleep(2) #das könnte man noch verbessern, indem man einen automatischen Stop&Go einbaut, welcher weitermacht, wenn die Seite geladen ist
    
    HTML<-read_html(remDr$getPageSource()[[1]])
    
    Bewertung<-HTML %>%
     html_nodes('div div div div a div span:first-child')%>%
    html_attr('title')%>%
      as.data.frame()%>%
      na.omit()%>%
      rename_at(1,~"Bewertung")%>%
      slice(1)%>%
      mutate(Rezept=i)
    
  final1 <- rbind(final1, Bewertung)
    
    Anzahl <- HTML %>%
     html_nodes('a span')%>%
    html_text()%>%
      as.data.frame()%>%
      rename_at(1,~"Anzahl")%>%
      filter(str_detect(Anzahl, '[()]'))%>%
      mutate(Rezept=i)
    
    final2 <- rbind(final2, Anzahl)
    
    Zubereitungszeit<-HTML %>%
      html_nodes('div div div div dl dd.recipe-duration__item-value')%>%
      html_text()%>%
    as.data.frame()%>%
      rename_at(1,~"Zubereitungszeit")%>%
      slice(n())%>%
      mutate(Rezept=i)
    
    final3 <- rbind(final3,Zubereitungszeit)
    
    Attribute<-HTML %>%
      html_nodes('ul li a.recipe-taxonomy-list__item-text')%>%
      html_text()%>%
      paste(collapse = '|')%>%
      as.data.frame()%>%
      rename_at(1,~"Attribute")%>%
      mutate(Rezept=i)
    
    final4 <- rbind(final4,Attribute)
  }
  write.csv(final1, '/Users/davidkaufmann/Documents/Bewerbungen/Job21/Digital Analyst Migros/Project/Backup/final_1_backup.csv') 
  write.csv(final2, '/Users/davidkaufmann/Documents/Bewerbungen/Job21/Digital Analyst Migros/Project/Backup/final_2_backup.csv') 
  write.csv(final3, '/Users/davidkaufmann/Documents/Bewerbungen/Job21/Digital Analyst Migros/Project/Backup/final_3_backup.csv') 
  write.csv(final4, '/Users/davidkaufmann/Documents/Bewerbungen/Job21/Digital Analyst Migros/Project/Backup/final_4_backup.csv') 
  
  final<-full_join(final1, final2)
  final<-full_join(final, final3)
  final<-full_join(final, final4)
  
  final<-unique(final)
  
  write.csv(final, '/Users/davidkaufmann/Documents/Bewerbungen/Job21/Digital Analyst Migros/Project/Backup/final_backup.csv') 
  
  Migusto_Daten <- final %>%
    mutate(Bewertung=str_remove_all(Bewertung, 'Bewertung: '))%>%
    mutate(Anzahl=str_remove_all(Anzahl, '[()]'))%>%
    mutate(Zubereitungszeit=str_remove_all(Zubereitungszeit, 'ca. '))%>%
    na.omit('Anzahl')%>%
    filter(Anzahl!=0)%>%
   filter(Bewertung!=0)%>%
    mutate('15 Min' = ifelse(str_detect(Attribute, '15 MIN.'),1,0))%>%
    mutate('30 Min' = ifelse(str_detect(Attribute, '30 MIN.'),1,0))%>%
    mutate('60 Min' = ifelse(str_detect(Attribute, '60 MIN.'),1,0))%>%
    mutate('APÉRO' = ifelse(str_detect(Attribute, 'APÉRO'),1,0))%>%
    mutate('VORSPEISE' = ifelse(str_detect(Attribute, 'VORSPEISE'),1,0))%>%
    mutate('HAUPTGERICHT' = ifelse(str_detect(Attribute, 'HAUPTGERICH'),1,0))%>%
    mutate('DESSERT' = ifelse(str_detect(Attribute, 'DESSERT'),1,0))%>%
    mutate('BRUNCHFRÜHSTÜCK' = ifelse(str_detect(Attribute, 'BRUNCH & FRÜHSTÜCK'),1,0))%>%
    mutate('BEILAGEN' = ifelse(str_detect(Attribute, 'BEILAGEN'),1,0))%>%
    mutate('GETRÄNKE' = ifelse(str_detect(Attribute, 'GETRÄNKE'),1,0))%>%
    mutate('VEGETARISCH' = ifelse(str_detect(Attribute, 'VEGETARISCH'),1,0))%>%
    mutate('VEGAN' = ifelse(str_detect(Attribute, 'VEGAN'),1,0))%>%
    mutate('GESUNDAUSGEWOGEN' = ifelse(str_detect(Attribute, 'GESUND & AUSGEWOGEN'),1,0))%>%
    mutate('FISCHFLEISCH' = ifelse(str_detect(Attribute, 'VEGAN|VEGETARISCH'),0, 1))%>%
    mutate('SCHWEIZ' = ifelse(str_detect(Attribute, 'SCHWEIZ'),1,0))%>%
    mutate('ITALIEN' = ifelse(str_detect(Attribute, 'ITALIEN'),1,0))%>%
    mutate('ASIEN' = ifelse(str_detect(Attribute, 'ASIEN'),1,0))%>%
    mutate('ORIENT' = ifelse(str_detect(Attribute, 'ORIENT'),1,0))%>%
    mutate('MEXIKO' = ifelse(str_detect(Attribute, 'MEXIKO'),1,0))%>%
    select(!('Attribute'))%>%
    select(!('Zubereitungszeit'))
  
write.csv(Migusto_Daten, '/Users/davidkaufmann/Documents/Bewerbungen/Job21/Digital Analyst Migros/Project/Migusto_Daten.csv')

###play around Analyse
str(Migusto_Daten)

Migusto_Daten_R <- Migusto_Daten %>%
  mutate(Bewertung=as.numeric(Bewertung))

class(Migusto_Daten_R$Bewertung)
summary(Migusto_Daten_R$Bewertung)

model<-lm(Bewertung~SCHWEIZ+ITALIEN+ASIEN+ORIENT+MEXIKO, Migusto_Daten_R)
summary(model)

model<-lm(Bewertung~VEGETARISCH, Migusto_Daten_R)
summary(model) 

model<-lm(Bewertung ~ VORSPEISE+HAUPTGERICHT+DESSERT+BRUNCHFRÜHSTÜCK+BEILAGEN+GETRÄNKE,   Migusto_Daten_R)
summary(model) 

model<-lm(Bewertung~SCHWEIZ+ITALIEN+ASIEN+ORIENT+MEXIKO+VEGETARISCH+VORSPEISE+HAUPTGERICHT+DESSERT+BRUNCHFRÜHSTÜCK+BEILAGEN+GETRÄNKE, Migusto_Daten_R)
summary(model)

###Not so good
#-unsicher, ob Tagsetzung systematisch und gut
#-kein Vergleich, ob 20 mal mit 5 Sterne bewertet oder nur 1 mal mit 5 Sternen
#
