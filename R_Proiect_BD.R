library(tidyverse)
#install.packages('RPostgreSQL')
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="Proiect_bd", user="postgres", 
                 host = 'localhost', password="postgres")
tables <- dbGetQuery(con, 
                     "select table_name from information_schema.tables where table_schema = 'public'")
tables

discipline <- dbReadTable(con, "discipline")
examene <- dbReadTable(con, "examene")
formatii_curs <- dbReadTable(con, "formatii_curs")
formatii_seminar_labor <- dbReadTable(con, "formatii_seminar_labor")
persoane <- dbReadTable(con, "persoane")
plan_invatamant <- dbReadTable(con, "plan_invatamant")
profi <- dbReadTable(con, "profi")
specializari <- dbReadTable(con, "specializari")
stud_cursuri <- dbReadTable(con, "stud_cursuri")
stud_sem_labor <- dbReadTable(con, "stud_sem_labor")
studenti <- dbReadTable(con, "studenti")
traseu_stud <- dbReadTable(con, "traseu_stud")

#1 Care sunt specializarile din FEAA ce organizeaza cursuri la distanta?
library(tidyverse)
library(lubridate)
temp <- specializari %>%
  inner_join(traseu_stud) %>%
  select(fstudii,codspec,numespec) %>%
  filter(fstudii != 'zi') %>%
  arrange(codspec)
view(temp)


#2 Care sunt studentii de la CIG care au dat examen la Baze de date I?
temp <- traseu_stud %>%
  inner_join(examene) %>%
  select(codspec, coddisc,matricol) %>%
  filter(codspec == 'CIG' & coddisc == 'AI2401')
view(temp)



#3 Ce profesori predau si la Zi si la ID?
temp <- dplyr::intersect(
  profi %>%
    inner_join(formatii_curs) %>%
    inner_join(discipline) %>%
    inner_join(examene) %>%
    inner_join(traseu_stud) %>%
    filter(fstudii=='zi') %>%
    select(codprof, numeprof, fstudii),
  profi %>%
    inner_join(formatii_curs) %>%
    inner_join(discipline) %>%
    inner_join(examene) %>%
    inner_join(traseu_stud) %>%
    filter(fstudii=='ID') %>%
    select(codprof,numeprof, fstudii)
)
view(temp)

#4 Care este cea mai numeroasa catedra din FEAA?
temp <- profi%>%
  group_by(catedra)%>%
  summarise(nr_catedre=n())%>%
  ungroup()%>%
  top_n(1,nr_catedre)
View(temp)

#5 Care este media examenelor promovate de studentul Popovici G Vasile?
temp <- persoane %>%
  filter(numepren == 'Georgescu C Ionut') %>%
  inner_join(studenti, by=c('idpers')) %>%
  inner_join(examene, by=c('matricol')) %>%
  filter(nota >=5) %>%
  summarise(medie_examene = mean(nota))
View(temp) 

#6 Care sunt disciplinele cu minimum 3 titulari de curs?
temp <- profi %>%
  select(numeprof) %>%
  inner_join(formatii_curs, by=c("codprof")) %>%
  select(coddisc) %>%
  inner_join(discipline, by=c("coddisc"="coddisc")) %>%
  group_by(coddisc) %>%
  filter(n()>1) %>%
  select(dendisc)
View(temp)

#7 Care sunt studentii din Roman (jud. Neamt) care au promovat toate examenele la care s-au prezentat?
temp <- persoane %>%
  inner_join(studenti,by=c("idpers")) %>%
  filter(loc=='Roman' & jud=='Neamt') %>%
  inner_join(examene,by=c("matricol"))%>%
  filter(nota>=5)%>%
  group_by(idpers,numepren)%>%
  summarise(n_examene=n())%>%
  inner_join(
    persoane %>%
      inner_join(studenti,by=c("idpers")) %>%
      filter(loc=='Roman' & jud=='Neamt') %>%
      inner_join(examene,by=c("matricol"))%>%
      filter(nota>=5)%>%
      group_by(idpers,numepren)%>%
      summarise(n_examene=n_distinct(coddisc))
  )
View(temp)

#8 Afisati top 10 studenti cu cel mai mare numar de examene sustinute
temp <- persoane %>%
  inner_join(studenti,by=c("idpers"))%>%
  inner_join(examene,by=c("matricol"))%>%
  filter(nota >= 5)%>%
  group_by(numepren)%>%
  summarise(n_of_exams=n())%>%
  arrange(desc(n_of_exams))%>%
  slice(1:10)
view(temp)

#9 Afisati disciplinele la care au fost examinati macar aceeasi studenti ca la disciplina Baze de Date
temp <- persoane %>%
  inner_join(studenti,by=c("idpers")) %>%
  inner_join(examene,by=c("matricol")) %>%
  inner_join(discipline,by=c("coddisc")) %>%
  select(numepren,dendisc)%>%
  filter(numepren %in% 
           (persoane %>%
              inner_join(studenti,by=c("idpers")) %>%
              inner_join(examene,by=c("matricol")) %>%
              inner_join(discipline,by=c("coddisc"))%>%
              filter(dendisc=='Baze de date')%>%
              select(numepren,dendisc)%>%
              pull(numepren))
  )
View(temp)

#10 Afisati top 5 discipline cu cei mai multi titulari de curs
temp <- discipline %>%
  select(coddisc,dendisc) %>%
  group_by(dendisc) %>%
  inner_join(formatii_curs, by =c('coddisc'='coddisc')) %>%
  inner_join(profi, by =c('codprof'='codprof')) %>%
  summarise (numar_profi = n()) %>%
  ungroup() %>%
  arrange(desc(numar_profi))  %>%
  top_n(5, numar_profi)
View(temp)

#11 Raport 1
temp<-formatii_seminar_labor %>%
  filter(anuniv=='2016-2017') %>%
  inner_join(plan_invatamant, by = c('coddisc'='coddisc')) %>%
  inner_join(specializari, by = c('codspec' ='codspec')) %>%
  inner_join(traseu_stud, by = c('codspec' ='codspec')) %>%
  inner_join(studenti, by = c('matricol' ='matricol')) %>%
  inner_join(persoane, by = c('idpers' ='idpers')) %>%
  group_by(idfsl)%>%
  summarise(lista_studenti=paste(numepren,'(',codspec,')', collapse=';'))%>%
  select(idfsl,lista_studenti)%>%
  inner_join(
    temp<-formatii_seminar_labor %>%
      filter(anuniv=='2016-2017')%>%
      inner_join(discipline,by = c('coddisc'='coddisc'))%>%
      mutate(gr_sem=paste(idfsl,'(',dendisc,')'))%>%
      distinct(gr_sem,idfsl))%>%
  distinct(gr_sem,lista_studenti)%>%
  transmute("Grupa de sem/labor"=gr_sem, "Lista Studenti"=lista_studenti) 
View(temp)  
  
  
  
  
  
  
  
  
  
  
  
  


