rm(list = ls())
library(tidyverse)
library(readxl)
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
data1 = read_xls('./Data/Mexico/01_03B_ESTATAL.xls',range = 'A9:E3408')
colnames(data1) = c('entidad','edad','PT','PM','PF')
data1 = data1 %>% 
  filter(entidad != 'Estados Unidos Mexicanos',!edad %in% c('Total','No especificado')) %>%
  mutate(edad = as.numeric(gsub('[[:alpha:][:blank:]]','',edad)),
         cod_level = substr(entidad,1,2),
         level = substr(entidad,4,nchar(entidad))) %>%
  select(level,cod_level,edad,PM,PF)
data1$edad[data1$edad %in% 0:9] = 1
data1$edad[data1$edad %in% 10:19] = 2 
data1$edad[data1$edad %in% 20:29] = 3 
data1$edad[data1$edad %in% 30:39] = 4 
data1$edad[data1$edad %in% 40:49] = 5 
data1$edad[data1$edad %in% 50:59] = 6 
data1$edad[data1$edad %in% 60:69] = 7
data1$edad[data1$edad %in% 70:79] = 8
data1$edad[data1$edad >= 80] = 9
data1 = data1 %>% mutate(edad = factor(edad,labels = edadesd80)) %>%
  group_by(cod_level,level,edad) %>% summarise(PM = sum(PM,na.rm = T),PF = sum(PF,na.rm = T)) %>% ungroup()
data2010 = bind_rows(data1 %>% mutate(sexo = 'MEN',KT = PM) %>% select(cod_level,edad,sexo,KT),
                     data1 %>% mutate(sexo = 'WOMEN',KT = PF) %>% select(cod_level,edad,sexo,KT))
data2 = read_xls('./Data/Mexico/Cont2005_NAL_Poblacion.xls',sheet = 'Cont2005_Nal_POB2',range = 'A8:G4067')
colnames(data2) = c('mexico','entidad','edad','grupo','PT','PM','PF')
data2 = data2 %>% 
  filter(entidad != 'Total Nacional',!edad %in% c('Total','No especificado'),grupo == 'Total') %>%
  mutate(edad = factor(edad),
         cod_level = substr(entidad,1,2),
         level = substr(entidad,4,nchar(entidad))) %>%
  select(level,cod_level,edad,PM,PF)
levels(data2$edad) = c(' 0-9','10-19','80+','10-19','20-29','20-29','30-39','30-39',
                       '40-49','40-49',' 0-9','50-59','50-59','60-69','60-69','70-79',
                       '70-79',rep('80+',4))
data2 = data2 %>% mutate(edad = factor(edad,levels = edadesd80)) %>%
  group_by(cod_level,level,edad) %>% summarise(PM = sum(PM,na.rm = T),PF = sum(PF,na.rm = T)) %>% ungroup()
data2005 = bind_rows(data2 %>% mutate(sexo = 'MEN',K0 = PM) %>% select(cod_level,edad,sexo,K0),
                     data2 %>% mutate(sexo = 'WOMEN',K0 = PF) %>% select(cod_level,edad,sexo,K0))
dataCensoMEX = full_join(data2010,data2005) %>% mutate(R = 1/(2010 - 2005)*log(KT/K0),
                                                 init = 2005,pais = 'Mexico')
levelMEX = data1 %>% group_by(cod_level,level) %>% summarise() %>% ungroup() %>% 
  mutate(pais = 'Mexico',level = toupper(level)) %>% select(cod_level,level,pais)
rm(list = ls()[!ls() %in% c('dataCensoMEX','levelMEX')])
save.image('./Data/dataCensoMEX.RData')
