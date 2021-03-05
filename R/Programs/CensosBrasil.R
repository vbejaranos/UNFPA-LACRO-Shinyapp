rm(list = ls())
library(tidyverse)
library(readxl)
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
muns = read_xls('./Data/Brasil/municipios.xls',skip = 2)
colnames(muns) = c('cod_uf','uf','cod_mesoreg','mesoreg','cod_microreg',
                   'microreg','cod_mun','mun')
muns =  muns %>% mutate(pais = 'Brazil')
# etnia 1-Branca 2- Preta 3- Amarela 4- Parda 5- Ind?gena 9- Ignorado
UFs = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','PB','PE','AL',
        'SE','BA','MG','ES','RJ','PR','SC','RS','MS','MT','GO','DF')
base2000 = function(uf) {
  base1 = read.table(paste0('./Data/Brasil/',uf,'2000.txt'),sep = '\n')
  base1 = base1 %>% mutate(cod_uf = substr(V1,1,2),
                           cod_mun = substr(V1,12,18),
                           sexo = factor(substr(V1,69,69),labels = c('MEN','WOMEN')),
                           edad = as.numeric(substr(V1,79,81)),
                           etnia = substr(V1,87,87))
  base1$edad[base1$edad %in% 0:9] = 1
  base1$edad[base1$edad %in% 10:19] = 2 
  base1$edad[base1$edad %in% 20:29] = 3 
  base1$edad[base1$edad %in% 30:39] = 4 
  base1$edad[base1$edad %in% 40:49] = 5 
  base1$edad[base1$edad %in% 50:59] = 6 
  base1$edad[base1$edad %in% 60:69] = 7
  base1$edad[base1$edad %in% 70:79] = 8
  base1$edad[base1$edad >= 80] = 9
  base1 = base1 %>% mutate(edad = factor(edad,labels = edadesd80)) %>%
    group_by(cod_uf,cod_mun,sexo,edad,etnia) %>% summarise(pop = n())  
  return(base1)
}
base2010 = function(uf) {
  base2 = read.table(paste0('./Data/Brasil/',uf,'2010.txt'),sep = '\n')
  base2 =  base2 %>% mutate(cod_uf = substr(V1,1,2),
                            cod_mun = paste0(cod_uf,substr(V1,3,7)),
                            sexo = factor(substr(V1,58,58),labels = c('MEN','WOMEN')),
                            edad = as.numeric(substr(V1,62,64)),
                            etnia = substr(V1,68,68))
  base2$edad[base2$edad %in% 0:9] = 1
  base2$edad[base2$edad %in% 10:19] = 2 
  base2$edad[base2$edad %in% 20:29] = 3 
  base2$edad[base2$edad %in% 30:39] = 4 
  base2$edad[base2$edad %in% 40:49] = 5 
  base2$edad[base2$edad %in% 50:59] = 6 
  base2$edad[base2$edad %in% 60:69] = 7
  base2$edad[base2$edad %in% 70:79] = 8
  base2$edad[base2$edad >= 80] = 9
  base2 = base2 %>% mutate(edad = factor(edad,labels = edadesd80)) %>%
    group_by(cod_uf,cod_mun,sexo,edad,etnia) %>% summarise(pop = n())
  return(base2)
}
data2000 = data2010 = NULL
for(uf in UFs) {
  data2000 = bind_rows(data2000,base2000(uf))
  data2010 = bind_rows(data2010,base2010(uf))
}
for (uf in c('RN1','RN2','SP')) {
  data2000 = bind_rows(data2000,base2000(uf))
}
for (uf in c('SP1','SP2','RN')) {
  data2010 = bind_rows(data2010,base2010(uf))
}
# subnacional
data1 = data2000 %>% group_by(sexo,edad,cod_uf) %>% 
  summarise(K0 = sum(pop,na.rm = T)) %>% ungroup()
data2 = data2010 %>% group_by(sexo,edad,cod_uf) %>% 
  summarise(KT = sum(pop,na.rm = T)) %>% ungroup()
dataCensoBRA = full_join(data1,data2) %>% 
  group_by(cod_uf,edad,sexo) %>% 
  summarise(K0 = sum(K0,na.rm = T),KT = sum(KT,na.rm = T)) %>% ungroup() %>% 
  mutate(R = 1/(2010 - 2000)*log(KT/K0),
         cod_level = cod_uf,pais = 'Brazil',init = 2000) %>%
  select(-cod_uf)
levelBRA = muns %>% group_by(cod_uf,uf,pais) %>% summarise() %>% ungroup() %>%
  mutate(level = toupper(uf),cod_level = cod_uf) %>% select(pais,cod_level,level)
rm(list = ls()[!ls() %in% c('dataCensoBRA','levelBRA')])
save.image('./Data/dataCensoBRA.RData')
