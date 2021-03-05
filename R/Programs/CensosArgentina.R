rm(list = ls())
library(tidyverse)
replace_na_with_last = function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a) + 1]]
}
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
POP1 = as.data.frame(readxl::read_xls("./Data/Argentina/reporte2001.xls",skip = 10))
data = POP1
data$Edad = data[,1]
data$PTotal = data[,4]
data$PM = data[,2]
data$PF = data[,3]
data = data %>% select(Edad,PTotal,PM,PF) %>% filter(Edad != 'Total')
edades = unique(data$Edad)[substr(unique(data$Edad),1,4) != 'AREA']
dists1 = data %>% filter(!is.na(Edad) & !Edad %in% edades) %>% 
  mutate(cod_dist = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',Edad)))),
         dist1 = gsub('[[:digit:]]','',gsub('[[:punct:]]','',PM))) %>%
  select(Edad,cod_dist,dist1)
data = left_join(data,dists1,"Edad") %>% 
  mutate(cod_dist = replace_na_with_last(cod_dist)) %>% 
  filter(Edad %in% edades[2:21]) %>% rename(edad = Edad)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_dist),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_dist))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = c(' 0-9','10-19','10-19','20-29','20-29','30-39','30-39','40-49',
                       '40-49',' 0-9','50-59','50-59','60-69','60-69','70-79','70-79',
                       rep('80+',4))
data1 = dataf %>% group_by(cod_dist,sexo,edad) %>% summarise(K0 = sum(as.numeric(POP),na.rm = T)) %>% ungroup()
POP2 = as.data.frame(readxl::read_xls("./Data/Argentina/reporte2010.xls",skip = 12))
data = POP2
data$Edad = data[,1]
data$PTotal = data[,4]
data$PM = data[,2]
data$PF = data[,3]
edades = unique(data$Edad)[substr(unique(data$Edad),1,4) != 'AREA']
dists2 = data %>% filter(!is.na(Edad) & !Edad %in% edades) %>% 
  mutate(cod_dist = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',Edad)))),
         dist2 = gsub('[[:digit:]]','',gsub('[[:punct:]]','',PM))) %>%
  select(Edad,cod_dist,dist2)
data = left_join(data,dists2,"Edad") %>% 
  mutate(cod_dist = replace_na_with_last(cod_dist)) %>% 
  filter(Edad %in% edades[3:22]) %>% rename(edad = Edad)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_dist),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_dist))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = c(' 0-9','10-19','10-19','20-29','20-29','30-39','30-39','40-49',
                       '40-49',' 0-9','50-59','50-59','60-69','60-69','70-79','70-79',
                       rep('80+',4))
data2 = dataf %>% group_by(cod_dist,sexo,edad) %>% summarise(KT = sum(as.numeric(POP),na.rm = T)) %>% ungroup()
#
dataCensoARG = full_join(data1,data2) %>% mutate(R = 1/(2010 - 2001)*log(KT/K0),
                                                 init = 2001,pais = 'Argentina') %>%
  rename(cod_level = cod_dist)
levelARG = dists1 %>% select(cod_dist,dist1) %>% 
  full_join(dists2 %>% select(cod_dist,dist2),by = 'cod_dist') %>% 
  rename(cod_level = cod_dist,level = dist1) %>% mutate(pais = 'Argentina') %>%
  select(cod_level,level,pais)
rm(list = ls()[!ls() %in% c('dataCensoARG','levelARG')])
save.image('./Data/dataCensoARG.RData')
