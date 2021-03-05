rm(list = ls())
library(tidyverse)
replace_na_with_last = function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a) + 1]]
}
muns = read_xls('./Data/Chile/CUT_2018_v04.xls')
muns = muns %>% mutate(cod_com = `C?digo Comuna 2018`,
                       cod_level = `C?digo Regi?n`,level = `Nombre Regi?n`) %>%
  select(cod_com,cod_level,level)
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
POP1 = as.data.frame(readxl::read_xlsx("./Data/Chile/reporte2002.xlsx",skip = 8))
data = POP1[,-1]
data$Edad = data[,1]
data$PTotal = data[,4]
data$PM = data[,2]
data$PF = data[,3]
data = data %>% select(Edad,PTotal,PM,PF) %>% filter(Edad != 'Total')
edades = unique(data$Edad)[substr(unique(data$Edad),1,4) != 'AREA']
dists1 = data %>% filter(!is.na(Edad) & !Edad %in% edades) %>% 
  mutate(com1 = gsub('[[:digit:]]','',gsub('[[:punct:]]','',PM)),
         cod_com = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',Edad))))) %>%
  select(Edad,com1,cod_com)
data = left_join(data,dists1,"Edad") %>% 
  mutate(cod_com = replace_na_with_last(cod_com)) %>% 
  filter(Edad %in% edades[2:18]) %>% rename(edad = Edad)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_com),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_com))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = c(' 0-9',rep(edadesd80[2:5],each = 2),' 0-9',
                       rep(edadesd80[6:8],each = 2),'80+')
data1 = dataf %>% inner_join(muns) %>%
  group_by(cod_level,sexo,edad) %>% summarise(K0 = sum(as.numeric(POP),na.rm = T)) %>% ungroup()

POP2 = as.data.frame(readxl::read_xlsx("./Data/Chile/reporte2017.xlsx",skip = 8))
data = POP2[,-1]
data$Edad = data[,1]
data$PTotal = data[,4]
data$PM = data[,2]
data$PF = data[,3]
edades = unique(data$Edad)[substr(unique(data$Edad),1,4) != 'AREA']
dists2 = data %>% filter(!is.na(Edad) & !Edad %in% edades) %>% 
  mutate(com2 = gsub('[[:digit:]]','',gsub('[[:punct:]]','',PM)),
         cod_com = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',Edad))))) %>%
  select(Edad,com2,cod_com)
data = left_join(data,dists2,"Edad") %>% 
  mutate(cod_com = replace_na_with_last(cod_com)) %>% 
  filter(Edad %in% edades[3:23]) %>% rename(edad = Edad)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_com),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_com))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = c(' 0-9','10-19', '80+','10-19','20-29','20-29','30-39','30-39','40-49',
                       '40-49',' 0-9','50-59','50-59','60-69','60-69','70-79','70-79',
                       rep('80+',4))
data2 = dataf %>% inner_join(muns) %>%
  group_by(cod_level,sexo,edad) %>% summarise(KT = sum(as.numeric(POP),na.rm = T)) %>% ungroup()
#
dataCensoCHL = full_join(data1,data2) %>% mutate(R = 1/(2017 - 2002)*log(KT/K0),
                                                 pais = 'Chile',init = 2002)
levelCHL = muns %>% group_by(cod_level,level)  %>% summarise() %>% ungroup() %>%
  mutate(pais = 'Chile') %>% select(cod_level,level,pais)
rm(list = ls()[!ls() %in% c('dataCensoCHL','levelCHL')])
save.image('./Data/dataCensoCHL.RData')
