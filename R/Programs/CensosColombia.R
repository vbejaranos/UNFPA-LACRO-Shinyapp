rm(list = ls())
library(tidyverse)
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
replace_na_with_last = function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a) + 1]]
}
division = read.csv('./Data/Colombia/DIVIPOLAmunicipios.csv',encoding = 'UTF-8') %>%
  rename(cod_dpto = Código.Departamento, cod_mun = Código.Municipio,
         dpto = Nombre.Departamento,mun = Nombre.Municipio) %>% select(-5) %>%
  mutate(cod_dpto = formatC(cod_dpto,flag = '0',width = 2),
         cod_mun = formatC(cod_mun,flag = '0',width = 5)) %>%
  group_by(cod_dpto,dpto) %>% summarise() %>% ungroup()
division$dpto[division$cod_dpto == '88'] = 'SAN ANDRES ISLAS'
## Poblaciones departamentos Colombia interpolacion censos
POP2005 = as.data.frame(readxl::read_xlsx("./Data/reporteCOL2005.xlsx",col_names = F))
data = POP2005
data$etnia = data[,1]
data$edad = data[,2]
data$PTotal = data[,5]
data$PM = data[,3]
data$PF = data[,4]
data = data %>% select(etnia,edad,PTotal,PM,PF) %>% mutate(etnia = replace_na_with_last(etnia))
edadesCol = unique(unlist(data$edad))[4:12]
deptos = data %>% filter(!(edad %in% c(edadesCol,'Total',"Edad en grupos decenales",NA))) %>% 
  mutate(dpto = gsub('[[:blank:]]','',gsub('[[:digit:]]','',gsub('[[:punct:]]','',edad))),
         cod_dpto = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',etnia))))) %>%
  select(edad,dpto,cod_dpto)
data = left_join(data,deptos,"edad") %>% 
  mutate(dpto = replace_na_with_last(dpto),
         cod_dpto = replace_na_with_last(cod_dpto)) %>% filter(edad %in% edadesCol)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_dpto,etnia),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_dpto,etnia))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = edadesd80
dataf$etnia = factor(dataf$etnia)
levels(dataf$etnia) = c('Indigena','Afro','Otros','Otros','Afro','Afro','Rom','Total')
data2005 = dataf %>% group_by(sexo,edad,cod_dpto,etnia) %>% summarise(K0 = sum(as.numeric(POP))) %>% ungroup()

POP2018 = as.data.frame(readxl::read_xlsx("./Data/reporteCOL2018.xlsx",skip = 11,col_names = F))
data = POP2018
data$etnia = data[,1]
data$edad = data[,2]
data$PTotal = data[,5]
data$PM = data[,3]
data$PF = data[,4]
data = data %>% select(etnia,edad,PTotal,PM,PF) %>% mutate(etnia = replace_na_with_last(etnia))
edadesCol = unique(unlist(data$edad))[4:14]
deptos = data %>% filter(!(edad %in% c(edadesCol,'Total',"Edades Decenales",NA))) %>% 
  mutate(dpto = gsub('[[:blank:]]','',gsub('[[:digit:]]','',gsub('[[:punct:]]','',edad))),
         cod_dpto = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',etnia))))) %>%
  select(edad,dpto,cod_dpto)
data = left_join(data,deptos,"edad") %>% 
  mutate(cod_dpto = replace_na_with_last(cod_dpto)) %>% filter(edad %in% edadesCol)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_dpto,etnia),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_dpto,etnia))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = c(edadesd80[1:2],'80+',edadesd80[3:8],rep('80+',2))
dataf$etnia = factor(dataf$etnia)
levels(dataf$etnia) = c('Rom','Indigena','Afro','Otros','Otros','Afro','Afro','Total')
data2018 = dataf %>% group_by(sexo,edad,cod_dpto,etnia) %>% summarise(KT = sum(as.numeric(POP))) %>% ungroup()

dataCensoCOL = full_join(data2005,data2018) %>% filter(etnia == 'Total') %>%
  group_by(cod_dpto,edad,sexo) %>%
  summarise(K0 = sum(K0,na.rm = T),KT = sum(KT,na.rm = T)) %>% ungroup() %>%
  mutate(R = 1/(2018 - 2005)*log(KT/K0),pais = 'Colombia',init = 2005) %>%
  rename(cod_level = cod_dpto)
levelCOL = division %>% select(dpto,cod_dpto) %>% rename(level = dpto,cod_level = cod_dpto) %>%
  mutate(pais = 'Colombia')
rm(list = ls()[!ls() %in% c('dataCensoCOL','levelCOL')])
save.image('./Data/dataCensoCOL.RData')
