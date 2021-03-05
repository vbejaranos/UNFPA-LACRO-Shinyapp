rm(list = ls())
library(tidyverse)
replace_na_with_last = function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a) + 1]]
}
muns = read_xls('./Data/Peru/rptUbigeo.xls',skip = 1)
muns = muns %>% select(DEPARTAMENTO,PROVINCIA,DISTRITO) %>% 
  rename(dpto = DEPARTAMENTO,prov = PROVINCIA,dist = DISTRITO) %>%
  filter(!is.na(dist),dpto != 'DEPARTAMENTO',prov != 'PROVINCIA',dist != 'DISTRITO') %>%
  mutate(cod_dist = paste0(substr(dpto,1,2),substr(prov,1,2),substr(dist,1,2)),
         cod_prov = paste0(substr(dpto,1,2),substr(prov,1,2)),
         cod_dpto = substr(dpto,1,2),
         dpto = toupper(substr(dpto,4,nchar(dpto))),
         prov = toupper(substr(prov,4,nchar(prov))),
         dist = toupper(substr(dist,4,nchar(dist))))
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
POP1 = as.data.frame(readxl::read_xls("./Data/Peru/reporte2007.xls"))
data = POP1
data$Edad = data[,1]
data$PTotal = data[,4]
data$PM = data[,2]
data$PF = data[,3]
data = data %>% select(Edad,PTotal,PM,PF) %>% filter(Edad != 'Total')
edades = unique(data$Edad)[substr(unique(data$Edad),1,4) != 'AREA']
dists1 = data %>% filter(!is.na(Edad) & !Edad %in% edades) %>% 
  mutate(dist1 = gsub('[[:digit:]]','',gsub('[[:punct:]]','',PM)),
        cod_dist = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',Edad))))) %>%
  select(Edad,cod_dist,dist1)
data = left_join(data,dists1,"Edad") %>% 
  mutate(cod_dist = replace_na_with_last(cod_dist)) %>% 
  filter(Edad %in% edades[2:21]) %>% rename(edad = Edad)
dataf = bind_rows(data %>% mutate(POP = PF,sexo = "WOMEN") %>% select(edad,POP,sexo,cod_dist),
                  data %>% mutate(POP = PM,sexo = "MEN") %>% select(edad,POP,sexo,cod_dist))
dataf$edad = factor(dataf$edad)
levels(dataf$edad) = c(rep(edadesd80[-length(edadesd80)],each = 2),rep('80+',4))
data1 = dataf %>% inner_join(muns) %>% group_by(cod_dpto,sexo,edad) %>% summarise(K0 = sum(as.numeric(POP),na.rm = T)) %>% ungroup()
POP2 = as.data.frame(readxl::read_xlsx("./Data/Peru/reporte2017.xlsx",skip = 4))
data = POP2[,-1]
data$Edad = data[,1]
data$PTotal = data[,4]
data$PM = data[,2]
data$PF = data[,3]
edades = unique(data$Edad)[substr(unique(data$Edad),1,4) != 'AREA']
dists2 = data %>% filter(!is.na(Edad) & !Edad %in% edades) %>% 
  mutate(dist2 = gsub('[[:digit:]]','',gsub('[[:punct:]]','',PM)),
         cod_dist = as.character(gsub('[[:blank:]]','',gsub('[[:alpha:]]','',gsub('[[:punct:]]','',Edad))))) %>%
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
data2 = dataf %>% inner_join(muns) %>% group_by(cod_dpto,sexo,edad) %>% summarise(KT = sum(as.numeric(POP),na.rm = T)) %>% ungroup()
#
levelPER = muns %>% group_by(cod_dpto,dpto) %>% summarise() %>% ungroup() %>%
  rename(cod_level = cod_dpto,level = dpto) %>% mutate(pais = 'Peru')
dataCensoPER = full_join(data1,data2) %>% 
  mutate(R = 1/(2017 - 2007)*log(KT/K0),pais = 'Peru',init = 2007) %>%
  rename(cod_level = cod_dpto)
rm(list = ls()[!ls() %in% c('dataCensoPER','levelPER')])
save.image('./Data/dataCensoPER.RData')
