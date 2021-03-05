rm(list = ls())
library(tidyverse)
theme_set(theme_minimal())
library(lubridate)
library(readxl)
library(sf)
library(openxlsx)
library(ggthemes)
library(RColorBrewer)
### colores y fecha #####
Pal.1 = Palb.1 = brewer.pal(n = 8, name = "RdPu")
Pal.2 = Palb.2 = brewer.pal(n = 8, name = "PuBu")
Palete = c(Pal.2[3],Pal.2[7],Pal.1[7],Pal.1[3])
Paleteb = c(Palb.2[8],Palb.2[2],Palb.1[8],Palb.1[2])
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
edadesnd80 = tibble(edad = edadesd80,edadn = seq(0,80,10))
edadesq80 = c(" 0-4"," 5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
edadesq = c(" 0-4"," 5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","M100")
meses = tibble(mes = c('Enero','Febrero','Marzo','Abril','Mayo',
                       'Junio','Julio','Agosto','Setiembre','Octubre',
                       'Noviembre','Diciembre'),num = 1:12)
replace_na_with_last = function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a) + 1]]
}
fechafin = ymd('2020/12/31')
## UN poblaciones ####
mil = 10^3
POPM = readxl::read_xlsx("./Data/WPP2019_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx",skip = 16)
POPF = readxl::read_xlsx("./Data/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx",skip = 16)
popLAC = function(anio) {
  nKxMEST = as.numeric(unlist(POPM %>% filter(`Region, subregion, country or area *` == "LATIN AMERICA AND THE CARIBBEAN" & `Reference date (as of 1 July)` == anio) %>% select(-c(1:8))))
  nKxFEST = as.numeric(unlist(POPF %>% filter(`Region, subregion, country or area *` == "LATIN AMERICA AND THE CARIBBEAN" & `Reference date (as of 1 July)` == anio) %>% select(-c(1:8))))
  nKx = tibble(edad = edadesq,sexo = "WOMEN",LAC = nKxFEST*mil)
  nKx = bind_rows(nKx,tibble(edad = edadesq,sexo = "MEN",LAC = nKxMEST*mil))
  nKx$edad = factor(nKx$edad)
  levels(nKx$edad) = c(rep(edadesd80[-length(edadesd80)],each = 2),rep('80+',5))
  nKx = nKx %>% group_by(sexo,edad) %>% summarise(LAC = sum(LAC)) %>% ungroup() %>%
    mutate(year = anio) %>% arrange(year,edad,sexo)
  return(nKx)
}
nKx_LAC = popLAC(2020)
popUN = function(anio, pais){
  edadesq = c(" 0-4"," 5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","M100")
  nKxM = as.numeric(unlist(POPM %>% filter(`Region, subregion, country or area *` == pais & `Reference date (as of 1 July)` == anio) %>% select(-c(1:8))))
  nKxF = as.numeric(unlist(POPF %>% filter(`Region, subregion, country or area *` == pais & `Reference date (as of 1 July)` == anio) %>% select(-c(1:8))))
  nKx = tibble(edad = edadesq,sexo = "WOMEN",pop = nKxF*mil)
  nKx = bind_rows(nKx,tibble(edad = edadesq,sexo = "MEN",pop = nKxM*mil))
  nKx$edad = factor(nKx$edad)
  levels(nKx$edad) = c(rep(edadesd80[-length(edadesd80)],each = 2),rep('80+',5))
  nKx = nKx %>% group_by(sexo,edad) %>% summarise(pop = sum(pop)) %>% ungroup() %>%
    mutate(year = anio,pais = pais) %>% arrange(pais,year,edad,sexo)
  return(nKx)
}
cumminus = function(x){
  a = vector('numeric',length(x))
  a[1] = x[1]
  for(i in 2:length(x)) {
    a[i] = x[i] - x[i - 1]
  }
  return(a)
}

###################################################
################# ARGENTINA #######################
###################################################
# DEFUNCIONES TOTALES ####
base = bind_rows(read.csv('./Data/Argentina/DefWeb15.csv') %>% mutate(year = 2015),
                 read.csv('./Data/Argentina/DefWeb16.csv') %>% mutate(year = 2016),
                 read.csv('./Data/Argentina/DefWeb17.csv') %>% mutate(year = 2017),
                 read.csv('./Data/Argentina/DefWeb18.csv') %>% mutate(year = 2018)) %>%
  mutate(sexo = factor(SEXO,labels = c('MEN','WOMEN','IND')),
         causa = CAUSA, edad = factor(GRUPEDAD),pais = 'Argentina',
         n = CUENTA,cod_level = formatC(PROVRES,width = 2,flag = '0'))
levels(base$edad) = c(rep(edadesd80[1:8],each = 2),'80+','desc')
ARGCausaEdadSexo = base %>% group_by(pais,year,cod_level,edad,sexo,causa) %>% 
  summarise(n = sum(n,na.rm = T)) %>% ungroup()
rm(base)
# POBLACION ####
nKxARG = bind_rows(popUN(2015,'Argentina'),popUN(2016,'Argentina'),popUN(2017,'Argentina'),
                   popUN(2018,'Argentina'),popUN(2019,'Argentina'),popUN(2020,'Argentina'))
###################################################
################## ECUADOR ########################
###################################################
# DEFUNCIONES TOTALES ####
base2015 = haven::read_sav('./Data/Ecuador/EDG_2015.sav') %>% 
  select(sexo,fecha_fall,edad,etnia,causa4,prov_res) %>% 
  mutate(Fecha = ymd(fecha_fall),
         anio_fall = year(Fecha),
         mes_fall = month(Fecha),
         Causa = as.character(substr(causa4,1,4)),
         sexo = factor(sexo),
         etnia = factor(etnia),cod_level = prov_res) %>% 
  select(-c(causa4,fecha_fall,prov_res)) %>%
  full_join(levelECU)
base2016 = read.csv('./Data/Ecuador/EDG_2016.csv',header = T,sep = ',',encoding = 'UTF-8') %>%
  select(sexo,fecha_fall,edad,etnia,causa4,prov_res) %>% 
  mutate(Fecha = mdy(fecha_fall),
         anio_fall = year(Fecha),
         mes_fall = month(Fecha),
         Causa = substr(causa4,1,4),sexo = factor(sexo,labels = c('1','2')),
         etnia = factor(etnia,labels = c('2','7','1','6','5','4','3','8','9')),
         level = toupper(prov_res)) %>% 
  select(-c(causa4,fecha_fall,prov_res))
base2016$level[base2016$level == 'SANTO DOMINGO DE LOS TSACHILAS'] = 'SANTO DOMINGO'
base2016 = base2016 %>% full_join(levelECU)
base2017 = read.csv('./Data/Ecuador/EDG_2017.csv',header = T,sep = ';') %>%
  select(sexo,fecha_fall,edad,etnia,causa4,prov_res) %>% 
  mutate(Fecha = mdy(fecha_fall),
         anio_fall = year(Fecha),
         mes_fall = month(Fecha),
         Causa = substr(causa4,1,4),sexo = factor(sexo),
         etnia = factor(etnia),cod_level = formatC(prov_res,width = 2,flag = '0')) %>% 
  select(-c(causa4,fecha_fall,prov_res)) %>%
  full_join(levelECU)
base2018 = read.csv('./Data/Ecuador/EDG_2018.csv',header = T,sep = ';',encoding = 'UTF-8') %>% 
  select(sexo,fecha_fall,edad,etnia,causa4,prov_res) %>% 
  mutate(Fecha = mdy(fecha_fall),
         anio_fall = year(Fecha),
         mes_fall = month(Fecha),
         Causa = substr(causa4,1,4),sexo = factor(sexo,labels = c('1','2')),
         etnia = factor(etnia,labels = c('2','7','1','6','5','4','3','8','9')),
         level = toupper(prov_res)) %>% 
  select(-c(causa4,fecha_fall,prov_res))
base2018$level[base2018$level == 'SANTO DOMINGO DE LOS TSACHILAS'] = 'SANTO DOMINGO'
base2018 = base2018 %>% full_join(levelECU)
base2019 = read.csv('./Data/Ecuador/EDG_2019.csv',header = T,sep = ';') %>% 
  select(sexo,fecha_fall,edad,etnia,causa4,prov_res) %>% 
  mutate(Fecha = mdy(fecha_fall),
         anio_fall = year(Fecha),
         mes_fall = month(Fecha),
         Causa = substr(causa4,1,4),sexo = factor(sexo),etnia = factor(etnia),
         cod_level = formatC(prov_res,width = 2,flag = '0')) %>% 
  select(-c(causa4,fecha_fall,prov_res)) %>% 
  full_join(levelECU)
base2020 = read_xlsx('./Data/Ecuador/Defunciones_Generales_2020_act25_OCT.xlsx') %>%
  mutate(Fecha = ymd(substr(`FECHA DEFUNCIÓN`,1,10))) %>%
  mutate(anio_fall = year(Fecha),mes_fall = month(Fecha),
         level = `PROVINCIA DEFUNCION`) %>% 
  select(anio_fall,mes_fall,level)
base2020$level[base2020$level == 'SANTO DOMINGO DE LOS TSACHILAS'] = 'SANTO DOMINGO'
base2020 = base2020 %>% full_join(levelECU)
deathsECU = bind_rows(base2015,base2016,base2017,base2018,base2019,base2020)
deathsECU$edad = as.numeric(deathsECU$edad)
deathsECU$edad[deathsECU$edad %in% 0:9] = 1
deathsECU$edad[deathsECU$edad %in% 10:19] = 2 
deathsECU$edad[deathsECU$edad %in% 20:29] = 3 
deathsECU$edad[deathsECU$edad %in% 30:39] = 4 
deathsECU$edad[deathsECU$edad %in% 40:49] = 5 
deathsECU$edad[deathsECU$edad %in% 50:59] = 6 
deathsECU$edad[deathsECU$edad %in% 60:69] = 7
deathsECU$edad[deathsECU$edad %in% 70:79] = 8
deathsECU$edad[deathsECU$edad >= 80 & deathsECU$edad != 999] = 9
#etnia 1-indigena 2-afro 3-negro 4-mulato 5-montubio 
#6-mestizo 7-blanco 8-otra 9-se ignora
deathsECU$etnia[deathsECU$etnia %in% c(4,3,2)] = 2
deathsECU$etnia[deathsECU$etnia %in% c(8,9)] = 8
deathsECU = deathsECU %>% mutate(year = as.numeric(anio_fall),
                                 mes = as.numeric(mes_fall),causa = Causa,
                                 sexo = factor(sexo,labels = c('MEN','WOMEN')),
                                 edad = factor(edad,labels = c(edadesd80,'desc')),pais = 'Ecuador',
                                 etnia = factor(etnia,labels = c('Indig','Afro','Mont','Mest','Blan','Otro'))) %>%
  select(pais,year,mes,sexo,causa,edad,etnia)
rm(base2015,base2016,base2017,base2018,base2019,base2020)
# POBLACION ####
nKxECU = bind_rows(popUN(2015,'Ecuador'),popUN(2016,'Ecuador'),popUN(2017,'Ecuador'),
                   popUN(2018,'Ecuador'),popUN(2019,'Ecuador'),popUN(2020,'Ecuador'))


###################################################
################### PERU ##########################
###################################################
# DEFUNCIONES TOTALES ####
base = read_xlsx('./Data/Peru/SINADEF_DATOS_ABIERTOS_17012021.xlsx',skip = 3)
deathsPER = base
deathsPER$EDAD = as.numeric(deathsPER$EDAD)
deathsPER$EDAD[deathsPER$`TIEMPO EDAD` %in% c('DIAS','HORAS','MESES','MINUTOS','SEGUNDOS')] = 0
deathsPER$EDAD[deathsPER$EDAD %in% 0:9] = 1
deathsPER$EDAD[deathsPER$EDAD %in% 10:19] = 2 
deathsPER$EDAD[deathsPER$EDAD %in% 20:29] = 3 
deathsPER$EDAD[deathsPER$EDAD %in% 30:39] = 4 
deathsPER$EDAD[deathsPER$EDAD %in% 40:49] = 5 
deathsPER$EDAD[deathsPER$EDAD %in% 50:59] = 6 
deathsPER$EDAD[deathsPER$EDAD %in% 60:69] = 7
deathsPER$EDAD[deathsPER$EDAD %in% 70:79] = 8
deathsPER$EDAD[deathsPER$EDAD >= 80] = 9
deathsPER$EDAD[deathsPER$`TIEMPO EDAD` %in% c('IGNORADO','SIN REGISTRO')] = 'desc'
deathsPER = deathsPER %>% mutate(year = as.numeric(AÑO),mes = as.numeric(MES),
                                 causa = `CAUSA A (CIE-X)`,
                                 sexo = factor(SEXO,labels = c('WOMEN','IND','MEN','IND')),
                                 edad = factor(EDAD,labels = c(edadesd80,'desc')),pais = 'Peru',
                                 level = `DEPARTAMENTO DOMICILIO`) %>%
  select(pais,year,mes,level,sexo,causa,edad)
deathsPER = deathsPER %>% full_join(levelPER)
# POBLACION ####
nKxPER = bind_rows(popUN(2015,'Peru'),popUN(2016,'Peru'),popUN(2017,'Peru'),
                   popUN(2018,'Peru'),popUN(2019,'Peru'),popUN(2020,'Peru'))

###################################################
################ COLOMBIA #########################
###################################################
# DEFUNCIONES TOTALES ####
data2015 = read.csv("./Data/Colombia/nofetal2015.csv", 
                    header = T, sep = ";",dec = ",")
data2016 = read.csv("./Data/Colombia/nofetal2016.csv", 
                    header = T, sep = ",",dec = ",")
data2017 = read.csv("./Data/Colombia/nofetal2017.csv", 
                    header = T, sep = ";",dec = ",")
colnames(data2017) = c("COD_DPTO",colnames(data2017)[-1])
data2018 = read.csv("./Data/Colombia/nofetal2018.csv", 
                    header = T, sep = ";",dec = ",")
colnames(data2018) = c("COD_DPTO",colnames(data2018)[-1])
data2019 = read_xlsx("./Data/Colombia/nofetal2019.xlsx")
data2020 = read_xlsx("./Data/Colombia/nofetal2020.xlsx")
dataanio = function(data){
  data$SEXO = factor(data$SEXO,levels = c(1,2,3),labels = c("MEN","WOMEN","IND"))
  data$GRU_ED1 = as.numeric(data$GRU_ED1)
  data$GRU_ED1[data$GRU_ED1 %in% 0:7] = 8
  data$GRU_ED1[data$GRU_ED1 %in% 24:28] = 24
  data$edad = factor(data$GRU_ED1,labels = c(edadesq[1:16],'80+','desc'))
  data$etnia = factor(data$IDPERTET)
  levels(data$etnia) = c('Indigena','Rom','Afro','Afro','Afro','Otros','SINFO')
  levels(data$edad) = c(rep(edadesd80[-length(edadesd80)],each = 2),'80+','desc')
  dataf = data %>% mutate(causa = gsub('[[:blank:]]','',toupper(C_DIR1)),
                          causa = ifelse(causa == '','desc',causa),
                          year = as.numeric(ANO),sexo = SEXO,
                          mes = as.numeric(MES),pais = 'Colombia',
                          ocur = formatC(CODOCUR,width = 2, flag = '0'),
                          cod_level = formatC(CODPTORE,width = 2,flag = '0')) %>% 
    select(year,mes,etnia,ocur,cod_level,causa,sexo,edad,pais)
  return(dataf)
}
deathsCOL = bind_rows(dataanio(data2015),dataanio(data2016),dataanio(data2017),
                      dataanio(data2018),dataanio(data2019),dataanio(data2020))
rm(data2015,data2016,data2017,data2018,data2019,data2020)
# POBLACION ####
nKxCOL = bind_rows(popUN(2015,'Colombia'),popUN(2016,'Colombia'),popUN(2017,'Colombia'),
                   popUN(2018,'Colombia'),popUN(2019,'Colombia'),popUN(2020,'Colombia'))

###################################################
################ COSTA RICA #######################
###################################################
# DEFUNCIONES TOTALES ####
CRI1 = function(anio) {
  base = read_xlsx(paste0('./Data/CostaRica/reporteEdadSexoMes',anio,'.xlsx'),skip = 9)
  colnames(base) = c('o','mes','edad','sexoH','sexoM','sexoT')
  base = base[,-1] %>% select(mes,edad,sexoH,sexoM) %>% mutate(mes = replace_na_with_last(mes)) %>%
    filter(!is.na(edad) & mes != 'Total')
  base = bind_rows(base %>% select(mes,edad,sexoH) %>% 
                     mutate(sexo = 'MEN',year = anio,n = as.numeric(sexoH)) %>% select(-sexoH),
                   base %>% select(mes,edad,sexoM) %>% 
                     mutate(sexo = 'WOMEN',year = anio,n = as.numeric(sexoM)) %>% select(-sexoM)) %>%
    mutate(edad = as.numeric(edad),pais = 'Costa Rica') %>% inner_join(meses) %>%
    mutate(mes = num) %>% select(-num)
  base$edad[base$edad %in% 0:9] = 1
  base$edad[base$edad %in% 10:19] = 2 
  base$edad[base$edad %in% 20:29] = 3 
  base$edad[base$edad %in% 30:39] = 4 
  base$edad[base$edad %in% 40:49] = 5 
  base$edad[base$edad %in% 50:59] = 6 
  base$edad[base$edad %in% 60:69] = 7
  base$edad[base$edad %in% 70:79] = 8
  base$edad[base$edad >= 80] = 9
  base = base %>% mutate(edad = factor(edad,labels = edadesd80),sexo = factor(sexo)) %>% 
    group_by(pais,mes,edad,sexo,year) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup()
  return(base)
}
CRIEdadSexoMes = bind_rows(CRI1(2015),CRI1(2016),CRI1(2017),CRI1(2018),CRI1(2019))
CRI2 = function(anio) {
  base = read_xlsx(paste0('./Data/CostaRica/reporteCausaEdadSexo',anio,'.xlsx'),skip = 9)
  colnames(base) = c('o','sexo','causa',' 0',' 1-4',edadesq[2:20],'Total')
  base = base[,-1] %>% mutate(sexo = replace_na_with_last(sexo)) %>%
    filter(!is.na(causa) & sexo != 'Total')
  base = base %>% pivot_longer(cols = -c(sexo,causa), names_to = "edad", values_to = "n") %>%
    mutate(n = as.numeric(n)) %>% filter(edad != 'Total')
  base$edad[base$edad %in% c(' 0',' 1-4',' 5-9')] = 1
  base$edad[base$edad %in% c('10-14','15-19')] = 2
  base$edad[base$edad %in% c('20-24','25-29')] = 3
  base$edad[base$edad %in% c('30-34','35-39')] = 4
  base$edad[base$edad %in% c('40-44','45-49')] = 5
  base$edad[base$edad %in% c('50-54','55-59')] = 6
  base$edad[base$edad %in% c('60-64','65-69')] = 7
  base$edad[base$edad %in% c('70-74','75-79')] = 8
  base$edad[base$edad %in% edadesq[17:20]] = 9
  base = base %>% mutate(edad = factor(edad,labels = edadesd80)) %>%
    group_by(sexo,causa,edad) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Costa Rica',sexo = factor(sexo,labels = c('MEN','WOMEN')))
  return(base)
}
CRICausaEdadSexo = bind_rows(CRI2(2015),CRI2(2016),CRI2(2017),CRI2(2018),CRI2(2019))
CRI3 = function(anio) {
  base = read_xlsx(paste0('./Data/CostaRica/reporteCausaMes',anio,'.xlsx'),skip = 9)
  colnames(base) = c('o','causa',meses$mes,'Total')
  base = base[,-1] %>% filter(!is.na(causa)) %>% mutate(Total = as.character(Total))
  base = base %>% pivot_longer(cols = c(-causa), names_to = "mes", values_to = "n") %>%
    mutate(n = as.numeric(n)) %>% filter(mes != 'Total') %>% inner_join(meses) %>%
    mutate(mes = num) %>% select(-num)
  base = base %>% group_by(causa,mes) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Costa Rica')
  return(base)
}
CRICausaMes = bind_rows(CRI3(2015),CRI3(2016),CRI3(2017),CRI3(2018),CRI3(2019))
rm(base)
# POBLACION ####
nKxCRI = bind_rows(popUN(2015,'Costa Rica'),popUN(2016,'Costa Rica'),popUN(2017,'Costa Rica'),
                   popUN(2018,'Costa Rica'),popUN(2019,'Costa Rica'),popUN(2020,'Costa Rica'))


###################################################
#################### MEXICO #######################
###################################################
# DEFUNCIONES TOTALES ####
base2015 = foreign::read.dbf('./Data/Mexico/DEFUN15.DBF')
base2016 = foreign::read.dbf('./Data/Mexico/DEFUN16.dbf')
base2017 = foreign::read.dbf('./Data/Mexico/DEFUN17.dbf')
base2018 = foreign::read.dbf('./Data/Mexico/DEFUN18.DBF')
deathsMEX = bind_rows(base2015,base2016,base2017,base2018) %>% 
  mutate(EDAD_AGRU = as.numeric(EDAD_AGRU))
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 1:6] = 1
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 7:8] = 2 
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 9:10] = 3 
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 11:12] = 4 
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 13:14] = 5 
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 15:16] = 6 
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 17:18] = 7
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 19:20] = 8
deathsMEX$EDAD_AGRU[deathsMEX$EDAD_AGRU %in% 21:29 ] = 9
deathsMEX = deathsMEX %>% 
  mutate(year = as.numeric(ANIO_OCUR),
         mes = as.numeric(MES_OCURR),causa = CAUSA_DEF,
         sexo = factor(SEXO,labels = c('MEN','WOMEN','IND')),
         edad = factor(EDAD_AGRU,labels = c(edadesd80,'Ind')),
         pais = 'Mexico',cod_level = as.character(ENT_RESID)) %>%
  select(pais,year,cod_level,mes,sexo,causa,edad)
rm(base2015,base2016,base2017,base2018)
# POBLACION ####
nKxMEX = bind_rows(popUN(2015,'Mexico'),popUN(2016,'Mexico'),popUN(2017,'Mexico'),
                   popUN(2018,'Mexico'),popUN(2019,'Mexico'),popUN(2020,'Mexico'))

###################################################
################ GUATEMALA ########################
###################################################
# DEFUNCIONES TOTALES ####
GTM1 = function(anio) {
  base = haven::read_sav(paste0('./Data/Guatemala/defun',anio,'.sav')) %>% 
    select(Sexo,Mesocu,Añoocu,Edadif,Puedif,Caudef,Dredif) %>% 
    mutate(year = as.numeric(Añoocu),
           mes = as.numeric(Mesocu),
           causa = as.character(substr(Caudef,1,4)),
           sexo = factor(Sexo),
           etnia = factor(Puedif),
           Edad = as.numeric(Edadif),
           cod_level = formatC(Dredif,width = 2,flag = '0'))
  base$Edad[base$Edad %in% 0:9] = 1
  base$Edad[base$Edad %in% 10:19] = 2 
  base$Edad[base$Edad %in% 20:29] = 3 
  base$Edad[base$Edad %in% 30:39] = 4 
  base$Edad[base$Edad %in% 40:49] = 5 
  base$Edad[base$Edad %in% 50:59] = 6 
  base$Edad[base$Edad %in% 60:69] = 7
  base$Edad[base$Edad %in% 70:79] = 8
  base$Edad[base$Edad >= 80 & base$Edad != 999] = 9
  base$etnia[base$etnia == 5] = 9
  base = base %>% mutate(edad = factor(Edad,labels = c(edadesd80,'desc')),
                         sexo = factor(sexo,labels = c('MEN','WOMEN')),
                         etnia = factor(etnia,labels = c('Maya','Gar','Xin','Mest','Otro')),
                         pais = 'Guatemala') %>% 
    select(pais,year,cod_level,mes,causa,edad,sexo,etnia)
  return(base)
}
#etnia 1-Maya 2-Garifuna 3-Xinca 4-Mestizo,Ladino 5-Otro 9-Ignorado
deathsGTM = bind_rows(GTM1(2015),GTM1(2016),GTM1(2017),GTM1(2018))
# POBLACION ####
nKxGTM = bind_rows(popUN(2015,'Guatemala'),popUN(2016,'Guatemala'),popUN(2017,'Guatemala'),
                   popUN(2018,'Guatemala'),popUN(2019,'Guatemala'),popUN(2020,'Guatemala'))

###################################################
##################### CHILE #######################
###################################################
# DEFUNCIONES TOTALES ####
deathsCHL = read.csv('./Data/Chile/DEF_2010-2017.csv',sep = ';')
deathsCHL2015 = deathsCHL %>% mutate(fecha = ymd(FECHA_DEF),
                                     sexo = factor(GLOSA_SEXO),
                                     edad = EDAD_CANT,
                                     edadtipo = GLOSA_EDAD_TIPO,
                                     causa1 = DIAG1,causa2 = DIAG2,pais = 'Chile',
                                     cod_level = formatC(REG_RES,width = 2,flag = '0')) %>%
  mutate(sexo = factor(sexo, labels = c('MEN','IND','WOMEN'))) %>%
  select(pais,fecha,cod_level,sexo,edad,edadtipo,causa1,causa2)
deathsCHL2015$edad[!deathsCHL2015$edadtipo %in% c('Edad en años','Ignorado')] = 0
deathsCHL2015$edad[deathsCHL2015$edad %in% 0:9] = 1
deathsCHL2015$edad[deathsCHL2015$edad %in% 10:19] = 2
deathsCHL2015$edad[deathsCHL2015$edad %in% 20:29] = 3
deathsCHL2015$edad[deathsCHL2015$edad %in% 30:39] = 4
deathsCHL2015$edad[deathsCHL2015$edad %in% 40:49] = 5
deathsCHL2015$edad[deathsCHL2015$edad %in% 50:59] = 6
deathsCHL2015$edad[deathsCHL2015$edad %in% 60:69] = 7
deathsCHL2015$edad[deathsCHL2015$edad %in% 70:79] = 8
deathsCHL2015$edad[deathsCHL2015$edad >= 80 & deathsCHL2015$edad != 999] = 9
deathsCHL2015 = deathsCHL2015 %>% mutate(edad = factor(edad,labels = c(edadesd80,'desc'))) %>%
  select(-edadtipo)
deathsCHL2015 = bind_rows(deathsCHL2015 %>% filter(causa2 == '') %>%
                            rename(causa = causa1) %>% select(-c(causa2)),
                          deathsCHL2015 %>% filter(causa2 != '') %>%
                            rename(causa = causa2) %>% select(-c(causa1)))
deathsCHL = read.csv('./Data/Chile/DEFUNCIONES_FUENTE_DEIS_2016_2021_14012021.csv',sep = ';',header = F) %>%
  mutate(fecha = ymd(V2), sexo = factor(V3),
         edad = V5,edadtipo = V4, causa1 = V9,causa2 = V18,pais = 'Chile',
         cod_level = substr(formatC(V6,flag = '0',width = 5),1,2)) %>%
  mutate(sexo = factor(sexo, labels = c('MEN','IND','WOMEN'))) %>%
  select(pais,fecha,sexo,edad,edadtipo,causa1,causa2,cod_level)
deathsCHL$edad[deathsCHL$edadtipo %in% 2:4] = 0
deathsCHL$edad[deathsCHL$edad %in% 0:9] = 1
deathsCHL$edad[deathsCHL$edad %in% 10:19] = 2
deathsCHL$edad[deathsCHL$edad %in% 20:29] = 3
deathsCHL$edad[deathsCHL$edad %in% 30:39] = 4
deathsCHL$edad[deathsCHL$edad %in% 40:49] = 5
deathsCHL$edad[deathsCHL$edad %in% 50:59] = 6
deathsCHL$edad[deathsCHL$edad %in% 60:69] = 7
deathsCHL$edad[deathsCHL$edad %in% 70:79] = 8
deathsCHL$edad[deathsCHL$edad >= 80 & deathsCHL$edad != 999 & deathsCHL$edadtipo != 9] = 9
deathsCHL$edad[deathsCHL$edadtipo %in% 9] = 10
deathsCHL = deathsCHL %>% mutate(edad = factor(edad,labels = c(edadesd80,'desc')))
deathsCHL = bind_rows(deathsCHL %>% filter(causa2 == '') %>%
                        rename(causa = causa1) %>% select(-c(causa2,edadtipo)),
                      deathsCHL %>% filter(causa2 != '') %>%
                        rename(causa = causa2) %>% select(-c(causa1,edadtipo)))
deathsCHL = bind_rows(deathsCHL2015 %>% filter(year(fecha) == 2015),
                      deathsCHL) %>% 
  mutate(year = year(fecha),mes = month(fecha)) %>% select(-fecha)
rm(deathsCHL2015)
# POBLACION ####
nKxCHL = bind_rows(popUN(2015,'Chile'),popUN(2016,'Chile'),popUN(2017,'Chile'),
                   popUN(2018,'Chile'),popUN(2019,'Chile'),popUN(2020,'Chile'))


###################################################
################### BRASIL ########################
###################################################
# DEFUNCIONES TOTALES ####
BRA1 = function(anio) {
  base = read.csv(paste0('./Data/Brasil/CausaMes',anio,'.csv'),sep = ';',skip = 3)
  colnames(base) = c('causa',meses$mes,'Total')
  base = base %>% filter(causa != 'Total') %>% mutate(Total = as.character(Total))
  base = base %>% pivot_longer(cols = c(-causa), names_to = "mes", values_to = "n") %>%
    mutate(n = as.numeric(n)) %>% filter(mes != 'Total') %>% inner_join(meses) %>%
    mutate(mes = num) %>% select(-num)
  base = base %>% group_by(causa,mes) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Brazil',causa = substr(causa,1,3))
  return(base)
}
BRACausaMes = bind_rows(BRA1(2015),BRA1(2016),BRA1(2017),BRA1(2018))
BRA2 = function(anio) {
  base = read.csv(paste0('./Data/Brasil/EdadSexo',anio,'.csv'),sep = ';',skip = 3,nrows = 13)
  colnames(base) = c('edad','MEN','WOMEN','ind','Total')
  base$ind = as.numeric(base$ind)
  base = base %>% select(-Total) %>%
    mutate(edad = factor(fct_inorder(factor(edad)),labels = c(rep(' 0-9',3),rep('10-19',2),edadesd80[-c(1,2)],'desc'))) %>%
    pivot_longer(-edad,names_to = 'sexo',values_to = 'n')
  base = base %>% group_by(edad,sexo) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Brazil')
  return(base)
}
BRAEdadSexo = bind_rows(BRA2(2015),BRA2(2016),BRA2(2017),BRA2(2018))
BRA3 = function(anio) {
  base = read.csv(paste0('./Data/Brasil/CausaRaza',anio,'.csv'),sep = ';',skip = 3,nrows = 1365)
  colnames(base) = c('causa','Blanco','Preta','Amarela','Parda','Indig','desc','Total')
  base = base %>% select(-Total) %>% filter(causa != 'Total') %>% 
    mutate(Blanco = as.numeric(Blanco),Preta = as.numeric(Preta), Amarela = as.numeric(Amarela),
           Parda = as.numeric(Parda),Indig = as.numeric(Indig),desc = as.numeric(desc),
           causa = substr(causa,1,3)) %>% filter(substr(causa,1,1) %in% LETTERS) %>%
    pivot_longer(-causa,names_to = 'etnia',values_to = 'n')
  base = base %>% group_by(causa,etnia) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Brazil')
  return(base)
}
BRACausaEtnia = bind_rows(BRA3(2015),BRA3(2016),BRA3(2017),BRA3(2018))
data = bind_rows(read.csv('./Data/Brasil/sim2019.csv',sep = ';') %>%
                   mutate(year = 2019),
                 read.csv('./Data/Brasil/domi2020.csv',sep = ';') %>%
                   mutate(year = 2020))
data2 = data %>% mutate(fecha = dmy(formatC(DTOBITO,width = 8,flag = '0')),
                        fechanac = dmy(formatC(DTNASC,width = 8,flag = '0')),
                        mes = month(fecha),year = year(fecha),
                        causa = CAUSABAS,etnia = RACACOR, 
                        sexo = factor(SEXO,labels = c('IND','MEN','WOMEN')),
                        edad = ifelse(!is.na(DTNASC),interval(fechanac,fecha) %/% years(1),
                                      NA),edad2 = IDADE,
                        pais = 'Brazil',
                        ocur = as.character(CODMUNOCOR), 
                        resid = as.character(CODMUNRES),
                        cod_level = substr(resid,1,2)) %>%
  select(pais,year,mes,etnia,ocur,resid,cod_level,causa,sexo,edad,edad2) %>%
  filter(substr(causa,1,1) != 'P')
deathsBRA = bind_rows(data2 %>% filter(!is.na(edad)) %>% select(-edad2),
                      data2 %>% filter(is.na(edad) & substr(edad2,1,1) == 4) %>% 
                        mutate(edad = as.numeric(substr(edad2,2,3))) %>%
                        select(-edad2),
                      data2 %>% filter(is.na(edad) & substr(edad2,1,1) != 4 & edad2 != 999) %>% 
                        mutate(edad = 0) %>% select(-edad2),
                      data2 %>% filter(is.na(edad) & substr(edad2,1,1) != 4 & edad2 == 999) %>% 
                        mutate(edad = 999) %>% select(-edad2))
deathsBRA$edad[deathsBRA$edad %in% 0:9] = 1
deathsBRA$edad[deathsBRA$edad %in% 10:19] = 2 
deathsBRA$edad[deathsBRA$edad %in% 20:29] = 3 
deathsBRA$edad[deathsBRA$edad %in% 30:39] = 4 
deathsBRA$edad[deathsBRA$edad %in% 40:49] = 5 
deathsBRA$edad[deathsBRA$edad %in% 50:59] = 6 
deathsBRA$edad[deathsBRA$edad %in% 60:69] = 7
deathsBRA$edad[deathsBRA$edad %in% 70:79] = 8
deathsBRA$edad[deathsBRA$edad >= 80 & deathsBRA$edad != 999] = 9
deathsBRA = deathsBRA %>% mutate(edad = factor(edad,labels = c(edadesd80,'desc')),
                                 etnia = factor(etnia,labels = c('Blanco','Preta','Amarela','Parda','Indig')))
# POBLACION ####
nKxBRA = bind_rows(popUN(2015,'Brazil'),popUN(2016,'Brazil'),popUN(2017,'Brazil'),
                   popUN(2018,'Brazil'),popUN(2019,'Brazil'),popUN(2020,'Brazil'))

###################################################
load('./DefuncionesGeneral.RData')
###################################################
############## DATAS COMPLETAS ####################
###################################################
deaths = bind_rows(deathsCHL,deathsCOL,deathsPER,deathsECU,deathsMEX,
                   deathsGTM,deathsBRA)
deaths = bind_rows(deaths %>% filter(year %in% 2015:2019,mes %in% 1:12),
                   deaths %>% filter(year == 2020,mes %in% 1:month(fechafin)))
deathsEdadSexoMes = bind_rows(deaths %>% group_by(year,pais,edad,sexo,mes) %>% 
                                summarise(n = n()) %>% ungroup(),
                              CRIEdadSexoMes)
deathsCausaEdadSexo = bind_rows(deaths %>% group_by(year,pais,edad,sexo,causa) %>% 
                                  summarise(n = n()) %>% ungroup(),
                                CRICausaEdadSexo,ARGCausaEdadSexo %>% group_by(causa,edad,sexo,pais,year) %>%
                                  summarise(n = sum(n,na.rm = T)) %>% ungroup())
deathsCausaMes = bind_rows(BRACausaMes,CRICausaMes,
                           deaths %>% group_by(year,pais,causa,mes) %>% 
                             summarise(n = n()) %>% ungroup())
#poblaciones
nKx = bind_rows(nKxARG,nKxPER,nKxCRI,nKxMEX,nKxBRA,nKxGTM,nKxECU,
                nKxCHL,nKxCOL)
#censos
dataCenso = bind_rows(dataCensoARG,dataCensoECU,dataCensoBRA,dataCensoCHL,
                      dataCensoCOL,dataCensoPER,dataCensoMEX)
datalevel = bind_rows(levelARG,levelBRA,levelCHL,levelCOL,levelECU,
                      levelPER,levelCUB,levelMEX)
censos = dataCenso %>% group_by(pais,edad,sexo,cod_level,init,K0,KT,R) %>%
  summarise(year = 2020,pop = K0*exp(R*(year - init))) %>%
  ungroup() %>% select(-c(K0,KT,R,init)) %>% 
  bind_rows(popCUB) %>%
  group_by(pais,edad,sexo,year) %>% 
  mutate(tot = sum(pop,na.rm = T),prop = pop/tot) %>% 
  ungroup %>% left_join(nKx %>% rename(popUN = pop)) %>% 
  mutate(POP = prop*popUN) %>% select(-c(prop,popUN,pop,tot)) %>%
  inner_join(datalevel %>% mutate(level = toupper(level)))
############################################################
############################################################
wb = createWorkbook()
deaths2 = deaths %>% filter(mes <= month(fechafin)) %>% mutate(causaf = NA)
deaths2$causaf[substr(deaths2$causa,1,3) %in% c(paste0('X6',0:9),paste0('X7',0:9),paste0('X8',0:4),'Y87')] = 'Suicidio'
deaths2$causaf[substr(deaths2$causa,1,3) %in% c(paste0('X8',5:9),paste0('X9',0:9),paste0('Y0',0:9))] = 'Homicidios'
deaths2 = deaths2 %>% filter(causaf %in% c('Suicidio','Homicidios')) %>% 
  group_by(year,pais,cod_level,mes,edad,sexo,causaf) %>%
  summarise(n = n()) %>% ungroup() 
# PIRAMIDE #####
data2 = deathsCausaEdadSexo %>% mutate(causaf = NA)
data2$causaf[substr(data2$causa,1,3) %in% c(paste0('X6',0:9),paste0('X7',0:9),paste0('X8',0:4),'Y87')] = 'Suicidio'
data2$causaf[substr(data2$causa,1,3) %in% c(paste0('X8',5:9),paste0('X9',0:9),paste0('Y0',0:9))] = 'Homicidios'
data2 = data2 %>% group_by(year,pais,edad,sexo,causaf) %>%
  summarise(n = sum(n,na.rm = T)) %>% ungroup() 

data3 = data2 %>% 
  filter(causaf %in% c('Suicidio','Homicidios')) %>% 
  group_by(pais,year,causaf,sexo,edad) %>% summarise(n = sum(n,na.rm = T)) %>% 
  ungroup() %>% inner_join(nKx) %>% 
  mutate(POP = n/pop*100*mil, sexo = factor(sexo,labels = c('HOMBRES','MUJERES')), Label = sexo) %>% 
  mutate(year = factor(year))
addWorksheet(wb,'Piramide_HyS')
writeData(wb,'Piramide_HyS',data3)
ggplot(data = data3,mapping = aes(x = edad, y = POP, fill = Label,alpha = year,group = causaf,color = Label)) + 
  geom_bar(data = subset(data3, Label == "MUJERES"), stat = "identity", width = 1.0,position = 'dodge')  +
  geom_bar(data = subset(data3, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity",position = 'dodge', width = 1.0)  + 
  coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "",alpha = "") +
  scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
  scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
  theme_minimal() + 
  scale_y_continuous(n.breaks = 6,labels = function(x){x %>% abs}) + 
  #facet_grid(pais~causaf,scales = 'free_x') +
  facet_grid(causaf~pais,scales = 'free_x') +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 16), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold")
  )
ggsave('./Piramide_HyS.jpeg',height = 25,width = 50,units = 'cm')
### SUICIDIOS Y HOMICIDIOS ###
data2 = deathsCausaMes %>% mutate(causaf = NA)
data2$causaf[substr(data2$causa,1,3) %in% c(paste0('X6',0:9),paste0('X7',0:9),paste0('X8',0:4),'Y87')] = 'Suicidio'
data2$causaf[substr(data2$causa,1,3) %in% c(paste0('X8',5:9),paste0('X9',0:9),paste0('Y0',0:9))] = 'Homicidios'
data2 = data2 %>% filter(causaf %in% c('Suicidio','Homicidios')) %>% 
  group_by(year,pais,mes,causaf) %>%
  summarise(n = sum(n,na.rm = T)) %>% ungroup() 
######
# MENSUAL GLOBAL SUCICIDIO ####
data3 = data2 %>% 
  filter(causaf %in% c('Suicidio')) %>% 
  group_by(pais,year,mes) %>% summarise(n = sum(n,na.rm = T)) %>% 
  ungroup() %>% inner_join(nKx %>% group_by(pais,year) %>% summarise(pop = sum(pop))) %>% 
  mutate(POP = n/pop*100*mil,year = factor(year))
addWorksheet(wb,'Mensual_S')
writeData(wb,'Mensual_S',data3)
ggplot(data = data3,mapping = aes(x = mes, y = POP, color = year)) + 
  geom_line(size = 1) + geom_point() +
  labs(y = "TEM", x = "Mes", color = "Año") +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~pais,scales = 'free_y') +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 16), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold")
  )
ggsave('./Mensual_S.jpeg',height = 25,width = 50,units = 'cm')
# MENSUAL GLOBAL HOMICIDOS ####
data3 = data2 %>% 
  filter(causaf %in% c('Homicidios')) %>% 
  group_by(pais,year,mes) %>% summarise(n = sum(n,na.rm = T)) %>% 
  ungroup() %>% inner_join(nKx %>% group_by(pais,year) %>% summarise(pop = sum(pop))) %>% 
  mutate(POP = n/pop*100*mil,year = factor(year))
addWorksheet(wb,'Mensual_H')
writeData(wb,'Mensual_H',data3)
ggplot(data = data3,mapping = aes(x = mes, y = POP, color = year)) + 
  geom_line(size = 1) + geom_point() +
  labs(y = "TEM", x = "Mes", color = "Año") +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~pais,scales = 'free_y') +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 16), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold")
  )
ggsave('./Mensual_H.jpeg',height = 25,width = 50,units = 'cm')
######
# MENSUAL EDAD SEXO SUICIDIOS ####
data3 = deaths2 %>% 
  filter(causaf %in% c('Suicidio')) %>% 
  group_by(pais,year,mes,edad,sexo) %>% summarise(n = sum(n,na.rm = T)) %>% 
  ungroup() %>% inner_join(nKx) %>% 
  mutate(POP = n/pop*100*mil,year = factor(year))
addWorksheet(wb,'Mensualedadsexo_S')
writeData(wb,'Mensualedadsexo_S',data3)
# 1 Brazil   
# 2 Chile    
# 3 Colombia 
# 4 Ecuador  
# 5 Guatemala
# 6 Mexico   
# 7 Peru 
ggplot(data = data3 %>% filter(pais == 'Mexico'),mapping = aes(x = mes, y = POP, color = year)) + 
  geom_line(size = 1) + geom_point() +
  labs(y = "TEM", x = "Mes", color = "Año") +
  facet_grid(sexo ~ edad,scales = 'free_y') +
  scale_x_continuous(breaks = 1:12) +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 16), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold")
  )
ggsave('./Mensual_S_MEX.jpeg',height = 25,width = 50,units = 'cm')
# MENSUAL EDAD SEXO HOMICIDOS ####
data3 = deaths2 %>% 
  filter(causaf %in% c('Homicidios')) %>% 
  group_by(pais,year,mes,edad,sexo) %>% summarise(n = sum(n,na.rm = T)) %>% 
  ungroup() %>% inner_join(nKx) %>% 
  mutate(POP = n/pop*100*mil,year = factor(year))
addWorksheet(wb,'Mensualedadsexo_H')
writeData(wb,'Mensualedadsexo_H',data3)
ggplot(data = data3 %>% filter(pais == 'Peru'),mapping = aes(x = mes, y = POP, color = year)) + 
  geom_line(size = 1) + geom_point() +
  labs(y = "TEM", x = "Mes", color = "Año") +
  facet_grid(sexo ~ edad,scales = 'free_y') +
  scale_x_continuous(breaks = 1:12) +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 16), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold")
  )
ggsave('./Mensual_H_PER.jpeg',height = 25,width = 50,units = 'cm')

saveWorkbook(wb,'CausaViolenta.xlsx',overwrite = T)
