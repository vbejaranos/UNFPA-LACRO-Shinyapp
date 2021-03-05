rm(list = ls())
library(tidyverse)
theme_set(theme_minimal())
library(lubridate)
library(readxl)
library(sf)
library(ggthemes)
library(openxlsx)
library(RColorBrewer)
############## Colores, fechas y edades ############
Pal.1 = Palb.1 = brewer.pal(n = 8, name = "RdPu")
Pal.2 = Palb.2 = brewer.pal(n = 8, name = "PuBu")
Palete = c(Pal.2[3],Pal.2[7],Pal.1[7],Pal.1[3])
Paleteb = c(Palb.2[8],Palb.2[2],Palb.1[8],Palb.1[2])
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
edadesq = c(" 0-4"," 5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","M100")
meses = tibble(mes = c('Enero','Febrero','Marzo','Abril','Mayo',
                       'Junio','Julio','Agosto','Setiembre','Octubre',
                       'Noviembre','Diciembre'),num = 1:12)
replace_na_with_last = function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a) + 1]]
}
fechafin = ymd('2020/12/31')
###################################################
###### UN poblaciones ####
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
# COVID19 ####
covidARG = read.csv('./Data/Argentina/Covid19Casos5.csv',sep = ',',encoding = 'UTF-8')
covidARG$residencia_provincia_id[covidARG$residencia_provincia_id == 99] =  
  covidARG$carga_provincia_id[covidARG$residencia_provincia_id == 99]
covidARG = covidARG %>% mutate(sexo = factor(sexo,labels = c('WOMEN','MEN','IND')),
                               edad = edad,edadtipo = edad_años_meses,pais = 'Argentina',
                               cod_level = formatC(residencia_provincia_id,width = 2,flag = '0')) %>%
  select(pais,cod_level,sexo,edad,edadtipo,fecha_apertura,fecha_diagnostico,fecha_inicio_sintomas,fecha_fallecimiento,clasificacion_resumen,fallecido)
covidARG$edad[covidARG$edadtipo != 'Años'] = 0
covidARG$edad[covidARG$edad %in% 0:9] = 1
covidARG$edad[covidARG$edad %in% 10:19] = 2 
covidARG$edad[covidARG$edad %in% 20:29] = 3 
covidARG$edad[covidARG$edad %in% 30:39] = 4 
covidARG$edad[covidARG$edad %in% 40:49] = 5 
covidARG$edad[covidARG$edad %in% 50:59] = 6 
covidARG$edad[covidARG$edad %in% 60:69] = 7
covidARG$edad[covidARG$edad %in% 70:79] = 8
covidARG$edad[covidARG$edad >= 80] = 9
casosARG = covidARG %>% filter(clasificacion_resumen != 'Descartado') %>% 
  mutate(fecha = ymd(fecha_apertura),fis = ymd(fecha_inicio_sintomas),edad = factor(edad,labels = edadesd80)) %>%
  select(pais,cod_level,fecha,edad,sexo)
covidARG = covidARG %>% filter(fallecido == 'SI') %>%
  mutate(fecha = ymd(fecha_fallecimiento),edad = factor(edad,labels = edadesd80)) %>%
  select(pais,cod_level,fecha,edad,sexo)
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
#deathsECU$etnia[deathsECU$etnia %in% c(8,9)] = 8
deathsECU = deathsECU %>% mutate(year = as.numeric(anio_fall),
                                 mes = as.numeric(mes_fall),causa = Causa,
                                 sexo = factor(sexo,labels = c('MEN','WOMEN')),
                                 edad = factor(edad,labels = c(edadesd80,'desc')),pais = 'Ecuador',
                                 etnia = factor(etnia,labels = c('Indig','Afro','Mont','Mest','Blan','Otro','desc')),
                                 cod_level = as.character(cod_level)) %>%
  select(pais,cod_level,year,mes,sexo,causa,edad,etnia)
rm(base2015,base2016,base2017,base2018,base2019,base2020)

# COVID19 ####
covidECU = NULL
covidECUMes = tibble(year = 2020,pais = 'Ecuador',mes = 1:12,
                     covid_deaths = cumminus(c(0,0,79,900+1453,3358+2154,4527+3071,5702+3470,6556+3741,11355,12670,13461,14034)))
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
deathsPER$EDAD[is.na(deathsPER$EDAD)] = 10
deathsPER$EDAD[deathsPER$`TIEMPO EDAD` %in% c('IGNORADO','SIN REGISTRO')] = 10
deathsPER = deathsPER %>% mutate(year = as.numeric(AÑO),mes = as.numeric(MES),
                                 causa = `CAUSA A (CIE-X)`,
                                 sexo = factor(SEXO,labels = c('WOMEN','IND','MEN','IND')),
                                 edad = factor(EDAD,labels = c(edadesd80,'desc')),pais = 'Peru',
                                 level = `DEPARTAMENTO DOMICILIO`) %>%
  select(pais,year,mes,level,sexo,causa,edad)
deathsPER = deathsPER %>% full_join(levelPER)
# COVID19 ####
casosPER = read.csv('./Data/Peru/positivos_covid5.csv',sep = ';')
casosPER$DEPARTAMENTO[casosPER$DEPARTAMENTO == 'LIMA REGION'] = 'LIMA'
casosPER$EDAD[casosPER$EDAD %in% 0:9] = 1
casosPER$EDAD[casosPER$EDAD %in% 10:19] = 2 
casosPER$EDAD[casosPER$EDAD %in% 20:29] = 3 
casosPER$EDAD[casosPER$EDAD %in% 30:39] = 4 
casosPER$EDAD[casosPER$EDAD %in% 40:49] = 5 
casosPER$EDAD[casosPER$EDAD %in% 50:59] = 6 
casosPER$EDAD[casosPER$EDAD %in% 60:69] = 7
casosPER$EDAD[casosPER$EDAD %in% 70:79] = 8
casosPER$EDAD[casosPER$EDAD >= 80] = 9
casosPER = casosPER %>% mutate(pais = 'Peru',level = DEPARTAMENTO,
                               sexo = factor(SEXO,labels = c('WOMEN','MEN')),
                               edad = factor(EDAD,labels = edadesd80),
                               fecha = ymd(FECHA_RESULTADO)) %>%
  left_join(levelPER) %>%
  select(pais,cod_level,fecha,edad,sexo)
covidPER = read.csv('./Data/Peru/fallecidos_covid5.csv',sep = ';')
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 0:9] = 1
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 10:19] = 2 
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 20:29] = 3 
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 30:39] = 4 
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 40:49] = 5 
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 50:59] = 6 
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 60:69] = 7
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA %in% 70:79] = 8
covidPER$EDAD_DECLARADA[covidPER$EDAD_DECLARADA >= 80] = 9
covidPER = covidPER %>% mutate(pais = 'Peru',sexo = factor(SEXO,labels = c('WOMEN','MEN','WOMEN','MEN')),
                               edad = factor(EDAD_DECLARADA,labels = edadesd80),
                               fecha = ymd(FECHA_FALLECIMIENTO), level = DEPARTAMENTO) %>%
  left_join(levelPER) %>%
  select(pais,cod_level,fecha,edad,sexo)
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
dataanio = function(data){
  data$SEXO = factor(data$SEXO,levels = c(1,2,3),labels = c("MEN","WOMEN","IND"))
  data$GRU_ED1 = as.numeric(data$GRU_ED1)
  data$GRU_ED1[data$GRU_ED1 %in% 0:7] = 8
  data$GRU_ED1[data$GRU_ED1 %in% 24:28] = 24
  data$edad = factor(data$GRU_ED1,labels = c(edadesq[1:16],'80+','desc'))
  data$etnia = factor(data$IDPERTET)
  levels(data$etnia) = c('Indig','Rom','Afro','Afro','Afro','Otro','desc')
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
                      dataanio(data2018))
rm(data2015,data2016,data2017,data2018)
exCOL = read_xlsx('./Data/Colombia/anexos-defunciones-covid-nal-2020-02mar-01nov.xlsx',
                  sheet = 'Tabla seguimiento mortalidad',skip = 9)
exCOLlevel = read_xlsx('./Data/Colombia/anexos-defunciones-covid-dept-sexo-2020-02mar-01nov.xlsx',
                       sheet = 'Tabla seguimiento mortalidad',skip = 10,n_max = 10752)

# COVID19 ####
covidCOL = read.csv('./Data/Colombia/Casos_positivos_de_COVID-19_en_Colombia5.csv',encoding = 'UTF-8')
covidCOL$Edad = as.numeric(covidCOL$Edad)
covidCOL$Edad[covidCOL$Unidad.de.medida.de.edad != '1'] = 0
covidCOL$Edad[covidCOL$Edad %in% 0:9] = 1
covidCOL$Edad[covidCOL$Edad %in% 10:19] = 2 
covidCOL$Edad[covidCOL$Edad %in% 20:29] = 3 
covidCOL$Edad[covidCOL$Edad %in% 30:39] = 4 
covidCOL$Edad[covidCOL$Edad %in% 40:49] = 5 
covidCOL$Edad[covidCOL$Edad %in% 50:59] = 6 
covidCOL$Edad[covidCOL$Edad %in% 60:69] = 7
covidCOL$Edad[covidCOL$Edad %in% 70:79] = 8
covidCOL$Edad[covidCOL$Edad >= 80] = 9
covidCOL$etnia = factor(covidCOL$Pertenencia.étnica,labels = c('Indig','Rom','Afro','Afro','Otro'))
#1-Indígena 2-ROM 3-Raizal 4-Palenquero 5-Negro 6-Otro
covidCOL$Sexo = toupper(covidCOL$Sexo)
casosCOL = covidCOL %>% 
  mutate(fecha = dmy_hms(Fecha.de.notificación),
         fis = dmy_hms(Fecha.de.inicio.de.síntomas),
         sexo = factor(Sexo,labels = c('WOMEN','MEN')),
         edad = factor(Edad,labels = edadesd80),
         etnia = etnia,pais = 'Colombia',
         cod_level = substr(formatC(Código.DIVIPOLA.municipio,flag = '0',width = 5),1,2)) %>% 
  select(pais,fecha,fis,edad,sexo,etnia,cod_level)
covidCOL = covidCOL %>% filter(Estado == 'Fallecido') %>% 
  mutate(fecha = dmy_hms(Fecha.de.muerte),
         sexo = factor(Sexo,labels = c('WOMEN','MEN')),
         edad = factor(Edad,labels = edadesd80),
         etnia = etnia,pais = 'Colombia',
         cod_level = substr(formatC(Código.DIVIPOLA.municipio,flag = '0',width = 5),1,2)) %>% 
  select(pais,fecha,edad,sexo,etnia,cod_level)
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
# COVID19 ####
covidCRI = read.csv('./Data/CostaRica/12_18_CSV_GENERAL.csv',header = T,sep = ';')
covidCRI = covidCRI %>% mutate(fecha = dmy(FECHA)) %>%
  group_by(fecha) %>% summarise(n = sum(nue_falleci,na.rm = T),
                                casos = sum(nue_posi,na.rm = T),
                                pais = 'Costa Rica') %>% ungroup() %>%
  select(pais,fecha,casos,n)
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
         edad = factor(EDAD_AGRU,labels = c(edadesd80,'desc')),
         pais = 'Mexico',cod_level = as.character(ENT_RESID),
         len = as.numeric(LENGUA),
         etnia = ifelse(len == 1,'L.Indig',ifelse(len == 2,'Otro',
                                                  ifelse(len == 8, 'N/A',
                                                         ifelse(len == 9,'desc','desc'))))) %>%
  select(pais,year,cod_level,mes,sexo,causa,edad,etnia)
rm(base2015,base2016,base2017,base2018)

# COVID19 ####
covidMEX = read.csv('./Data/Mexico/210116COVID19MEXICO.csv',encoding = 'UTF-8')
covidMEX$EDAD[covidMEX$EDAD %in% 0:9] = 1
covidMEX$EDAD[covidMEX$EDAD %in% 10:19] = 2 
covidMEX$EDAD[covidMEX$EDAD %in% 20:29] = 3 
covidMEX$EDAD[covidMEX$EDAD %in% 30:39] = 4 
covidMEX$EDAD[covidMEX$EDAD %in% 40:49] = 5 
covidMEX$EDAD[covidMEX$EDAD %in% 50:59] = 6 
covidMEX$EDAD[covidMEX$EDAD %in% 60:69] = 7
covidMEX$EDAD[covidMEX$EDAD %in% 70:79] = 8
covidMEX$EDAD[covidMEX$EDAD >= 80] = 9
covidMEX$etnia = ifelse(covidMEX$INDIGENA == 1,'Indig','Otro')
casosMEX = covidMEX %>% filter(CLASIFICACION_FINAL %in% c(1,2,3,6)) %>%
  mutate(pais = 'Mexico',sexo = factor(SEXO,labels = c('WOMEN','MEN')),
         edad = factor(EDAD,labels = edadesd80),fecha = ymd(FECHA_INGRESO),
         cod_level = formatC(ENTIDAD_RES,flag = '0',width = 2)) %>%
  select(pais,cod_level,fecha,edad,sexo,etnia)
covidMEX = covidMEX %>% filter(CLASIFICACION_FINAL %in% c(1,2,3,6)) %>% 
  filter(FECHA_DEF != '9999-99-99') %>% 
  mutate(pais = 'Mexico',sexo = factor(SEXO,labels = c('WOMEN','MEN')),
         edad = factor(EDAD,labels = edadesd80),fecha = ymd(FECHA_DEF),
         cod_level = formatC(ENTIDAD_RES,flag = '0',width = 2)) %>%
  select(pais,cod_level,fecha,edad,sexo,etnia)
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
  #base$etnia[base$etnia == 5] = 9
  base = base %>% mutate(edad = factor(Edad,labels = c(edadesd80,'desc')),
                         sexo = factor(sexo,labels = c('MEN','WOMEN')),
                         etnia = factor(etnia,labels = c('Maya','Gar','Xin','Mest','Otro','desc')),
                         pais = 'Guatemala') %>% 
    select(pais,year,cod_level,mes,causa,edad,sexo,etnia)
  return(base)
}
#etnia 1-Maya 2-Garifuna 3-Xinca 4-Mestizo,Ladino 5-Otro 9-Ignorado
deathsGTM = bind_rows(GTM1(2015),GTM1(2016),GTM1(2017),GTM1(2018))
# COVID19 ####
casosGTM = read.csv('./Data/Guatemala/Confirmados por municipio, fecha de emisión de resultado del 2020-02-13 al 2021-01-16.csv')
casosGTM = casosGTM %>%
  pivot_longer(contains('2020'),names_to = 'fecha',values_to = 'casos') %>%
  mutate(fecha = make_date(substr(fecha,2,5),substr(fecha,7,8),substr(fecha,10,11))) %>% 
  group_by(fecha) %>% 
  summarise(casos = sum(as.numeric(casos),na.rm = T),
            pais = 'Guatemala') %>% ungroup() %>% 
  select(pais,fecha,casos)
covidGTM = read.csv('./Data/Guatemala/Fallecidos por municipio, fecha de fallecimiento del 2020-02-13 al 2021-01-16.csv')
covidGTM = covidGTM %>%
  pivot_longer(contains('2020'),names_to = 'fecha',values_to = 'n') %>%
  mutate(fecha = make_date(substr(fecha,2,5),substr(fecha,7,8),substr(fecha,10,11))) %>% 
  group_by(fecha) %>% 
  summarise(n = sum(as.numeric(n),na.rm = T),
            pais = 'Guatemala') %>% ungroup() %>% 
  select(pais,fecha,n) %>% full_join(casosGTM)
rm(casosGTM)
# POBLACION ####
nKxGTM = bind_rows(popUN(2015,'Guatemala'),popUN(2016,'Guatemala'),popUN(2017,'Guatemala'),
                   popUN(2018,'Guatemala'),popUN(2019,'Guatemala'),popUN(2020,'Guatemala'))

###################################################
################## JAMAICA ########################
###################################################
# COVID19 ####
covidJAM = read_xlsx('./Data/Jamaica/Jamaica - Covid Deaths.xlsx')
covidJAM$Age[covidJAM$Age %in% 0:9] = 1
covidJAM$Age[covidJAM$Age %in% 10:19] = 2 
covidJAM$Age[covidJAM$Age %in% 20:29] = 3 
covidJAM$Age[covidJAM$Age %in% 30:39] = 4 
covidJAM$Age[covidJAM$Age %in% 40:49] = 5 
covidJAM$Age[covidJAM$Age %in% 50:59] = 6 
covidJAM$Age[covidJAM$Age %in% 60:69] = 7
covidJAM$Age[covidJAM$Age %in% 70:79] = 8
covidJAM$Age[covidJAM$Age >= 80] = 9
covidJAM = covidJAM %>% select(Sex,Age,`Date of report`) %>%
  mutate(sexo = factor(Sex,labels = c('WOMEN','MEN')),
         edad = factor(Age,levels = 1:9,labels = edadesd80),
         fecha = ymd(`Date of report`),
         pais = 'Jamaica') %>%
  select(pais,fecha,sexo,edad)
# POBLACION ####
nKxJAM = popUN(2020,'Jamaica')

###################################################
##################### CUBA ########################
###################################################
# COVID19 ####
casosCUB = read.csv('./Data/Cuba/covid19-casos5.csv',encoding = 'UTF-8')
casosCUB$edad[casosCUB$edad %in% 0:9] = 1
casosCUB$edad[casosCUB$edad %in% 10:19] = 2 
casosCUB$edad[casosCUB$edad %in% 20:29] = 3 
casosCUB$edad[casosCUB$edad %in% 30:39] = 4 
casosCUB$edad[casosCUB$edad %in% 40:49] = 5 
casosCUB$edad[casosCUB$edad %in% 50:59] = 6 
casosCUB$edad[casosCUB$edad %in% 60:69] = 7
casosCUB$edad[casosCUB$edad %in% 70:79] = 8
casosCUB$edad[casosCUB$edad >= 80] = 9
casosCUB = casosCUB %>% mutate(fecha = ymd(fecha_confirmacion),
                               pais = 'Cuba',
                               sexo = factor(sexo,labels = c('IND','MEN','WOMEN')),
                               edad = factor(edad,levels = 1:9,labels = edadesd80)) %>% 
  select(pais,fecha,edad,sexo)
covidCUB = jsonlite::fromJSON('https://covid19cubadata.github.io/data/covid19-fallecidos.json')
covidCUB = covidCUB$casos$dias
covidCUB = tibble(fecha = sapply(covidCUB,function(x){x$fecha}),
                  data = lapply(covidCUB,function(x){x$fallecidos})) %>% unnest()
covidCUB$edad[covidCUB$edad %in% 0:9] = 1
covidCUB$edad[covidCUB$edad %in% 10:19] = 2 
covidCUB$edad[covidCUB$edad %in% 20:29] = 3 
covidCUB$edad[covidCUB$edad %in% 30:39] = 4 
covidCUB$edad[covidCUB$edad %in% 40:49] = 5 
covidCUB$edad[covidCUB$edad %in% 50:59] = 6 
covidCUB$edad[covidCUB$edad %in% 60:69] = 7
covidCUB$edad[covidCUB$edad %in% 70:79] = 8
covidCUB$edad[covidCUB$edad >= 80] = 9
covidCUB = covidCUB %>% mutate(fecha = ymd(fecha),
                               pais = 'Cuba',
                               edad = factor(edad,levels = 1:9,labels = edadesd80),
                               sexo = factor(sexo,labels = c('MEN','WOMEN')),
                               cod_level = as.character(dpacode_provincia_deteccion)) %>%
  select(pais,fecha,edad,sexo,cod_level)
# POBLACION ####
nKxCUB = popUN(2020,'Cuba')

###################################################
################# VENEZUELA #######################
###################################################
# COVID19 ####
covidVEN = jsonlite::fromJSON('https://covid19.patria.org.ve/api/v1/timeline')
covidVEN$Confirmed.New = covidVEN$Confirmed$New
covidVEN$Deaths.New = covidVEN$Deaths$New
covidVEN = covidVEN %>% mutate(fecha = ymd(Date)) %>%
  group_by(fecha) %>% summarise(casos = sum(Confirmed.New,na.rm = T),
                                n = sum(Deaths.New,na.rm = T),
                                pais = 'Venezuela') %>% ungroup() %>%
  select(pais,fecha,casos,n)
# POBLACION ####
nKxVEN = popUN(2020,'Venezuela (Bolivarian Republic of)')  %>% mutate(pais = 'Venezuela')

###################################################
################### URUGUAY ########################
###################################################
# COVID19 ####
covidURY = read.csv('./Data/Uruguay/estadisticas-covid-195.csv')
covidURY = covidURY %>% mutate(fecha = ymd(Fecha),Indicador = gsub('[[:blank:]]','',Indicador)) %>% 
  filter(Indicador %in% c('Casosnuevos','Fallecidos'),Territorio == 'Todo el país') %>%
  select(fecha,Indicador,Valor) %>% 
  pivot_wider(id_cols = fecha,names_from = Indicador,values_from = Valor) %>%
  rename(casos = `Casosnuevos`,n = Fallecidos) %>% unnest(cols = c('casos','n')) %>%
  group_by(fecha) %>% summarise(n = sum(n,na.rm = T),casos = sum(casos,na.rm = T)) %>%
  ungroup() %>% mutate(n = cumminus(n),pais = 'Uruguay') %>% arrange(fecha)
# POBLACION ####
nKxURY = popUN(2020,'Uruguay')

###################################################
################### PANAMA ########################
###################################################
# COVID19 ####
covidPAN = read.csv('./Data/Panama/CASOS_ACUMULATIVOS__PU_5.csv')
covidPAN = covidPAN %>% mutate(fecha = ymd(substr(FECHA,1,10)),casos = NUEVOS,n = FALLECIDO) %>%
  group_by(fecha) %>% summarise(n = sum(n,na.rm = T),casos = sum(casos,na.rm = T)) %>%
  ungroup() %>% mutate(n = cumminus(n),pais = 'Panama') %>% arrange(fecha)
# POBLACION ####
nKxPAN = popUN(2020,'Panama')
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
covidCHL2 = deathsCHL %>% filter(substr(causa,1,3) == 'U07') %>% select(-causa)
deathsCHL = bind_rows(deathsCHL2015 %>% filter(year(fecha) == 2015),
                      deathsCHL) %>% 
  mutate(year = year(fecha),mes = month(fecha)) %>% select(-fecha)
rm(deathsCHL2015)
# COVID19 ####
covidCHL = read_csv('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv')
covidCHL = covidCHL %>% mutate(fecha = ymd(Fecha)) %>% group_by(fecha) %>% 
  summarise(casos = sum(`Casos nuevos totales`,na.rm = T),
            fallecidos = sum(Fallecidos)) %>% ungroup() %>%
  mutate(n = cumminus(fallecidos),pais = 'Chile') %>% 
  select(pais,fecha,casos,n)
# POBLACION ####
nKxCHL = bind_rows(popUN(2015,'Chile'),popUN(2016,'Chile'),popUN(2017,'Chile'),
                   popUN(2018,'Chile'),popUN(2019,'Chile'),popUN(2020,'Chile'))


###################################################
################### BRASIL ########################
###################################################
# DEFUNCIONES TOTALES ####
data = bind_rows(read.csv('./Data/Brasil/obitos-2019.csv') %>%
                   mutate(year = 2019),
                 read.csv('./Data/Brasil/obitos-2020.csv') %>%
                   mutate(year = 2020))
data = data %>% mutate(edad = factor(faixa_etaria),
                       pais = 'Brasil',sexo = factor(sexo,labels = c('IND','WOMEN','IND','MEN')),
                       n = total) %>%
  select(year,edad,pais,sexo,n)
levels(data$edad) = c(' 0-9','80+',edadesd80[2:9],'80+','desc')
BRAEdadSexo = data %>% mutate(edad = factor(edad,levels = edadesd80),
                              sexo = factor(sexo,levels = c('MEN','WOMEN','IND'))) %>% 
  group_by(pais,year,edad,sexo) %>%
  summarise(n = sum(n,na.rm = T)) %>% ungroup()
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
BRA4 = function(anio) {
  base = read.csv(paste0('./Data/Brasil/CausaEdad',anio,'.csv'),sep = ';',header = T,skip = 3,nrows = 1357)
  colnames(base) = c('causa','m1',' 1-4',' 5-9','10-14','15-19',edadesd80[3:9],'desc','Total')
  base = base %>% 
    mutate_at(colnames(base)[-1],as.numeric) %>%
    select(-Total) %>% filter(causa != 'Total') %>% 
    mutate(causa = substr(causa,1,3)) %>%
    filter(substr(causa,1,1) %in% LETTERS) %>%
    pivot_longer(-causa,names_to = 'edad',values_to = 'n')
  base$edad[base$edad %in% c('m1',' 1-4', ' 5-9')] = ' 0-9'
  base$edad[base$edad %in% c('10-14', '15-19')] = '10-19'
  base = base %>% group_by(causa,edad) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Brazil')
  return(base)
}
BRACausaEdad = bind_rows(BRA4(2015),BRA4(2016),BRA4(2017),BRA4(2018))
ufs = tibble(uf = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB',
                    'PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS',
                    'MT','GO','DF'),
             cod_level = as.character(c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,
                                        33,35,41,42,43,50,51,52,53)))
BRA5 = function(anio) {
  base = read.csv(paste0('./Data/Brasil/CausaRegion',anio,'.csv'),sep = ';',skip = 3,nrows = 1357,header = T)
  colnames(base)[1] = 'causa'
  base = base %>% mutate_at(colnames(base)[-1],as.numeric) %>%
    select(-Total) %>% filter(causa != 'Total') %>% 
    mutate(causa = substr(causa,1,3)) %>% 
    filter(substr(causa,1,1) %in% LETTERS) %>%
    pivot_longer(-causa,names_to = 'uf',values_to = 'n') %>% full_join(ufs) %>% select(-uf)
  base = base %>% group_by(causa,cod_level) %>% summarise(n = sum(n,na.rm = T)) %>% ungroup() %>%
    mutate(year = anio,pais = 'Brazil')
  return(base)
}
BRACausaRegion = bind_rows(BRA5(2015),BRA5(2016),BRA5(2017),BRA5(2018))

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
deathsBRA$etnia[is.na(deathsBRA$etnia)] = 6
deathsBRA = deathsBRA %>% mutate(edad = factor(edad,labels = c(edadesd80,'desc')),
                                 etnia = factor(etnia,labels = c('Blanco','Preta','Amarela','Parda','Indig','desc')))
# COVID19 ####
# etnia 1.blanca 2.preta 3.Amarela 4.Parda 5.Indigena 9.Ignorado
iragBRA = read.csv('./Data/Brasil/INFLUD-11-01-2021.csv',sep = ';')
iragBRA = iragBRA %>% filter(CLASSI_FIN == 5) %>% 
  mutate(fecha = dmy(DT_NOTIFIC), 
         fechafall = dmy(DT_EVOLUCA),fis = dmy(DT_SIN_PRI),
         fallece = EVOLUCAO,
         sexo = factor(CS_SEXO,labels = c('WOMEN','IND','MEN')),
         edad = NU_IDADE_N,tipoedad = TP_IDADE,
         etnia = CS_RACA,uf = as.character(SG_UF)) %>%
  inner_join(ufs) %>%
  select(fecha,fechafall,fis,fallece,sexo,edad,tipoedad,etnia,cod_level)
iragBRA$edad[iragBRA$tipoedad != 3] = 0
iragBRA$edad[iragBRA$edad %in% 0:9] = 1
iragBRA$edad[iragBRA$edad %in% 10:19] = 2
iragBRA$edad[iragBRA$edad %in% 20:29] = 3
iragBRA$edad[iragBRA$edad %in% 30:39] = 4
iragBRA$edad[iragBRA$edad %in% 40:49] = 5
iragBRA$edad[iragBRA$edad %in% 50:59] = 6
iragBRA$edad[iragBRA$edad %in% 60:69] = 7
iragBRA$edad[iragBRA$edad %in% 70:79] = 8
iragBRA$edad[iragBRA$edad >= 80] = 9
iragBRA$etnia[is.na(iragBRA$etnia)] = 9
iragBRA = iragBRA %>% mutate(edad = factor(edad,labels = edadesd80),
                             etnia = factor(etnia,labels = c('Blanco','Preta','Amarela','Parda','Indig','Otros')),
                             pais = 'Brazil')
casosBRA = iragBRA %>% select(pais,fecha,fis,sexo,edad,etnia,cod_level)
covidBRA = iragBRA %>% filter(fallece == 2) %>% mutate(fecha = fechafall) %>%
  select(pais,fecha,sexo,edad,etnia,cod_level)
# rm(iragBRA)
# POBLACION ####
nKxBRA = bind_rows(popUN(2015,'Brazil'),popUN(2016,'Brazil'),popUN(2017,'Brazil'),
                   popUN(2018,'Brazil'),popUN(2019,'Brazil'),popUN(2020,'Brazil'))

###################################################
load('ActualizacionDatos.RData')
###################################################
################## DATAS COMPLETAS ################
baja = c('Jamaica','Cuba','Uruguay','Costa Rica','Venezuela')
media = c('Colombia','Argentina','Guatemala','Panama')
alta = c('Brazil','Mexico','Peru','Chile')
#microdata
#
casos = bind_rows(casosPER,casosARG,casosMEX,casosCUB,casosBRA,casosCOL) %>% group_by(pais) %>% 
  arrange(fecha) %>% ungroup() %>% filter(fecha <= fechafin) %>%
  mutate(incidencia = case_when(pais %in% baja ~ 'Bajo',pais %in% media ~ 'Medio',pais %in% alta ~ 'Alto'),
         incidencia = factor(incidencia,levels = c('Alto','Medio','Bajo')))
#
covid = bind_rows(covidPER,covidARG,covidMEX,covidCUB,covidBRA,covidJAM,covidCOL,covidCHL2) %>% group_by(pais) %>% 
  arrange(fecha) %>% ungroup() %>% filter(fecha <= fechafin) %>%
  mutate(incidencia = case_when(pais %in% baja ~ 'Bajo',pais %in% media ~ 'Medio',pais %in% alta ~ 'Alto'),
         incidencia = factor(incidencia,levels = c('Alto','Medio','Bajo')))
deaths = bind_rows(deathsCHL,deathsCOL,deathsPER,deathsECU,deathsMEX,
                   deathsGTM,deathsBRA) %>%
  mutate(causa = toupper(causa))
deaths = bind_rows(deaths %>% filter(year %in% 2015:2019,mes %in% 1:12),
                   deaths %>% filter(year == 2020,mes %in% 1:month(fechafin)))
#tabulado nacional total
covidtab = bind_rows(covidCRI,covidGTM,covidCHL,covidVEN,covidURY,covidPAN,
                     covid %>% filter(pais != 'Chile') %>% group_by(pais,fecha) %>%
                       summarise(n = n()) %>% ungroup() %>%
                       full_join(casos %>% group_by(pais,fecha) %>%
                                   summarise(casos = n()) %>% ungroup()) %>% 
                       group_by(pais,fecha) %>%
                       summarise(n = sum(n,na.rm = T),casos = sum(casos,na.rm = T)) %>% ungroup()) %>% 
  group_by(pais) %>% arrange(fecha) %>% ungroup() %>% filter(fecha <= fechafin) %>%
  mutate(incidencia = case_when(pais %in% baja ~ 'Bajo',pais %in% media ~ 'Medio',pais %in% alta ~ 'Alto'),
         incidencia = factor(incidencia,levels = c('Alto','Medio','Bajo')))

deathsEdadSexoMes = bind_rows(deaths %>% group_by(year,pais,edad,sexo,mes) %>% 
                                summarise(n = n()) %>% ungroup(),
                              CRIEdadSexoMes)
deathsCausaEdadSexo = bind_rows(deaths %>% group_by(year,pais,edad,sexo,causa) %>% 
                                  summarise(n = n()) %>% ungroup(),
                                CRICausaEdadSexo,ARGCausaEdadSexo)
deathsCausaMes = bind_rows(BRACausaMes,CRICausaMes,
                           deaths %>% group_by(year,pais,causa,mes) %>% 
                             summarise(n = n()) %>% ungroup())
#poblaciones
nKx = bind_rows(nKxARG,nKxPER,nKxCRI,nKxMEX,nKxBRA,nKxGTM,nKxECU,
                nKxJAM,nKxCUB,nKxVEN,nKxCHL,nKxURY,nKxPAN,nKxCOL)
#censos
dataCenso = bind_rows(dataCensoARG,dataCensoBRA,dataCensoCHL,dataCensoCOL,
                      dataCensoPER,dataCensoMEX)
datalevel = bind_rows(levelARG,levelBRA,levelCHL,levelCOL,
                      levelPER,levelCUB,levelMEX) %>%
  mutate(level = toupper(level))
censos = dataCenso %>% group_by(pais,edad,sexo,cod_level,init,K0,KT,R) %>%
  summarise(year = 2020,pop = K0*exp(R*(year - init))) %>%
  ungroup() %>% select(-c(K0,KT,R,init)) %>% 
  bind_rows(popCUB) %>%
  group_by(pais,edad,sexo,year) %>% 
  mutate(tot = sum(pop,na.rm = T),prop = pop/tot) %>% 
  ungroup %>% left_join(nKx %>% rename(popUN = pop)) %>% 
  mutate(POP = prop*popUN) %>% select(-c(prop,popUN,pop,tot)) %>%
  inner_join(datalevel %>% mutate(level = toupper(level)))
###################################################
wb = createWorkbook()
######################## COVID19 ##################
###################################################
# CASE FATALITY RATE ####
data1 = covidtab %>% filter(pais != 'Jamaica') %>% group_by(pais) %>%
  arrange(fecha) %>%
  mutate(ncum = cumsum(n),casoscum = cumsum(casos),
         cfrcum = ifelse(casoscum == 0,0,ncum/casoscum),
         cfr = ifelse(casos == 0,0,n/casos),
         fecha = ymd(fecha)) %>% ungroup()
addWorksheet(wb,'CFR1')
writeData(wb,'CFR1',data1)
ggplot(data = data1,aes(x = fecha)) + theme_minimal() + 
  facet_wrap(.~pais,scales = 'free_y') +
  scale_x_date(date_breaks = '1 month',date_labels = '%b') +
  geom_line(aes(y = cfrcum*100,color = pais),size = 2) +
  labs(x = 'Dia',y = '% tasa letalidad COVID19',color = 'Pais') +
  theme(axis.text = element_text(size =  16),
        axis.title = element_text(size = 18),
        legend.position = 'none',
        strip.text.x = element_text(size = 18, face = "bold"))
ggsave('./CFR1.jpeg',height = 25,width = 50,units = 'cm')
data2 = data1 %>% group_by(pais) %>% arrange(fecha) %>%
  mutate(time = 0:(length(pais) - 1)) %>% ungroup()
addWorksheet(wb,'CFR2')
writeData(wb,'CFR2',data2)
ggplot(data = data2,aes(x = time,y = cfrcum*100)) + theme_minimal() + 
  geom_line(aes(color = pais),size = 2) + 
  scale_y_continuous(n.breaks = 8) + scale_x_continuous(n.breaks = 10) +
  theme(axis.text = element_text(size =  16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text.x = element_text(size = 16,face = 'bold'),
        legend.key.size = unit(2,'line')) + 
  scale_color_discrete() + facet_wrap(incidencia~.,scales = 'free_y') +
  labs(x = 'Dia',y = '% tasa letalidad COVID19',color = 'Pais')
ggsave('./CFR2.jpeg',height = 25,width = 70,units = 'cm')
# TCM ESTANDARIZADA: Diferentes POBLACION ESTANDAR ####
data2 = covid %>% mutate(semana = week(fecha)) %>% group_by(pais,semana,edad,sexo) %>% 
  summarise(n = n()) %>% inner_join(nKx %>% filter(year == 2020)) %>% mutate(Prop = n/pop)
data3 = bind_rows(data2 %>% left_join(nKx_LAC %>% mutate(Std = 'Std: LAC',Popest = LAC,tot = sum(LAC)) %>% 
                                        select(-c(LAC,year))),
                  data2 %>% left_join(popUN(2020,'Bolivia (Plurinational State of)') %>% 
                                        mutate(Std = 'Std: Bolivia',Popest = pop,tot = sum(pop)) %>% 
                                        select(-c(pais,year,pop))),
                  data2 %>% left_join(nKx %>% filter(pais == 'Uruguay',year == 2020) %>% 
                                        mutate(Std = 'Std: Uruguay',Popest = pop,tot = sum(pop)) %>% 
                                        select(-c(pais,year,pop)))) %>%
                    mutate(nest = Popest*Prop)
data4 = data3 %>% group_by(pais,semana,tot,Std) %>% 
  summarise(nest = sum(nest)) %>%
  mutate(PropEst = nest/tot*100*mil) %>% ungroup()
addWorksheet(wb,'TCM std')
writeData(wb,'TCM std',data4)
ggplot(data = data4,aes(x = semana,y = PropEst)) + theme_minimal() + 
  facet_wrap(pais~.,scales = 'free_y') +
  geom_line(aes(linetype = Std),size = 1) + 
  labs(x = 'Semana',y = 'COVID19 TCM estandarizada',linetype = '') +
  scale_y_continuous(n.breaks = 8) + scale_x_continuous(n.breaks = 10) +
  scale_linetype_manual(values = c(2,1,3)) + 
  theme(axis.text = element_text(size =  16),
        axis.title = element_text(size = 18),
        legend.position = 'bottom',
        strip.text = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        legend.key.size = unit(2,'line'))
ggsave('./CovidTCMstd.jpeg',height = 25,width = 50,units = 'cm')
# TASA CRUDA MORTALIDAD COVID19 ####
data1 = covidtab %>% mutate(semana = week(fecha)) %>% group_by(incidencia,semana,pais) %>%
  summarise(n = sum(n)) %>% ungroup()
data2 = data1 %>% 
  inner_join(nKx %>% filter(year == 2020) %>% group_by(pais) %>% summarise(pop = sum(pop))) %>% 
  mutate(Prop = n/pop*100*mil)
addWorksheet(wb,'TCM')
writeData(wb,'TCM',data2)
ggplot(data = data2,aes(x = semana,y = Prop)) + theme_minimal() +
  theme(axis.text = element_text(size =  20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 16,face = 'bold'),
        legend.key.size = unit(1.5,'line')) +
  geom_line(aes(col = pais),size = 2) + labs(x = 'Semana',y = 'TCM') +
  scale_y_continuous(n.breaks = 10) + facet_wrap(incidencia~.,scales = 'free_y') +
  scale_color_discrete("Pais") + scale_x_continuous(n.breaks = 10)
ggsave('./TCM.jpeg',height = 25,width = 50,units = 'cm')
ggplot(data = data2,aes(x = semana,y = Prop)) + theme_minimal() +
  theme(axis.text = element_text(size =  20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20, face = "bold"),
        legend.position = 'none') +
  facet_wrap(pais~.,ncol = 4,scales = 'free_y') +
  geom_line(aes(col = pais),size = 2) + labs(x = 'Semana',y = 'TCM') +
  scale_y_continuous(n.breaks = 10) + scale_x_continuous(n.breaks = 10)
ggsave('./TCM_a.jpeg',height = 25,width = 50,units = 'cm')
# TASA ESPECIFICA DE MORTALIDAD ####
data1 = covid %>% mutate(semana = week(fecha))%>% filter(edad %in% edadesd80,
                                                         sexo %in% c('MEN','WOMEN')) %>% 
  group_by(pais,semana,sexo,edad) %>% summarise(n = n())
data2 = left_join(data1,nKx %>% filter(year == 2020)) %>% ungroup() %>% 
  mutate(n = n/pop*100*mil,sexo = factor(sexo,labels = c('HOMBRES','MUJERES')))
addWorksheet(wb,'TEM')
writeData(wb,'TEM',data2)
data2 %>% 
  ggplot(aes(x = semana,y = n)) + theme_minimal() +
  theme(axis.text = element_text(size =  16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 20, face = "bold")) +
  facet_grid(sexo~edad,scales = 'free_y') + scale_y_continuous(n.breaks = 10) +
  geom_line(aes(color = pais),size = 2) + labs(x = 'Semana',y = 'TEM') +
  scale_color_discrete("Pais") + scale_x_continuous(n.breaks = 6)
ggsave('./ASMR.jpeg',height = 25,width = 50,units = 'cm')

# PIRAMIDE COVID19 ####
data1 = covid %>% group_by(pais,edad,sexo) %>% summarise(n = n()) %>% ungroup()
data2 = data1 %>% inner_join(nKx %>% filter(year == 2020) %>% select(-year)) %>% 
  mutate(POP = n/pop*100*mil,sexo = factor(sexo,labels = c('HOMBRES','MUJERES')), Label = paste(sexo))
addWorksheet(wb,'Piramide')
writeData(wb,'Piramide',data2)
ggplot(data = data2,mapping = aes(x = edad, y = POP, fill = Label, color = Label)) + 
  facet_wrap(pais~.,scales = 'free_x') +
  geom_bar(data = subset(data2, Label == "MUJERES"), stat = "identity", width = 1.0)  +
  geom_bar(data = subset(data2, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity", width = 1.0)  + 
  coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "") +
  scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
  scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
  theme_minimal() + scale_y_continuous(n.breaks = 10,labels = function(x){x %>% abs}) +
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 12), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 18, face = "bold")
  )
ggsave('./piramidecovid.jpeg',height = 25,width = 50,units = 'cm')
# PIRAMIDE MORTALIDAD COVID19 POR MES ####
data1 = covid %>% mutate(mes = month(fecha)) %>% group_by(pais,mes,sexo,edad) %>% summarise(n = n()) 
data1 = data1 %>% inner_join(nKx %>% filter(year == 2020),all = T) %>% ungroup() %>% 
  mutate(POP = n/pop*100*mil,Pais = factor(pais),
         sexo = factor(sexo,labels = c('HOMBRES','MUJERES')),
         Label = factor(sexo),mes = as.factor(mes)) %>% 
  select(Pais,edad,mes,POP,Label)
addWorksheet(wb,'Piramide mes')
writeData(wb,'Piramide mes',data1)

ggplot(data1,aes(x = edad, y = POP, fill = Label, color = Label,alpha = mes, group = Pais)) + 
  facet_wrap(Pais~.,scales = 'free_x') +
  geom_bar(data = subset(data1, Label == "MUJERES"), stat = "identity", width = 1)  +
  geom_bar(data = subset(data1, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity", width = 1) +
  coord_flip() + labs(y = "TEM", x = "Edad", color = "", fill = "",alpha = "Mes") +
  scale_color_manual(values = c(Pal.2[c(3)],Pal.1[c(3)])) + 
  scale_fill_manual(values = c(Palb.2[c(8)],Palb.1[c(8)])) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_minimal() + scale_y_continuous(n.breaks = 10,labels = function(x){x %>% abs}) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 16),
        legend.title = element_text(face = 'bold',size = 14),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 12), 
        axis.text = element_text(size =  20),
        axis.title = element_text(size = 20),
        strip.text.x = element_text(size = 16, face = "bold"))
ggsave('./PirCOVIDmes.jpeg',height = 25,width = 50,units = 'cm')
# NIVEL SUBNACIONAL ###############################
# Argentina, Brazil, Chile, Colombia, Peru, Cuba, Mexico
data1 = covid %>% group_by(pais,cod_level,edad,sexo) %>%
  summarise(n = n()) %>% ungroup() %>% inner_join(censos %>% rename(pop = POP)) %>%
  mutate(POP = n/pop*100*mil,sexo = factor(sexo,labels = c('HOMBRES','MUJERES')),Label = paste(sexo)) %>%
  filter(pais == 'Argentina')
data2 = data1 %>% group_by(level) %>% summarise(maxs = max(POP)) %>%
  arrange(maxs)
data1 = data1 %>% mutate(level = factor(level,levels = data2$level))
addWorksheet(wb,'Subnacional')
writeData(wb,'Subnacional',data1)

ggplot(data = data1 %>% filter(pais == 'Argentina'),mapping = aes(x = edad, y = POP, fill = Label, color = Label)) + 
facet_wrap(level~.,scales = 'free_x') +
geom_bar(data = subset(data1, Label == "MUJERES"), stat = "identity", width = 1.0)  +
geom_bar(data = subset(data1, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity", width = 1.0)  + 
coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "") +
scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
scale_y_continuous(labels = function(x){x %>% abs},n.breaks = 5) +
theme(legend.position = "bottom", legend.text = element_text(size = 16),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle  = element_text(size = 12), 
      axis.text = element_text(size =  16),
      axis.title = element_text(size = 18),
      strip.text.x = element_text(size = 18, face = "bold")
)
ggsave('./PirARG.jpeg',height = 25,width = 50,units = 'cm')
###################################################
######## MORTALIDAD PRE-PANDEMIA Comparaciones ####
###################################################
# Comparación COVID19 contra otras causas ####
data2 = deaths %>% filter(mes <= month(fechafin)) %>% group_by(year,pais,edad,sexo,causa) %>%
  summarise(n = n()) %>% ungroup()
data2$causa[substr(data2$causa,1,3) %in% paste0('I1',0:5)] = 'Hipertension'
data2$causa[substr(data2$causa,1,3) %in% c(paste0('C',formatC(c(0:79,97),width = 2,flag = 0)),
                                           paste0('D',formatC(0:48,width = 2,flag = 0)))] = 'Cancer'
data2$causa[substr(data2$causa,1,3) %in% c(paste0('E1',0:4))] = 'Diabetes'
data3 = data2 %>% 
  filter(causa %in% c('Hipertension','Suicidio','Cancer','Diabetes','Homicidios','COVID19')) %>% 
  group_by(pais,year,causa,sexo,edad) %>% summarise(n = sum(n,na.rm = T)) %>% 
  ungroup() %>% inner_join(nKx) %>% 
  mutate(POP = n/pop*100*mil, sexo = factor(sexo,labels = c('HOMBRES','MUJERES'))) %>% 
  group_by(pais,causa,sexo,edad) %>% summarise(POP = mean(POP))
data3 = bind_rows(data3,
                  covid %>% group_by(pais,edad,sexo) %>%
                  summarise(n = n()) %>% mutate(causa = 'COVID19') %>%
                  inner_join(nKx %>% filter(year == 2020) %>% select(-year)) %>% 
                  mutate(POP = n/pop*100*mil,sexo = factor(sexo,levels = c('MEN','WOMEN'),labels = c('HOMBRES','MUJERES'))) %>%
                  select(pais,causa,sexo,edad,POP)
  ) %>%
  mutate(causa = factor(causa,levels = c('COVID19',sort(c('Hipertension','Suicidio','Cancer','Diabetes','Homicidios')))),
         Label = sexo) %>% 
  filter(pais %in% c('Chile','Colombia','Mexico','Peru'))
addWorksheet(wb,'Otras causas')
writeData(wb,'Otras causas',data3)
ggplot(data = data3,mapping = aes(x = edad, y = POP, fill = Label,group = causa,color = Label)) + 
  facet_grid(causa~pais,scales = 'free_x') +
  geom_bar(data = subset(data3, Label == "MUJERES"), stat = "identity", width = 1.0,position = 'dodge')  +
  geom_bar(data = subset(data3, Label == "HOMBRES"), aes(x = edad, y = -1*POP), stat = "identity",position = 'dodge', width = 1.0)  + 
  coord_flip()  + labs(y = "TEM", x = "Edad", color = "", fill = "") +
  scale_fill_manual(values = c(Pal.2[7],Pal.1[7])) + 
  scale_color_manual(values = c(Palb.2[3],Palb.1[3])) + 
  theme_minimal() + 
  scale_y_continuous(n.breaks = 6,labels = function(x){x %>% abs}) + 
  theme(legend.position = "bottom", legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle  = element_text(size = 16), 
        axis.text = element_text(size =  16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold")
  )
ggsave('./Pircausas.jpeg',height = 25,width = 50,units = 'cm')
###################################################
############### EXCESO DE MORTALIDAD ##############
###################################################
# Edad y sexo ####
total_deaths = deaths %>%  
  #sacando las causas externas de mortalidad= V01-Y98
  filter(!{substr(causa,1,1) %in% c('V','W','X','Y')}) %>% 
  group_by(pais,year,mes,edad,sexo) %>%
  summarise(total_deaths = n()) %>%
  ungroup()
total_deaths = bind_rows(total_deaths %>% filter(pais != 'Colombia'))

data_covid = covid %>% mutate(mes = month(fecha),year = year(fecha)) %>% 
  group_by(year,mes,edad,sexo,pais) %>%
  summarise(covid_deaths = n()) %>% ungroup() %>%
  bind_rows(expand.grid(pais = unique(covid$pais),
                        year = 2015:2019, # Bind on rows with 0 covid deaths before 2017
                        mes = 1:12,
                        edad = edadesd80,
                        sexo = c('MEN','WOMEN'),
                        covid_deaths = 0)) %>%
  arrange(pais,year,mes) %>%
  group_by(pais, year, mes,edad,sexo) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm = T)) %>% ungroup()
# unir muertes totales y por covid
total_covid_deaths = data_covid %>%
  right_join(total_deaths) %>% ungroup() %>%
  dplyr::select(pais,year,mes,total_deaths,covid_deaths,edad,sexo)
expected_deaths = total_covid_deaths %>%
  filter(year == 2020) %>%
  right_join(total_covid_deaths %>% filter(year >= 2015,year <= 2019, 
                           sexo %in% c('MEN','WOMEN'), edad %in% edadesd80) %>%
               group_by(pais,mes,sexo,edad) %>%
               summarise(expected_deaths = mean(total_deaths,na.rm = T)) %>% 
               ungroup()) %>% select(-covid_deaths) %>%
  full_join(data_covid %>% filter(year == 2020) %>% select(-year))
excess_deaths = expected_deaths %>%
  mutate(excess_deaths = total_deaths - expected_deaths,
         non_covid_deaths = total_deaths - covid_deaths)
# Pais: Chile,Brazil, Peru.. Solo esperadas: Ecuador,Guatemala,Mexico

data2 = excess_deaths %>% filter(edad %in% edadesd80,sexo %in% c('MEN','WOMEN')) %>% 
  mutate(edad = factor(edad),sexo = factor(sexo,levels = c('MEN','WOMEN'),
                                           labels = c('HOMBRES','MUJERES'))) 
addWorksheet(wb,'Exceso edad_sexo')
writeData(wb,'Exceso edad_sexo',data2)
data2 %>% filter(pais == 'Guatemala') %>%
  ggplot(aes(x = mes,y = covid_deaths)) + geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
  geom_point(aes(col = 'Muertes COVID19\n observadas')) +
  geom_line(aes(col = 'Muertes COVID19\n observadas')) + theme_minimal() + 
  theme(axis.text = element_text(size =  16),
        strip.text = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16)) +
  geom_line(aes(y = expected_deaths,linetype = 'Muertes esperadas\n (promedio)')) + 
  facet_grid(sexo~edad,scales = 'free_y') +
  scale_x_continuous(breaks = seq(1,12,2)) +
  scale_fill_manual('',values = 'blue') + scale_color_manual('',values = 'red') +
  scale_linetype_manual('',values = 2) + labs(x = 'Mes',y = 'Conteo de muertes')
ggsave('./ExcesoGTM.jpeg',height = 25,width = 50,units = 'cm')

# COLOMBIA ####
colnames(exCOL) = c('year','semana','nat_tot','nat_men','nat_women','nat_ind',
                    'vio_tot','vio_men','vio_women','vio_ind',
                    'ees_tot','ees_men','ees_women','ees_ind')
exCOL = exCOL %>% slice(-1) %>%
  mutate(year = as.numeric(substr(replace_na_with_last(year),1,4))) %>%
  filter(semana != 'Total') %>%
  mutate(semana = as.numeric(gsub('[[:alpha:]]','',semana))) %>%
  select(contains(c('ees','nat','year','semana'))) %>%
  pivot_longer(contains(c('ees','nat')),names_to = 'sexo',values_to = 'deaths') %>%
  mutate(sexo = factor(substr(sexo,4,nchar(sexo)),labels = c('IND','MEN','TOTAL','WOMEN'))) %>%
  group_by(year,sexo,semana) %>% summarise(total_deaths = sum(as.numeric(deaths),na.rm = T),
                                           pais = 'Colombia') %>% ungroup() %>%
  filter(sexo != 'TOTAL')
weeks = tibble(semana = 1:52,mes = c(rep(1,5),rep(2:4,each = 4),
                                     rep(5,5),rep(6:7,each = 4),
                                     rep(8,5),rep(9,4),
                                     rep(10,5),rep(11:12,each = 4)))
exCOLtot = exCOL %>% inner_join(weeks) %>% select(-semana) %>%
  group_by(pais,year,mes) %>% summarise(total_deaths = sum(total_deaths,na.rm = T))
exCOLtot = bind_rows(exCOLtot %>% filter(year %in% 2015:2019),
                     exCOLtot %>% filter(year %in% 2020,mes <= month(fechafin)))

colnames(exCOLlevel) = c('year','level','semana','nat_tot','nat_men','nat_women','nat_ind',
                    'vio_tot','vio_men','vio_women','vio_ind',
                    'ees_tot','ees_men','ees_women','ees_ind')
exCOLlevel = exCOLlevel %>% slice(-1) %>%
  mutate(year = as.numeric(substr(replace_na_with_last(year),1,4)),
         level = toupper(replace_na_with_last(level))) %>%
  filter(semana != 'Total',level %in% unique(levelCOL$level)) %>%
  mutate(semana = as.numeric(gsub('[[:alpha:]]','',semana))) %>%
  #select(contains(c('ees','vio','nat','year','semana'))) %>%
  select(contains(c('ees','nat','year','level','semana'))) %>%
  pivot_longer(contains(c('ees','nat')),names_to = 'sexo',values_to = 'deaths') %>%
  mutate(sexo = factor(substr(sexo,4,nchar(sexo)),labels = c('IND','MEN','TOTAL','WOMEN'))) %>%
  group_by(year,level,sexo,semana) %>% summarise(total_deaths = sum(as.numeric(deaths),na.rm = T),
                                           pais = 'Colombia') %>% ungroup() %>%
  filter(sexo != 'TOTAL') %>% inner_join(levelCOL)
exCOLlevel = exCOLlevel %>% inner_join(weeks) %>% select(-semana) %>%
  group_by(pais,cod_level,year,mes) %>% summarise(total_deaths = sum(total_deaths,na.rm = T)) %>%
  ungroup()
exCOLlevel = bind_rows(exCOLlevel %>% filter(year %in% 2015:2019),
                     exCOLlevel %>% filter(year %in% 2020,mes <= month(fechafin)))

# GLOBALES ####
total_deaths = deathsCausaMes %>% 
  filter(pais != 'Colombia') %>%  
  #sacando las causas externas de mortalidad= V01-Y98
  filter(!{substr(causa,1,1) %in% c('V','W','X','Y')}) %>%
  group_by(year,pais,mes) %>%
  summarise(total_deaths = sum(n,na.rm = T)) %>% ungroup() %>%
  bind_rows(exCOLtot) %>% arrange(pais,year,mes)
total_deaths = bind_rows(total_deaths %>% filter(pais != 'Colombia'),
                         total_deaths %>% filter(pais == 'Colombia',year %in% 2015:2018),
                         total_deaths %>% filter(pais == 'Colombia',year == 2019) %>%
                           mutate(total_deaths = total_deaths*1.05),
                         total_deaths %>% filter(pais == 'Colombia',year == 2020) %>%
                           mutate(total_deaths = total_deaths*1.15))
data_covid = covidtab %>% select(pais,fecha,n) %>%
  mutate(mes = month(fecha),year = year(fecha)) %>%
  group_by(year,mes,pais) %>%
  summarise(covid_deaths = sum(n,na.rm = T)) %>% ungroup() %>%
  bind_rows(covidECUMes) %>%
  bind_rows(expand.grid(pais = unique(covidtab$pais),
                        year = 2015:2019, # Bind on rows with 0 covid deaths before 2017
                        mes = 1:12,
                        covid_deaths = 0)) %>%
  group_by(pais, year, mes) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm = T)) %>% ungroup()
# unir muertes totales y por covid
total_covid_deaths = data_covid %>% 
  full_join(total_deaths) %>% group_by(pais,year,mes) %>% 
  summarise(total_deaths = sum(total_deaths,na.rm = T),
            covid_deaths = sum(covid_deaths,na.rm = T)) %>%
  ungroup() %>%
  mutate(non_covid_deaths = if_else(total_deaths != 0 & 
                                      covid_deaths < total_deaths,total_deaths - covid_deaths,NaN))
total_covid_deaths %>% mutate(year = factor(year)) %>% filter(mes %in% 1:12) %>% 
  ggplot(aes(x = mes,y = total_deaths,col = year)) + geom_line(size = 1) + theme_bw() +
  facet_wrap(pais~.,scales = 'free_y')
# Calculo de muertes esperadas
# por Pais, global
expected_deaths = total_covid_deaths %>%
  filter(year == 2020,mes %in% 1:12) %>%
  right_join(total_covid_deaths %>% filter(year >= 2015,year <= 2019,total_deaths != 0,mes %in% 1:12) %>%
               group_by(pais,mes) %>%
               summarise(expected_deaths = mean(total_deaths,na.rm = T)))
excess_deaths = expected_deaths %>%
  mutate(excess_deaths = total_deaths - expected_deaths) %>% 
  group_by(pais,year,mes)
addWorksheet(wb,'Exceso')
writeData(wb,'Exceso',excess_deaths)

ggplot(excess_deaths,aes(x = mes,y = covid_deaths)) + 
  geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
  geom_point(aes(col = 'Muertes COVID19\n observadas')) +
  geom_line(aes(col = 'Muertes COVID19\n observadas')) + theme_minimal() + 
  theme(axis.text = element_text(size =  16),
        strip.text = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16)) +
  geom_line(aes(y = expected_deaths,linetype = 'Muertes esperadas\n (promedio)')) + 
  facet_wrap(pais~.,scales = 'free_y') +
  scale_x_continuous(breaks = seq(1,12)) +
  scale_fill_manual('',values = 'blue') + scale_color_manual('',values = 'red') +
  scale_linetype_manual('',values = 2) + labs(x = 'Mes',y = 'Conteo de muertes')

ggsave('./ExcesoNAC.jpeg',height = 25,width = 50,units = 'cm')

# HOLT-WINTERS ####
total_deaths = deathsCausaMes %>%  
  filter(pais != 'Colombia') %>%
  #sacando las causas externas de mortalidad= V01-Y98
  filter(!{substr(causa,1,1) %in% c('V','W','X','Y')}) %>%
  group_by(year,pais,mes) %>%
  summarise(total_deaths = sum(n,na.rm = T)) %>% ungroup() %>%
  bind_rows(exCOLtot) %>% arrange(pais,year,mes)
total_deaths = bind_rows(total_deaths %>% filter(pais != 'Colombia'),
                         total_deaths %>% filter(pais == 'Colombia',year %in% 2015:2018),
                         total_deaths %>% filter(pais == 'Colombia',year == 2019) %>%
                           mutate(total_deaths = total_deaths*1.05),
                         total_deaths %>% filter(pais == 'Colombia',year == 2020) %>%
                           mutate(total_deaths = total_deaths*1.15))
data_covid = covidtab %>% select(pais,fecha,n) %>%
  mutate(mes = month(fecha),year = year(fecha)) %>%
  group_by(year,mes,pais) %>%
  summarise(covid_deaths = sum(n,na.rm = T)) %>% ungroup() %>%
  bind_rows(covidECUMes) %>%
  bind_rows(expand.grid(pais = unique(covidtab$pais),
                        year = 2015:2019, # Bind on rows with 0 covid deaths before 2017
                        mes = 1:12,
                        covid_deaths = 0)) %>%
  group_by(pais, year, mes) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm = T)) %>% ungroup()
# unir muertes totales y por covid
total_covid_deaths = data_covid %>% 
  full_join(total_deaths) %>% group_by(pais,year,mes) %>% 
  summarise(total_deaths = sum(total_deaths,na.rm = T),
            covid_deaths = sum(covid_deaths,na.rm = T)) %>%
  ungroup() %>%
  mutate(non_covid_deaths = if_else(total_deaths != 0 & 
                                      covid_deaths < total_deaths,total_deaths - covid_deaths,0))
# Calculo de muertes esperadas
# por Pais, global
expected = bind_rows(total_covid_deaths %>%
                       filter(year >= 2015,year <= 2019,total_deaths != 0) %>%
                       group_by(pais,mes) %>% 
                       summarise(expected_deaths = mean(total_deaths), expect = 'Promedio'),
                     total_covid_deaths %>%
                       filter(year >= 2015,year <= 2019,total_deaths != 0) %>%
                       group_by(pais) %>% nest() %>% 
                       mutate(expected = map(data,function(x)tibble(mes = 1:12,as_tibble(predict(HoltWinters(ts(x$total_deaths,frequency = 12)),12,T))))) %>%
                       select(-data) %>% unnest(c(expected)) %>% mutate(expect = 'Holt-Winters',expected_deaths = fit) %>% select(-c(fit))
) %>% mutate(expect = factor(expect,levels = c('Promedio','Holt-Winters')))
expected_deaths = total_covid_deaths %>%
  filter(year == 2020) %>%
  mutate(total_deaths = if_else(total_deaths == 0,NaN,total_deaths)) %>%
  right_join(expected) %>% 
  mutate(non_covid_deaths = if_else(!is.na(total_deaths) & covid_deaths != 0 &
                                      covid_deaths < total_deaths,total_deaths - covid_deaths,NaN))

data1 = expected_deaths  %>% filter(expect %in% c('Promedio','Holt-Winters')) %>%
  mutate(expect = factor(expect,levels = c('Promedio','Holt-Winters')))
addWorksheet(wb,'ExcesoHW')
writeData(wb,'ExcesoHW',data1)
data1 %>%
  ggplot(aes(x = mes,y = expected_deaths,group = expect)) + theme_minimal() +
  geom_ribbon(data = expected_deaths,aes(x = mes,ymin = lwr,ymax = upr),alpha = 0.2,col = 'gray') +
  theme(axis.text = element_text(size =  16),
        strip.text.x = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(2,'line')) + facet_wrap(.~pais,scales = 'free_y') +
  geom_line(aes(col = expect),size = 0.8) + 
  labs(x = 'Mes',y = 'Conteo de muertes',color = 'Muertes esperadas') +
  geom_line(aes(y = total_deaths,linetype = 'Muertes\ntotales 2020'),size = 1,color = 'black') +
  geom_line(aes(y = non_covid_deaths,linetype = 'Muertes sin \nCOVID19\n'),color = 'black',size = 1) +
  scale_x_continuous(breaks = 1:12) + scale_linetype_manual('',values = c(2,1))

ggsave('./ExcesoHW.jpeg',height = 25,width = 50,units = 'cm')


# SUBNACIONALES ####
total_deaths = deaths %>% 
  filter(pais != 'Colombia') %>%  
  #sacando las causas externas de mortalidad= V01-Y98
  filter(!{substr(causa,1,1) %in% c('V','W','X','Y')}) %>%
  group_by(year,pais,cod_level,mes) %>%
  summarise(total_deaths = n()) %>% ungroup() %>%
  bind_rows(exCOLlevel) %>% arrange(pais,cod_level,year,mes)
total_deaths = bind_rows(total_deaths %>% filter(pais != 'Colombia'),
                         total_deaths %>% filter(pais == 'Colombia',year %in% 2015:2018),
                         total_deaths %>% filter(pais == 'Colombia',year == 2019) %>%
                           mutate(total_deaths = total_deaths*1.05),
                         total_deaths %>% filter(pais == 'Colombia',year == 2020) %>%
                           mutate(total_deaths = total_deaths*1.15))
data_covid = covid %>% select(pais,cod_level,fecha) %>%
  mutate(mes = month(fecha),year = year(fecha)) %>%
  group_by(year,pais,cod_level,mes) %>%
  summarise(covid_deaths = n()) %>% ungroup() %>%
  bind_rows(expand.grid(pais = unique(covidtab$pais),
                        cod_level = unique(total_deaths$cod_level),
                        year = 2015:2019, # Bind on rows with 0 covid deaths before 2017
                        mes = 1:12,
                        covid_deaths = 0)) %>%
  group_by(pais, cod_level, year, mes) %>%
  summarise(covid_deaths = sum(covid_deaths,na.rm = T)) %>% ungroup()
# unir muertes totales y por covid
total_covid_deaths = data_covid %>% 
  full_join(total_deaths) %>% inner_join(datalevel) %>%
  group_by(pais,cod_level,year,mes) %>% 
  summarise(total_deaths = sum(total_deaths,na.rm = T),
            covid_deaths = sum(covid_deaths,na.rm = T)) %>%
  ungroup() %>%
  mutate(non_covid_deaths = if_else(total_deaths != 0 & 
                                      covid_deaths < total_deaths,total_deaths - covid_deaths,NaN))
expected1 = total_covid_deaths %>%
  filter(year >= 2015,year <= 2019,total_deaths != 0) %>%
  group_by(pais,cod_level,mes) %>% 
  summarise(promedio = mean(total_deaths,na.rm = T)) %>%
  ungroup() %>% 
  mutate(promedio = ifelse(promedio < 0,0,promedio))
expected2 = total_covid_deaths %>%
  filter(year >= 2015,year <= 2019,total_deaths != 0,pais != 'Brazil') %>%
  group_by(pais,cod_level) %>% nest() %>% 
  mutate(expected = map(data,function(x){tibble(mes = 1:12,as_tibble(predict(HoltWinters(ts(x$total_deaths,frequency = 12)),12,T)))})) %>%
  select(-data) %>% unnest(c(expected)) %>% rename(HW = fit) %>%
  ungroup() %>%
  mutate(HW = ifelse(HW < 0,0,HW),lwr = ifelse(lwr < 0,0,lwr), upr = ifelse(upr < 0,0,upr))
expected_deaths = total_covid_deaths %>%
  filter(year == 2020) %>%
  full_join(expected1) %>%
  full_join(expected2) %>%
  arrange(pais,cod_level,year,mes) %>%
  mutate(covid_deaths = ifelse(is.na(covid_deaths) & mes %in% 1:month(fechafin),0,covid_deaths),
         non_covid_deaths = if_else(!is.na(total_deaths) &
                                      covid_deaths < total_deaths,total_deaths - covid_deaths,NaN))
# 2 Brazil   
# 3 Chile    
# 4 Colombia 
# 6 Mexico   
# 7 Peru 
data1 = expected_deaths %>% inner_join(datalevel)
addWorksheet(wb,'Exceso Subnacional')
writeData(wb,'Exceso Subnacional',data1)
data1 %>% filter(pais == 'Peru') %>% 
  ggplot(aes(x = mes,y = covid_deaths)) + facet_wrap(level~.,scales = 'free') +
  geom_area(aes(y = non_covid_deaths,fill = 'Otras muertes por\ncausas internas'),alpha = 0.3) + 
  geom_line(aes(y = promedio, linetype = 'Muertes esperadas\nPromedio')) +
  #geom_line(aes(y = promedio, linetype = 'Muertes observadas\n2019')) +
  geom_line(aes(y = HW, linetype = 'Muertes esperadas\nHolt-Winters')) +
  geom_ribbon(aes(ymin = lwr,ymax = upr),alpha = 0.2,col = 'gray') +
  geom_point(aes(col = 'Muertes COVID19\n observadas')) +
  geom_line(aes(col = 'Muertes COVID19\n observadas')) + theme_minimal() + 
  theme(axis.text = element_text(size =  16),
        strip.text = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(1,12,2)) +
  scale_fill_manual('',values = 'blue') + 
  scale_color_manual('',values = 'red') +
  scale_linetype_manual('',values = c(2,1)) +
  #scale_linetype_manual('',values = c(1)) +
  labs(x = 'Mes',y = 'Conteo de muertes',linetype = 'Muertes esperadas')
ggsave('./ExcesoSUB_PER.jpeg',height = 25,width = 50,units = 'cm')

###############
saveWorkbook(wb,'Tablas.xlsx',overwrite = T)
