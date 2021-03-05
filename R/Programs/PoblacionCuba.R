library(tidyverse)
edadesd80 = c(" 0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
PROV = c('PINAR DEL RIO','ARTEMISA','LA HABANA','MAYABEQUE','MATANZAS','CIENFUEGOS',
         'VILLA CLARA','SANCTI SPIRITUS','CIEGO DE AVILA','CAMAGUEY','LAS TUNAS',
         'GRANMA','HOLGUIN','SANTIAGO DE CUBA','GUANTANAMO','ISLA DE LA JUVENTUD')
levelCUB = tibble(level = PROV,cod_level = c('21','22','23','24','25','27','26',
                                             '28','29','30','31','33','32','34','35','40.01'))
cubmen = function(prov) {
  data1 = read.csv(paste0('./Data/Cuba/',prov,'m.txt'),header = T,sep = ' ')
  data1 = data1 %>% select(c(EDADES,X2020)) %>%
    mutate(edad = as.numeric(EDADES),pop = as.numeric(X2020)) %>% select(edad,pop) %>%
    mutate(year = 2020,sexo = 'MEN',level = prov)
  return(data1)
}
cubwomen = function(prov) {
  data1 = read.csv(paste0('./Data/Cuba/',prov,'f.txt'),header = T,sep = ' ')
  data1 = data1 %>% select(c(EDADES,X2020)) %>%
    mutate(edad = as.numeric(EDADES),pop = as.numeric(X2020)) %>% select(edad,pop) %>%
    mutate(year = 2020,sexo = 'WOMEN',level = prov)
  return(data1)
}
data = NULL
for (prov in PROV) {
  data = bind_rows(data,cubmen(prov),cubwomen(prov))
}
popCUB = data %>% inner_join(levelCUB)
popCUB$edad[popCUB$edad %in% 0:9] = 1
popCUB$edad[popCUB$edad %in% 10:19] = 2 
popCUB$edad[popCUB$edad %in% 20:29] = 3 
popCUB$edad[popCUB$edad %in% 30:39] = 4 
popCUB$edad[popCUB$edad %in% 40:49] = 5 
popCUB$edad[popCUB$edad %in% 50:59] = 6 
popCUB$edad[popCUB$edad %in% 60:69] = 7
popCUB$edad[popCUB$edad %in% 70:79] = 8
popCUB$edad[popCUB$edad >= 80] = 9
popCUB = popCUB %>% mutate(edad = factor(edad,labels = edadesd80),pais = 'Cuba') %>%
  group_by(pais,cod_level,year,sexo,edad) %>% summarise(pop = sum(pop,na.rm = T)) %>%
  ungroup()
levelCUB = levelCUB %>% mutate(pais = 'Cuba')
rm(list = ls()[!ls() %in% c('popCUB','levelCUB')])
save.image('./Data/popCUB.RData')
