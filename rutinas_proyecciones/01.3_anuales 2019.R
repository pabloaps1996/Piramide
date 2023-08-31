#
rm(list=ls())
#
# librerias
#
library(rio)
library(glue)
library(tidyverse)
#
# lectura de los datos de proyecciones
#
pp_jun19 <- read_rds("productos/proyeccion_jun19of.rds") %>% 
  rename( "jun19" = '201906')

# pp_jun19 <- pp_jun19 %>%  
#   mutate(area = ifelse(provincia =="01"| dominio =="02"| dominio =="03" |
#                        dominio =="04"| dominio =="05"| dominio =="06"|
#                        dominio =="07"| dominio =="08","1",
#                        ifelse(dominio =="09"| dominio =="10"| dominio =="11","2","1")))

# total de pp de juio 2019 es #17267986
# 17332989 con la publicada
# area
base_area <- pp_jun19 %>% 
  group_by(area) %>% 
  summarise(ykn = sum(jun19)) %>%
  ungroup()

#sexo
base_sexo <- pp_jun19 %>% 
  group_by(p02) %>% 
  summarise(ykn = sum(jun19)) %>% 
  ungroup()

# pet
base_pet <- pp_jun19 %>% 
  group_by(gedad) %>% 
  summarise(ykn = sum(jun19)) %>% 
  ungroup()

# 25 provincias
base_prv <- pp_jun19 %>% 
  group_by(provincia) %>% 
  summarise(ykn = sum(jun19)) %>% 
  ungroup()

# 25 provincias y area
base_prv_area <- pp_jun19 %>% 
  group_by(provincia, area) %>% 
  summarise(ykn = sum(jun19)) %>% 
  ungroup()


sum(base_area $ykn)
sum(base_sexo$ykn)
sum(base_pet$ykn)
sum(base_prv$ykn)



#poblaciones = rbind(base_area[,2], base_sexo[,2], base_pet[,2], base_prv[,2])

# guardar las proyecciones poblacionales por grupos de dominio mensuales 
#
saveRDS(base_prv_area, "productos/proyeccion_jun19_2escario.rds")
