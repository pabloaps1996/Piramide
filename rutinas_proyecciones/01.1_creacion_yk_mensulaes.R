#
rm(list=ls())
#
# librerias
#
library(rio)
library(glue)
library(tidyverse)
#
# funcion
#
source("rutinas/funciones/tot_pob.R")
#
# lectura de los datos de proyecciones
#
base <- read_rds("productos/td_pp_ago-jun23.rds")
#
# nombres con el o  los meses y años 
#
nombres <- colnames(base[8])
#
meses <- c("01", "02", "03", "04", "05",  "06", "07", "08", "09", "10", "11", "12")
anios <- str_pad(as.character(2021), 4, "left", "0")

comb <- expand.grid(anios=anios, meses=meses)
comb_v <- paste0(comb$anios, comb$meses)
comb_v <- comb_v[order(as.numeric(comb_v))]
comb_v <- comb_v[8]
#
# proyecciones poblacionales mensuales
#
for (i in 1:length(nombres)) {

  loli <- tot_pob(bp = base,
                  dominio = "dom12",
                  si.area = T,
                  si.sexo = T,
                  gedad = c(0, 14, 15, 100),
                  #gedad = c(0, 14,15, 24,25, 34,35, 64,65, 100),
                  anio = nombres[i]) %>% 
    rename(dominio=dom12, p02=sexo) 
     # colnames(loli)[5]<- comb_v[i]

  if(i == 1){
    proyeccion <- loli[1:5]
  }
  else{
    loli <- loli[5]
    proyeccion <- cbind(proyeccion,loli)

  }
}
# proyecciones por área
base1 <- proyeccion %>% group_by(area) %>% summarise(ago21= sum(ago_21)) 


# 2 GRUPOS DE EDAD
pp <-proyeccion  %>% spread(p02, ago_21)
names(pp) <- c("dominio", "area","gedad", "X1",  "X2")
menor_15 <- pp %>% filter(gedad==1) %>%  select(-gedad)
mayor_15 <- pp %>% filter(gedad==2) %>%  select(-gedad)
yk <- left_join(menor_15, mayor_15, by=c("dominio", "area"))
colnames(yk) <- c("dominio", "area","menor_15_h", "menor_15_m", "mayor_15_h", "mayor_15_m")

yk <- yk %>% mutate(
  dominio = ifelse(dominio=="01","QUITO",
                   ifelse(dominio=="02","GUAYAQUIL",
                          ifelse(dominio=="03", "CUENCA",
                                 ifelse(dominio=="04","MACHALA",
                                        ifelse(dominio=="05", "AMBATO", 
                                               ifelse(dominio=="06", "RESTO SIERRA URBANO",
                                                      ifelse(dominio=="07", "RESTO COSTA URBANO" , 
                                                             ifelse(dominio=="08","AMAZONIA URBANO" , 
                                                                    ifelse(dominio=="09", "SIERRA RURAL", 
                                                                           ifelse(dominio=="10","COSTA RURAL" ,
                                                                                  ifelse(dominio=="11", "AMAZONIA RURAL",
                                                                                         ifelse(dominio=="12" & area =="1", 
                                                                                                "INSULAR URBANO","INSULAR RURAL"))))))))))))) %>% 
  select(-area)

# 5 grupos de edad
pp <-proyeccion  %>% spread(gedad, ago_21)
names(pp) <- c("dominio", "area","p02", "X1",  "X2", "X3", "X4", "X5")
hombres <- pp %>% filter(p02==1) %>%  select(-p02)
colnames(hombres) <- c("dominio", "area","menor_15_h","h_15_24", "h_25_34", "h_35_64", "mayor_65_h")
mujeres <- pp %>% filter(p02==2)%>%  select(-p02)
colnames(mujeres) <- c("dominio", "area","menor_15_m","m_15_24", "m_25_34", "m_35_64", "mayor_65_m")

yk <- left_join(hombres, mujeres, by=c("dominio", "area"))
yk <- yk %>%  select(dominio, area,menor_15_h, menor_15_m, h_15_24,m_15_24 ,h_25_34, m_25_34, h_35_64, m_35_64, mayor_65_h,mayor_65_m )

yk <- yk %>% mutate( nacional = "00",
  dominio = ifelse(dominio=="01","QUITO",
                    ifelse(dominio=="02","GUAYAQUIL",
                           ifelse(dominio=="03", "CUENCA",
                                  ifelse(dominio=="04","MACHALA",
                                         ifelse(dominio=="05", "AMBATO", 
                                                ifelse(dominio=="06", "RESTO SIERRA URBANO",
                                                       ifelse(dominio=="07", "RESTO COSTA URBANO" , 
                                                              ifelse(dominio=="08","AMAZONIA URBANO" , 
                                                                     ifelse(dominio=="09", "SIERRA RURAL", 
                                                                            ifelse(dominio=="10","COSTA RURAL" ,
                                                                                   ifelse(dominio=="11", "AMAZONIA RURAL",
                                                                                        ifelse(dominio=="12" & area =="1", 
                                                                                            "INSULAR URBANO","INSULAR RURAL"))))))))))))) %>% 
  select(nacional, 1:length(yk))

# base1 <- proyeccion %>% group_by(area) %>% summarise(ago21= sum(ago_21),
#                                                      sep21= sum(sep_21),
#                                                      oct21= sum(oct_21),
#                                                      nov21= sum(nov_21),
#                                                      dic21= sum(dic_21),
#                                                      ene22= sum(ene_22),
#                                                      feb22= sum(feb_22),
#                                                      mar22= sum(mar_22),
#                                                      abr22= sum(abr_22),
#                                                      may22= sum(may_22),
#                                                      jun22= sum(jun_22),
#                                                      jul22= sum(jul_22),
#                                                      ago22= sum(ago_22),
#                                                      sep22= sum(sep_22),
#                                                      oct22= sum(oct_22),
#                                                      nov22= sum(nov_22),
#                                                      dic22= sum(dic_22),
#                                                      ene23= sum(ene_23),
#                                                      feb23= sum(feb_23),
#                                                      mar23= sum(mar_23),
#                                                      abr23= sum(abr_23),
#                                                      may23= sum(may_23),
#                                                      jun23= sum(jun_23))
#
# guardar las proyecciones poblacionales por grupos de dominio mensuales 
# #
 saveRDS(proyeccion, "productos/proyeccion_dom_sep21.rds")
 export(yk, "productos/yk_sep21.xlsx")
 export(base1, "productos/proyeccion_area_ago_dic23.xlsx")
 
 export(yk, "productos/proyeccion_dom13_5gedad_agos21.xlsx")
#  #


