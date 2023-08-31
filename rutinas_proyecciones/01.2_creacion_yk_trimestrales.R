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
base <- read_rds("productos/td_tpp_07_24.rds")
#
# nombres con los meses y años 
#
nombres <- colnames(base [89:214])

meses <- c("01", "02", "03", "04", "05",  "06", "07", "08", "09", "10", "11", "12")
anios <- str_pad(as.character(2014:2024), 4, "left", "0")

comb <- expand.grid(anios=anios, meses=meses)
comb_v <- paste0(comb$anios, comb$meses)
comb_v <- comb_v[order(as.numeric(comb_v))]
comb_v <- comb_v[1:126]
#
# proyecciones poblacionales trimestrales
#
for (i in 1:length(nombres)) {
  
  loli <- tot_pob(bp = base,
                  dominio = "dom7",
                  si.area = T,
                  si.sexo = T,
                  gedad = c(0, 14, 15, 64, 65, 100),
                  anio = nombres[i]) %>% 
    rename(dominio=dom7, p02=sexo) 
  colnames(loli)[5]<- comb_v[i]
  
  
  if(i == 1){
    proyeccion_t <- loli[1:5]
  }
  else{
    loli <- loli[5]
    proyeccion_t <- cbind(proyeccion_t,loli)
    
  }
}
#
# guardar las proyecciones poblacionales por grupos de dominio trimestrales
#
#  saveRDS(proyeccion_t, "productos/enemdu_trimestral_42grupos_20_24.rds")
#  export(proyeccion_t, "productos/enemdu_trimestral_42grupos_20_24.xlsx")

