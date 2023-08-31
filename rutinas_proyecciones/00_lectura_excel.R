#
rm(list=ls())
#
# librerias
#
library(readxl)
library(lazyeval)
library(rio)
library(glue)
library(tidyverse)
#
# funcion
#
source("rutinas_proyecciones/funciones/tot_pob.R")
#
# mes y años de las proyecciones
#
meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
anios <- str_pad(as.character(23:24), 2, "left", "0")
#anios <- str_pad(as.character(7:24), 2, "left", "0")

comb <- expand.grid(meses=meses, anios=anios)
comb_v <- paste0(comb$meses, "_", comb$anios)

nombres <- comb_v[7:16]
ls <- as.numeric(length(nombres))

rm(meses, anios, comb, comb_v)
#
# dominios de proyeccion: provincia por sexo, area urbana y rural
#
dp <- str_pad(as.character(c(1:24, 25:29)), 2, "left", "0")
sexo <- c("1", "2") # arriba-abajo
area <- c("1", "2") # izquierda-derecha

dom1 <-  expand.grid(dp=dp[1:24], sexo=sexo, area=area)
dom2 <-  expand.grid(dp=dp[25:29], sexo=sexo, area=area[1])

dom_v1 <- paste0(dom1$dp, "-", dom1$area, "-", dom1$sexo)
dom_v1 <- dom_v1[order(dom_v1)] # prov-area-sexo

dom_v2 <- paste0(dom2$dp, "-", dom2$area, "-", dom2$sexo)
dom_v2 <- dom_v2[order(dom_v2)] # 5ciudades-area-sexo

rm(sexo, area, dom1, dom2)
#
# rangos de lectura en los archivos excel por provincia 
#
rangos <- c("O5:X105", "O111:X211", "AA5:AJ105", "AA111:AJ211") # UH-UM-RH-RM jun-dic 2023
#
# rangos de lectura en los archivos excel por 5 ciudades 
#
rangos1 <- c("O5:X105", "O111:X211")# UH-UM jun-dic 2023
#
# lectura de la base de datos de proyecciones
#
n <- 24*2*2 # provincias
pp <- vector("list", n)

n1 <- 5*2*2 # 5 ciudades
pp1 <- vector("list", n1)

for(kp in 1:24){
  for(kr in 1:length(rangos)){
    
    k <- kr + (kp-1)*length(rangos)
    print(paste0(kp,"-",kr,"-",k))
    
    # lectura de las proyecciones
    pp[[k]] <- read_excel(path = "insumos/Proyecciones_mensuales_ultimas.xlsx",
                          sheet = kp+1,
                          range = rangos[kr],
                          col_names = nombres) %>% 
      mutate(nac = "00",
             dp = dp[kp],
             area = ifelse(kr<=2, "1", "2"),
             sexo = ifelse(kr %in% c(1, 3), "1", "2"),
             edad = 0:100) %>% 
      select(nac, dp, area, sexo, edad, 1:all_of(ls))
    # asignacion de nombres a las listas
    names(pp)[k] <- dom_v1[k]
    
  }
}

for(kp in 25:29){  
  
  for(kr in 1:length(rangos1)){
    
    k <- kr + (kp-1)*length(rangos1)
    print(paste0(kp,"-",kr,"-",k))
    
    # lectura de las proyecciones
    pp1[[k]] <- read_excel(path = "insumos/Proyecciones_mensuales_ultimas.xlsx",
                           sheet = kp+1,
                           range = rangos1[kr],
                           col_names = nombres)%>% 
      mutate(nac = "00",
             dp = dp[kp],
             area = ifelse(kr<=2, "1", "2"),
             sexo = ifelse(kr %in% c(1, 3), "1", "2"),
             edad = 0:100) %>% 
      select(nac, dp, area, sexo, edad, 1:all_of(ls))
    
    # asignacion de nombres a las listas
    names(pp1)[k] <- dom_v2[k]
  }
}

#
# proyecciones
#
td_pp1 <- do.call(rbind.data.frame, pp) # provincias
td_pp2 <- do.call(rbind.data.frame, pp1) # 5 ciudades

td_pp <- rbind(td_pp1,td_pp2) %>% 
  mutate(rnatura = ifelse(dp %in% c("01", "02", "03", "04", "05", "06", "10", "11", "17", "18", "23", "25", "27", "29"), "1",
                          ifelse(dp %in% c("07", "08", "09", "12", "13", "24", "26", "28"), "2",
                                 ifelse(dp %in% c("14", "15", "16", "19", "21", "22"), "3", "4"))),
         dom12 = ifelse(dp=="25" & area=="1", "01",
                        ifelse(dp=="26" & area=="1", "02",
                               ifelse(dp=="27" & area=="1", "03",
                                      ifelse(dp=="28" & area=="1", "04",
                                             ifelse(dp=="29" & area=="1", "05",
                                                    ifelse(rnatura=="1" & !(dp %in% c("25", "27", "29")) & area=="1", "06",
                                                           ifelse(rnatura=="2" & !(dp %in% c("26", "28")) & area=="1", "07",
                                                                  ifelse(rnatura=="3" & area=="1", "08",
                                                                         ifelse(rnatura=="1"  & area=="2", "09",
                                                                                ifelse(rnatura=="2" & area=="2", "10",
                                                                                       ifelse(rnatura=="3" & area=="2", "11", "12"))))))))))))  

# td_pp <- td_pp %>% 
#   select(nac, rnatura, dp, dom12, area, sexo, edad, 8:all_of(length(td_pp)))  # desde ago21

td_pp <- td_pp %>% 
  select(nac, rnatura, dp, dom12, area, sexo, edad, 5:all_of(length(td_pp))) 

rm(kp, kr, k, n, n1, dom_v1, dom_v2, dp, rangos, rangos1, td_pp1, td_pp2, pp, pp1)
names(td_pp)
td_pp<- td_pp %>% select(nac,rnatura,dp,dom12,area,sexo,edad,ago_23)
#
# guardar las proyecciones
#
saveRDS(td_pp, "productos/td_pp.rds")
#export(td_pp, "productos/td_pp_ago-dic23.xlsx")

