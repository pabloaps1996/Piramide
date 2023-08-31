library(tidyverse)
library(rio)
# importar linelist de casos ## muestra
linelist <- import("insumos/Personas_Confidencial.sav")  %>% filter(Rn=="4")
#%>% 


  linelist <- linelist %>% mutate(age_cat5=cut(linelist$p03, breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,100),
                                       labels = c("0-4","5-9", "10-14", "15-19", "20-24",
                                                  "25-29","30-34", "35-39", "40-44", "45-49",
                                                  "50-54", "55-59", "60-64", "65-69", "70-74",
                                                  "75-79", "80-84", "85+")),
                                gender=ifelse(p02=="1","m","f"))



  pop2<- import("productos/td_pp.rds") %>% filter(rnatura=="4")  
  
  #%>% 

pop2 <- pop2 %>% mutate(age_cat5=cut(pop2$edad, breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,100),
                                       labels = c("0-4","5-9", "10-14", "15-19", "20-24",
                                                  "25-29","30-34", "35-39", "40-44", "45-49",
                                                  "50-54", "55-59", "60-64", "65-69", "70-74",
                                                  "75-79", "80-84", "85+"))) %>% select(age_cat5,sexo,ago_23)


pop2<-pop2 %>% group_by(age_cat5,sexo) %>% summarise(n=sum(ago_23))

pop2<-pop2 %>% pivot_wider(names_from = sexo, values_from = n)

names(pop2)<-c("age_cat5","m","f")

# registrar correctamente los niveles de las categorías de edad
age_levels <- c("0-4","5-9", "10-14", "15-19", "20-24",
                "25-29","30-34", "35-39", "40-44", "45-49",
                "50-54", "55-59", "60-64", "65-69", "70-74",
                "75-79", "80-84", "85+")


# crear/transformar datos de población, con porcentaje del total
################################################################
pop_data <- pop2 %>% 
  pivot_longer(      # pivota largo las columnas de género
    cols = c(m, f),
    names_to = "gender",
    values_to = "counts") %>% 
  mutate(
    percent  = round(100*(counts / sum(pop2$m+pop2$f)),1),  # % of total
    percent  = case_when(
      gender == "f" ~ percent,
      gender == "m" ~ -percent,               # si es hombre, convierte el % en negativo
      TRUE          ~ NA_real_))


# crear datos de casos por edad/género, con porcentaje del total
################################################################
case_data <- linelist %>%
  count(age_cat5, gender, name = "counts") %>%   # recuentos por grupos de edad/género
  ungroup() %>% 
  mutate(
    percent = round(100*(counts / sum(counts, na.rm=T)),1),  # calcula el % del total por grupos de edad-género
    percent = case_when(                                     # convierte % en negativo si es hombre
      gender == "f" ~ percent,
      gender == "m" ~ -percent,
      TRUE          ~ NA_real_))

# combina datos de casos y de población (mismos nombres de columna, valores de age_cat y valores de género)
pyramid_data_I<- bind_rows("cases" = case_data, "population" = pop_data, .id = "data_source") %>%  mutate(Piramide="Insular")
summary(case_data$percent)
summary(pop_data$percent)


export(pyramid_data_I, "pyramid_data_I.rds")

# 
# pyramid_data<-pyramid_data_R
# # Define la extensión del eje porcentual, utilizado para los límites del gráfico
# max_per <- max(pyramid_data$percent, na.rm=T)
#   min_per <- min(pyramid_data$percent, na.rm=T)
# # comienza ggplot
# ##############
# ggplot()+  # el eje-x por defecto es la edad en años;
#   
#   # gráfico de datos de población
#   geom_col(
#     data = pyramid_data %>% filter(data_source == "population"),
#     mapping = aes(
#       x = age_cat5,
#       y = percent,
#       fill = gender),
#     colour = "black",                               # color negro alrededor de las barras
#     alpha = 0.2,                                    # más transparente
#     width = 1)+                                     # anchura completa
#   
#   # gráfico de datos de casos
#   geom_col(
#     data = pyramid_data %>% filter(data_source == "cases"), 
#     mapping = aes(
#       x = age_cat5,                               # categorías de edad como eje-X original
#       y = percent,                                # % como eje-Y original
#       fill = gender),                             # relleno de barras por género
#     colour = "black",                               # color negro alrededor de las barras
#     alpha = 1,                                      # no transparente 
#     width = 0.3)+                                   # mitad anchura
#   
#   # invierte los ejes X e Y para hacer la pirámide vertical
#   coord_flip()+
#   
#   # asegura manualmente que el eje de edad está ordenado correctamente
#   scale_x_discrete(limits = age_levels)+     # definido en el trozo (chunk) anterior
#   
#   # establecer el eje de porcentajes
#   scale_y_continuous(
#     limits = c(min_per, max_per),                                          # min y max definidos arriba
#     breaks = seq(floor(min_per), ceiling(max_per), by = 2),                # de min% a max% por 2  
#     labels = paste0(                                                       # para las etiquetas, pegar juntas...  
#       abs(seq(floor(min_per), ceiling(max_per), by = 2)), "%"))+                                                  
#   
#   # designar colores y etiquetas de leyenda manualmente
#   scale_fill_manual(
#     values = c("f" = "orange",         # asigna colores a los valores de los datos
#                "m" = "darkgreen"),
#     labels = c("f" = "Femenino",
#                "m"= "Masculino"),      # cambia las etiquetas que aparecen en la leyenda, observa el orden
#   ) +
#   
#   # etiquetas, títulos y pies de foto 
#   labs(
#     title = "Piramide poblacional ENEMDU (Proyecciones y Muestra)",
#     subtitle = "",
#     x = "Categorias de edad",
#     y = "Porcentaje total",
#     fill = NULL)+
#   # temas estéticos opcionales
#   theme(
#     legend.position = "bottom",                             # mueve la leyenda hacia abajo
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(colour = "black"),
#     plot.title = element_text(hjust = 0), 
#     plot.caption = element_text(hjust=0, size=11, face = "italic"))
