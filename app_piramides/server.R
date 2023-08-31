#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gapminder)
library(tidyverse)
library(rio)
library(ggplot2)



base_A<-import("pyramid_data_A.rds")
base_C<-import("pyramid_data_C.rds")
base_I<-import("pyramid_data_I.rds")
base_N<-import("pyramid_data_N.rds")
base_R<-import("pyramid_data_R.rds")
base_S<-import("pyramid_data_S.rds")
base_U<-import("pyramid_data_U.rds")

base<- rbind(base_A,base_C,base_I,base_N,base_R,base_S,base_U)%>% rename(pir=Piramide) %>% mutate(pir2=pir)


#base<-import("base_final.xlsx") %>% rename(pir=Piramide) %>% mutate(pir2=pir)
function(input, output){
  output$graf <- renderPlot({
  #-------------------PREPROCESAMIENTO----------------  
    
      base <- base %>%  filter(pir == input$pir)
      age_levels <- c("0-4","5-9", "10-14", "15-19", "20-24",
                      "25-29","30-34", "35-39", "40-44", "45-49",
                      "50-54", "55-59", "60-64", "65-69", "70-74",
                      "75-79", "80-84", "85+")
      max_per <- max(base$percent, na.rm=T)
      min_per <- min(base$percent, na.rm=T)
      
    
    
  #---------------------CREACIÓN GRÁFICO---------------
    ggplot()+  # el eje-x por defecto es la edad en años;
      # gráfico de datos de población
      geom_col(
        data = base %>% filter(data_source == "population"),
        mapping = aes(
          x = age_cat5,
          y = percent,
          fill = gender),
        colour = "black",                               # color negro alrededor de las barras
        alpha = 0.2,                                    # más transparente
        width = 1)+                                     # anchura completa
      
      # gráfico de datos de casos
      geom_col(
        data = base %>% filter(data_source == "cases"), 
        mapping = aes(
          x = age_cat5,                               # categorías de edad como eje-X original
          y = percent,                                # % como eje-Y original
          fill = gender),                             # relleno de barras por género
        colour = "black",                               # color negro alrededor de las barras
        alpha = 1,                                      # no transparente 
        width = 0.3)+                                   # mitad anchura
      
      # invierte los ejes X e Y para hacer la pirámide vertical
      coord_flip()+
      
      # asegura manualmente que el eje de edad está ordenado correctamente
      scale_x_discrete(limits = age_levels)+     # definido en el trozo (chunk) anterior
      
      # establecer el eje de porcentajes
      scale_y_continuous(
        limits = c(min_per, max_per),                                          # min y max definidos arriba
        breaks = seq(floor(min_per), ceiling(max_per), by = 2),                # de min% a max% por 2  
        labels = paste0(                                                       # para las etiquetas, pegar juntas...  
          abs(seq(floor(min_per), ceiling(max_per), by = 2)), "%"))+                                                  
      
      # designar colores y etiquetas de leyenda manualmente
      scale_fill_manual(
        values = c("f" = "orange",         # asigna colores a los valores de los datos
                   "m" = "darkgreen"),
        labels = c("f" = "Femenino",
                   "m"= "Masculino"),      # cambia las etiquetas que aparecen en la leyenda, observa el orden
      ) +
      
      # etiquetas, títulos y pies de foto 
      labs(
        title = "Piramide poblacional ENEMDU (Proyecciones y Muestra)",
        subtitle = "",
        x = "Categorias de edad",
        y = "Porcentaje total",
        fill = NULL)+
      # temas estéticos opcionales
      theme(
        legend.position = "bottom",                             # mueve la leyenda hacia abajo
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0), 
        plot.caption = element_text(hjust=0, size=11, face = "italic"))
  }) 
  output$graf2 <- renderPlot({
    #-------------------PREPROCESAMIENTO----------------  
    
    base <- base %>%  filter(pir2 == input$pir2)
    age_levels <- c("0-4","5-9", "10-14", "15-19", "20-24",
                    "25-29","30-34", "35-39", "40-44", "45-49",
                    "50-54", "55-59", "60-64", "65-69", "70-74",
                    "75-79", "80-84", "85+")
    max_per <- max(base$percent, na.rm=T)
    min_per <- min(base$percent, na.rm=T)
    
    
    
    #---------------------CREACIÓN GRÁFICO---------------
    ggplot()+  # el eje-x por defecto es la edad en años;
      # gráfico de datos de población
      geom_col(
        data = base %>% filter(data_source == "population"),
        mapping = aes(
          x = age_cat5,
          y = percent,
          fill = gender),
        colour = "black",                               # color negro alrededor de las barras
        alpha = 0.2,                                    # más transparente
        width = 1)+                                     # anchura completa
      
      # gráfico de datos de casos
      geom_col(
        data = base %>% filter(data_source == "cases"), 
        mapping = aes(
          x = age_cat5,                               # categorías de edad como eje-X original
          y = percent,                                # % como eje-Y original
          fill = gender),                             # relleno de barras por género
        colour = "black",                               # color negro alrededor de las barras
        alpha = 1,                                      # no transparente 
        width = 0.3)+                                   # mitad anchura
      
      # invierte los ejes X e Y para hacer la pirámide vertical
      coord_flip()+
      
      # asegura manualmente que el eje de edad está ordenado correctamente
      scale_x_discrete(limits = age_levels)+     # definido en el trozo (chunk) anterior
      
      # establecer el eje de porcentajes
      scale_y_continuous(
        limits = c(min_per, max_per),                                          # min y max definidos arriba
        breaks = seq(floor(min_per), ceiling(max_per), by = 2),                # de min% a max% por 2  
        labels = paste0(                                                       # para las etiquetas, pegar juntas...  
          abs(seq(floor(min_per), ceiling(max_per), by = 2)), "%"))+                                                  
      
      # designar colores y etiquetas de leyenda manualmente
      scale_fill_manual(
        values = c("f" = "orange",         # asigna colores a los valores de los datos
                   "m" = "darkgreen"),
        labels = c("f" = "Femenino",
                   "m"= "Masculino"),      # cambia las etiquetas que aparecen en la leyenda, observa el orden
      ) +
      
      # etiquetas, títulos y pies de foto 
      labs(
        title = "Piramide poblacional ENEMDU (Proyecciones y Muestra)",
        subtitle = "",
        x = "Categorias de edad",
        y = "Porcentaje total",
        fill = NULL)+
      # temas estéticos opcionales
      theme(
        legend.position = "bottom",                             # mueve la leyenda hacia abajo
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0), 
        plot.caption = element_text(hjust=0, size=11, face = "italic"))
  })
  
}



