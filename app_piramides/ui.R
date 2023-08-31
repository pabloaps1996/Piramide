#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library (shinythemes)

# 
fluidPage(
  theme = shinytheme("readable"),
  titlePanel("Pirámides Poblacionales"),
  fluidRow (
    column (5 ,offset = 1,
            wellPanel (
            selectInput("pir",
                        "Escoja ",
                        selected = "Escoja la desagregación que desea",
                        choices=c("Nacional", "Urbano", "Rural", "Costa","Sierra","Amazonia","Insular")),
            plotOutput('graf')
        
    )),
    column (5 ,offset = 1,
            wellPanel (
              selectInput("pir2",
                          "Escoja ",
                          selected = "Escoja la desagregación que desea",
                          choices=c("Nacional", "Urbano", "Rural", "Costa","Sierra","Amazonia","Insular")),
              plotOutput('graf2')
))))



# # Ejemplo 3 pestañas
# fluidPage(
#   theme = shinytheme("flatly"),
#   titlePanel("Pirámides Poblacionales"),
#   mainPanel (
#     tabsetPanel (
#       tabPanel (selectInput("pir",
#                             "Escoja ",
#                             selected = "Escoja la desagregación que desea",
#                             choices=c("Nacional", "Urbano", "Rural", "Costa","Sierra","Amazonia","Insular")),
#                 plotOutput('graf')),
#       tabPanel (selectInput("pir",
#                             "Escoja ",
#                             selected = "Escoja la desagregación que desea",
#                             choices=c("Nacional", "Urbano", "Rural", "Costa","Sierra","Amazonia","Insular")),
#                 plotOutput('graf2')),
#   )))







# Ejemplo 2
# fluidPage(
#   theme = shinytheme("flatly"),
#   titlePanel("Pirámides Poblacionales"),
#   fluidRow (
#     column (3 ,
#             selectInput("pir",
#                         "Escoja ",
#                         selected = "Escoja la desagregación que desea",
#                         choices=c("Nacional", "Urbano", "Rural")),
#             plotOutput('graf')
#     ))
# )



  



#Ejemplo1
  # 
  # fluidPage(
  #   theme = shinytheme("flatly"),
  #   titlePanel("Pirámides Poblacionales"),
  #   selectInput("pir",
  #               "Escoja ",
  #               selected = "Escoja la desagregación que desea",
  #               choices=c("Nacional", "Urbano", "Rural")),
  #   plotOutput('graf')
  # )