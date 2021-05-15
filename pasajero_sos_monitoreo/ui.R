#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(RMySQL)
library(DT)
library(dplyr)
library(shinycssloaders) 
library(hms)
library(stringr)


shinyUI(fluidPage(
  
  # Application title
  
  
  titlePanel("Sistema Pasajero_SOS:Monitoreo"),
  tags$head(
    tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
               .inline .form-group{display: table-row;}"),
    
    tags$style(
      HTML('#title {
           color: black;
           background-color:#97bddb;
           font-size: 20px;
           font-style: bold;
           }')),
    tags$link(rel="icon",sizes="196x196" ,href="head_image.PNG"),
    
    tags$script(src='get_info_mon.js')
    #
    ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(id="title",
      
      h1(tags$em(p("Monitoreo de asaltos en buses@Tiempo Real",class='fa fa-cloud',align="middle",style="background-color:#97bddb;color:#fff;height:75px;"))),
      
       # ,
      
      
      actionButton("toggleAccessGroup",label = "Oprimir para Continuar",icon =icon("map","fa-1x",lib="font-awesome"),class = "btn btn-warning"),
      #id-card | map
      shinyjs::hidden(div(id="alc0",
                          passwordInput("dpi","Codigo de Ingreso:"),                
        #actionButton("lof",label = "Salir",icon =icon("power-off","fa-1x",lib="font-awesome"),class = "btn btn-light btn-x"),#btn-sm btn-xs
        box(
          title = "Alertas actuales", status = "warning",
          #"Box content here", br(), "More box content",
          actionButton("ala",label = "Mostrar ubicaciones",icon =icon("bus","fa-2x"),class = "btn btn-info btn-sm")#btn-sm btn-xs
          #icon =icon("road",lib="glyphicon") this could be uses instead of the bus on the above line
        )#,
        ,
        
        actionButton("lof",label = "Salir",icon =icon("power-off","fa-1x",lib="font-awesome"),class = "btn btn-secondary btn-xs")#btn-sm btn-xs

      ))
      
    ),
    
    
    mainPanel(
      #h1(tags$em(p("Monitoring theft on bus@Real Time",class='fa fa-cloud',align="middle",style="background-color:#97bddb;color:#fff;height:75px;"))),

      useShinyjs(),
      checkboxInput("dha", "Quitar Accuracy grande", FALSE),
      
      uiOutput("alm_signs"),
      leafletOutput("mon_map")#%>%withSpinner(color="#00394d",type=4)
      #))
      
    )
  )
))
