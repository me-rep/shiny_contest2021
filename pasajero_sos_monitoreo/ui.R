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
library(shinyjs)#para notificaciones
library(RMySQL)
library(DT)#for show dt
library(dplyr)#for filter dt to map
#library(shinyWidgets)#for styled checkbox | actually not used
library(shinycssloaders) #for spinner
library(hms)#make the new time =actual - two 
library(stringr)#for str_split 
#library(htmltools)
####### to kill connections
killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  
  #titlePanel(h1(align="center",tags$em(p("Monitoreo de asaltos en buses@Tiempo Real",class='fa fa-cloud',align="middle",style="background-color:#97bddb;color:#fff;height:75px;")))),
  #titlePanel(h1(id="title","Sistema Pasajero_SOS:Monitoreo")),
  titlePanel("Sistema Pasajero_SOS:Monitoreo"),
  tags$head(
    tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
               .inline .form-group{display: table-row;}"),
    #tags$style("#toggleAccessGroup{padding:0px;text-align:center;}"),
    #tags$style("#toggleAccessGroup{vertical-algin:middle;text-align:center;}"),
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
      #box(
        #useShinyjs(),
        #checkboxInput("dha", "Drop Higher Accuracy", FALSE),
        #useShinyjs(),
        #checkboxInput("dpu", "Drop PopUps", FALSE)),
        #helpText("Bienvenido!")
        #)
      #h1(id="title","HOLa"),
      h1(tags$em(p("Monitoreo de asaltos en buses@Tiempo Real",class='fa fa-cloud',align="middle",style="background-color:#97bddb;color:#fff;height:75px;"))),
      #h1(align="center",tags$em(p("Monitoreo de asaltos en buses@Tiempo Real",class='fa fa-cloud',align="middle",style="background-color:#97bddb;color:#fff;height:75px;")))
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
      #box(
      #    useShinyjs(),
      #    checkboxInput("dha", "Quitar alta Accuracy", FALSE),
      #    useShinyjs(),
      #    checkboxInput("dpu", "Quitar PopUps", FALSE))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #h1(tags$em(p("Monitoring theft on bus@Real Time",class='fa fa-cloud',align="middle",style="background-color:#97bddb;color:#fff;height:75px;"))),

      useShinyjs(),
      checkboxInput("dha", "Quitar Accuracy grande", FALSE),
      #useShinyjs(),
      #checkboxInput("dpu", "Quitar PopUps", FALSE),
      #shinyjs::hidden(div(id="alc1",
      uiOutput("alm_signs"),
      leafletOutput("mon_map")#%>%withSpinner(color="#00394d",type=4)
      #))
      
    )
  )
))
