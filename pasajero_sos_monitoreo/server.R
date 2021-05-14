#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(dbplyr)



assaults_reported_df<- data.frame(c_id=4,n_reg=76543211,lat=14.595995,lon=-90.5180138,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.102",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE)
assaults_reported_df<-bind_rows(assaults_reported_df,data.frame(c_id=5,n_reg=76543211,lat=14.595107,lon=-90.517896,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
assaults_reported_df<-bind_rows(assaults_reported_df,data.frame(c_id=6,n_reg=76543211,lat=14.5947822,lon=-90.5177992,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))


#con <-  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#copy_to(con, assaults_reported_df,"assaults_reported_df",temporary =FALSE)
createConnection <- function() {
  cona <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  copy_to(cona, assaults_reported_df,"assaults_reported_df",temporary =FALSE)
  return(cona)
  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  shinyjs::hide("dha")
  #shinyjs::hide("dpu")
  ###global variables
  df_download_r<-reactiveVal(data.frame()) # 
  numberOfRous<- reactiveValues(nrf = 0)
#
  shinyjs::onclick("toggleAccessGroup",
                   shinyjs::toggle(id = "alc0", anim = TRUE))
#
#
  observe({
  if(input$dpi=="sh-2021"){shinyjs::enable("ala")
    shinyjs::enable("lof")

    }
    else { shinyjs::disable("ala")  
      shinyjs::disable("lof")

      }
  })
#
  observeEvent(input$lof,{stopApp()})
#
  
  observeEvent(input$ala,{ 
    req(input$dpi)
    isolate(input$dpi)
    #2two option
    #if(input$rb=="2two"){
      #library(lubridate)
      print(paste("printing time:",input$ttd))
      #print(paste("with input example:",input$example))
      ###
      
      ###
      
      
      if(length(input$ttd)>0){ 
        print(paste("printing time:",input$ttd))
        ##
        time_limit<-unlist(str_split(input$ttd,":"))
        new_time=vector()
        for(i in 1:3)
        {
          if(i==1){
            #if(as.numeric(time_limit[i])==1){
            #  print("idenitied 00")
            # new_time[1]<-23}
            #else{new_time[i]<-as.numeric(time_limit[i])-2}
            new_time[i]<-as.numeric(time_limit[i])-2
            #print(new_time)
          }
          else{new_time[i]<-time_limit[i]}
          if(i==3){
            #print(paste(new_time,collapse = ":"))
            start_time<-paste(new_time,collapse = ":")
            #print(new_time2)
          }
          #print(new_time)
        }
        ##
        print(paste("printing start time:",start_time))
        print(paste("printing time minus 2 hours:",as.hms(start_time)))
        #start_time<-as.hms(start_time)
        
        #df_download_r(download_today_db("test",input$dtd))
        if(length(input$dtd)>0){
          print("entrando al if del download de la db")
          
          date_limit<-unlist(str_split(input$dtd,"/"))
          new_date=vector()
          for(i in 1:3)
          {
            if(i==1){
              #if(as.numeric(date_limit[i])<=9){
              print("idenitied 00")
              print(as.numeric(date_limit[i]))
              new_date[1]<-as.numeric(date_limit[i])
              #}
              #else{new_date[i]<-as.numeric(date_limit[i])}
              #new_date[i]<-as.numeric(time_limit[i])-2
              #print(new_time)
            }
            else{new_date[i]<-date_limit[i]}
            if(i==2){new_date[i]<-as.numeric(date_limit[i])}
            if(i==3){
              #print(paste(new_time,collapse = ":"))
              start_date<-paste(new_date,collapse = "/")
              print(start_date)
            }
            #print(paste("new date is:",start_date))
          }
          ##
          
          print(paste("date is:",input$dtd))
          #temp_download_two<-download_today_db("test",input$dtd)
          print(start_date)
          temp_download_two<-download_today_db(start_date) 
          #temp_download_two<-download_today_db("test","19/3/2020")
          #temp_download_two%>%filter(time>=start_time)
          df_download_r(temp_download_two%>%filter(as.hms(time)>=as.hms(start_time)))
          print(df_download_r())
          #Adding elapsed_time in order to get states on alerts
          temp_color_two<-df_download_r()%>%mutate(elapsed_time=as.numeric(difftime(as.hms(input$ttd),as.hms(time),units="min")))
          #test_dfh<-df%>%mutate(elapsed_time=as.numeric(difftime(as.hms("15:49:00"),as.hms(as.character(Name)),units="min")))
          temp_color_two$elapsed_state<-put_Color_State(temp_color_two)
          temp_color_two$elapsed_image<-put_Image_State(temp_color_two)
          print(temp_color_two)
          df_download_r(temp_color_two)
          #
          
          if(nrow(df_download_r())==0)shinyjs::alert("No hay alarmas al momento.")#showNotification("No hay alarmas al momento.")
        }
        
      }
      
      print("Last two hours")
    #}
  
  }) 
#
  
  
  
  
  output$mon_map <- renderLeaflet({
    req(input$ala)
    isolate(input$ala)
    req(input$dpi)
    isolate(input$dpi)
    
    if(input$dpi!="sh-2021" ){shinyjs::hide("dha") 
      #shinyjs::hide("dpu")
      }
    if(input$dpi=="sh-2021" ){
    #####BusIcon <- makeIcon(
    #####  iconUrl = "https://i.dlpng.com/static/png/3880704-red-bus-icon-png-png-image-red-bus-png-256_256_preview.png",
      #iconWidth = 38, iconHeight = 95,
      #"bus1.PNG", lo sustitui por el bus rojo con la linea de abajo
      #"bus_poster2.PNG",# i should to comment this line and replace it with the http from begining iconURL
      ####iconWidth = 60, iconHeight = 80
    #####    iconWidth = 40, iconHeight = 40
      #iconAnchorX = 22, iconAnchorY = 94
      #shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      #shadowWidth = 50, shadowHeight = 64,
      #shadowAnchorX = 4, shadowAnchorY = 62
    #####  )
    BusIcon<-makeIcon(iconUrl = "bus_poster2.png",
                      iconWidth =  30, 
                      iconHeight =  30)#,
    mensaje <- paste(sep = "<br/>",
                     "<b><font color='red'>Ubicacion del bus::</font></b> ",
                     "<img src='general_state_icon.PNG' style='width:20px;height:20px;'> "
                     
    )
    ###added feature, from watch_pos to here, in order to connect the 3 gps points recorded 19march 2020 9:56pm
    signIcons <- iconList(
      bus = makeIcon(iconUrl = "bus_poster2.png",iconWidth =  30,iconHeight =  30),
      #makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
      bus_nst = makeIcon(iconUrl = "general_state_icon.png",iconWidth =  15,iconHeight = 15)
      #bus_nst = makeIcon(iconUrl = "hole_icon.PNG",iconWidth =  10,iconHeight = 10)#iconWidth =  15,iconHeight = 15
      #makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
    )
    ###
    if(nrow(df_download_r())==0){ assaults_map<- leaflet() %>%addTiles() %>% setView(lng = -90.513083, lat = 15.7418237, zoom = 7) %>%addGraticule(interval = 5, style = list(color = "#FF0000", weight = 3))
    }else{ 
      #df_top_assaults<-tail(df_download_r(),60)%>%filter(c_accuracy<600)
      #Not selecte any checkbox  
      if(!input$dha){
        df_top_assaults<-df_download_r()
        #df_top_assaults%>%filter(c_accuracy<600)
        #print(df_top_assaults)
        
        ###added feature, from watch_pos to here, in order to connect the 3 gps points recorded 19march 2020 9:56pm
        #names(df_top_assaults)<- c("c_id","n_reg","lat","lon","u_g","ip","time","dt","br_name","version","cookies","language","online","plat","c_name","c_accuracy","heading","c_altitud","c_altitud_acc","c_speed","scr_h","scr_w","scr_aval_h","scr_aval_w","scr_cd","scr_pd","sys_inf")
        print(names(df_top_assaults))
        df_top_assaults$micon<-as.factor(ifelse(df_top_assaults$c_id==4,"bus","bus_nst"))
        print(str(df_top_assaults))
        df_top_assaults$speed_km<-ifelse(df_top_assaults$c_speed!="null",as.numeric(df_top_assaults$c_speed)*3.6,NA)
        print("VOY X AQUI")
        df_top_assaults<-df_top_assaults%>%mutate(to_show=paste(c_id,speed_km,"km/h",sep = " "))
        df_top_assaults$time<-as.factor(df_top_assaults$time)
        
        df.tempp<-  split(df_top_assaults, df_top_assaults$dt)
        
        #da_map<-leaflet(df.tempp) %>% addTiles()
        print("df.tempp printing")      
        print(df.tempp)

        assaults_map<-leaflet(df.tempp) %>% addTiles()
        names(df.tempp) %>%
          purrr::walk( function(df) {
            assaults_map <<-  assaults_map %>%addProviderTiles(providers$Stamen.TonerLines,options = providerTileOptions(opacity = 0.85)) %>%
              addMarkers(data=df.tempp[[df]],
                         lng=~lon, lat=~lat,
                         label=~as.character(time),
                         #popup=~as.character(id),
                         #popup=~as.character(to_show),
                         popup=paste(sep=" ","<strong>Ubicacion del bus:</strong>",df_top_assaults$lat,df_top_assaults$lon," Accuracy:",df_top_assaults$c_accuracy,"id:",df_top_assaults$n_reg,check_hour_color(df_top_assaults),df_top_assaults$time,"Fecha:",df_top_assaults$dt,"Km/h:",df_top_assaults$speed_km,"Heading:",df_top_assaults$heading,"<br/>",check_image_color(df_top_assaults),whatsup_Message(df_top_assaults)),
                         icon=~signIcons[micon],
                         group = df,
                         #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                         labelOptions = labelOptions(noHide = F,
                                                     direction = 'auto'))%>%
               
            #addPopups(data=df.tempp[[df]],lng=~lon, lat=~lat,paste(sep=" ",mensaje,df_top_assaults$lon,df_top_assaults$lat," Id:",df_top_assaults$n_reg,"Hora:",df_top_assaults$time),
             #                                                                        options = popupOptions(closeButton = TRUE))#%>%
              addCircles(data=df.tempp[[df]],lng=~lon, lat=~lat, radius = df_top_assaults$c_accuracy,color=check_elapsed(df_top_assaults))#%>%
              
              #addMarkers(data=df.tempp[[df]],lng=~lon, lat=~lat, popup=paste(sep=" ","<strong>Ubicacion del bus:</strong>",df_top_assaults$lat,df_top_assaults$lon," Accuracy:",df_top_assaults$c_accuracy,"id:",df_top_assaults$n_reg,check_hour_color(df_top_assaults),df_top_assaults$time,"Fecha:",df_top_assaults$dt,"<br/>",check_image_color(df_top_assaults),whatsup_Message(df_top_assaults)),icon = BusIcon,
              #           clusterOptions = markerClusterOptions())
              #addMarkers(data=df.tempp[[df]],lng=~lon, lat=~lat, popup=paste(sep=" ","<strong>Ubicacion del bus:</strong>",df_top_assaults$lat,df_top_assaults$lon," Accuracy:",df_top_assaults$c_accuracy,"id:",df_top_assaults$n_reg,check_hour_color(df_top_assaults),df_top_assaults$time,"Fecha:",df_top_assaults$dt,"<br/>",check_image_color(df_top_assaults),whatsup_Message(df_top_assaults)))
          })
        
        ###added feature, from watch_pos to here, in order to connect the 3 gps points recorded 19march 2020 10:44pm
        for(group in levels(df_top_assaults$time)){
          assaults_map = addPolylines(assaults_map,
                                lng= ~ lon,
                                lat= ~ lat,
                                data = df_top_assaults[df_top_assaults$time==group,],
                                color= ~elapsed_state,#"red",
                                stroke=0.2,
                                fillColor = "transparent",
                                label=~as.character(c_id),
                                weight = 3)
          
        }
        ###
        
        

      }
      #Selected Drop Higher Accuracy
      #if(input$dha){
      else{ 
        df_top_assaults<-df_download_r()%>%filter(c_accuracy<600)
        #df_top_assaults%>%filter(c_accuracy<600)
        #print(df_top_assaults)
        df_top_assaults$micon<-as.factor(ifelse(df_top_assaults$c_id==4,"bus","bus_nst"))
        df_top_assaults$speed_km<-ifelse(df_top_assaults$c_speed!="null",as.numeric(df_top_assaults$c_speed)*3.6,NA)
        df_top_assaults<-df_top_assaults%>%mutate(to_show=paste(c_id,speed_km,"km/h",sep = " "))
        df_top_assaults$time<-as.factor(df_top_assaults$time)
        
        df.tempp<-  split(df_top_assaults, df_top_assaults$dt)
        
        #da_map<-leaflet(df.tempp) %>% addTiles()
        print("df.tempp printing")      
        print(df.tempp)
        
        assaults_map<-leaflet(df.tempp) %>% addTiles()
        names(df.tempp) %>%
          purrr::walk( function(df) {
            assaults_map <<-  assaults_map %>%addProviderTiles(providers$Stamen.TonerLines,options = providerTileOptions(opacity = 0.85)) %>%
              addMarkers(data=df.tempp[[df]],
                         lng=~lon, lat=~lat,
                         label=~as.character(time),
                         #popup=~as.character(id),
                         #popup=~as.character(to_show),
                         popup=paste(sep=" ","<strong>Ubicacion del bus:</strong>",df_top_assaults$lat,df_top_assaults$lon," Accuracy:",df_top_assaults$c_accuracy,"id:",df_top_assaults$n_reg,check_hour_color(df_top_assaults),df_top_assaults$time,"Fecha:",df_top_assaults$dt,"Km/h:",df_top_assaults$speed_km,"Heading:",df_top_assaults$heading,"<br/>",check_image_color(df_top_assaults),whatsup_Message(df_top_assaults)),
                         icon=~signIcons[micon],
                         group = df,
                         #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                         labelOptions = labelOptions(noHide = F,
                                                     direction = 'auto'))%>%
              
              #addPopups(data=df.tempp[[df]],lng=~lon, lat=~lat,paste(sep=" ",mensaje,df_top_assaults$lon,df_top_assaults$lat," Id:",df_top_assaults$n_reg,"Hora:",df_top_assaults$time),
              #                                                                        options = popupOptions(closeButton = TRUE))#%>%
              addCircles(data=df.tempp[[df]],lng=~lon, lat=~lat, radius = df_top_assaults$c_accuracy,color=check_elapsed(df_top_assaults))#%>%
            
            #addMarkers(data=df.tempp[[df]],lng=~lon, lat=~lat, popup=paste(sep=" ","<strong>Ubicacion del bus:</strong>",df_top_assaults$lat,df_top_assaults$lon," Accuracy:",df_top_assaults$c_accuracy,"id:",df_top_assaults$n_reg,check_hour_color(df_top_assaults),df_top_assaults$time,"Fecha:",df_top_assaults$dt,"<br/>",check_image_color(df_top_assaults),whatsup_Message(df_top_assaults)),icon = BusIcon,
            #           clusterOptions = markerClusterOptions())
            #addMarkers(data=df.tempp[[df]],lng=~lon, lat=~lat, popup=paste(sep=" ","<strong>Ubicacion del bus:</strong>",df_top_assaults$lat,df_top_assaults$lon," Accuracy:",df_top_assaults$c_accuracy,"id:",df_top_assaults$n_reg,check_hour_color(df_top_assaults),df_top_assaults$time,"Fecha:",df_top_assaults$dt,"<br/>",check_image_color(df_top_assaults),whatsup_Message(df_top_assaults)))
          })
        
        ###added feature, from watch_pos to here, in order to connect the 3 gps points recorded 19march 2020 10:44pm
        for(group in levels(df_top_assaults$time)){
          assaults_map = addPolylines(assaults_map,
                                      lng= ~ lon,
                                      lat= ~ lat,
                                      data = df_top_assaults[df_top_assaults$time==group,],
                                      color= ~elapsed_state,#"red",
                                      stroke=0.2,
                                      fillColor = "transparent",
                                      label=~as.character(c_id),
                                      weight = 3)
          
        }
        

        
      }
      

      
      #
      shinyjs::show("dha") 
      #shinyjs::show("dpu")
      #
      
    }
    
    assaults_map #Print the map
    }
    #else{shinyjs::alert("Ingreso no autorizado.")}
    
  })

  
  output$alm_signs <- renderUI({
    nred<-nrow(df_download_r()%>%filter(elapsed_state=="red"))/3
    norange<-nrow(df_download_r()%>%filter(elapsed_state=="orange"))/3
    nblack<-nrow(df_download_r()%>%filter(elapsed_state=="black"))/3
    box(
      fluidRow(
        column(2,
               #h1("hola")
              # h1(tags$em(p(nred,class='fa fa-cloud',align="middle",style="background-color:#f45c43;color:#fff;")))
              h4(tags$em(p(nred,align="middle",style="background-color:#605C3C;color:#fff;background-image: url('red_state_icon.PNG');background-size: cover;background-position: center;background-repeat: no-repeat;")))
        ),
        column(3,
              # h1(tags$em(p(norange,class='fa fa-cloud',align="middle",style="background-color:#f7b733;color:#fff;")))
              h4(tags$em(p(norange,align="middle",style="background-color:#605C3C;color:#fff;background-image: url('orange_state_icon.PNG');background-size: cover;background-position: center;background-repeat: no-repeat;")))
        ),    
        column(4,
               #h1(tags$em(p(nblack,class='fa fa-cloud',align="middle",style="background-color:#605C3C;color:#fff;background-image: url('black_state_icon.PNG');background-size: cover;")))
               h4(tags$em(p(nblack,align="middle",style="background-color:#605C3C;color:#fff;background-image: url('black_state_icon.PNG');background-size: cover;background-position: center;background-repeat: no-repeat;")))

        )
      )
    )
    
  })
  ###Functions section
  dpu_function<-function(map_To_Work) 
  {
    return(map_To_Work %>% clearPopups())
    
  }
  put_Color_State <- function(df_to_work) {
    sapply(df_to_work$elapsed_time, function(elapsed_time) {
      if(elapsed_time > 0.0 & elapsed_time <= 30.99  ) {
        "red"
      } else if(elapsed_time > 30.9 &elapsed_time <= 60.99 ) {
        "orange"
      } else {
        "black"
      } })
  }
  put_Image_State<-function(df_to_work){
    
    sapply(df_to_work$elapsed_state, function(elapsed_state) {
      if(elapsed_state=="red") {
        "<img src='red_state_icon.PNG'>"
        
      } else if(elapsed_state=="orange" ) {
        "<img src='orange_state_icon.PNG'>" 
        
      } else {
        "<img src='black_state_icon.PNG'> "
        
      } })
    
  }
  
  #To put color to the circles radio | last two hour option
  check_elapsed<-function(df_to_work){
    print(colnames(df_to_work))
    if("elapsed_time"%in% colnames(df_to_work)){
      return(df_to_work$elapsed_state)
    }
    
    else{ df_to_work$elapsed_state<-"#526cad"
    return(df_to_work$elapsed_state)}
    
  }
  #To put color to Hora string by  state color | last two hour option
  check_hour_color<-function(df_to_work){
    #my_color<-"yellow"
    mv1<-"<b><font color="
    mv2<-">Hora:</font></b>"
    #mv2<-">Hora:"
    #mv3<-"</font></b>"
    if("elapsed_time"%in% colnames(df_to_work)){
      #df_to_work$hour_color_message<-paste(sep = "<br/> ",mv1,df_to_work$elapsed_state,mv2,df_to_work$elapsed_image)
      df_to_work$hour_color_message<-paste(sep = "",mv1,df_to_work$elapsed_state,mv2)
      #df_to_work$hour_color_message<-paste(sep = " ",mv1,df_to_work$elapsed_state,mv2,df_to_work$time ,mv3)
      #return(df_to_work$hour_color_message)
    }
    
    else{ #df_to_work$elapsed_state<-"#526cad"
      #mensaje <-paste(sep = "" ,
      #                 "<b><font color='black'>Estado:</font></b> ",
      #                  "<img src='general_state_icon.PNG' style='width:20px;height:20px;'> "
      #                 )
      mensaje <-paste(sep = "","<b><font color='black'>Hora:</font></b> ")               
      #df_to_work$hour_color_message<-"<b><font color='black'>Hora:</font></b> "
      df_to_work$hour_color_message<-mensaje
    }#return(df_to_work$hour_color_message)}
    return(df_to_work$hour_color_message)
    
    
    
  }
  #To put image  by  state color | last two hour option
  check_image_color<-function(df_to_work){
    if("elapsed_time"%in% colnames(df_to_work)){
      #df_to_work$hour_color_message<-paste(sep = "<br/> ",mv1,df_to_work$elapsed_state,mv2,df_to_work$elapsed_image)
      #df_to_work$hour_color_message<-paste(sep = "",mv1,df_to_work$elapsed_state,mv2)
      #df_to_work$hour_color_message<-paste(sep = " ",mv1,df_to_work$elapsed_state,mv2,df_to_work$time ,mv3)
      return(df_to_work$elapsed_image)
    }
    else{ #df_to_work$elapsed_state<-"#526cad"
      mensaje <-paste(sep = "<br/>" ,
                      "<b><font color='black'>State:</font></b> ",
                      "<img src='general_state_icon.PNG' style='width:20px;height:20px;'> "
      )
      
      #df_to_work$hour_color_message<-"<b><font color='black'>Hora:</font></b> "
      df_to_work$elapsed_image<-mensaje
      return(df_to_work$elapsed_image)}
    
  }
whatsup_Message<-function(df_to_work)
{
  #"<b><a href='whatsapp://send?text=Hello%2C%20World!'>Enviar mensaje via WhatsApp</a></b>"
  #https://wa.me/?text=urlencodedtext
  #my_color<-"yellow"
  startm<-"<b><a href='whatsapp://send?text="
  #mv2<-"<b><font color='red'>Alarma de asalto en bus:</font></b>"
  mv2<-"Alarma de asalto en bus."
  mv3<-"Ubicacion:"
  #mv22<-"<b><font color='red'>Alarma de asalto en bus:</font></b> "
  #mv2<-">Hora:</font></b>"
  #mv2<-">Hora:"
  #mv3<-"</font></b>" 
  mv4<-"Accuracy:"
  mv5<-"Hora:"
  finishm<-"'>Reportar via WhatsApp</a></b>"
  #df_to_work$wmessage<-paste(sep=" ",startm,mv2,mv3,df_to_work$lat,df_to_work$lon,mv4,df_to_work$c_accuracy,mv5,df_to_work$time,df_to_work$elapsed_image,finishm)
  startm2<-"<b><a href='https://wa.me/?text="
  df_to_work$wmessage<-paste(sep=" ",startm,mv2,mv3,df_to_work$lat,",",df_to_work$lon,mv4,df_to_work$c_accuracy,mv5,df_to_work$time,finishm)
  
  return(df_to_work$wmessage)
}
  
###
  ###FOR DB CONNECT

  download_today_db<-function(lday){
    
    print(paste0("SELECT * FROM assaults_reported_df WHERE dt = '",lday,"'"))
    con<-createConnection()
    rs <- dbGetQuery(
      con,
      paste0("SELECT * FROM assaults_reported_df  WHERE dt = '",lday,"'")
    )
    #print(rs)
    dbDisconnect(con)
    rs

  }


  
  
  ##  
  
})
