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


#Generating data(example) that actually comes from another app and it is stored in a database
assaults_reported_df<- data.frame(c_id=4,n_reg=76543211,lat=14.595995,lon=-90.5180138,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.102",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE)
assaults_reported_df<-bind_rows(assaults_reported_df,data.frame(c_id=5,n_reg=76543211,lat=14.595107,lon=-90.517896,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
assaults_reported_df<-bind_rows(assaults_reported_df,data.frame(c_id=6,n_reg=76543211,lat=14.5947822,lon=-90.5177992,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))



createConnection <- function() {
  cona <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  copy_to(cona, assaults_reported_df,"assaults_reported_df",temporary =FALSE)
  return(cona)
  
}

updateDf <- function(df,updt,updd,n_alrt) {
  
  aux_df<-df
  
    if(n_alrt==1){
    auxt_c<-assign_LT(sample(c("r","o","b"),1),FALSE)
    aux_df$time<-c(auxt_c)
    aux_df$dt<-c(updd)
    return(as.data.frame(aux_df))
    
  }
  if(n_alrt==2){ 
    aux_df<-generateData(aux_df,n_alrt)
    aux_df$dt<-c(updd)
    return(as.data.frame(aux_df))
    
  }
  if(n_alrt==3){
    aux_df<-generateData(aux_df,n_alrt)
    aux_df$dt<-c(updd)
    return(as.data.frame(aux_df))
    
    
  }
 
}
generateDefault<-function(){
  default_df<- data.frame(c_id=4,n_reg=76543211,lat=14.595995,lon=-90.5180138,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.102",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE)
  default_df<-bind_rows(default_df,data.frame(c_id=5,n_reg=76543211,lat=14.595107,lon=-90.517896,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
  default_df<-bind_rows(default_df,data.frame(c_id=6,n_reg=76543211,lat=14.5947822,lon=-90.5177992,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=26.2910003662109,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
  
  return(default_df)
  
}
generateData<-function(df,n_alrt){
  
  if(n_alrt==2){
    
    aux_df<- data.frame(c_id=4,n_reg=76543211,lat=14.623081,lon=-90.552081,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.102",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=16.6509990692139,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE)
    aux_df<-bind_rows(aux_df,data.frame(c_id=5,n_reg=76543211,lat=14.622588,lon=-90.550895,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=24.0370006561279,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
    aux_df<-bind_rows(aux_df,data.frame(c_id=6,n_reg=76543211,lat=14.621441,lon=-90.548964,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=18.2280006408691,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
    
    s1<-sample(c("r","o","b"),1)
    s2<-sample(c("r","o","b"),1)
    elt1<-assign_LT(s1,FALSE)
    elt2<-assign_LT(s2,FALSE)
    
    elt2<-ifelse(elt1==elt2,assign_LT(s2,TRUE),elt2)
    
    aux_df$time<-c(elt2)
    df$time<-c(elt1)
    
    
    
    
    df<-bind_rows(df,aux_df)
    return(as.data.frame(df))
    
  }
  
  if(n_alrt==3){
    
    aux_df<- data.frame(c_id=4,n_reg=76543211,lat=14.623081,lon=-90.552081,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.102",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=16.6509990692139,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE)
    aux_df<-bind_rows(aux_df,data.frame(c_id=5,n_reg=76543211,lat=14.622588,lon=-90.550895,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=24.0370006561279,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
    aux_df<-bind_rows(aux_df,data.frame(c_id=6,n_reg=76543211,lat=14.621441,lon=-90.548964,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=18.2280006408691,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
    
    aux2_df<- data.frame(c_id=4,n_reg=76543211,lat=14.641100,lon=-90.512081,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.102",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=45.5999984741211,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE)
    aux2_df<-bind_rows(aux2_df,data.frame(c_id=5,n_reg=76543211,lat=14.642125,lon=-90.512550,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=77.5999984741211,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
    aux2_df<-bind_rows(aux2_df,data.frame(c_id=6,n_reg=76543211,lat=14.642426,lon=-90.513231,ug="Mozilla/5.0 (Linux; Android 5.1; HUAWEI LUA-L03) A...",ip="190.148.53.103",time="15:56:07",dt="14/5/2021",br_name="Netscape",plat="Linux armv7",c_accuracy=23.2530002593994,heading="153.203201293945",c_altitud="NULL",c_altitud_acc="NULL",c_speed="0.246648341417313",scr_h=570,scr_w=320,scr_cd=24,stringsAsFactors = FALSE))
    
    
    
    s1<-sample(c("r","o","b"),1)
    s2<-sample(c("r","o","b"),1)
    s3<-sample(c("r","o","b"),1)
    elt1<-assign_LT(s1,FALSE)
    elt2<-assign_LT(s2,FALSE)
    elt3<-assign_LT(s3,FALSE)
    
    
    elt2<-ifelse(elt1==elt2,assign_LT(s2,TRUE),elt2)
    elt3<-ifelse(elt1==elt3,assign_LT(s3,TRUE),elt3)
    elt3<-ifelse(elt2==elt3,assign_LT(s3,TRUE),elt3)
    
    
    aux_df$time<-c(elt2)
    aux2_df$time<-c(elt3)
    df$time<-c(elt1)
    
    df$c_accuracy[3]<-c(600.291000366211)
    
    
    df<-bind_rows(df,aux_df)
    df<-bind_rows(df,aux2_df)
    
    
    return(as.data.frame(df))
    
  }
  
}
#Assign Lapsed Time
assign_LT<-function(c_alrt,repeated){ 
  if(!repeated){
    if(c_alrt=="r"){
      auxt<-Sys.time()-minutes(5) 
      answ<-paste(hour(auxt),minute(auxt),trunc(second(auxt)),sep = ":")
      return(answ)}
    if(c_alrt=="o"){
      auxt<-Sys.time()-minutes(35)
      answ<-paste(hour(auxt),minute(auxt),trunc(second(auxt)),sep = ":")
      
      return(answ)}
    if(c_alrt=="b"){
      auxt<-Sys.time()-minutes(65) 
      answ<-paste(hour(auxt),minute(auxt),trunc(second(auxt)),sep = ":")
      
      return(answ)}
  }
  
  if(repeated){
    if(c_alrt=="r"){
      auxt<-Sys.time()-minutes(6) 
      answ<-paste(hour(auxt),minute(auxt),trunc(second(auxt)),sep = ":")
      
      return(answ)}
    if(c_alrt=="o"){
      auxt<-Sys.time()-minutes(36)
      answ<-paste(hour(auxt),minute(auxt),trunc(second(auxt)),sep = ":")
      return(answ)}
    if(c_alrt=="b"){
      auxt<-Sys.time()-minutes(66) 
      answ<-paste(hour(auxt),minute(auxt),trunc(second(auxt)),sep = ":")
      
      return(answ)}
    
  }
  
  
  
}


shinyServer(function(input, output,session) {
  shinyjs::hide("dha")

  df_download_r<-reactiveVal(data.frame()) # 
  numberOfRous<- reactiveValues(nrf = 0)
#
  shinyjs::onclick("toggleAccessGroup",
                   shinyjs::toggle(id = "alc0", anim = TRUE))
#
#
#To update the time from the device into the generated Data(example data)
  observeEvent(input$ttd,{
    n_alrt<-sample(c(1,2,3),1)
    default_df<-generateDefault()
    assaults_reported_df<<-updateDf(default_df,isolate(input$ttd),isolate(input$dtd),n_alrt)
  })
  
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
 
      print(paste("printing time:",input$ttd))
     
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
        
        
        if(length(input$dtd)>0){
          
          
          date_limit<-unlist(str_split(input$dtd,"/"))
          new_date=vector()
          for(i in 1:3)
          {
            if(i==1){
              #if(as.numeric(date_limit[i])<=9){
              print("idenitied 00")
              print(as.numeric(date_limit[i]))
              new_date[1]<-as.numeric(date_limit[i])
              
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
          
          print(start_date)           
          temp_download_two<-download_today_db(input$dtd) 
          
          df_download_r(temp_download_two%>%filter(as.hms(time)>=as.hms(start_time)))
          
          #Adding elapsed_time in order to get states on alerts
          temp_color_two<-df_download_r()%>%mutate(elapsed_time=as.numeric(difftime(as.hms(input$ttd),as.hms(time),units="min")))
          #test_dfh<-df%>%mutate(elapsed_time=as.numeric(difftime(as.hms("15:49:00"),as.hms(as.character(Name)),units="min")))
          temp_color_two$elapsed_state<-put_Color_State(temp_color_two)
          temp_color_two$elapsed_image<-put_Image_State(temp_color_two)
          
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
   
    BusIcon<-makeIcon(iconUrl = "bus_poster2.png",
                      iconWidth =  30, 
                      iconHeight =  30)#,
    mensaje <- paste(sep = "<br/>",
                     "<b><font color='red'>Ubicacion del bus::</font></b> ",
                     "<img src='general_state_icon.PNG' style='width:20px;height:20px;'> "
                     
    )
    
    signIcons <- iconList(
      bus = makeIcon(iconUrl = "bus_poster2.png",iconWidth =  30,iconHeight =  30),
      #makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
      bus_nst = makeIcon(iconUrl = "general_state_icon.png",iconWidth =  15,iconHeight = 15)
      
    )
    ###
    if(nrow(df_download_r())==0){ assaults_map<- leaflet() %>%addTiles() %>% setView(lng = -90.513083, lat = 15.7418237, zoom = 7) %>%addGraticule(interval = 5, style = list(color = "#FF0000", weight = 3))
    }else{ 
      #df_top_assaults<-tail(df_download_r(),60)%>%filter(c_accuracy<600)
      #Not selecte any checkbox  
      if(!input$dha){
        df_top_assaults<-df_download_r()
        
        
        df_top_assaults$micon<-as.factor(ifelse(df_top_assaults$c_id==4,"bus","bus_nst"))
        
        df_top_assaults$speed_km<-ifelse(df_top_assaults$c_speed!="null",as.numeric(df_top_assaults$c_speed)*3.6,NA)
        
        df_top_assaults<-df_top_assaults%>%mutate(to_show=paste(c_id,speed_km,"km/h",sep = " "))
        df_top_assaults$time<-as.factor(df_top_assaults$time)
        
        df.tempp<-  split(df_top_assaults, df_top_assaults$dt)
        

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
              
              
          })
        
        
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
            
            
          })
        
       
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
    if(nrow(df_download_r())>0){
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
    }
    
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
    
    }
    
    else{ #df_to_work$elapsed_state<-"#526cad"
   
      mensaje <-paste(sep = "","<b><font color='black'>Hora:</font></b> ")               
      #df_to_work$hour_color_message<-"<b><font color='black'>Hora:</font></b> "
      df_to_work$hour_color_message<-mensaje
    }#return(df_to_work$hour_color_message)}
    return(df_to_work$hour_color_message)
    
    
    
  }
  #To put image  by  state color | last two hour option
  check_image_color<-function(df_to_work){
    if("elapsed_time"%in% colnames(df_to_work)){
     
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
  startm<-"<b><a href='whatsapp://send?text="
  #mv2<-"<b><font color='red'>Alarma de asalto en bus:</font></b>"
  mv2<-"Alarma de asalto en bus."
  mv3<-"Ubicacion:"
  
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
