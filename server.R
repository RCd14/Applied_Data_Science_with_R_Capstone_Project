# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
library("dplyr")  
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  
  cities_max_bike<-  city_weather_bike_df[c("CITY_ASCII", "LNG",  "LAT",   "BIKE_PREDICTION", "BIKE_PREDICTION_LEVEL", "LABEL", "DETAILED_LABEL"  )] %>% 
    group_by(CITY_ASCII) %>% top_n(1,BIKE_PREDICTION)
                     
  #print(cities_max_bike[cities_max_bike$CITY_ASCII=="Seoul",])
  
  
  
  # Observe drop-down event
  
  # Then render output plots with an id defined in ui.R
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  
 
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      
      # If just one specific city was selected, then render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
      
      output$city_bike_map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
          addCircleMarkers(data = cities_max_bike[cities_max_bike$CITY_ASCII==input$city_dropdown,], lng = ~LNG, lat = ~LAT, 
                           radius= ~ifelse(BIKE_PREDICTION_LEVEL=='small', 6, ifelse(BIKE_PREDICTION_LEVEL=='medium', 10,12 ) ),
                           color =  ~ifelse(BIKE_PREDICTION_LEVEL=='small', "green", ifelse(BIKE_PREDICTION_LEVEL=='medium', "yellow","red" ) ),
                           popup = ~DETAILED_LABEL,
                           popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)
          )
        })
      
      output$temp_line<- renderPlot({
        ggplot(data=city_weather_bike_df[city_weather_bike_df$CITY_ASCII==input$city_dropdown,], aes(x=1:length(TEMPERATURE), y=TEMPERATURE)) +
          geom_line(color="yellow") +
          geom_point(size=2)+
          geom_text(aes(label=paste(TEMPERATURE, " C")),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("TEMPERATURE (C) ") +
          ggtitle("Temperature Chart") 
      })
      
      output$bike_line<- renderPlot({
        ggplot(data=city_weather_bike_df[city_weather_bike_df$CITY_ASCII==input$city_dropdown,], aes(x=as.POSIXct(FORECASTDATETIME, format= "%Y-%m-%d %H:%M:%S"), y=BIKE_PREDICTION)) +
          geom_line(color="green") +
          geom_point(size=2)+
          geom_text(aes(label=paste(BIKE_PREDICTION)),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("Predicted Bike Count") 
      })
      
      output$bike_date_output <- renderText({
                paste0("Time=", as.POSIXct(as.integer(input$plot_click$x), origin = "1970-01-01"),
               "\nBikeCountPred=", as.integer(input$plot_click$y))
        })
      
      output$humidity_pred_chart<- renderPlot({
        ggplot(data=city_weather_bike_df[city_weather_bike_df$CITY_ASCII==input$city_dropdown,], aes(x=HUMIDITY, y=BIKE_PREDICTION)) +
        geom_point(size=2)+
        geom_smooth(method=lm, formula=y ~ poly(x, 5), se = FALSE)
      })
      
    }
    else {
      #Render the specific city map
      output$city_bike_map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
          addCircleMarkers(data = cities_max_bike, lng = ~LNG, lat = ~LAT, 
                           radius= ~ifelse(BIKE_PREDICTION_LEVEL=='small', 6, ifelse(BIKE_PREDICTION_LEVEL=='medium', 10,12 ) ),
                           color =  ~ifelse(BIKE_PREDICTION_LEVEL=='small', "green", ifelse(BIKE_PREDICTION_LEVEL=='medium', "yellow","red" ) ),
                           popup = ~LABEL,
                           popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)
          )
      })
      output$temp_line<- renderPlot({
        ggplot(data=city_weather_bike_df, aes(x=1:length(TEMPERATURE), y=TEMPERATURE)) +
          geom_line(color="yellow") +
          geom_point(size=1)+
          #geom_text(aes(label=paste(TEMPERATURE, " C")),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("TEMPERATURE (C) ") +
          ggtitle("Temperature Chart") 
      })
      
      output$bike_line<- renderPlot({
        ggplot(data=city_weather_bike_df, aes(x=as.POSIXct(FORECASTDATETIME, format= "%Y-%m-%d %H:%M:%S"), y=BIKE_PREDICTION)) +
          geom_line(color="green") +
          geom_point(size=2)+
          #geom_text(aes(label=paste(BIKE_PREDICTION)),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("Predicted Bike Count") 
      })
        
      output$bike_date_output <- renderText({
        paste0("Time=", as.POSIXct(as.integer(input$plot_click$x), origin = "1970-01-01"),
               "\nBikeCountPred=", as.integer(input$plot_click$y))
      })
        
      output$humidity_pred_chart<- renderPlot({
        ggplot(data=city_weather_bike_df, aes(x=HUMIDITY, y=BIKE_PREDICTION)) +
          geom_point(size=2)+
          geom_smooth(method=lm, formula=y ~ poly(x, 5), se = FALSE)
      })
        
       
        
        
                           
                           
                           
                           
                           
                           
                           
    }
      })
      
    }) 
    
  
