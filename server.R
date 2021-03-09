library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)

shiny_server <- function(session, input, output){
  
  output$plot_df <- renderUI({
    plots2 <- plots %>% filter(Unit_Code %in% input$park) %>% droplevels()
    selectizeInput(inputId = 'plot', 
                   label = h5("Zoom to a plot"), 
                   choices = c("Choose a plot" = "", unique(plots2$Plot_Name)))
  })
  
  # Make NPS map Attribution
  NPSAttrib <-
    HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  
  output$forestMap <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -76.35, lat = 38.6, zoom = 6) %>% 
      setMaxBounds(lng1 = -73.25,
                   lng2 = -80.2,
                   lat1 = 41.03,
                   lat2 = 36.9) %>% 
      addTiles(
        group = "Map",
        urlTemplate = NPSbasic,
        attribution = NPSAttrib) %>%
      addTiles(
        group = "Imagery",
        urlTemplate = NPSimagery,
        attribution = NPSAttrib) %>%
      addTiles(
        group = "Light",
        urlTemplate = NPSlight,
        attribution = NPSAttrib,
        options = tileOptions(minZoom = 8)
      ) %>%
      addTiles(
        group = "Slate",
        urlTemplate = NPSslate,
        attribution = NPSAttrib,
        options = tileOptions(minZoom = 8)
      ) %>%
      addLayersControl(
        map = .,
        baseGroups = c("Map", "Imagery", "Light", "Slate"),
        options = layersControlOptions(collapsed = T)
      ) 

  })
  
  # Zoom to a plot
  observe({
    req(input$forestMap_zoom)
    
    leafletProxy("forestMap") %>% 
        addCircleMarkers(
          data = plots,
          radius = 4,
          lng = plots$Longitude,
          lat = plots$Latitude,
          layerId = plots$plot_name2, 
          label = if(input$forestMap_zoom > 11) plots$plot_number else NULL,
          labelOptions = labelOptions(noHide = TRUE, 
                                      textOnly = TRUE, 
                                      direction = "bottom", 
                                      textsize = "11px"),
          fillColor = "ForestGreen",
          fillOpacity = 0.75,
          weight = 1,
          color = "DimGrey"
    )
    
  })
  
  observeEvent(input$park, {
   req(input$park)
   
   leafletProxy('forestMap') %>% 
     clearControls() %>% 
     clearPopups() %>% 
     setView(lng = bboxes[bboxes$ParkCode == input$park, "lng"],
             lat = bboxes[bboxes$ParkCode == input$park, "lat"], 
             zoom = 9.5) 
  })
  
  # Set up ability to zoom to given plot
  observeEvent(input$plot, {
    req(input$plot)
    
    plot_selected <- plots %>% filter(Plot_Name == input$plot) %>%  droplevels()
    
    photoUR<- as.character(plots %>% filter(Plot_Name == plot_selected$Plot_Name) %>% 
                             mutate(photoUR = paste0(UR)) %>%  
                             select(photoUR) %>% droplevels())
    
    output$UR <- renderText({c('<img src="', photoUR,'" width="99%"/>')})
    
    photoBR<- as.character(plots %>% filter(Plot_Name == plot_selected$Plot_Name) %>% 
                             mutate(photoBR = paste0(BR)) %>%  
                             select(photoBR) %>% droplevels())
    
    output$BR <- renderText({c('<img src="', photoBR,'" width="99%"/>')})
    
    photoBL<- as.character(plots %>% filter(Plot_Name == plot_selected$Plot_Name) %>% 
                             mutate(photoBL = paste0(BL)) %>%  
                             select(photoBL) %>% droplevels())
    
    output$BL <- renderText({c('<img src="', photoBL,'" width="99%"/>')})
    
    photoUL<- as.character(plots %>% filter(Plot_Name == plot_selected$Plot_Name) %>% 
                             mutate(photoUL = paste0(UL)) %>%  
                             select(photoUL) %>% droplevels())
    
    output$UL <- renderText({c('<img src="', photoUL,'" width="99%"/>')})
    
    leafletProxy('forestMap') %>% 
      clearControls() %>% 
      setView(
        lng =  plot_selected$Longitude, 
        lat = plot_selected$Latitude, 
        zoom = 16) 

  })
    
  observeEvent(input$forestMap_marker_click, {
      
      MarkerClick <- input$forestMap_marker_click
      plot <- plots[plots$plot_name2 == MarkerClick$id, ]
   
      plots_park <- plots %>% filter(Unit_Code %in% input$park) %>% droplevels()
      
      updateSelectizeInput(session, 'plot',
                           choices = c(plot$Plot_Name, unique(plots_park$Plot_Name)),
                           selected = paste(plot$Plot_Name))
      
      updateSelectizeInput(session, 'park',
        choices = c("Choose a park" = "", 
                    "APCO", "BOWA", "COLO", "FRSP", "GETT", 
                    "GEWA", "HOFU", "PETE", "RICH", "THST", "VAFO"),
        selected = paste(plot$Unit_Code)
        )
      
      
      photoUR<- as.character(plots %>% filter(plot_name2 == MarkerClick$id) %>% 
                             mutate(photoUR = paste0(UR)) %>%  
                             select(photoUR) %>% droplevels())
      
      output$UR <- renderText({c('<img src="', photoUR,'" width="99%"/>')})
      
      photoBR<- as.character(plots %>% filter(plot_name2 == MarkerClick$id) %>% 
                               mutate(photoBR = paste0(BR)) %>%  
                               select(photoBR) %>% droplevels())
      
      output$BR <- renderText({c('<img src="', photoBR,'" width="99%"/>')})
      
      photoBL<- as.character(plots %>% filter(plot_name2 == MarkerClick$id) %>% 
                               mutate(photoBL = paste0(BL)) %>%  
                               select(photoBL) %>% droplevels())
      
      output$BL <- renderText({c('<img src="', photoBL,'" width="99%""/>')})
      
      photoUL<- as.character(plots %>% filter(plot_name2 == MarkerClick$id) %>% 
                               mutate(photoUL = paste0(UL)) %>%  
                               select(photoUL) %>% droplevels())
      
      output$UL <- renderText({c('<img src="', photoUL,'" width="99%"/>')})
 
    })
    
   


}
