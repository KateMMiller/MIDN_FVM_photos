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
  
  # NPS map urls
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
          lng = plots$Long,
          lat = plots$Lat,
          layerId = plots$Plot_Name, 
          label = if(input$forestMap_zoom > 11) plots$Plot_Name else NULL,
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
  
  # Set up ability to zoom to given plot
  observeEvent(input$plotZoom, {
    req(input$plot)
    
    plot_selected <- plots %>% filter(Plot_Name == input$plot)
    
    output$UR<-renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$BR<-renderText({c('<p> </p>')})
    output$BL<-renderText({c('<p> </p>')})
    output$UL<-renderText({c('<p> </p>')})
    
    leafletProxy('forestMap') %>% 
      clearControls() %>%
      #clearPopups() %>% 
      setView(
        lng =  plot_selected$Long, 
        lat = plot_selected$Lat, 
        zoom = 16) 
    delay(400, leafletProxy("forestMap") %>% 
            addCircles(
              lng = plot_selected$Long,
              lat = plot_selected$Lat,
              layerId = substr(plot_selected$Plot_Name, 6, 9),
              group = 'pulse',
              radius = 19,
              color = '#00ffff',
              fillOpacity = 0,
              weight = 5)) 
    delay(1000, 
          leafletProxy('forestMap') %>% 
            clearShapes())
  })
  
  # Reset view of map panel
  observeEvent(input$reset_view, {
    reset("plotZoom")
    
    updateSelectizeInput(session, 'park',
                         choices = c("Choose a park" = "",
                                     "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA",
                                     "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO"))
    
    updateSelectizeInput(session, 'plot',
                         choices = c("Choose a plot" = "", unique(plots$Plot_Name)))
    
    output$UR <- renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$BR <- renderText({c('<p> </p>')})
    output$BL <- renderText({c('<p> </p>')})
    output$UL <- renderText({c('<p> </p>')})
    
    leafletProxy("forestMap") %>% 
      clearPopups() %>%
      clearControls() %>% 
      setView(lng = -76.35, lat = 38.6, zoom = 6) 
    
    
  })
  
  
  observeEvent(input$park, {
    req(input$park)
    
    park_coords <- plots %>% filter(Unit_Code %in% input$park) %>% summarize(long = mean(Long),
                                                                             lat = mean(Lat))  
    
    park_plots <- plots %>% filter(Unit_Code %in% input$park) %>% select(Plot_Name) %>% unique()
    
    
    zoom_level <- case_when(input$park %in% c("ASIS") ~ 9.5,
                            input$park %in% c("APCO", "BOWA", "GEWA", "HOFU", "VAFO", "PETE") ~ 15, 
                            input$park %in% c("COLO", "FRSP", "GETT", "RICH") ~ 13,
                            TRUE ~ 12.5)
    
    updateSelectizeInput(session, 'plot',
                         choices = c("Choose a plot" = "", park_plots$Plot_Name))
    
    leafletProxy('forestMap') %>% 
      #clearControls() %>% 
      clearPopups() %>% 
      setView(lng = park_coords$long,
              lat = park_coords$lat, 
              zoom = zoom_level) 
  })
  
  # Set up ability to zoom to given plot
  observeEvent(input$plot, {
    req(input$plot)
    
    plot_selected <- plots %>% filter(Plot_Name %in% input$plot) 
    
    photoUR <- as.character(plot_selected %>% 
                              mutate(photoUR = paste0(UR)) %>%  
                              select(photoUR) %>% droplevels())
    
    output$UR <- renderText({c('<img src="', photoUR,'" width="99%"/>')})
    
    photoBR <- as.character(plot_selected %>%  
                              mutate(photoBR = paste0(BR)) %>%  
                              select(photoBR) %>% droplevels())
    
    output$BR <- renderText({c('<img src="', photoBR,'" width="99%"/>')})
    
    photoBL <- as.character(plot_selected %>% 
                              mutate(photoBL = paste0(BL)) %>%  
                              select(photoBL) %>% droplevels())
    
    output$BL <- renderText({c('<img src="', photoBL,'" width="99%"/>')})
    
    photoUL <- as.character(plot_selected %>% 
                              mutate(photoUL = paste0(UL)) %>%  
                              select(photoUL) %>% droplevels())
    
    output$UL <- renderText({c('<img src="', photoUL,'" width="99%"/>')})
    
    leafletProxy('forestMap') %>% 
      #clearControls() %>% 
      #clearPopups() %>% 
      setView(
        lng =  plot_selected$Long,
        lat = plot_selected$Lat,
        zoom = 16)
    
  })
  
  observeEvent(input$forestMap_marker_click, {
    
    MarkerClick <- input$forestMap_marker_click
    plot_click <- plots[plots$Plot_Name == MarkerClick$id, ]
    
    plots_park <- plots %>% filter(Unit_Code %in% input$park) %>% select(Plot_Name) %>% unique()
    
    tempdata <- plot_click %>% select(Plot_Name, Long, Lat, Last_Sampled = SampleYear, Panel, Physio,
                                      Num_Live_Trees, Num_Dead_Trees, Inv_Shrub_Cov, 
                                      Num_Seedlings, Num_Saplings, Num_Quad_Species, 
                                      Num_Add_Species, Location_Notes, Directions)
    
    content <- paste0("<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:250px;'>",
                      "<b>", h4("Plot Info:"), "</b>",
                      tagList(tags$table(
                        class = 'table',
                        tags$thead(tags$th("Metric"), tags$th("Values")),
                        tags$tbody(
                          mapply(FUN = function(Name, Value){
                            tags$tr(tags$td(sprintf("%s: ", Name)),
                                    tags$td(align = 'right', sprintf("%s", Value)))},
                            Name = names(tempdata[,c(1, 4:15)]),
                            Value = tempdata[,c(1, 4:15)],
                            SIMPLIFY = FALSE)))), "</div>")
    
    
    updateSelectizeInput(session, 'park',
                         choices = c("Choose a park" = "",
                                     "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA",
                                     "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO"),
                         selected = paste(plot_click$Unit_Code)
    )
    
    updateSelectizeInput(session, 'plot',
                         choices = c("Choose a plot" = "", unique(plots_park$Plot_Name)),
                         selected = paste(plot_click$Plot_Name))
    
    photoUR<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                             mutate(photoUR = paste0(UR)) %>%  
                             select(photoUR) %>% droplevels())
    
    output$UR <- renderText({c('<img src="', photoUR,'" width="99%"/>')})
    
    photoBR<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                             mutate(photoBR = paste0(BR)) %>%  
                             select(photoBR) %>% droplevels())
    
    output$BR <- renderText({c('<img src="', photoBR,'" width="99%"/>')})
    
    photoBL<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                             mutate(photoBL = paste0(BL)) %>%  
                             select(photoBL) %>% droplevels())
    
    output$BL <- renderText({c('<img src="', photoBL,'" width="99%""/>')})
    
    photoUL<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                             mutate(photoUL = paste0(UL)) %>%  
                             select(photoUL) %>% droplevels())
    
    output$UL <- renderText({c('<img src="', photoUL,'" width="99%"/>')})
    
    leafletProxy('forestMap') %>% 
      setView(
        lng =  tempdata$Long,
        lat = tempdata$Lat,
        zoom = 16) %>% 
      addPopups(
        lng = tempdata$Long,
        lat = tempdata$Lat,
        popup = content
      ) 
  })
  
  
  
  
}
