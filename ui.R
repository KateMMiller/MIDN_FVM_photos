library(shiny)
library(leaflet)

shinyUI(
  navbarPage(
    tags$head(includeCSS("./www/mapstyles.css")),
    tags$head(
      tags$style(HTML('.navbar-nav, .navbar-brand{padding-top:7px;
                                                padding-bottom:2px;
                                                height: 65px;
                                                font-size: 24px;
                                                font-family:
                                                  Frutiger,
                                                  "Frutiger Linotype",
                                                  Calibri,
                                                  "Helvetica Neue",
                                                  Helvetica,
                                                  Arial,
                                                  sans-serif 
                                                }
                    .navbar {min-height:65px !important;}
                    .alignbottom{
                    vertical-align:text-bottom;
                    }
                    .aligncenter{
                    vertical-align:center;
                    }'
      ))
    ),
    title = HTML(
      "<div> <img src='AH_small.jpg', alt='MIDN/NCBN Forest Snapshot'> MIDN/NCBN Forest Plot Snapshots</div>"
      
    ),
    position = "static-top", 
    inverse = TRUE, 
    collapsible = FALSE, 
    fluid = TRUE,
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css",
    windowTitle = "MIDN/NCBN Forest Plot Snapshots", 
    id = "MainNavBar",
    
    #--------------------------------------------------------------
    #  FluidRows to view map and photos
    #--------------------------------------------------------------
    fluidRow(
      column(2, style = 'padding:0 1px 1px 3px;', 
             div(id = "MapPanel", class = "panel panel-default controls",
                 h4('Map Controls', class = 'panel-heading'),
                 tags$style(type='text/css', 
                            ".selectize-input{font-size: 12px;} 
                               .selectize-dropdown{font-size: 12px;}"),
                 
                 tags$div(title = 'Zoom to a park',
                          style = 'padding:5px 1px 1px 3px',
                          selectizeInput(
                            inputId = 'park',
                            label = h5('Zoom to a park:'),
                            choices = c("Choose a park" = "", 
                                        "APCO", "ASIS", "BOWA", "COLO", "FRSP", "GETT", "GEWA",
                                        "HOFU", "PETE", "RICH", "SAHI", "THST", "VAFO"
                            ),
                            selected = NULL
                          )
                 ),
                 tags$div(title = 'Zoom to a plot:',
                          style = 'padding:0 1px 1px 3px',
                          uiOutput("plot_df")
                 )
             ),
             tags$div(title = "Reset the Map",
                      actionButton('reset_view', "Reset Map", 
                                   style="color:white;background-color: #5F9EA0; 
                         border-color:#436e70;font-size:11px"))
      ),
      
      column(10, style = "padding: 1px 20px 10px 5px", 
             tags$div(title = "Map of Forest Plots",
                      div(leafletOutput("forestMap", 
                                        height = "600px")
                      ))), 
      br(),
      
      tags$div(title = "Photopoints from site", label = "Photopoints from site",
               column(3, htmlOutput(outputId = "UR")),
               column(3, htmlOutput(outputId = "BR")),
               column(3, htmlOutput(outputId = "BL")),
               column(3, htmlOutput(outputId = "UL"))
      )#ends tags$div
    ) # end fluidRow
    #)
  )
)  