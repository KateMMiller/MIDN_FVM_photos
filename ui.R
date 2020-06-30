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
      "<div> <a class='alignbottom' 'href='https://www.nps.gov/im/midn/'> 
             <img class='aligncenter' src='ah_small_black.gif', 
             alt='MIDN Forest Photopoint Viewer'></a>
      MIDN FVM Photopoint Viewer</div>"
    ),
    position = "static-top", 
    inverse = TRUE, 
    collapsible = FALSE, 
    fluid = TRUE,
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css",
    windowTitle = "MIDN FVM Photopoint Viewer", 
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
                                          "APCO", "BOWA", "COLO", "FRSP", "GETT", 
                                          "GEWA", "HOFU", "PETE", "RICH", "THST", "VAFO"
                              ),
                              selected = NULL
                            )
                   ),
                   tags$div(title = 'Zoom to a plot:',
                            style = 'padding:0 1px 1px 3px',
                            uiOutput("plot_df")
                            )
               )
               ),
        
        column(10, style = "padding: 1px 20px 10px 5px", 
               tags$div(title = "Map of Forest Plots",
                        div(leafletOutput("forestMap", 
                                          height = "425px")
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