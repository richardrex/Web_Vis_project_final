library(shiny)
library(ggplot2)
library(ggiraph)
library(dygraphs)
library(leaflet)
library(tidyverse)
library(flexdashboard)
library(DT)
library(readxl)
source("load_data.r")

latitudes = c(50.7, 48.8, 50.1)
longitudes = c(15.7, 16.6, 12.3)
location_labels = c("Sněžka", "Pálava", "Komorní hůrka")


ui <- fluidPage(
    theme = "bootstrap.css",
    includeCSS("www/styles.css"),
    tags$style('.container-fluid {
                             background-color: #E0F3FF;
              }'),
    
        
    sidebarPanel(
        conditionalPanel(condition="input.tableselected==1",h4("Demo of conditionalPanel()")),
        conditionalPanel(condition="input.tabselected==2",
                         selectInput("variable", "Variable:", levels(data$variable)),
                         
                         checkboxGroupInput("location", "Locations:", levels(data$location),
                                            selected = levels(data$location))),
        conditionalPanel(condition="input.tabselected==3", h4("Just to try"))
    ),
    
    
    mainPanel(
        tabsetPanel(
            tabPanel("About", value=1, helpText("It was created by Egor Suslin, student of CZU University as Project for Web Visualization Class")),
            tabPanel("Main", value=2,div(style = "font-size: 0px; padding: 0px 0px; margin-top:-2em"),
                     titlePanel(title = 'Plot fot Snow and Temp'),
                     plotOutput("time_series_plot", click = "plot_click"),
                     verbatimTextOutput("plot_clicked_points"),
                     titlePanel(title = 'One more plot on Temperature or Snow using Girafe'),
                     
                     girafeOutput("plot_girafe"),
                     titlePanel(title = 'Next plot was created by dygraph library'),
                     dygraphOutput("plot_dygraphs"),
                     titlePanel(title = 'Data Table of all data according applied filtering'),
                     DTOutput('tbl') ),
            tabPanel("Map", value=3,uiOutput("currentTime", container = span), titlePanel(title = 'Map OutPut with Leafly'),
                     leafletOutput("map")),
            id = "tabselected"
        )
    ),
    
    
    
)   
   


server <- function(input, output, session) {
    session$allowReconnect("force")
    
    output$currentTime <- renderUI({
        invalidateLater(20 * 1000, session) 
        
        
        h2(paste0("Current Time: ", format(Sys.time() + (60 * 60), "%H:%M")))  
    })
    output$plot_clicked_points <- renderPrint({
        nearPoints(data_to_plot(), input$plot_click, threshold = 10)
    })  
    
    data_to_plot <- reactive({
        data[location %in% input$location & variable == input$variable]
    })    
    
    output$time_series_plot <- renderPlot({
        ggplot(data_to_plot(), aes(x = time, y = value, colour = location)) + geom_line()
    })
   
    
    output$tbl = renderDT(
        data[location %in% input$location & variable == input$variable]
        )
    
   
    
    output$plot_girafe <- renderGirafe({
        p = ggplot(data_to_plot(), aes(x = time, y = value, colour = location, tooltip = paste(location, value)))
        p = p + geom_point_interactive()
        girafe(ggobj = p)
    })
    
    output$plot_dygraphs <-renderDygraph({
        dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% dyRangeSelector()
    })
    
    output$map <- renderLeaflet({
        leaflet() %>% setView(lng = 15, lat = 50, zoom = 7) %>% addTiles() %>% 
            addMarkers(lng = longitudes, lat = latitudes, label = location_labels,
                       options = markerOptions(draggable = TRUE, opacity = 0.6))
    })
    output$data1_table = renderDT({
        data[location %in% input$location1 & variable == input$variable1]
    })
}

shinyApp(ui = ui, server = server)