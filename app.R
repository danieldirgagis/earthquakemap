library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)



dataeq <- read.csv("data/query.csv")
dataeq$time <- as.Date(dataeq$time, format = "%Y-%m-%dT%H:%M:%S")


dataeq$depth_type <- ifelse(dataeq$depth <= 70, "shallow", 
                            ifelse(dataeq$depth <= 300 | dataeq$depth >70, "intermediate", 
                                   ifelse(dataeq$depth > 300, "deep", "other")))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  titlePanel("Earthquake data in Indonesia"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Set Magnitude Range", min(dataeq$mag), max(dataeq$mag), value = range(dataeq$mag)
                ),
                dateRangeInput("daterange", "Set Date Range ",
                               start = "2019-01-01",
                               end = 2020-02-02,
                               min(dataeq$time),
                               max(dataeq$time))
  )
)




########################server###############################
server <- function(input, output, session) 
{
  
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = dataeq$mag)
  
  
  #define the color of for the depth of the earquakes
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = dataeq$depth_type
  )
  
  #create reactive data based on slider input
  
  sliderData1 <- reactive({
    dataeq[dataeq$mag >= input$range[1] & 
             dataeq$mag <= input$range[2] & 
             dataeq$time >= input$daterange[1] & 
             dataeq$time <= input$daterange[2], ]
  })
  
  #create base map
  output$map <- renderLeaflet({
    leaflet(dataeq) %>% addTiles() %>%
      fitBounds(92, 6, 141.2753, -11)
  })
  

  
  #create  map
  observe({
    leafletProxy("map", data = sliderData1()) %>%
      clearMarkers() %>%
      addCircleMarkers(data = sliderData1(),lat = ~ latitude, lng = ~ longitude, weight = 1, 
                       radius = ~3^(mag)/100, popup = ~as.character(mag), 
                       label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), 
                       color = ~pal(mag), fillOpacity = 0.5,group = "myMarkers")
    
  })
  
  #create legend map
  observe({
    proxy <- leafletProxy("map", data = sliderData1())
    proxy %>% clearControls()%>% 
      addLegend(position = "bottomright",
                pal = pal, values = ~mag
      )
  })
  
  
  
  
  
  
}


shinyApp(ui, server)


