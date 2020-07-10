library(shiny)
library(leaflet)
library(shinydashboard)

data <- read.csv(file.choose(),header = T)
str(data)
data$lon
data$lat
data$Sales.Price
server <- function(input, output) {
  output$s <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(
        lng = data$lon,
        lat = data$lat, 
        radius = log(data$Sales.Price), 
        label = data$Neighborhood, 
        weight = 1)
  })
}

body <- dashboardBody( 
  leafletOutput("s")
)

ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  body = body
)

shinyApp(ui, server)