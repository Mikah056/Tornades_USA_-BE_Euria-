library(shiny)
library(leaflet)

shinyServer(function(input, output) {
points <- eventReactive(input$recalc, {
  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

output$mymap <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    addMarkers(data = points())
})






})