library(shiny)
library(leaflet)
library(lubridate)
library(nycflights13)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

shinyUI(fluidPage(


  leafletOutput("mymap"),
  p(),
  dateRangeInput(inputId = "idYearRange", label="Filter by year range",
                 start="2011", end="2022", format="yyyy",
                 language="en", separator="through")









))