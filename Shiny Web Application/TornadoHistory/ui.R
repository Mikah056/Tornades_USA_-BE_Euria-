library(shiny)
library(leaflet)
library(lubridate)
library(nycflights13)
library(tidyverse)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

shinyUI(fluidPage(


  leafletOutput("mymap"),
  dateRangeInput(inputId = "idYearRange", label="Filter by year range",
                 start="2001-01-01", end="2022-11-01", format="yyyy-mm-dd", 
                 language="en", separator="to"),
  
  checkboxGroupInput(inputId = "idCheckGroup", label = "Filter by magnitude",
                     choiceNames = list("EF0","EF1","EF2","EF3","EF4","EF5"),
                     choiceValues = list("EF0","EF1","EF2","EF3","EF4","EF5"))




))