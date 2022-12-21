#library(shiny)
#library(leaflet)
#library(lubridate)
#library(nycflights13)
#library(tidyverse)


#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()

#shinyUI(fluidPage(


#  leafletOutput("mymap"),
#  dateRangeInput(inputId = "idYearRange", label="Filter by year range",
 #                start="2001-01-01", end="2022-11-01", format="yyyy-mm-dd", 
#                 language="en", separator="to"),
  
 # checkboxGroupInput(inputId = "idCheckGroup", label = "Filter by magnitude",
  #                   choiceNames = list("EF0","EF1","EF2","EF3","EF4","EF5"),
   #                  choiceValues = list("EF0","EF1","EF2","EF3","EF4","EF5"))




#))









navbarPage("Tornadoe's Tracker",
           tabPanel("Map",
              fluidPage(theme = shinytheme("flatly")),
                    tags$head(
                      tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                    pageWithSidebar(
                      headerPanel(''),
                      sidebarPanel(width = 3,
                             #     selectInput('player', 'Choose a player:',paste(data$player,"-",data$team)),
                             #     sliderInput("overall", "Overall:",
                             #                 min = 50, max = 100,
                             #                 value = c(50,100)),
                             #     sliderInput("height", "Height (cm):",
                             #                 min = 155, max = 203,
                             #                 value = c(155,203)),
                                   checkboxGroupInput(inputId = "Echelle",
                                                      label = "Filter by Fujita's scale", choices = c("EF0","EF1","EF2","EF3","EF4","EF5"), 
                                                      selected = c("EF0"="EF0","EF1"="EF1","EF2"="EF2"),inline=FALSE),
                             #     checkboxGroupInput(inputId = "foot",
                             #                        label = 'Foot:', choices = c("Right foot" = "Right foot",
                             #                                                     "Left foot" = "Left foot"), 
                             #                        selected = c("Right foot" = "Right foot",
                             #                                     "Left foot" = "Left foot"),inline=TRUE),
                                
                  ##### Bout de code copie dans Masters Pokemon #####        
                
                                            p(tags$b("Filter by years", style = "font-size: 102%")),
                                            fluidRow(column(5, align = 'center',
                                                            selectInput(inputId = 'year_inf',
                                                                        label = NULL,
                                                                        choices = 1990:2022, 
                                                                        selected = 2001)),
                                                     column(2, style = 'margin-top: 7px', align = 'center', p("to")),
                                                     column(5, align = 'center',
                                                            selectInput(inputId = 'year_sup',
                                                                        label = NULL,
                                                                        choices = 1990:2022, 
                                                                        selected = 2015))),
                
                                            
                             
                  ##### Fin du bout copie ####
                  
                  selectInput('tri_mois', 'Filter by months',choices=month.name,selected=c("April","May"),multiple=TRUE),
                  
                  
                             submitButton("Update filters")
                      ),
                  
                  
                      mainPanel(
                        leafletOutput("mymap", width = 1200, height=595)
                        
                      )
                    )),
           
           
           
           tabPanel("Stat_desc",p("We used a data set consisting of 39 attributes from 11,158 players registered
                          in Pro Evolution Soccer 2019 (PES 2019), an electronic soccer game. The data set
                          was obtained from ", a("PES Data Base", href="http://pesdb.net/", target="_blank"),
                              "website using web scraping. This app is an interactive tool that allows any user to choose a soccer player from the game
                         and find the ten players most similar whith him. The similarity between the players is determined using a data mining technique
                         called", a("k-nearest neighbors", href="https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm", target="_blank"), ".",style = "font-size:25px"),
                    ),
           
           
           
           
           tabPanel("Main",
                    p(a("Romain AKAKPO", target="_blank"),style = "font-size:25px"),
                    p(a("Chaymae GRAOUI", target="_blank"),style = "font-size:25px"),
                    p(a("Hakim MOUNDE", target="_blank"),style = "font-size:25px"),
)

)



