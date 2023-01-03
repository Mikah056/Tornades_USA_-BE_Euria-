library(shiny)

# Définition de l'interface utilisateur
ui <- fluidPage(
  
  # Barre de titre
  titlePanel("Tornades aux USA"),
  
  # Barre latérale avec les filtres
  sidebarLayout(
    sidebarPanel(
      # Intervalle d'années
      sliderInput("year_range", "Années :", min = 1990, max = 2022, value = c(1990, 2022), step = 1),
      
      # Intensités
      checkboxGroupInput("magnitudes", "Intensités :",
                         c("EF0" = tags$div(style = "display: flex; align-items: center", 
                                            tags$span("EF0"),
                                            tags$div(style = "width: 20px; height: 20px; background-color: #26C4EC; margin-left: 10px; border-radius: 5px")),
                           "EF1" = tags$div(style = "display: flex; align-items: center", 
                                            tags$span("EF1"),
                                            tags$div(style = "width: 20px; height: 20px; background-color: #00FF00; margin-left: 10px; border-radius: 5px")),
                           "EF2" = tags$div(style = "display: flex; align-items: center", 
                                            tags$span("EF2"),
                                            tags$div(style = "width: 20px; height: 20px; background-color: #FFFF00; margin-left: 10px; border-radius: 5px")),
                           "EF3" = tags$div(style = "display: flex; align-items: center", 
                                            tags$span("EF3"),
                                            tags$div(style = "width: 20px; height: 20px; background-color: #CC5500; margin-left: 10px; border-radius: 5px")),
                           "EF4" = tags$div(style = "display: flex; align-items: center", 
                                            tags$span("EF4"),
                                            tags$div(style = "width: 20px; height: 20px; background-color: #FF0000; margin-left: 10px; border-radius: 5px")),
                           "EF5" = tags$div(style = "display: flex; align-items: center", 
                                            tags$span("EF5"),
                                            tags$div(style = "width: 20px; height: 20px; background-color: #A10684; margin-left: 10px; border-radius: 5px"))))
      ,
      
      # Mois
      checkboxGroupInput("months", "Mois :",
                         c("Janvier" = "1", "Février" = "2", "Mars" = "3", "Avril" = "4", "Mai" = "5", "Juin" = "6",
                           "Juillet" = "7", "Août" = "8", "Septembre" = "9", "Octobre" = "10", "Novembre" = "11", "Décembre" = "12"))
    ),
    
    # Carte Leaflet
    mainPanel(leafletOutput("map"))
  )
)






