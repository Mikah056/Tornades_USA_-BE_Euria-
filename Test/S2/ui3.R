
# Créer une échelle de couleur en fonction de l'intensité de la tornade
tornado_colors <- c("#009900", "#00FF00", "#FFFF00", "#FF6600", "#FF0000", "#660000")
names(tornado_colors) <- unique(data$TOR_F_SCALE)

# Créer une échelle de couleur pour les comtés en fonction du nombre de tornades
county_colors <- colorRamp(c("#00FF00", "#FFFF00", "#FF6600", "#FF0000", "#660000"))

# Créer l'interface utilisateur
ui <- fluidPage(
  
  # Ajouter un titre
  titlePanel("Tornades aux États-Unis"),
  
  # Ajouter une barre de navigation latérale
  sidebarLayout(
    sidebarPanel(
      
      # Ajouter un filtre par plage d'années
      sliderInput("year_range", "Plage d'années :",
                  min = 1990, max = 2022, value = c(1990, 2022)),
      
      # Ajouter un filtre par intensité de la tornade
      checkboxGroupInput("tornado_intensity", "Intensité de la tornade :",
                         choices = names(tornado_colors),
                         selected = names(tornado_colors)),
      
      # Ajouter un filtre par mois
      checkboxGroupInput("month", "Mois :",
                         choices = month.name,
                         selected = month.name)
    ),
    
    # Ajouter la carte à l'interface utilisateur
    mainPanel(
      leafletOutput("map")
    )
  )
)












