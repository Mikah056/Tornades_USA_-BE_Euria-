
# Packages utilisés
pacotes = c("sp","geojsonio","geojson","RColorBrewer","htmltools","DT","dplyr","tidyverse","nycflights13","lubridate","leaflet","shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN","ggplot2")

# Installation des packages manquants
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})




