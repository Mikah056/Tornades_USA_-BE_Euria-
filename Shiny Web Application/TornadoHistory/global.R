
#Definition du repertoire de travail
setwd("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data")


# Packages utilisés
pacotes = c("sf","sp","geojsonio","geojson","RColorBrewer","htmltools","DT","dplyr","tidyverse","nycflights13","lubridate","leaflet","shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN","ggplot2")

# Installation des packages manquants
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


## Source : https://github.com/ThiagoValentimMarques/The-ten-most-similar-players-Pro-Evolution-Soccer-2019/blob/master/global.R





