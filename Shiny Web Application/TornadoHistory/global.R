
# Packages utilisés
pacotes = c("RColorBrewer","htmltools","DT","dplyr","tidyverse","nycflights13","lubridate","leaflet","shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN")

# Installation des packages manquants
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})




