library(shiny)

# Charger les fichiers interface et logique
source("interface.R")
source("logique.R")

# Lancer l'application Shiny
shinyApp(ui = interface, server = logique)
