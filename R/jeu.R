#' Application Shiny du jeu Takuzu
#'
#' Cette fonction charge et lance l'application Shiny pour le jeu Takuzu en utilisant une interface graphique et une logique de gestion du jeu.
#' L'application Shiny comprend une interface utilisateur pour interagir avec le jeu et une logique serveur pour gérer les actions et les mises à jour de l'état du jeu.
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyalert
#'
#' @return Une application Shiny qui démarre le jeu Takuzu.
#' @export
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)


# Charger les fichiers interface et logique
source("interface.R")
source("logique.R")

# Lancer l'application Shiny
shinyApp(ui = interface, server = logique)
