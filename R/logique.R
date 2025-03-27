#' Logique du jeu Takuzu
#'
#' Cette fonction gère la logique du jeu Takuzu dans une application Shiny. Elle permet de gérer la création d'une nouvelle partie, le chronomètre, la vérification de la grille et les mises à jour des cases du jeu.
#'
#' @param input L'objet `input` de Shiny contenant les valeurs de l'interface utilisateur (taille de la grille, niveau de difficulté, etc.).
#' @param output L'objet `output` de Shiny où les résultats du jeu (comme la grille et le temps écoulé) seront rendus.
#' @param session L'objet `session` de Shiny utilisé pour interagir avec la session de l'utilisateur (par exemple, mettre à jour les boutons).
#'
#' @return Aucun. Cette fonction modifie directement les objets `input`, `output` et `session` de Shiny pour gérer la logique du jeu Takuzu.
#' @export
logique <- function(input, output, session) {
  source("fonctions.R")

  nRows <- reactive({ as.numeric(input$grid_size) })
  nCols <- nRows
  niveau <- reactive({ input$niveau })
  debut_temps <- reactiveVal(NULL)
  depart_chrono <- reactiveVal(FALSE)

  rv <- reactiveValues(grille = NULL, verrouillees = NULL)

  observeEvent(input$new_game, {
    debut_temps(Sys.time())
    depart_chrono(TRUE)
    showNotification(paste("Nouvelle partie - Niveau :", niveau(), "- Taille :", nRows(), "x", nCols()), type = "message")
    grille_init <- generer_takuzu(nRows(), niveau())
    rv$grille <- grille_init
    rv$verrouillees <- !is.na(grille_init)
    output$result <- renderText("Nouvelle partie commencée ! Bonne chance ")
  })

  output$timer <- renderText({
    req(debut_temps(), depart_chrono())
    invalidateLater(1000, session)
    temps_ecoule <- difftime(Sys.time(), debut_temps(), units = "secs")
    paste("Temps écoulé :", round(temps_ecoule), "secondes")
  })

  output$grille_boutons <- renderUI({
    boutons <- lapply(1:nRows(), function(i) {
      fluidRow(
        lapply(1:nCols(), function(j) {
          valeur_case <- rv$grille[i, j]
          actionButton(inputId = paste("bouton", i, j, sep = "_"),
                       label = ifelse(is.na(rv$grille[i, j]), "", as.character(rv$grille[i, j])),
                       style = "width: 50px; height: 50px; font-size: 18px; margin: 5px;",
                       disabled = rv$verrouillees[i, j])
        })
      )
    })
    tagList(boutons)
  })

  observe({
    lapply(1:nRows(), function(i) {
      lapply(1:nCols(), function(j) {
        observeEvent(input[[paste("bouton", i, j, sep = "_")]], {
          valeur_actuelle <- rv$grille[i, j]
          if (is.na(valeur_actuelle)) {
            valeur_nouvelle <- 0
          } else if (valeur_actuelle == 0) {
            valeur_nouvelle <- 1
          } else {
            valeur_nouvelle <- NA
          }
          rv$grille[i, j] <- valeur_nouvelle

          updateActionButton(
            session,
            paste("bouton", i, j, sep = "_"),
            label = ifelse(is.na(valeur_nouvelle), "", as.character(valeur_nouvelle))
          )
        })
      })
    })
  })

  observeEvent(input$check_grid, {
    if (verifier_grille(rv$grille)) {
      depart_chrono(FALSE)
      delta_temps <- difftime(Sys.time(), debut_temps(), units = "secs")
      output$timer <- renderText({paste("Temps écoulé :", round(delta_temps), "secondes")})
      output$result <- renderText("🎉 Bravo, vous avez réussi !")
    } else {
      output$result <- renderText("La grille n'est pas bonne, réessayez !")
    }
  })
}
