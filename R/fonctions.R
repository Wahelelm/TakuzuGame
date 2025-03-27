#' Choisir le nombre de cases pré-remplies en fonction du niveau
#'
#' Cette fonction calcule le nombre de cases pré-remplies dans une grille de Takuzu en fonction du niveau de difficulté choisi.
#'
#' @param taille Un entier représentant la taille de la grille (par exemple, 6 pour une grille 6x6).
#' @param niveau Un caractère représentant le niveau de difficulté ("Facile", "Moyen", "Difficile", ou "Einstein").
#' @return Un entier, le nombre de cases pré-remplies dans la grille.
#' @examples
#' choisir_difficulte(6, "Moyen")  # Devrait retourner un nombre entre 0 et 14 (selon la taille 6x6 et le niveau "Moyen")
#' @export
choisir_difficulte <- function(taille, niveau) {
  niveaux <- list("Facile" = 0.5, "Moyen" = 0.4, "Difficile" = 0.3, "Einstein" = 0.1)
  if (!(niveau %in% names(niveaux))) {
    stop("Choisissez un niveau parmi : Facile, Moyen, Difficile, Einstein")
  }
  nb_cases = round(taille * taille * niveaux[[niveau]])
  return(nb_cases)
}

#' Générer une grille de Takuzu valide
#'
#' Cette fonction génère une grille de Takuzu valide de taille spécifiée, avec un certain nombre de cases pré-remplies
#' en fonction du niveau de difficulté choisi.
#'
#' @param taille Un entier représentant la taille de la grille (par exemple, 6 pour une grille 6x6).
#' @param niveau Un caractère représentant le niveau de difficulté ("Facile", "Moyen", "Difficile", ou "Einstein").
#' @return Une matrice de taille `taille x taille` représentant une grille de Takuzu valide.
#' @examples
#' generer_takuzu(6, "Moyen")  # Devrait retourner une grille 6x6 valide avec un nombre adapté de cases pré-remplies.
#' @export
generer_takuzu <- function(taille, niveau) {
  nb_cases_prepremplies = choisir_difficulte(taille, niveau)
  grille = matrix(NA, nrow = taille, ncol = taille)

  est_valide_partiel <- function(vec) {
    if (sum(vec == 0, na.rm = TRUE) > length(vec) / 2 ||
        sum(vec == 1, na.rm = TRUE) > length(vec) / 2) {
      return(FALSE)
    }
    if (any(rle(vec)$lengths > 2, na.rm = TRUE)) {
      return(FALSE)
    }
    return(TRUE)
  }

  for (i in 1:taille) {
    for (j in 1:taille) {
      candidats = sample(c(0, 1))
      for (val in candidats) {
        grille[i, j] <- val
        if (est_valide_partiel(grille[i, ]) && est_valide_partiel(grille[, j])) {
          break
        }
        grille[i, j] = NA
      }
    }
  }

  indices = sample(1:(taille^2), taille^2 - nb_cases_prepremplies)
  grille[indices] <- NA

  return(grille)
}

#' Vérifier si une grille de Takuzu est valide
#'
#' Cette fonction vérifie si une grille de Takuzu respecte toutes les règles du jeu :
#' - Chaque ligne et chaque colonne contient le même nombre de 0 et de 1.
#' - Il ne peut y avoir plus de deux cases consécutives avec la même valeur (0 ou 1) dans une ligne ou une colonne.
#' - Les lignes et les colonnes doivent être uniques.
#'
#' @param grille Une matrice représentant la grille de Takuzu (taille x taille).
#' @return Un booléen (TRUE si la grille est valide, FALSE sinon).
#' @examples
#' verifier_grille(matrix(c(0, 1, 0, 1, 1, 0, 0, 1), nrow = 4, ncol = 4))  # Devrait retourner TRUE ou FALSE selon la grille.
#' @export
verifier_grille <- function(grille) {
  N <- ncol(grille)

  if (any(!grille %in% c(0, 1))) {
    return(FALSE)
  }

  if (any(rowSums(grille) != N / 2) || any(colSums(grille) != N / 2)) {
    return(FALSE)
  }

  if (any(apply(grille, 1, function(x) any(rle(x)$lengths > 2))) ||
      any(apply(grille, 2, function(x) any(rle(x)$lengths > 2)))) {
    return(FALSE)
  }

  lignes_uniques <- unique(apply(grille, 1, paste, collapse = ""))
  colonnes_uniques <- unique(apply(grille, 2, paste, collapse = ""))
  if (length(lignes_uniques) != N || length(colonnes_uniques) != N) {
    return(FALSE)
  }

  return(TRUE)
}
