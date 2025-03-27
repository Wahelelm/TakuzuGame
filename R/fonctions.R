# Fonctions de base Takuzu
choisir_difficulte <- function(taille, niveau) {
  niveaux <- list("Facile" = 0.5, "Moyen" = 0.4, "Difficile" = 0.3, "Einstein" = 0.1)
  if (!(niveau %in% names(niveaux))) {
    stop("Choisissez un niveau parmi : Facile, Moyen, Difficile, Einstein")
  }
  nb_cases = round(taille * taille * niveaux[[niveau]])
  return(nb_cases)
}

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
