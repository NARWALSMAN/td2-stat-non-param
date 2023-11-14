
  # 2. Pour une configuration expérimentale avec une distribution normale N(7, 4^2) et une taille d'échantillon n = 15
  taille_echantillon <- 15
  moyenne <- 7
  ecart_type <- 4
  
  # (a) Simuler un échantillon et tracer la fonction de distribution empirique F_n(x)
  echantillon <- rnorm(taille_echantillon, moyenne, ecart_type)
  F_n <- ecdf(echantillon)
  
  # Tracer la CDF empirique
  plot(F_n, main="CDF Empirique vs. CDF Théorique", xlab="x", ylab="F(x)", col="blue", lwd=2)
  
  # Superposer la CDF théorique pour N(7,4^2)
  curve(pnorm(x, moyenne, ecart_type), add=TRUE, col="red", lwd=2, lty=2)
  
  # (b) Calculer une enveloppe de dispersion à 95% pour F_n(x)
  alpha <- 0.05
  z <- qnorm(1 - alpha/2)
  
  # Calcul des bandes de confiance
  valeurs_x <- seq(min(echantillon), max(echantillon), length.out=100)
  bandes_confiance <- sapply(valeurs_x, function(x) {
    F_chapeau <- mean(echantillon <= x)
    erreur_type <- sqrt(F_chapeau * (1 - F_chapeau) / taille_echantillon)
    c(F_chapeau - z*erreur_type, F_chapeau + z*erreur_type)
  })
  
  # Ajout des bandes de confiance au tracé
  matlines(valeurs_x, t(bandes_confiance), col="green", lty=3, lwd=1)
  
  # (c) Déterminer la taille d'échantillon nécessaire
  
  
# Exercice 3

# Simuler un échantillon selon une distribution N(7;4) et calculer h pour le noyau uniforme
taille_echantillon <- 15
echantillon <- rnorm(taille_echantillon, mean = 7, sd = 4)

# Estimer sigma à partir de l'échantillon
sigma_chapeau <- sd(echantillon)

# Valeurs des moments pour le noyau gaussien (noyau gaussien standard)
mu_0_K2_gaussien <- 1
mu_2_K_gaussien <- (1 / (2 * sqrt(pi)))

# Valeurs des moments pour le noyau uniforme
mu_0_K2_uniforme <- 1/3  # L'intégrale de K^2 sur [-1, 1] pour le noyau uniforme
mu_2_K_uniforme <- 1/3   # Le second moment du noyau uniforme sur [-1, 1]

# Calculer h pour le noyau gaussien
h_gaussien <- (8 * sqrt(pi) * mu_0_K2_gaussien / (3 * mu_2_K_gaussien^2))^(1/5) / (sigma_chapeau * taille_echantillon^(-1/5))

# Calculer h pour le noyau uniforme
h_uniforme <- (8 * sqrt(pi) * mu_0_K2_uniforme / (3 * mu_2_K_uniforme^2))^(1/5) / (sigma_chapeau * taille_echantillon^(-1/5))

# Définir la fonction de noyau uniforme
noyau_uniforme <- function(u) {
  ifelse(abs(u) <= 1, 0.5, 0)  # Valeur du noyau est 0.5 à l'intérieur de l'intervalle [-1, 1], et 0 à l'extérieur.
}

# Définir la fonction de noyau gaussien
noyau_gaussien <- function(u) {
  dnorm(u)  # Le noyau gaussien standard
}

# Fonction d'estimation de noyau (identique à celle d'avant)
estimateur_noyau <- function(x, echantillon, h, noyau) {
  taille <- length(echantillon)
  est <- sapply(x, function(xi) {
    mean(noyau((xi - echantillon) / h))
  }) / h
  return(est)
}

# Définir une séquence de valeurs x pour le tracé
sequence_x <- seq(min(echantillon) - 3, max(echantillon) + 3, length.out = 300)

# Calculer les estimations pour le noyau uniforme
f_chapeau_n_uniforme <- estimateur_noyau(sequence_x, echantillon, h_uniforme, noyau_uniforme)

# Calculer les estimations pour le noyau gaussien
f_chapeau_n_gaussien <- estimateur_noyau(sequence_x, echantillon, h_gaussien, noyau_gaussien)

# Tracer les estimateurs
plot(sequence_x, f_chapeau_n_uniforme, type = 'l', col = 'blue', main = 'Estimation de densité par noyau', xlab = 'x', ylab = 'Densité')
lines(sequence_x, f_chapeau_n_gaussien, type = 'l', col = 'red')
