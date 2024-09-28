# Cas d'une proportion #
# Paramètres
#n est suffisamment grand, on peut faire une approximation d'apres le TCL X ~ N(0,1)
# Détermination d'un intervalle de fluctuation pour la proportion d'un échantillon
# Étape 1 : Définir les paramètres
p <- 0.25  # Proportion estimée population
n <- 300       # Taille de l'échantillon

# Étape 2 : Calculer l'écart type
sigma <- sqrt(p * (1 - p) / n)

# Niveau de confiance (ici 95%)
alpha <- 0.05
z <- qnorm(1 - alpha / 2)

borne_basse <- p - z * sigma
borne_haute <- p + z * sigma

# Afficher les résultats
cat("Intervalle de fluctuation à 95% pour la proportion d'un échantillon : [", borne_basse, ";", borne_haute, "]\n")





# Cas d'une Moyenne #
# Définir les paramètres
n <- 1000
Moy_observee <- 50
sigma <- 10

# Calculer l'erreur standard de la moyenne
ss <- sigma / sqrt(n)

# Définir le niveau de confiance et notre risque alpha :
Prob_confiance <- 0.95
alpha <- 1 - Prob_confiance
z <- qnorm(1 - alpha / 2)

# Calculer la marge d'erreur
M <- z * ss

# Déterminer l'intervalle de fluctuation
Borne_basse <- Moy_observee - M
Borne_haute <- Moy_observee + M

# Afficher les résultats
cat("Valeur critique z pour un intervalle de confiance à 80%: ", z, "\n")
cat("Intervalle de fluctuation: [", Borne_basse, ", ", Borne_haute, "]\n")



# Cas d'une variance #
# Définition des paramètres
mu <- 10^6  # Moyenne
sigma2 <- 25000  # Variance
n <- 20  # Taille de l'échantillon
df <- n - 1  # Degrés de liberté

# Calcul des quantiles de la distribution chi-carré on a la proportion et le degré de liberté en paramètre :
chi2_0.025 <- qchisq(0.025, df)
chi2_0.975 <- qchisq(0.975, df)

# Calcul des bornes de l'intervalle de fluctuation
borne_basse <- (sigma2 * chi2_0.025) / df
borne_haute <- (sigma2 * chi2_0.975) / df

# Affichage des résultats
cat("Intervalle de fluctuation à 95% pour la variance de l'échantillon :\n")
cat("Borne inférieure :", borne_basse, "\n")
cat("Borne supérieure :", borne_haute, "\n")
cat("Intervalle de fluctuation: [", borne_basse, ", ", borne_haute, "]\n")

