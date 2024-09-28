# loi Binomiale : Approximation d'une loi normale '

# Nombre total de pieds de vigne choisis
n <- 450
# Probabilité qu'un pied de vigne soit malade
p <- 0.4
# 1. Loi de X
# X suit une loi binomiale avec paramètres n et p
# X ~ Binomiale(n, p)
# 2. Espérance et variance de X
esperance <- n * p # E(X)
variance <- n * p * (1 - p) #Var(X)

# Utilisation de l'approximation normale car n est grand
mu <- esperance         # Notre paramètre N : moyenne
sigma <- sqrt(variance) # Notre paramètre N : écart-type

# Calcul des bornes normalisées
Borne_basse <- (180 - mu) / sigma
Borne_haute <- (195 - mu) / sigma

# Calcul de la probabilité avec la fonction pnorm
prob <- pnorm(Borne_haute) - pnorm(Borne_basse)  

# Afficher la probabilité
cat("P(180 ≤ X ≤ 195) :", prob, "\n")

#LOI EXACTE : 

# Définir les paramètres de la loi normale
mean <- 2
sd <- sqrt(4)  # L'écart-type est la racine carrée de la variance

# Calculer la probabilité
p <- pnorm(3.2, mean, sd) - pnorm(2.2, mean, sd)
print(p)
    
    
