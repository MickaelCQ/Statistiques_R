#####################################################################################################################################################
#                                                 Mickael COQUERELLE , 28 septembre 2024                                                                    #
#                                             Statistiques descriptives et inférentielles                                                           #
#  Construction d'une simulation empirique pour illustrer la fluctuation échantillonalle (chapitre2) et la convergence des estimateurs (chapitre 3) #
#####################################################################################################################################################

# L'objectif pour nous est d'illustrer la convergence d'un estimateur vers le paramètre donné pour une loi usuelle donnée :
# X ~ B(pi)  ; X ~ B(n,pi)  ; X ~ N(mu,sigma)
# L'étude d'une variable aléatoire donnée X pour un estimateur  de pi(x), de V(X) ou de E(X).
# On monte un petit programme de simulation échantillonnalle (basé sur la structure itérative for pour chacun des cas avec les consignes du cours :

##################################################################################
#               Cas d'une proportion pi^ comme estimateur                        #
#         Représentation des fluctuations empiriques  de l'estimateur            #
##################################################################################
# n ICI est une série de tirage  qui correspond à une expérience
# Simulation pour n = 100 et n = 1000 avec 1000 répétitions :

  pi <- 0.4               # Probabilité de succès de la loi de Bernoulli (P(x =1) à l'inverse l'échec P(x=0))
  nSimulationsA <- 100    # Nombre de répétitions de l'expérience A (n = 100)
  nAbin <- 100            # Taille de l'échantillon pour l'expérience A (n = 100)
  nSimulationsB <- 1000   # Nombre de répétitions de l'expérience B (n = 1000)
  nBbin <- 1000           # Taille de l'échantillon pour l'expérience B (n = 1000)

# Vecteurs de stockage pour stocker les moyennes estimées
  mu_estim_Abin <- numeric(nSimulationsA)
  mu_estim_Bbin <- numeric(nSimulationsB)
  
# Simulations pour un échantillon de taille 100
  for (i in 1:nSimulationsA) {
    Tirage_bernoulli <- rbinom(nAbin, 1, pi)  # Chaque tour de boucle fait un tirage de Bernoulli jusqu'a nSimulationsA
    mu_estim_Abin[i] <- mean(Tirage_bernoulli)# La moyenne des tirages est recalculés à chaque tour de boucle jussqu'a nSimulationsA
  }
# Simulations pour un échantillon de taille 1000
  for (i in 1:nSimulationsB) {
    Tirage_bernoulli <- rbinom(nBbin, 1, pi)  # Chaque tour de boucle fait un tirage de Bernoulli jusqu'a nSimulationsB
    mu_estim_Bbin[i] <- mean(Tirage_bernoulli)# La moyenne des tirages est recalculés à chaque tour de boucle jussqu'a nSimulationsB
  }
  
# Calcule des moyennges générales des nSimulations respectives :
  mdem_Abin <- mean(mu_estim_Abin)  # Moyenne des moyennes de nAbin
  mdem_Bbin <- mean(mu_estim_Bbin)# Moyenne des moyennes de nBbin
  
  print(paste("Moyenne estimée pour un échantillon de taille 100 : ", mdem_Abin))
  print(paste("Moyenne estimée pour un échantillon de taille 1000 : ", mdem_Bbin))
  
# Affichage des fluctuations par des histogrammes
  par(mfrow = c(1, 2)) #Découpage de l'écran de sortie, vu en cours 
  hist(mu_estim_Abin, main = "Fluctuation des moyennes pour un échantillon de taille 100", 
       xlab = "Moyennes estimées", breaks = 20)
  hist(mu_estim_Bbin, main = "Fluctuation des moyennes pour un échantillon de taille 1000", 
       xlab = "Moyennes estimées", breaks = 20)
  
# Conclusion  :
# Plus la taille n de l'expérience est grande , moins l'estimateur est biaisé. 
# La variabilité des moyennes estimées diminue avec l'augmentation de la taille de n.
  
# L'estimateur pi^ converge vers la vraie valeur de pi au fur et à mesure que la taille de l'échantillon augmente.
# Cette convergence est plus rapide pour des expérience de plus grande taille, (c'est cohérent avec la loi des grands nombres).
  
##################################################################################
#                    Cas d'une moyenne comme estimateur                         #
##################################################################################

# Définition des paramètres initiaux à renseigner pour chaque cas de simulation :
  Exm <- 2          # Moyenne mu paramètre de N pour le cas d'une moyenne
  Sigmam <- sqrt(4) # Ecart-type définit comme la racine carré de la variance

# Simulation pour différentes valeurs de n et estimation de la moyenne :
  n_diff <- c(10, 100, 300, 500, 1000,10000) # On peut stocker dans un vecteur les différentes tailles des réalisations.
  moy_estime <- numeric(length(n_diff))

# On peut passer par une boucle for pour itérer sur les différentes tailles de réalisation.
  for (i in 1:length(n_diff)) {
  n <- n_diff[i]           # On cherche à itérer sur chaque position i du vecteur n_diff pour tester nos différentes tailles de n 
  X <- rnorm(n,Exm,Sigmam)  # Génération de notre loi normale pour le n courant d'indice i.
  moy_estime[i] <- mean(X) # Génération de l'estimation de la moyenne courante sur la base de la loi normale du n d'indice i.     
}

  moy_estime  # Affichage de toutes les réalisations de nos moyennes pour n de différentes tailles, ici 5 dans n_diff.

#En répétant le script, on observe des moyennes estimées différentes, ce qui illustre la fluctuation échantillonnale pour un nn fixé.
#On remarque surtout que, plus la taille nn de l'échantillon augmente, plus notre estimateur μ^ converge vers la valeur théorique 
#de la moyenne μ=2

#Autre chose , la distribution Gaussienne devient plus « précise » avec l'augmentation de n, car elle tend à être plus 
  #concentrée autour de μ, avec une symétrie renforcée. Cela se reflète par une courbe ayant moins de modes et
  #une étendue de valeurs réduite sur l'axe des abscisses, ce qui resserre les observations autour de la moyenne.
  
  ##################################################################################
  #                    Cas d'une variance comme estimateur                         #
  ##################################################################################
  # Paramètres de la loi normale centrée réduite
  mu <- 0
  sigma <- 1
  
  ite1 <- 100   # Début de l'itération
  ite2 <- 10000 # Fin de l'itération
  
  # Générer un vecteur n_diff avec un pas de 100
  n_diff <- seq(ite1, ite2, by = 100)
  
  # Vecteur pour stocker les estimations de la variance à chaque itération de notre boucle for en dessous : 
  var_estime <- numeric(length(n_diff)) # nulméric c'est une fonction qui créer un vecteur numérique ... dpnc la de la taille de ndiff
  
  # Boucle for sur le vecteur n_diff
  for (i in 1:length(n_diff)) {
    n <- n_diff[i]             # Obtenir la valeur de n pour l'itération courante (n stocke la taille courante de l'échantillon +100)
    X <- rnorm(n, mu, sigma)   # Générer une réalisation de loi normale à chaque i.
    var_estime[i] <- var(X)    # Estimer la variance de X et la stocker directement dans notre vecteur à l'indice i
  }
  
  # Affichage des fluctuations de l'estimateur de la variance sous forme de points
  plot(n_diff, var_estime, type = "p", col = "blue", pch = 16, cex = 0.7,
       xlab = "Taille de l'échantillon (n)", ylab = "Estimation de la variance",
       main = "Fluctuations de l'estimateur de la variance pour N(0, 1)")
  abline(h = sigma^2, col = "red", lwd = 2, lty = 2)  # Ligne de la vraie variance (1)
  
  # Ajouter une légende
  legend("topright", legend = c("Estimation de la variance", "Variance réelle (1)"),
         col = c("blue", "red"), pch = c(40, NA), lwd = 2, lty = c(NA, 2))

#On s'apercoit encore que les petites tailles d'échantillons n  montre une forte dispersion des estimations de la variance.
#Conclusion : À fur et à mesure que n augmente , l'estimation de la variance va converger vers la valeur réell  e de 1.
  
# SI on veut être pratico-pratique il faut TOUJOURS se méfier d'une variance qui montre une forte dispersion devant un n petit,
#   ca n'a que peu de valeur, c'est peut être vrai ou peut etre faut. Il faut réfléchir à la taille de l'échantillon au dela de la valeur
# des parametres de dispersions.
  
  
