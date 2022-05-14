

########## TP3 ESTIMATION ET TCL ########## 


# METHODE DES MOMENTS (on remplace les moments théoriques par les moments empiriques)
"
loi uniforme sur [0, a] 
on cherche un estimateur avec la méthode des moments
"


# sample(10:20, 1) -> renvoie une valeur aléatoire entre 10 et 20
runifa <- function(n) {
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}



# Q1 E(X) = a/2, fonction estim => l’estimation de a par la méthode des moments
estim <- function(x) {
  return(2*mean(x))
}


# Q2 un échantillon de taille 100. Quel semble être le paramètre a ?
n <- 100
x <- runifa(n)
estim(x)

# replicate -> exécuter plusieurs fois une instruction et accumule les résultats 
a <- replicate(1000, estim(runifa(n)))


# Q3 diagramme en boîte de 1000 estimations successives de a

# la moyenne est 11 ~ mean(a)
boxplot(a)
hist(a)


"
E(X^k) = a^k / (k+1) pour tout k≥1.
Trouver pour chaque moment l’estimateur
"
estim_k <- function(x, k) {
  return( ((k+1) * mean(x^k)) ^ (1/k) )
}


# variance des estimateurs baisse lorsque k augmente -> estimateur = max Xi
a1 <- replicate(1000, estim_k(runifa(n), 1))
a2 <- replicate(1000, estim_k(runifa(n), 2))
a5 <- replicate(1000, estim_k(runifa(n), 5))
boxplot(a1, a2, a5, names = c("k=1", "k=2", "k=5"))



########## TCL ########## 


# génère un échantillon de loi inconnue de taille n
runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}


runknown(1000)
x <- runknown(1000)
hist(x)
mean(x)    # verifions que μ = 7.2
var(x)     # écart-type = sqrt(32.36)


# Q7
"
Tracer fonction de répartition empirique de la loi inconnue avec ecdf et plot
ecdf -> permet de construire des fonctions de repartition
"
plot(ecdf(x))
ecdf(x)(2)  # fonction de répartition empirique en 2


# Q8
"
T = (X_b − μ) / (σ/√n) converge en loi vers une loi normale centrée réduite 
lorsque n augmente. vérifier que réalisations issues de la loi de T suivent 
une gaussienne centrée réduite
"


# une réalisation de T
mu <- 7.2
sigma <- sqrt(32.36)
(mean(x) - mu)/(sigma/sqrt(n))


randomT <- function(n) {
  x <- runknown(n)
  (mean(x) - mu)/(sigma/sqrt(n))
}


# 1000 réalisations de T
t1000 <- replicate(1000, randomT(n))


# moyenne semble nulle et variance empirique égale 1 -> loi normale centrée réduite
mean(t1000)
var(t1000)
hist(t1000)


# pour tracer fonction de repartition à partir des réalisations
plot(ecdf(t1000))  
curve(pnorm, add= TRUE)

hist(t1000, probability = TRUE)
curve(dnorm, add = TRUE)





########## ESTIMATION PAR MAXIMUM DE VRAISEMBLANCE ########## 


# Q14 loi exponentielle de paramètre λ > 0 et de densité λe(−λx) si x ≥ 0 et 0 sinon
f <- function(lambda, x) {
  dexp(x, rate = lambda)
}


# Q15 fonction L(λ,x) de vraisemblance avec la fonction prod
L <- function(lambda, x) {
  prod(f(lambda,x))
}


# fonction logL de log–vraisemblance
logL <- function(lambda, x) {
  sum(log(L(lambda, x))) # somme des log car plus stable que le log des produits
}

log(1e-200 * 1e-200)
log(1e-200) + log(1e-200)


# Q17 loi exp de λ=3, quel λ est le plus probable entre λ = 3.1 et λ = 2.8?
n <- 100
x <- rexp(n, rate = 3)

logL(3.1, x)
logL(2.8, x) # le plus probable pour l’échantillon x


# Q18 calcule log–vraisemblance des lambdas et affiche le graphe correspondant
lambdas <- seq(0, 6, 0.01)
logLlambdas <- sapply(lambdas, function(lambda) logL(lambda, x))
plot(lambdas, logLlambdas, type = "l")


# Q19 trouver λ le plus vraisemblable de la loi exponentielle ayant générée x 
"
fonction optimize
upper et lower fixe l’intervalle de recherche et maximum spécifie qu’on 
recherche maximum et non minimum
"
x <- rexp(n, rate = 3)
opt <- optimize(logL, lower = 0, upper = 6, maximum = TRUE, x = x)
opt$maximum


# Q20 fonction sim.EMV définissant un échantillon et calcule la réalisation du MV
sim.EMV <- function() {
  x <- rexp(n, 3)
  # Maximization de la log-vraisemblance
  opt <- optimize(logL, lower = 0, upper = 10, maximum = TRUE, x = x)
  opt$maximum
}

sim.EMV()
sim.EMV()


# Q21 déterminer estimation de E(λˆ) et de Var(λˆ)avec
# 10000 simulations de EMV et méthode des moments
sim.EMV.10000 <- replicate(10000, sim.EMV())
boxplot(sim.EMV.10000)


# Estimation de l’espérance et de la variance de λˆ
mean(sim.EMV.10000)
var(sim.EMV.10000)

# Estimation du biais
mean(sim.EMV.10000) - 3

# Valeur théorique du biais
n / (n - 1) * 3 - 3


########## Information de Fisher ########## 


# Installer puis charger la bibliothèque pracma
install.packages("pracma")
library(pracma)


# Q23 Créer fonction sim.Fisher génèrant échantillon x et calcule info de Fisher λ=3
# grad -> calcule la dérivée d’une fonction en un point
sim.Fisher <- function() {
  x <- rexp(n, rate = 3)
  logLx <- function(lambda) logL(lambda, x) # Log-vraisemblance par rapport à x
  (grad(logLx, 3))^2  # Information de Fisher
}


# Q24 simuler 1000 fois le calcul de IF d’un échantillon et donner une estimation
# Estimation de l’information de Fisher
inf.Fisher <- mean(replicate(10000, sim.Fisher()))


# Q25
(1/inf.Fisher) 
var(sim.EMV.10000)


# Q26 Retrouver la formule de IF In(λ)
# définir grad2(f,x) et calcule la dérivée seconde de la fonction f en x
grad2 <- function(f, x) {
  df <- function(x) { grad(f, x) }
  grad(df, x) 
}


sim.Fisher <- function() {
  x <- rexp(n, 3)
  logLx <- function(lambda) logL(lambda, x) # Log-vraisemblance par rapport à x
  grad2(logLx, 3)   # Information de Fisher
}

-mean(replicate(1000, sim.Fisher()))



