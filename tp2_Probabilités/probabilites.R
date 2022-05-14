

########## TP2 PROBABILITÉS ##########

library(MASS)   # charger une bibliothèque
head(painters)   # painters est un dataFrame


# visualiser la distribution des notes pour chaque critère avec un hist
hist(painters$Composition, main = "Composition", xlab = "Note")
hist(painters$Drawing, main = "Dessin", xlab = "Note")
hist(painters$Colour, main = "Couleur", xlab = "Note")
hist(painters$Expression, main = "Expression", xlab = "Note")


# moyenne des quatre notes de chaque peintre puis histogramme
somme <- (painters$Composition+painters$Drawing+painters$Colour+painters$Expression)
moyenne <- somme / 4
hist(moyenne) 


# Résumé Numériques
n <- length(moyenne)
moy_empiriq <- sum(moyenne) / n  # ou mean(moy_empiriq)


# variance non corrigée
v_nc <- sum((moyenne - moy_empiriq)^2) / n   
v_nc <- (n-1) * var(moyenne) / n  


# variance empirique corrigée
v_c <- sum((moyenne - moy_empiriq)^2) / (n - 1)       
var(moyenne)  


# écart type corrigé
sd(moyenne)    
sqrt(v_c) 
sqrt(var(moyenne))


# écart type non corrigé
sqrt(v_nc) 



########## Proba ########## 

"
norm -> loi normale
unif -> loi uniforme
t -> loi de Student
f -> loi de Fisher
exp -> loi exponentielle
pois -> loi de Poisson
binom -> loi binomiale
chisq -> chi square χ2

pour une loi quelconque LL

rLL -> simuler des données r=random
dLL -> densité de probabilité
pLL -> fonction de repartition
qLL -> fractiles
"


# densité de la loi uniforme entre [0,2] 
dunif(1, min=0, max=2)  # en 1
dunif(6, min=0, max=2)  # en 6

# fonction de repartition de la loi uniforme entre [0,2] 
punif(1, min=0, max=2)  # en 1
punif(2, min=0, max=2)  # en 2

# générer 3 valeurs suivant une loi uniforme entre [2,5]
runif(3, min=2, max=5)  

# fractile de la loi loi uniforme entre [2,5]
qunif(0.25, min=2, max=5)  # 1er quartile  


# probabilitée qu'une variable normale centrée-réduite est supérieure à 3
1 - pnorm(3,mean=0,sd=1)  # ou juste pnorm(3)

# proba que variable normale d’espérance 35, d’écart-type 6 est inférieure à 42
pnorm(42,mean=35,sd=6)


# proba que variable normale d’espérance 35, d’écart-type 6 est comprise entre 40 et 50
pnorm(50,mean=35,sd=6) - pnorm(40,mean=35,sd=6)


# proba d'obtenir n−1 faces sur n lancers de pièce équilibrée avec n = 5, 10, 30
n <- 5
dbinom(n-1, size=n, prob=0.5)
dbinom(9, size=10, prob=0.5)
dbinom(29, size=30, prob=0.5)


# proba d'obtenir + de 14 faces sur 20 lancers d’une pièce équilibrée
1-pbinom(14, size=20, prob=0.5)


# proba d'obtenir entre 10 et 15 faces sur 20 lancers d’une pièce équilibrée
pbinom(15, size=20, prob=0.5) - pbinom(9, size=20, prob=0.5)
sum(dbinom(10:15, size=20, prob=0.5))  # idem


# fractiles d’ordre 0.05, 0.1, 0.9 pour les lois suivantes
alpha <- c(0.05, 0.1, 0.9)


# loi normale centrée réduite 
qnorm(alpha, mean=0,sd=1)


# loi du χ2 à 10 degrés de liberté
qchisq(alpha, df=10)


# loi de Student à 5 degrés de liberté
qt(alpha, df=5)


# loi de Fisher à 2 et 5 degrés de liberté
qf(alpha, df1=2, df2=5)



########## Implémentation d’une loi de probabilité ########## 


carre <- function(x) {
  y <- x * x
  return(y)
}

carre(3)
carre(1:5)


# loi L(b)
dloi <- function(x,b){
  if (b <= 0)
    stop("on doit avoir b > 0")
  a <- 2 / b^2
  y <- a * x
  m <- x<0 | x>b
  x[m] <- 0
  return(y)
}


# from et to = bornes entre lesquelles tracer la courbe
curve(dloi(x, 3), from = -5, to = 5)
dloi(-1:5, 3)


# fonction ploi(x, b) donne la fonction de répartition F de L(b)
ploi <- function(x,b){
  if (b <= 0)
    stop("on doit avoir b > 0")
  y <- x^2 / b^2
  y[x<0] <- 0
  y[x>=b] <- 1
  return(y)
}


# from et to = bornes entre lesquelles tracer la courbe
curve(ploi(x, 3), from = -5, to = 5)
ploi(-1:5, 3)


# fonction qloi(alpha, b) qui renvoie les fractiles fα = F−1(α) de la loi L(b)
# F−1(0) = 0 et F−1(1) = b, F−1 (α) = b√α.
qloi <- function(alpha,b){
  if (b <= 0)
    stop("on doit avoir b > 0")
  if (any(alpha < 0) | any(alpha > 1))
    stop("on doit avoir 0 <= alpha <= 1")
  y <- b * sqrt(alpha)
  y[alpha==0] <- 0
  y[alpha==1] <- 1
  return(y)
}


# from et to = bornes entre lesquelles tracer la courbe
curve(qloi(x, 3), from = 0, to = 1)
qloi(0.25, 3)


# générer des réalisations suivant la loi L(b)
rloi <- function(n, b){
  if (b <= 0)
    stop("on doit avoir b > 0")
  if (n < 1)
    stop("n doit être un entier strictement positif")
  x <- runif(n)  # n realisation d'une U[0,1]
  u <- qloi(x,b)
  return(u)
}


# visualiser la distribution
hist(rloi(5, 3))
rloi(5, 3)

plot(1:100, dnorm(1:100, 50,10))
