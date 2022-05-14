

########## TP4 INTERVALLES DE CONFIANCE ########## 


########## FONCTIONS PIVOTALES ########## 


# Q1 les Xi sont gaussiens, donnons une observation du théorème de Fisher
# 1ere formule ~ loi du chisq (esp de loi de chisq E ~ n-1 si n deg de libertés)
chisq1<- function() {
  n <- 1000
  mu <- sample(0:10, 1)
  sigma <- sample(1:10, 1)
  x <- rnorm(n, mean = mu, sd = sigma)
  (n - 1) * var(x) / sigma^2
}


a1 <- replicate(1000, chisq1())
hist(a1, probability = TRUE)
curve(dchisq(x, df = n-1), add=TRUE)


# Q2 les Xi sont gaussiens, donnons une observation du théorème de Fisher
# 2e formule ~ loi de Student 
stu1<- function() {
  n <- 1000
  mu <- sample(0:10, 1)
  sigma <- sample(1:10, 1)
  x <- rnorm(n, mean = mu, sd = sigma)
  (mean(x)-mu) * sqrt(n) / sd(x)
}


a2 <- replicate(1000, chisq2())
hist(a2, probability = TRUE)
curve(dt(x, df = n-1), add=TRUE)



########## UNTERVALLES DE CONFIANCE ########## 

# Q3 cas sigma connu, Xi gaussien, donnons une réalisation de cet IC
# 1-alpha = 0.95, cas sigma connu

n <- 1000
alpha <- 0.05
mu <- 42
sigma <- pi
x <- rnorm(n, mean=mu, sd=sigma)

mean(x) + c(-1,1) * qnorm(1-alpha/2)*sigma/sqrt(n)


# Q4 cas sigma inconnu, Xi gaussien, donnons une réalisation de cet IC
mean(x) + c(-1,1) * qt(1-alpha/2, df=n-1)*sigma/sqrt(n)

t.test(x, conf.level = 1 - alpha)$conf.int



# Q5 fonction renvoyant IC sur l’espérance sous forme d’un vecteur de longueur 2

gen_IC <- function (x, alpha) {
  n <- length(x)
  mean(x) + c(-1,1) * qt(1-alpha/2, df=n-1)*sigma/sqrt(n)
}


# retourne une matrice
ICS <- replicate(100, gen_IC(rnorm(n, mean=mu, sd=sigma), alpha))
ICS_200<- replicate(200, gen_IC(rnorm(n, mean=mu, sd=sigma), alpha))



# Q7 visualiser IC avec fonction plot_ICs 
# relation entre niveau de IC=95%, nb d’intervalles verts, nb total d’intervalles
# vert / total = 95%

setwd("documents/school/utc/sem02/sy02/tp")
source("utils.R")

plot_ICs(ICS, mu)
plot_ICs(ICS_200, mu)


# Q8 avec xlim
n <- 1000
sigma <- 6
mu <- 3
alpha <- 0.01
ICs1 <- replicate(100, gen_IC(rnorm(n, mean=mu, sd=sigma), alpha))
plot_ICs(ICs1, mu)

plot_ICs(ICs1, mu, xlim=c(2,4))


# Q9 influence de la dispersion de l’échantillon sur la largeur moyenne de IC
# renvoit TRUE si l’intervalle contient le paramètre

ICs <- replicate(4, gen_IC(rnorm(n, mean=mu, sd=sigma), alpha))

ICs[2,] > mu  # plus petit que borne sup
ICs[1,] < mu  # plus grand que born inf

m <- ICs[2,] & ICs[1,] 
mean(m)

plot_ICs(ICs[ICs<mu], mu, xlim=c(6,8))


# Q10 proportion d’intervalles contenant le paramètre = taux de recouvrement
n <- 100
alpha <- 0.05
param <- 3

