

##########  RÉGRESSION LINÉAIRE ########## 

# rég lin avec fonction lm(s~e, data=donnees) avec e et s = colonnes d’un data.frame


# y=2x+1, Intercept=ordonnée à l'origine = 1 et  varx=pente=2
donneese <- data.frame(varx = c(0, 1, 2, 3),
                       vary = c(1, 3, 5, 7))

lm(vary~varx, data = donneese)


# Q1 les estimations de l’ordonnée à l’origine (intercept) â et de la pente ^b
# intercept=1.033 et pente=1.789
donnees <- data.frame(varx = c(0, 0.2, 0.3, 0.6),
                      vary = c(1.01, 1.44, 1.55, 2.1))

reg <- lm(vary~varx, data = donnees)
summary(reg)


# Q2 tracer les points (x,y) avec plot, droite des moindres carrés avec abline

plot(donnees$varx, donnees$vary)

abline(1.033,1.789) # en argument, intercept et la pente



# Q3 somme des résidus = 0, image de x_bar par droite des moindres carrés = y_bar

residuals(reg)
sum(residuals(reg)) # somme des résidus = 0

xb <- mean(donnees$varx)
yb <- mean(donnees$vary)

a_chapeau <- reg$coefficients[1]
b_chapeau <- reg$coefficients[2]

a_chapeau * xb + b_chapeau
points(xb,yb)  # placer sur le graphique


# EQUATION D'ANALYSE DE LA VARIANCE

# y_chapeau_i = m$fitted.values = fitted(reg)
# yi-y_chapeau_i = m$residuals = residuals(m)
# â = ord_origine = m$coefficients[1], b_chapeau = pente = m$coefficients[2]


# Q4 vérifions 


# variance totale S_y^2
n <- length(vary)
x <- donnees$varx 
y <- donnees$vary

var(y) * (n-1)/n
Sy2 = sum((y-mean(y))^2) / n


# variance expliquée par le régression S_reg (corrigée)
S_reg <- sum((fitted(reg) - mean(y))^2) / n
(n-1)/n *var(fitted(donnees))

# variance résiduelle S_res
S_res <- sum((residuals(reg) - mean(y))^2) / n
mean(residuals(reg)^2)

# S_y^2 = S_reg+S_res
S_reg+S_res
Sy2


# coefficient de détermination R2 = S_reg / Sy2
cor(y, fitted(reg))



# HOMOSCÉDASTICITÉ, INDEPENDANCE, NORMALITÉ DES RESIDUS 
attach(anscombe)  # jeu de données (x1,y1), (x2,y2), (x3,y3), (x4,y4)

plot(x1,y1) # on utilise ça car plus proche d'une reg
lm(y1 ~ x1)

plot(x2,y2)
lm(y2 ~ x2)

plot(x3,y3)
lm(y3 ~ x3)

plot(x4,y4)
lm(y4 ~ x4)


# Q6 analyse des résidus et validité des hypothèses
' hypothèses
Lin = relation entre yi et xi est linéaire
Norm = normalité des résidus car normalité des erreurs
Ind = indépendance des résidus car indépendance des erreurs
Hom = homoscédasticité => ∀i, Var(εi) = σ2 '

reg1 <- lm(y1 ~ x1, data=anscombe)
reg2 <- lm(y2 ~ x2, data=anscombe)
reg3 <- lm(y3 ~ x3, data=anscombe)
reg4 <- lm(y4 ~ x4, data=anscombe)


# Pour Norm, diagramme quantile–quantile des résidus avec qqnorm et qqline

qqnorm(residuals(reg1))
qqline(residuals(reg1))

qqnorm(residuals(reg2))
qqline(residuals(reg2))

qqnorm(residuals(reg3))
qqline(residuals(reg3))

qqnorm(residuals(reg4))
qqline(residuals(reg4))


# Pour Hom et éventuellement Ind et Lin), résidus standardisés (rstandard) en fonction
# des prédictions (fitted.values) ou en fonction des valeurs de la var explicative x

rstandard((reg1))
plot(fitted(reg1), rstandard((reg1))) # il ne doit pas avoir de lien

plot(fitted(reg2), rstandard((reg2))) # dependance des bruits
plot(fitted(reg3), rstandard((reg3))) 
plot(fitted(reg4), rstandard((reg4))) 



# PREDICTION

df <- read.csv(('tp5_Reg_Lin/hooker-data.data'))

# Q7 étude de régression linéaire expliquant la pression atmosphérique

summary(df)
Pression <- df$Pression 
Temp <- df$Temp

regP <- lm(Pression ~ Temp)

plot(df$Temp, df$Pression)
abline(regP$coefficients[1], regP$coefficients[2])


# Q8 IC sur les coefficients de la droite des moindres carrés pour 1-alpha=0.99

confint(regP,level=0.99)

# Q9 IC sur la pression pour une température = 97°C

test <- data.frame(Temp=c(60,97))
predict(regP, newdata=test)
predict(regP, newdata=test, interval = "prediction")




getwd()
setwd("documents/school/utc/sem02/sy02/tp")
load("cctp_P2022.RData")




