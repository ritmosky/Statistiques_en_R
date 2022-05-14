

########## TP1 STATISTIQUES EXPLORATOIRES ##########

sqrt(36)  # racine carrée

sin(pi/15)**2 + cos(pi/15)**2

log(exp(1.6)) # ln

log2(2**42)   # log en base 2

log10(10**3)  # log en base 10


########## VECTEUR ########## 


# les notes sur 20
notes <- c(18, 1.5, 9.5, 15.5, 0.5, 14.5, 10)
notes <- c(notes, 4)

v <- c(1, 2, 3, 4, 5)
u <- c(5, 4, 3, 2, 1)
v + 1 # on fait +1 à tous les éléments
2 * v # on fait x2 à tous les éléments

u * v   # il faut qu'ils aient la même taille
v > pi
u == 2


# nombre d’étudiants ayant plus de 6/10
notes10 <- notes/2
notes10 > 6   

sum(notes10 > 6)     # ou length(notes10[notes10>6])

sum(notes10[notes10>6])  # somme des notes > 6


# nombre d’étudiants ayant une note > 10
sum(notes > 10)    # ou length(notes[notes > 10])


# la note la plus basse parmi les notes non fractionnaires
notes[notes==floor(notes)]  # notes non fractionnaires
min(notes[notes==floor(notes)])


# notes abaissées de 2 points, mettre les notes négatives à 0
notes2 <- notes - 2
length(notes2[notes2 < 0])  # ou avec sum(notes2 < 0)
notes2[notes2 < 0] <- 0


########## FACTEUR = VECTEUR DE VARIABLES QUALITATIVES NON ORDONNÉES ########## 


# on utilise factor pour transformer en facteur
collection <- c("R", "R", "V", "B", "V")
f <- factor(collection)


# utiliser ordered pour fixer un ordre entre les modalités
ff <- factor(collection, ordered=TRUE)
ff < "V"


# utiliser levels pour changer l'ordre des modalités 
ff <- factor(collection, ordered=TRUE, levels=c("R", "V", "B"))
ff < "R"


length(ff)    # taille du vecteur
nlevels(ff)   # nombre de modalités
levels(ff)    # lister les modalités


##########  EXO ADN ########## 


# séquence d'ADN = ADNACAAGATGCCATTGTC
ADN <- c("A", "C", "A","A","G","A","T","G","C","C","A","T","T","G","T","C")

ADN <- factor(ADN)
nlevels(ADN)
levels(ADN)


# nombre de nucléotides de chaque type
table(ADN)
summary(ADN)

length(ADN[ADN==levels(ADN)[1]])
length(ADN[ADN==levels(ADN)[2]])
length(ADN[ADN==levels(ADN)[3]])
length(ADN[ADN==levels(ADN)[4]])


########## DATAFRAMES ########## 


# afficher le répertoire courant et changer de répertoire
getwd() 
#setwd("/.../.../")


# création de dataframe à partir de facteur
v <- 5:10
f <- factor(c("R", "V", "B", "R", "V", "B"))
df <- data.frame(v, f, v>7)


########## MANIPULATIONS DE FICHIERS CSV ########## 


X <- read.csv("/Users/taoufiq/Documents/school/Utc/sem02/SY02/TP/tp1_Statistiques exploratoire/sy02.data")


View(X) # afficher l'objet X dans une nouvelle fenêtre
head(X)   # lister les 6 premiers éléments
summary(X)  # résumé numérique de chaque variable


ncol(X)   # nombre de variables
nrow(X)   # nombre de lignes
names(X)  # nom des variables


# transformer le type des valeurs des colonnes
X$correcteur.median <- factor(X$correcteur.median)
X$correcteur.final <- factor(X$correcteur.final)


########## EXTRACTIONS D'ÉLÉMENTS ########## 


X[1,1]            # Extraire un élément
X[,3]             # Extraire la 3ème colonne
X[4,]             # Extraire la 4ème ligne
X[1:10,]          # Extraire les 10 premières lignes
X[c(1,3), c(1,4)]
X[,c(2, ncol(X))]     # Extraire la 2e et la dernière colonne

X[,"median"]      # Extraire la colonne median
X$median          # Extraire les 10 premières lignes


# Les étudiants ayant plus de 10 au médian
X[X$median > 10,] 

# la moyenne des notes données par le correcteur EG lors du médian
mean(X[X$correcteur.median == "EG", 2])
mean(X[X$correcteur.median == "EG", "median"])


# la proportion d’étudiants qui ont progressé
nrow(X[X$median < X$final, ]) / nrow(X)

# ou encore
mean(X$median < X$final)


########## STATISTIQUES DESCRIPTIVES ########## 


mean(X$median)        # moyenne
sd(X$median)          # écart type empirique corrigé
var(X$median)         # variance empirique corrigé
median(X$median)      # median
max(X$median)         # max
min(X$median)         # min

summary(X$median)     # résumé numérique

quantile(X$median)  # quartiles
quantile(X$median, probs = seq(0, 1, 0.1))   # déciles
quantile(X$median, probs = c(0.25, 0.75))  # 1er et 3e quartiles

IQR(X$median)         # étenndu inter-quartiles
iqr <- quantile(X$median)[4]-quantile(X$median)[2]


# la moyenne tronquée d’ordre 10 sur les notes du médian 
# cad sans les 10 premières et dernières valeurs

sort(X$median)
mean(X$median, trim=0.1)
mean(X$median[11:(length(X$median)-10)])


########## Variable qualitative


### diagramme en bâton
t <- table(ADN) # compte nombre d'occurrences de chaque modalité

# diagramme en bâtons du nombre de copies de médian corrigées par correcteur
barplot(table(X$correcteur.median))


########## VARIABLES QUANTITATIVES ########## 


# boîte à moustaches des notes obtenues au final
boxplot(X$final)

quantile(X$final, probs = c(0.25, 0.75))  # 1er et 3e quartiles
mean(X$final)

iqr <- quantile(X$final)[4]-quantile(X$final)[2]
m_inf <- quantile(X$final)[2] - 1.5 * iqr
m_sup <- quantile(X$final)[3] + 1.5n* iqr


# nombre de valeurs aberrantes
sum(X$final<m_inf)
sum(X$final>m_sup)


# Diagramme en tige et feuilles
stem(X$moyenne)   


# Histogramme
hist(X$final)  

# séparer les étudiants ayant eu plus de 15 des étudiants ayant eu moins
hist(X$final, breaks = c(0, 15, 20))

sum(X$final<15) # 115
sum(X$final>15) # 173
sum(X$final)  # 4383


length(X$final[X$final<=15]) 
length(X$final[X$final<=15])/length(X$final) / 15
length(X$final[X$final>15])/length(X$final) / 5



########## ANALYSE BIVARIÉE ########## 

# quantitative vs quantitative
plot(final ~ median, data=X)  # les 2 variables sont correlés final = f(median)


# tracer les boîtes à moustaches des notes du final en fonction des correcteurs
boxplot(final ~ correcteur.final, data=X)

# boîte à moustaches du correcteur DH
filtre <- X[X$correcteur.final == "DH","final"]
boxplot(filtre)


# graphique de dispersion des notes du final en fonction des correc- teurs
stripchart(final ~ correcteur.final, data=X)
stripchart(final ~ correcteur.final, data=X, jitter=0.1)

