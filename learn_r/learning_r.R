
ls()  # AFFICHER TOUS LES OBJETS CRÉES ou objects()
rm(a) # supprimer objet

" null, logical, numeric, complex, character, infinite, na, nan, na+1=na "

is.numeric(a) # tester si objet est une valeur numérique
as.character(a) # convertir en caractère

mode(a)  # le type de l'objet 
length(a)  # longueur de a

##### Vecteurs numériques (var quantitatives) ##### 

v=1:9 # séquence de 1 à 9

seq(1, 5, by=0.5) # avec un pas de 0.5

seq(1, 5, length=8) # on veut 8 éléments

c(1, 2, 3, 4) # avec le collecteur -> on liste les éléments

rep(1, 7) # en répétant une valeur un nombre de fois

rep(c(1,2), 3)             # en alternant
rep(c(1,2), times=3)       # en alternant
rep(c(1,2), each=3)        # en répétant chaque membre d'abord



##### Vecteurs de caractères (var qualitatives) #####

c("bonjour","hello","hej")

rep(c("rouge","bleu"), 2)
rep(c("rouge","bleu"), times=2)

rep(c("rouge","jaune","bleu"), each=2)

rep(c("rouge","jaune","bleu"), times=c(1,4,2))

format(rep(c(1,2), time=3)) # transforme chaque nombre en str

toString(rep(c(1,2), time=3)) # transforme tout en str e1,e2,e3


##### Vecteurs logiques ##### 

x <- c(-1,0,2)
x>1    # retourne un vecteur avec une comparaison pour chaque elem

(1+x^2)*(x>1) # 0 0 5


# all teste si tous les éléments correspondent
all(x>1)  

# any teste si au - un élément correspond
any(x>1)  


##### FACTEURS ##### 

y <- c("M","F","F","M","F")
yf <- factor(y) # en définissant directement les différents éléments

attributes(yf) 
levels(yf)        # liste des modalités par ordre alphabétique
nlevels(yf)       # nombre de modalités


# renommer les modalités
levels(yf) <- c("Femme","Homme")

# convertir un vecteur en facteur
z <- c(1:5,5:1)
zf <- as.factor(z) 


##### Créer des facteurs ordonnés #####

# Levels: débutant < moyen < champion

niveau<- ordered(
  c("débutant","débutant","champion","champion","moyen"),
  levels = c("débutant","moyen","champion"))


##########

X <- c(rep(10,3), rep(12,2), rep(13,4))
is.factor(X) # FALSE
is.numeric(X) # TRUE

summary(X) # affiche résumé de la variable X numeric
summary(y) # y est character  

Xf <- factor(X)

summary(Xf)  # résumé de l'objet


##### Conversion d'un facteur en numérique # ( facteur -> str -> numeric )

# conversion sans recodage des modalités
provisoire <- as.character(Xf) # "10" "10" "10" "12" "12" "13" "13" "13" "13"
as.numeric(provisoire) # 10 10 10 12 12 13 13 13 13


# conversion avec recodage des modalités à éviter
as.numeric(Xf) # 1 1 1 2 2 3 3 3 3



##### MATRICE #####

matrix(3:1,ncol=3)
matrix(c(1:6),nrow=2,ncol=3,byrow=TRUE) # range les éléments par ligne
m<-matrix(c(1:9),nrow=3,ncol=3)
matrix(2,nrow=3, ncol=3)  # matrice d’un élément unique

is.matrix(m)

mm <- seq(1,10,by=2)
as.matrix(mm)

m2 <- matrix(9:17, nrow=3, ncol=3)

ncol(m)
nrow(m)
dim(m)


##### Concaténation #####

cbind(c(1,2),c(3,4))  # par colonne

rbind(c(1,2),c(3,4))  # par ligne


##### Opérations sur matrice #####

m %*% m2  # produit matriciel

t(m)  # transposition 

diag(3)  # matrice identité d’ordre 3

diag(1:4)  # matrice diagonale avec les valeurs de vec dans la diagonale

crossprod(m, m2)  # produit croisé t(m) %*% m2

det(m)  # déterminant

eigen(m)  # diagonalisation d’une matrice


apply(m, MARGIN = 2, sum) # sommes par colonne
apply(m, MARGIN = 1, mean) # moyennes par ligne


##### LISTES #####

l <- list(c("A","B","C","A"), matrix(1:4,2,2)) 

length(l) # 2
mode(l)  # list
is.list(l)

d = list()    # ou d = list(1:3, 4:6)
d[[1]] = 1:3
d[[2]] = 4:6


d$e=7:9  # rajoute un autre élément en le nommant

###### nommer les composantes de la liste #####

names(l)   # NULL car pas de nom actuellement

names(l) <- c("vec","mat")
names(l)

l$vec # on accède à l'objet vec de l
l$mat # on accède à l'objet mat de l


##### DATAFRAME #####

a <- c("A","B","C","A")
b <- 1:4

df <- data.frame(a,b)

length(df)  # 2
attributes(df) # $names $class $row.names


##### Visualisation d’un dataframe #####

str(df)  # résumé de chaque colonne du dataframe: nom type, valeur 

View(df)  # visualisation 


##### Sélection par indice dans dataframe ##### &, |, !

v <- -2:5
v > 0        # résultat = vecteur de booléen
length(v)    # 8


# avec vecteur d'entier positif -> élément à sélect

v[c(3,5)]       # select le 3e et 5e élément
v[4]            # select le 4e élément
v[3:5]          # select le 3e 4e et 5e élément


# avec vecteur d'entier négatif -> exclure élément 

v[-2]              # exclure le 2e élément
v[c(-3,-5)]        # exclure le 3e 4e et 5e élément
v[-c(3,5)]         # exclure le 3e 4e et 5e élément
v[-5:-3]           # exclure le 3e 4e et 5e élément



##### Sélection par condition dans dataframe ##### 

v[v>0 & v!=5]       # tous les éléments du vecteurs >0 et !=5

v[v!=0]             # tous les éléments du vecteurs >0

v[!v==0]            # tous les éléments du vecteurs !=0



##### Sélection par indice dans une matrice #####

# avec vecteur d'entiers positif
m[2,2]      # élément de la 2e ligne et 2e colonne
m[2,]       # élément de la 2e ligne
m[,3]       # élément de la 2e ligne et 3e colonne

m[,3,drop=F]  # conserver la structure de matrice
m[2,,drop=F]  # conserver la structure de matrice

m[,c(2,2,1)]  # 2 fois la colonne 2 et 1 fois la colonne 1
m[c(2,2,1),]  # 2 fois la ligne 2 et 1 fois la ligne 1


# avec vecteur d'entier négatif
m[-1,] # matrice m sans la 1ere ligne
m[1:2,-1] # ligne 1 et 2  sans la 1ère colonne


##### Sélection par condition dans une matrice #####

M <- matrix(1:12,nrow=3,ncol=4)

M[,M[1,]>2]  # renvoit les colonnes oû les éléments de la 1ere ligne est > 2
M[M>9]  # renvoit les éléments de la matrice >9
M[M<5] <-NA # remplace les éléments <5 par NA
  

##### Sélection par indice dans une liste #####

l[1] # la liste composée de l'élément 1 -> length(l[1]) = 1, avec son nom

l[[2]] # retourne le 2e élément de la liste -> length(l[[1]]) = taille de l'obj

x <- c("a","a","b","c")
X <- matrix(1:8,ncol=4)
y <- c(T, T, T, F, F)
z <- matrix(c("A","B","C","D"),ncol=2)

malist <- list(comp1=x, comp2=X, comp3=y, element4=z)
malist[c(1,3)] # renvoit la liste avec les éléments 1 et 3


##### Sélection par nom dans une liste #####

malist$comp1

malist["comp1"] # la liste composée de l'élément comp1 -> length(l[1]) = 1, avec son nom

malist[["comp1"]]  # retourne l'élément comp2



##### Sélection dans un dataframe ##### 

x1 <- c("A","B","C",rep("D",3))
y1 <- 1:6
z1 <- c(seq(10,45,length=5),-10)
mondf <- data.frame(x1, y1, z1)

##### Sélection par indice ou par nom #####

mondf[1:4,2:3]  # sélectionner les 4 premières lignes et les colonnes 2 et 3

mondf$z1  # select la colonne z1
mondf["z1"]  # select la colonne z1 sous forme de tableau

mondf$z1[2:3]  # sélect les lignes 2 à 3 de la colonne z1


##### Sélection par condition #####

mondf[mondf$y1>4,]  # sélect toutes les lignes qui satisfont la condition

mondf[(mondf$y1>4)|(mondf$z1>17),]

mondf[(mondf$y>4)&(mondf$z>17),]

mondf[mondf$y>4,1:2] # équivalent à
mondf[mondf$y1>4,c('x1','y1')]


##### Tableau de données ##### 

T <- matrix(c(1,2,3,4),2,2)
rownames(T) <- paste("ligne", 1:2, sep=".")  # renommer les lignes
colnames(T) <- paste("X", 1:2, sep=".")  # renommer les colonnes

Y <- matrix(11:16,3,2)
colnames(Y) <- paste("Y",1:2,sep="")

rbind(T,Y)  # concaténation de lignes

x <- c("A","B","C","A")
y <- 1:4
mondf1<- data.frame(x,y)
mondf2<- data.frame(x=c("C","D"), y=1:2) # on renomme les colonnes pour effectuer la concaténation 
cbind(mondf1, mondf2)
rbind(mondf1, mondf2)



##### Fusionnez plusieurs dataframes #####

mondf4 <- data.frame(x=c("A","A","D"), taille=seq(180,190,by=5))

merge(mondf1,mondf4) # jointure interne

merge(mondf1,mondf4, all.x=TRUE) # jointure à gauche

merge(mondf1,mondf4, all.y=TRUE) # jointure à droite

merge(mondf1,mondf4, all=TRUE) # jointure externe


##### Localiser données manquantes #####
ozoneNA <- readRDS("ozoneNA.rds")
ozo <- ozoneNA[1:4,1:7]
is.na(ozR)

# which -> ressortir les indices des éléments satisfaisant une condition
which(is.na(ozR), arr.ind=TRUE)  # connaitre les indices et lignes des NA
indligneNA <- which(is.na(ozR),arr.ind=TRUE)[,1]


"
sample -> tirage aléatoire de n valeurs entre 1 et 10
replace -> si tirage est fait avec ou sans remise
letters -> contient alphabet minuscule letters[1] = a
LETTERS -> contient alphabet majuscule letters[1] = A 
"

# 10 nb entre 1 et 10 | 100 lettres entre a et c en majuscule et minuscule
dfr <- data.frame(nb = sample(1:10, 100, replace = TRUE),
                 LT = sample(LETTERS[1:3], 100, replace = TRUE),
                 lt = sample(letters[1:3], 100, replace = TRUE))


"
fonction table permet
- la distribution d’une variable s'il y a 1 seul paramètre
- un tableau croisé s'il y a 2 paramètres
"

table(dfr$lt)  # distribution des éléments de lt
table(dfr$nb)  # distribution des éléments de nb
table(dfr$lt,dfr$LT) # tableau croisé


##### EXO #####

# importation des données
notes <- read.csv("notes.csv", sep=";", header=TRUE)
eleves <- read.csv("eleves.csv", sep=",", header=TRUE)


fusion <- merge(eleves, notes, all.x=TRUE) # jointure à gauche
merge(eleves, notes, by='identifiant')

View(notes)
View(eleves)
View(fusion)


# connaitre les valeurs manquantes
which(is.na(fusion),arr.ind=TRUE)  # voir s'il existe des valeurs manquantes
length(which(is.na(fusion),arr.ind=TRUE)[,1]) # le nb de NA


# connaitre  les id des personnes ayant eu 20
unique(notes[notes["notes"]==20, "identifiant"])
notes[notes$notes==20,]

# connaitre si il existe des valeurs manquantes
which(is.na(notes), arr.ind = TRUE)

# connaitres les notes renseignées
notes[!is.na(notes[4]), "notes"]
mean(notes[!is.na(notes[4]), "notes"])

# connaitres les personnes ayant eu 0
notes[!is.null(notes[4]), "identifiant"]
length(notes[!is.null(notes[4]), "identifiant"])
notes[notes$notes==0,]


# moyenne globale des femmes > celle des hommes
fusion[fusion$sexe=="F", "notes"]
mean(fusion[fusion$sexe == 'F', 'notes'], na.rm=TRUE)
fusion[fusion$sexe=="M", "notes"]
mean(fusion[fusion$sexe == 'M', 'notes'], na.rm=TRUE)


# les notes en mats sont plus élevées qu'en svt
mean(fusion[fusion$matieres=="SVT", "notes"], na.rm=TRUE)
mean(fusion[fusion$matieres=="Math", "notes"], na.rm=TRUE)

# eleve le plus jeune et age
unique(fusion[fusion$age==min(fusion$age), c("noms","age")])

# eleve ayant eu 20
unique(fusion[fusion["notes"]==20, "noms"])

##
temp = c(20, 21, 17.5, 32, 30, 25)
jour = c('Lu', 'Lu', 'Lu', 'Ma', 'Ma', 'Ma')
lieu = c('Nice', 'Lyon', 'Paris', 'Nice', 'Lyon', 'Paris')

dff = data.frame(temp, jour, lieu)
# tous les lieux dont la temp sur un autre jour que mardi est = max du lundi
dff[dff$jour != 'Ma' & dff$temp == max(dff$temp[dff$jour == 'Lu']), 'lieu']

