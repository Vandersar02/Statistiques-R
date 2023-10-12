X<-7+2
X
chiffre <- c(1,2,3,4)
mode(chiffre)
lettres <- c("a", "b", "c")
mode(lettres)
class(lettres)
b <- as.integer(chiffre)
class(b)
obj <- seq(2,10,by=3)

comp <- 1:9
neg <- -9:1

vect <- rep(c(2,3,7),times=2)
h <- 4:10
h[5]
h[c(2,5,3)]
h[-c(2,5,3)]
cours <- c("maths", "francais", "R")
"Francais" %in% cours
A <- matrix(c(3,6,7,5),c(2,2))
B <- matrix(c(3,4,3,9),nrow = 2, ncol = 2)
A%*%B
B%*%A

A%*%A - 2*A%*%B+B%*%B

array(1:12, c(2,4,3))

bd <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/ex1.csv")
dim(bd)
max(bd$height)
min(bd$height)
mean(bd$height)
var(bd$height)
sd(bd$height)
sort(bd$height)
sort(bd$height, decreasing = TRUE)


aw <-c(3,2,5,1,6)
max(aw)
min(aw)

arr <- 0
for (i in 1:100){
  arr[i] = i
}
sum(arr)
prod(arr)

sum(c(1,10))



bgt <- 0
for (i in 1:100){
  bgt[i] = i*i
}

sum(bgt)


sum(c(1,20),times=2)

ls()

# Cours 2


bd <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/ex1.csv")

#affiche le nombre de ligne et le nombre de colonnes
dim(bd)

#nbre de variable
length(bd)

#donne des infos sur le fichier sa structure
str(bd)

#Matrix
#-------------------------------------------------------------------------------
X <- matrix(c(3,4,3,9),nrow = 2, ncol = 2)


#affiche le nombre de ligne et le nombre de colonnes
dim(X)

#permet d'afficher la deuxieme  colonnes
X[,2]
#deuxieme ligne, premier colonnes
X[2,1]

#Analyse univarie
enfant <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9, 2, 1, 9, 2, 9, 1,1, 9, 3, 9, 1)

# Trier par ordre de grandeur
sort(enfant, decreasing = TRUE)

# le plus grand
max(enfant)
# Le plus petit
min(enfant)

# nbre total d'observation
length(enfant)

# question 4
# *frequence absolue simple
freq <- table(enfant)
sum(freq)

h <- data.frame(freq)
sum(h$Freq)

# *frequence relative
freq/length(enfant)
prop.table((freq))


G <- round(freq/length(enfant),3)
data.frame(G)


# Cours 3
effant <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9, 2, 1, 9, 2, 9, 1,1, 9, 3, 9, 1)

#tableau de frequence
effective <- table(enfant)

#
table1 <- data.frame(effective)

x <- data.frame(
  Nom = c("Jean", "Regis", "Henry"),
  Prenom = c("Marline", "Carline", " Charles"),
  age = c(34, 42, 29),
  sexe = c("F", "F", "M"),
  taille = c(1.74, 1.65, 1.80)
)

names(x) <- c("Name", "Last-Name"," age", "sex", "heigth")
x

#_______________________________________________________________________________
# With freq
library(questionr)
freq(effective, cum=TRUE, total=TRUE, exclude = "n/a")


#arrondi 
freq(effective, cum=TRUE, total=TRUE, exclude = "n/a", digit=0)

#with variable enfant
pq <- freq(enfant, cum=TRUE, total=TRUE, exclude = "n/a", digit = 2)

names(pq) <- c("n", "eff-rel", "eff-cum", "%cum","Val-cum")


#_______________________________________________________________________________
# With





child <- c( 7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9, 2, 1, 9, 2, 9, 1,
            1, 9, 3, 9, 1)

min(child)
max(child)
length(child)

eff <- table(child)
v <- data.frame(eff)
n <- sum(eff)
round(eff/n,2)
datenfant <- round(prop.table(eff),2)
data.frame(datenfant)
tablette <- transform(eff)
x <- data.frame(eff)
l <- data.frame(eff,  Fequence= round(x$Freq/sum(x$Freq),2)*100,FrequenceCum= cumsum(eff),FrequenceCumcrois = round(cumsum(eff)/sum(eff), 2)*100,FreqCumDecroissant= with(x, rev(cumsum(rev(Freq))))) 
o <- round(mean(child),2)
median(child)

plot(eff, main= "Répartition du nombre d'enfants\n par famille", 
     xlab="Nombres d'enfants", ylab = "Nombre de familles", col=c("blue"),
     col.main="blue", las=1, col.axis="red")

library(ggplot2)
col= c("red", "pink", "yellow", "purple","green", "orange", "gray")
ggplot(tablette) + aes(x = child, weight = Freq) + geom_bar(fill = col, width = 0.3)

library(questionr)
z <- freq(child, cum = TRUE, total = TRUE, digits = 2, exclude = NA)
names(z) <- c("Effectif", "Frequence", " FreqCumu")






#_______________________________________________________________________________

d <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/ex1.csv")

#affiche le commencement
head(d,1)

#or

d[1,]

#par la fin
tail(d)

#type du contenu
str(d)

#
levels(d$sex)

#nombre de lignes et de colonnes
dim(d)

#noms des colonnes qui le composent
names(d)

d$height

attach(d)
height

#donne le min et le max
range(height)

#...fait la meme chose mais avec plus de details
summary(height)

#pou verifye konbyen fwa li repete
table(cut(height,7))





tab <- fdt(d$height, start = 117, end = 208, h=13)
tab
#******************************************************************************
#==============================================================================
enfants <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9,
             2, 1, 9, 2, 9, 1, 1, 9, 3, 9, 1)

tate <- table(enfants)
sum(tate)
x <- data.frame(table(enfants))
sum(x$Freq)
length(tate)
round(tate/length(enfants),2)
tate/sum(tate)
round(prop.table(tate),3)
cumsum(tate)
install.packages("questionr")
library(questionr)

#Tableau complet
enf <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9,
         2, 1, 9, 2, 9, 1, 1, 9, 3, 9, 1)
#effectif <- table(enf)
tab1 <- transform(effectif)
library(questionr)
Y <-freq(enf, cum = TRUE, total = TRUE, digits = 2, exclude = NA)
names(Y)<- c("Effectif", "%Freq", "%FreqCum")


# Creation data.frame
X <- data.frame(
  Nom = c("Jean", "Bobby", "Nixson"),
  Prenom = c("Marta", "Luc", "Henry"),
  age = c(32, 54, 72),
  sexe = c("F", "M", "M"),
  taille = c(1.56, 1.74, 1.73)
)
names(X) <- c("First Name", "Last Name", "Age", "Sex", "Heigth")

#Creation de son prob tableau
effect <- table(enf)
tab <- data.frame(effect)
tab2 <- data.frame(effect, rel= round(tab$Freq/sum(effect),2), EffCum= cumsum(tab$Freq), FreCum= round(cumsum(tab$Freq)/sum(effect),2)*100)

plot(effect, main= "Répartition du nombre d'enfants\n par famille", 
     xlab="Nombres d'enfants", ylab = "Nombre de familles", col=c("blue"),
     col.main="blue", las=1, col.axis="red")
#library(ggplot2)
#col= c("red", "pink", "yellow", "purple","green", "orange", "gray")
#ggplot(tab1) + aes(x = enf, weight = Freq) + geom_bar(fill = col, width = 0.3)

boxplot(c(enf), main="Boîte à moustache associée à la variable\nnombre
d'enfants par famille", xlab="Nombres d'enfants", ylab="Nombre de
familles",
        col=c("yellow"), col.main ="purple", col.lab = "purple")

#Variable quantitative continue
bd <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/ex1.csv")
head(bd,1)
#ou
bd[1,]
#Par defaut li afiche 6 premye ligne yo
head(bd)
#6 derniers yo
tail(bd)
#Pou nou jwenn structure fichier a
str(bd)
levels(bd$sex)
nrow(bd)
ncol(bd)
dim(bd)
names(bd)
bd$height
attach(bd)
height

range(height)
summary(height) 

#Pou divize done yo nan interval
a0 <- cut(height,7)
data.frame(table(a0))
a <- cut(height, c(117, 130, 143, 156, 169, 182, 195, 208),
         include.lowest= TRUE, right = FALSE)
bd <- table(a)
bd2 <- data.frame(bd)
bd3 <- data.frame(bd, cumul = cumsum(bd2$Freq), rel = prop.table(bd2$Freq))
names(bd2) <- c("Classes", "Effectifs")
bd2

library(fdth)
tab <- fdt(height, start =117, end=208, h=13)

#Calcul de la moyyenne
x.bar <- sum(height)/length(height)
#ou
mean(height)

#Calcul de variance
var(height)
#Ou
x.var <- sum((height - x.bar)^2)/(length(height) -1 )

# Calcul de l'ecart type ou deviation standard
sqrt(x.var)
#Ou
sd(height)

#Representation graphique
boxplot(height) 






#===============================================================================
#*******************************************************************************
enfants <- c(7,7,3,7,7,7,9,2,8,9,8,2,8,8,2,7,9,8,2,9,9,2,1,9,2,9,1,1,9,3,9,1)
sort(enfants)
sort(enfants) #2.On trie les valeurs du veucteurs enfants par ordre croissant
min(enfants)
max(enfants)
length(enfants) #3.La taille de l'échantillon
rm(list=ls())

library(questionr)
a <- freq(enfants, cum = TRUE, total = TRUE,  digits = 0, exclude = NA)
#=============================================================================

freq <- table(enfants)
data.frame(freq)
round(freq/sum(freq), 2)
round(freq/length(enfants), 2)
tab <- transform(freq, FreqCum=cumsum(freq), rel=round(prop.table(Freq),2), 
                 FreqCumRel=round(cumsum(freq)/sum(freq), 2))

#=============================================================================

x <- data.frame(freq)
data.frame(freq, FreqCum=cumsum(freq), FreqRel=round((x$Freq/sum(x$Freq)),2), 
           FreqCumRel=round(cumsum(freq)/sum(freq),2))


plot(table(enfants), main="Répartition de 32 étudiants de l'UE selon le nombre 
     d'enfants par famille", xlab="nombre d'enfants par famille", ylab="Fréquences 
     simples", col=I("purple"), col.main="purple", col.lab="blue")

boxplot(c(enfants), main="Boîte à moustache associée à la variable nombre 
        d'enfants/famille", xlab="nombre d'enfants par famille", ylab="l'axe des
        Y", col.main="purple", col=I("yellow"))

# la fonction de répartition empirique ou empirical cumulative distribution function
#Empirical Cumulative Distribution Function 

plot(ecdf(enfants), main= "Courbe cumulative de la fonction de répartition", 
     xlab="Hauteur en cm", ylab = "fonction de répartition empirique", 
     col.main ="purple", col=c("green"))

#=============================================================================

x <- enfants
median(x)
x.bar <- sum(x)/length(x)
mean(x)

x.var <- sum((x - x.bar)^2)/(length(x)-1)
var(x)

std1 <- (x.var)^0.5
std2 <- sqrt(x.var)
std3 <- sd(x)

#=============================================================================

ex.1 <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/ex1.csv") #1a

head(ex.1,6) #1b
tail(ex.1, 3)

names(ex.1) #3
str(ex.1)   #2

nrow(ex.1) #2a
ncol(ex.1) #2b

length(ex.1$sex)
length(ex.1$height)

dim(ex.1)
ex.1$height #5

range(ex.1$height)

h7class <- cut(ex.1$height, 7)
table(h7class)

h7class2 <- cut(ex.1$height, c(117, 129, 141, 153, 165, 176, 188, 200), 
                include.lowest = TRUE, right = FALSE) 

height.tab <- table(h7class2)
d <- data.frame(height.tab)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

h7class3 <- cut(ex.1$height, c(117, 130, 143, 156, 169, 182, 195, 208), 
                include.lowest= TRUE, right = FALSE)

height.tab <- table(h7class3)
d <- data.frame(height.tab)

c = with(ex.1, rev(cumsum(rev(d$Freq)))) 


sum(height.tab)
sum(a$rel)


install.packages('fdth')
library(fdth)
Height_hdt <- fdt(ex.1$height, start=117, end=208, h=13)
summary(ex.1$height)

#==========================================================================================
c = with(ex.1, rev(cumsum(rev(d$Freq)))) 

a <- data.frame(height.tab, cumul=cumsum(d$Freq), rel=round(d$Freq/sum(d$Freq),2)*100,
                cumrel=round(cumsum(d$Freq)/sum(d$Freq), 2), c)

summary(ex.1$height)
boxplot(ex.1$height, main="Hauteur des individus en cm.", col.main ="blue", col="light blue")
boxplot(ex.1$height ~ ex.1$sex, main="Graphiques de la hauteur des deux sexes", col = c("pink","light blue"), col.main="blue", na.rm=TRUE)


h <- hist(ex.1$height, main="Distribution des hauteurs en cm.", border = "black",
          xlab = "Hauteur en cm", ylab = "Nombre de personnes",
          col=c("blue", "yellow", "green", "red", "pink", "light blue", "yellow","orange", "purple"))
points(h$mids, h$counts, lwd=3, pch=19, col="blue")
lines(h$mids, h$counts, lwd=3, col=I("dark gray"))



x <- c(12, 4, 21, 17, 13, 9)
hist(y, freq = FALSE, col = "thistle1")
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4", lwd = 2, add = TRUE)

# On définit une fonction pour calculer le mode

x <- ex.1$height

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[which.max(tabulate(match(x, u)))]
}

find_mode(x)

a <- c(3, 5, 7, 9, 11, 13, 13, 19)

# Mean of Height 
x.bar <- sum(ex.1$height)/length(ex.1$height)
mean(ex.1$height)

# variance of Height
x.var <- sum((ex.1$height - x.bar)^2)/(length(ex.1$height) - 1)
var(ex.1$height)

# Standard deviation of the height

sqrt(x.var)
sd(ex.1$height)

median(ex.1$height)

#===============================================================================


h7class4 <- cut(ex.1$height, c(117, 130, 143, 156, 169, 182, 195, 208), include.lowest= TRUE, right = FALSE)
height.tab <- table(h7class4)
d <- data.frame(height.tab)

c = with(ex.1, rev(cumsum(rev(d$Freq)))) 
a <- data.frame(height.tab, cumul=cumsum(d$Freq), rel=round(d$Freq/sum(d$Freq),2)*100,
                cumrel=round(cumsum(d$Freq)/sum(d$Freq), 2), c)

plot(x = 1:length(a$cumrel),  y= a$cumrel, main="Courbe des fréquences cumulées croissantes associée  
     à la variable hauteur des sujets en cm", col.main="purple", col=I("red"), lwd=3,
     xlab="x-axis", ylab = "y-axis", type = 'o' )

plot(x = 1:length(a$c),  y= a$c, main="Courbe des fréquences cumulées décroissantes associée  
     à la variable hauteur des sujets en cm", col.main="purple", col=I("red"), lwd=3,
     xlab="x-axis", ylab = "y-axis", type = 'o' )


# questionr: contient un unsemble de fonctions permettant de faciliter le 
# traitement et l'analyse des enquêtes.


# tidyverse: Ce paquet aide à transformer et à mieux présenter les données. 
# Il permet d'importer, de trier, de manipuler et de visualiser les données. 

# psych: est une boîte à outils utilisée dans l'analyse multivariée, 
# la psychologie, les tests de personalité, l'analyse de fiabilité, etc..



library(ggplot2)
library(tidyverse)

#=================== Paquet Mass =========================
library(questionr)
data(package="questionr")
data(hdv2003)
db1 <- hdv2003

library(MASS)
data(package="MASS")
data(rp2012) #db3
db3 <- rp2012
data(rp2018) #db4
db4 <- rp2018
data(women)
db5 <- women

library(plotrix)
data(package="plotrix")
data(death_reg)
db6 <- death_reg
data(soils)
db7 <- soils
data(12010)

library(dplyr)
data(package="dplyr")
data(storms)
db8 <- storms
data(starwars)
db9 <- starwars

library(psych)
data(package="psych")
data(Bechtoldt)
db10 <- Bechtoldt
data(Bechtoldt.1)
db11 <- Bechtoldt.1
data(Schmid)
db12 <- Schmid
data(Dwyer)
db13 <- Dwyer
data(Gleser)
db14 <- Gleser
data(Gorsuch)
db15 <- Gorsuch
data(Harman.5)
db16 <- Harman.5
data(Harman.8)
db17 <- Harman.8
#+++++++ Others +++++++++++

data(package="ggplot2")



data(happy)
db2 <- happy

data(rp2012)  #Pour importer et télécharger la base de données
db3 <- rp2012

db <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/Forbes Global.csv")

#==================================================================================

# 1ère facon
tab <- table(db$Continent)
tb2 <- data.frame(round(prop.table(tab), 2))
names(tb2)= c("Continents", "Frequences")

# 2ème facon
library(questionr)
tb3 <- freq(tab, cum=TRUE, total=TRUE, sort="inc", digits=2, exclude = NA )
tb4 <- data.frame(addmargins(tab))

# Graph
library(MASS)
pie(xtabs(~db$Continent), main="continents", col=c("gray", "red", "yellow", "blue", "green", "pink"))

library(plotrix)
lbls =c("Europe", "Australia","Asia", "Africa", "South America", "North America")
pie3D(xtabs(~db$Continent), main="continents", col=c("gray", "red", "yellow", "blue", "green", "pink"),
      labels=lbls, explode = 0.2)
barplot(xtabs(~db$Continent), main="continents", col=c("gray", "red", "yellow", "blue", "green", "pink"))

#=================================================================================

data(package = .packages(all.available = TRUE))
library(questionr)
data(package="questionr")

data(hdv2003)
bd <- hdv2003

str(bd)

tb1 <- table(bd$relig)
tb2 <- data.frame(tb1 <- table(bd$relig))
names(tb2) <- c("Regligion", "Effectifs")

tb3 <- data.frame(round(prop.table(tb1),2))
names(tb3) <- c("Regligion", "Fréquences")

# 2ème façon
library(questionr)
tab <- freq(bd$relig, cum = TRUE, total = TRUE, digits = 2, exclude = NA)
names(tab) <- c("Effectifs", "Rel(%)", "Cum(%)")

#===================================================================================

library(MASS) 

fillieres <- c(13, 12, 21, 15, 17)  # On constuit un vecteur pour les valeurs 
lbls <- c("SVT", "SES", "SMP", "LLA", "TIC") # Un vecteur pour les labels (les étiquettes)

pct <- round(fillieres/sum(fillieres)*100)  # On calcule le pourcentage d’élèves dans chaque fillière
lbls2 <- paste(lbls, pct, "%")  # On ajoute les pourcentages aux étiquettes

pie(fillieres, labels = lbls2, main = "Graphique circulaire des Fillières", col = c("blue", "yellow", "green",  "red", "pink"))

pie(xtabs(~fillieres), labels = lbls2, main = "Graphique circulaire des Fillières", col = c("blue", "yellow", "green",  "red", "pink"))

library(plotrix)

fillieres <- c(13, 12, 21, 15, 17)
lbls <- c("SVT", "SES", "SMP", "LLA", "TIC")

pct <- round(fillieres/sum(fillieres)*100) # Pourcentage
lbls2 <- paste(lbls, pct, "%") # Ajouter les pourcentages aux étiquettes
pie3D(fillieres, labels = lbls2, explode=0.4, main = "Explosion du Graphique Circulaire en 3D", col = c("blue", "yellow", "green", "red", "pink"))

pie3D(xtabs(~fillieres), labels = lbls2, explode=0.4, main = "Explosion du Graphique Circulaire en 3D", col = c("blue", "yellow", "green", "red", "pink"))


fillieres <- c(SVT = 13, SES = 12, SMP = 21, LLA = 15, TIC = 17)
barplot(fillieres, main = "Distribution des Fillières", xlab = "Type de fillière", 
        ylab = "Fréquences",  col = c("beige", "blanchedalmond", "bisque1", "bisque2",
                                             "bisque3", "bisque4"),
                                             horiz = TRUE, las = 0, border = NA)

#===============================================================================================

library(questionr)
data(hdv2003)

bd <- hdv2003
pie(xtabs(~bd$relig), main="Distribution de l’ensemble des données",  col = c("green", "yellow", "pink", "blue", "red"))

lbls <- c("SVT", "SES", "SMP", "LLA", "TIC")
pct = round(fillieres/sum(fillieres), 2)*100
lbls2 = paste(lbls, pct, "%")
pie3D(xtabs(~bd$relig), explode = 0.3, labels = lbls2, main="Distribution de l’ensemble des données",  col = c("green", "yellow", "pink", "blue", "red"))


barplot(xtabs(~bd$relig), main = "Distribution des Fillières", xlab = "Type de fillière", 
        ylab = "Fréquences",  col = c("beige", "blanchedalmond", "bisque1", "bisque2",
                                             "bisque3", "bisque4"),border = NA)
                                             
#============================== Mardi 10 =======================================


x <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9,
       2, 1, 9, 2, 9, 1, 1, 9, 3, 9, 1)
x.bar <- mean(x)
x.var <- sum((x - x.bar)^2)/(length(x)-1)
var(x)
length(x)

matrice <- matrix(c(1, 3, 5, 7, 9, 11), nrow = 3, ncol = 2) 

A <- matrix(c(0,1,-1,2), c(2,2))
det(A)
inv

# plotrix, ggplot2, questionr
# Install Rtools
x <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/ex1.csv")

dim(x)
length(x$height)
str(x)
A <- matrix(c(-3,2,0,7,4,5,8,2,-2,3,5,9), nrow=3, ncol=4)
dim(A)
A[,3]
A[3,1]

b <- 3

?rm()
?dim()
?str()
?library()

enfants <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9, 2, 1, 9, 2, 9, 1,
             1, 9, 3, 9, 1)

info <- data.frame(
  Nom = c("Dina", "Dona", "Ina", "Kénel", "Nixson"),
  Prénom = c("Chéry", "Abdu", "Lucien", "Jean", "Henry"),
  Sexe = c("Female","Female","Female","Male","Male"),
  Phone = c("2978-0011", "3878-2525", "5454-1200", "4544-1119", "4004-6541"),
  Taille = c("1,63 m", "1,67 m", "1,69 m", "1,90 m", "1,73 m")
)
attach(info)
info2 <- factor(Phone)  
nlevels(info2)

library(questionr)
tab1 <- freq(enfants, total = TRUE, cum = TRUE, digits = 2, exclude = NA)

effect <- table(enfants)
tab2 <- data.frame(effect)  
attach(tab2)

tab3 <- data.frame(effect, relfreq = round(Freq/sum(Freq),4)*100, 
                   cumul = cumsum(effect), 
                   cumrelfreq = round(cumsum(effect)/sum(effect),4)*100,
                   d = with(tab2, rev(cumsum(rev(Freq)))))  
names(tab3) = c("enf", "ni", "%freq", "cumul", "%cumfreq", "decr.")

plot(effect, main= "Répartition du nombre d'enfants\n par famille", 
     xlab="Nombres d'enfants", ylab = "Nombre de familles", col=c("blue", "yellow",
                                                                        "red", "green", "pink", "purple"), col.main="blue", las=1, col.axis="red")
                                                                        

library(ggplot2)
col = c("blue", "yellow","red", "green", "pink", "purple")

ggplot(tab2) +
  aes(x = enfants, weight = Freq) +
  geom_bar(fill = col, width = 0.40) +
  labs(
    x = "Nombre d'enfants",
    y = "Effectifs",
    title = "Répartition de 32 étudiants en Science de Gestion à l'Université \n Espoir selon le nombre d'enfants par famille"
  ) +
  
  theme_minimal()

plot(ecdf(tab1$`%cum`), main= "Courbe des Fréquences cumulées
croissantes\n associée à la variable X représentant\nle nombre enfants par
famille", 
     xlab="Nombres d'enfants par famille", ylab = "fréquences cumulées", 
     col.main ="purple", col=c("green"), ylim = c(0, 1),col.axis= "black", 
     col.lab = "blue", las =1)

boxplot(c(enfants), main="Boîte à moustache associée à la variable\nnombre
d'enfants par famille", xlab="Nombres d'enfants", ylab="Nombre de
familles",
        col=c("yellow"), col.main ="purple", col.lab = "purple")


x <- data.frame(
  Nom = c("Jean", "Bobby", "Nixson"),
  Prenom = c("Marta", "Luc", "Henry"),
  age = c(32, 54, 72),
  sexe = c("F", "M", "M"),
  taille = c(1.56, 1.74, 1.73)
)


names(x) <- c("First Name", "Last Name", "Age", "Sex", "Height")
names(x) <- c("First Name", "Last Name", "age", "Sexe", "taille")
x

library(questionr)
x <- freq(enfants, cum = TRUE, total = TRUE, digits = 2, exclude = NA)

names(x)
names(x) <- c("effect", "%freq", "%freqcum")
x

effect <- table(enfants)
tab <- data.frame(effect)

tab1 <- data.frame(effect, rel=round(tab$Freq/sum(effect),2), EffCum = cumsum(tab$Freq), fréqCum = round(cumsum(tab$Freq)/sum(effect),2)*100)
sum(effect)


plot(effect, main= "Répartition des étud du nombre d'enfants\n par famille", 
     xlab="Nombres d'enfants", ylab = "Nombre de familles", col=c("blue"),
     col.main="blue",las=1)

col = c("red", "pink", "yellow", "purple", "green", "orange", "gray")

ggplot(tab1) +
  aes(x = enfants, weight = Freq) +
  geom_bar(fill = col, width = 0.3) +
  theme_minimal()

plot(ecdf(tab1$fréqCum), main= "Courbe des Fréquences cumulées
croissantes\n associée à la variable X représentant\nle nombre enfants par
famille", 
     xlab="Nombres d'enfants par famille", ylab = "fréquences cumulées", 
     col.main ="purple", col=c("green"), ylim = c(0, 1),col.axis= "black", 
     col.lab = "blue", las =1)

x <- enfants
mean(x)

summary(enfants)

enfants <- c(7, 7, 3, 7, 7, 7, 9, 2, 8, 9, 8, 2, 8, 8, 2, 7, 9, 8, 2, 9, 9, 2, 1, 9, 2, 9, 1,
             1, 9, 3, 9, 1, -13, 21)
boxplot(enfants)
#==========================================================================================
#*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*__*_*_*_*_*_*_*_*


#Pour importer un package
data(package= .packages(all.available = TRUE))
data(package= "MASS")
data(package= "questionr")


library(questionr)
data(hdv2003)
bd <- hdv2003

str(bd)

bd2 <- table(bd$relig)
bd3 <- data.frame(bd2)

t <- data.frame(bd2, rel = round(prop.table(bd3$Freq),2))

# Package MASS permet de creer des graphiques
library(MASS)

filieres <- c(13, 12, 21, 15, 17)
lbl1 <- c("SVT", "SES", "SMP", "LLA", "TIC")

pct <- round(filieres/sum(filieres)*100)
lbl <- paste(lbl1, pct, "%")
pie(filieres, labels = lbl, main = "Camembert", col= rainbow(5))

# Ou
pie(xtabs(~filieres), labels = lbl, main = "Camembert", col= rainbow(5))

# 3D graphique
library(plotrix)
pie3D(filieres, labels = lbl, main = "Graphique circulaire des filieres", explode = 0.4, col = rainbow(5))




















