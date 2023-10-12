#1
#Calcul de la somme des 25 premiers entiers naturels pairs non nuls

somme <- sum(seq(from = 2, to = 25, by = 2))

#2
X = 0

for (i in seq(1,25)) {

    X = X + (i ^ 3) 
  
}
X


#____________________________________
Y =0
for (i in seq(1,9)) {
  
  Y = Y + (2 * (i^2) + 1)
  
}
Y

#____________________________________
Z = 0
for (i in seq(1,10)) {
  
  Z = Z + (3 ^ i)
  
}
Z


#____________________________________
somme <- sum(seq(from = 1, to = 25))

#_______________________________________________________________________________
#exo1
#1
(25)*(25+1)/(2)
#2
(((25)*(25+1)/(2))^(2))
#3
(2*((1)^2)+1)+(2*((2)^2)+1)+(2*((3)^2)+1)+(2*((4)^2)+1)+(2*((5)^2)+1)+(2*((6)^2)+1)+(2*((7)^2)+1)+(2*((8)^2)+1)+(2*((9)^2)+1)
#4
(3^1)+(3^2)+(3^3)+(3^4)+(3^5)+(3^6)+(3^7)+(3^8)+(3^9)+(3^10)
#----------------------------------------------------------------------------------------------------


#exo2
#1Construisez un vecteur “tab” à l’aide des informations ci-dessus; Et précisez le type de données inclus dans le vecteur tab. Combien y a-t-il d’observations dans cet objet?

tab<-c(5, 2, 1, 3, 4, 4, 3, 2, 1, 2, 5, 3, 4, 2, 1,3, 4, 2, 3, 4, 1, 2, 5, 2, 1, 2, 4, 3, 2, 1, 4, 3, 2, 5, 4, 1, 2, 3, 4, 1, 2, 5, 2, 3, 1, 4, 
       2, 3, 4, 1, 2, 3, 4, 5, 1, 2, 3, 4, 2, 1, 3, 5, 2, 1, 4, 3, 2, 4, 1, 5, 2, 3, 4, 1, 2, 3, 4, 5, 2, 1,5, 2, 1, 3, 4, 4, 3, 2, 1, 2, 5, 3,
       4, 2, 1, 3, 4, 2, 3, 4)

#nombre d'observation
length(tab)

#2 Passez ce vecteur en argument dans la fonction matrix() pour créer la matrice “A” de format 10 x 10. Puis, une nouvelle matrice B = 𝐴^2 − 3A
A<-matrix(c(tab),c(10,10))
A
B<-(A%*%A)-(3*A)
B

#3 En utilisant la méthode d’index dans les questions suivantes:

#3a Écrivez la commande permettant d’accéder à l’élément situant sur la 4em ligne et la 3em colonne de la matrice B.
B[4,3]

#3b Quelle commande permettant d’afficher tous les éléments de la 5 ligne?
B[5,]

#3c Quelle commande permettant d’accéder à tous les éléments de la 7 colonne? 
B[,7]

#4 calculer

#4-1 La produit de tous les éléments de la 4 em ligne de la matrice A
c<-prod(A[4,])
c

#4-2 La somme des éléments de la 3em,5em,7em lignes de la matrice B 
x<-sum(B[3,])
x
r<-sum(B[5,])
r
t<-sum(B[7,])
t
sum(x+r+t)
sum(sum(B[3,]), sum(B[5,]), sum(B[7,]))

#4-3 Le somme des éléments de la 1er,3em,7em colonne de la matrice A
sum(sum(A[,1]), sum(A[,3]), sum(A[,7]))
v<-sum(A[,1])
v
e<-sum(A[,3])
e
y<-sum(A[,7])
y
sum(v+e+y)

#5 Représentez les données du vecteur “tab” (créé dans la 1ere question) dans un tableau de fréquences. (en indiquant les colonnes des fréquences simples et des fréquences cumulées).
library(questionr)
fr<-freq(tab,cum = TRUE,total = TRUE,exclude = NA,digits = 0)
names(fr)<-c("ni","%frequence simples","%frequence cumule")
fr

#6 Calculez les paramètres de tendance centrale et de dispersion par la méthode de votre choix.

#Calcul de la moyenne
moyenne<- sum(tab)/length(tab)
moyenne

#Calcul de la variance
variance<- sum((tab-x)^2)/(length(tab)-1)
variance

#Calcul ce l'ecartype
ecartType<-sqrt(var)
ecartType


#---------------------------------------------------------------------------------------------
#Exo3
titan <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/titanic.csv")
titan
#1 Écrivez la commande permettant :

#1a d’imprimer les 3 premières lignes à l'écran
head(titan,3)

#1b  de déterminer la structure du fichier et le nombre d’observations de ce fichier.
str(titan)

# 2 Listez toutes les variables contenues dans ce fichier. Pour chacune d'elles, indiquer son type (qui pourrait-être entier, facteur ou numérique).
names(titan)
attach(titan)
typeof(PassengerId)
typeof(Survived)  
typeof(Pclass) 
typeof(Name)
typeof(Sex)   
typeof(Age)  
typeof(SibSp) 
typeof(Parch)
typeof(Ticket)   
typeof(Fare)  
typeof(Cabin) 
typeof(Embarked)
typeof(X)   
typeof(Survived)


# 3a De quel type de variable s’agit-il?
# Quantitative continue

#3b Que sont ici les individus statistiques observés?
# l'ensemble des passagers sur le titanic

#3c c) Recopiez et complétez ce tableau. Les calculs nécessaires pour déterminer les valeurs manquantes dans les colonnes des effectifs 𝑛 et des fréquences devront figurer sur la 𝑖 𝑓𝑖copie. Les fréquences seront données à 10 près.

ni<- cut(Age, c(0.42,10,20,30,40,50,60,70,80), include.lowest = TRUE, right =  FALSE)
var1 <- table(ni)
el<-data.frame(var1)
var2 <- data.frame(  el,  frequences=prop.table(el$Freq),   Effectifcum=cumsum(el$Freq),   frecum=cumsum(el$Freq/sum(el$Freq)))
var2

r <- cut(Age,c(0.42,10,20,30,40,50,60,70,80),include.lowest = TRUE,right =  FALSE)

tr <- table(r)

j <- data.frame(tr, frequences=prop.table(tr$Freq),   Effectifcum=cumsum(tr$Freq),   frecum=cumsum(tr$Freq/sum(tr$Freq)))

#4 Donnez une interprétation concrète des nombres obtenus dans les cases marquées d’une étoile.

#N=714 : observation faites 714 passagers selon la classe d'age
#n2=102 : pour 102 passager la classe d'age entre 10 a 20 ans
#f6=0.067226891 : pour 6,7% d'ages le nombre d'ages est entre 50 a 60 ans.
#f5=0.89635854 : pour 89,63 d'ages le nombre d'ages est moin que 50 ans

#5a Déterminer, par le calcul, le pourcentage de passagers sur ce navire dont l’âge est supérieur à 35 ans. Ce résultat sera donné à 10 près.
#p(X=>35)=f(35)=0.77170868
#pour 7717.087*10^-4 des personnes observer a depasse 35 ans

#5b  Déterminer, par le calcul, le pourcentage de passagers sur ce navire dont l’âge est inférieur à 18 ans. Ce résultat sera donné à 10 près. 
#p(x<=18)=f(18)= 0.22969188
#pour 2296.919*10^-4 des personnes observer n'a pas depasse 18 ans
min(na.omit(Age))
max(na.omit(Age))

#6 Écrire la commande permettant de réaliser dans R le tableau ci-dessus d’amplitude de classes 𝑎 = 10. les fréquences simples et les fréquences cumulées croissantes).
k<-(1+10/3*log10(714))
k
#pour l'amplitude
a<-(80-0.42)/10
a
#d'ou a=8 on prendra 8 comme amplitude de classe

#7a À l’aide du logiciel R, tracer soigneusement :
#- l’histogramme et le polygone des fréquences simples
h<-hist(Age,col=rainbow(8))
points(h$mids,h$counts,lwd=3,pch=13,col="black")
lines(h$mids,h$counts,lwd=3,col=I("black"))

#7b la courbe des fréquences cumulées
plot(x=1:length(var2$ni), y=var2$frecum, type="o", main= "Courbe des fréquences cumulées", col.main= "Red")

#8 Représentez à l'aide d’un boxplot (une boite à moustache) la variable âge. Que peut-on en déduire?
boxplot(na.omit(Age))
# ce qu'on peut en deduire c'est qu'il y a des valeurs aberante

#9 En utilisant la formule, vérifiez par le calcul le [Q1 − 1. 5 * 𝐼𝑄𝑅; 𝑄3 + 1. 5 * 𝐼𝑄𝑅] résultat obtenu dans la question 8. (ie, on vérifie s’il y a des outliers).
#calcul I=[Q1,Q3]=[20.12;38.00]
#e=Q3-Q1
e<-38.00-20.12
e

#maintenant on calcul les valeurs aberantes
#[Q1-1.5*e;Q3+1.5*e]
cal<-(0.42-1.5*18)
cal
ca<-(38.00+1.5*18)
ca
#d'ou on a [-26.58;65] min=0.42 et max=80 les valeurs ne sont pa inclus dans l'intervale donc il y a des valeurs aberantes

#10 Calculez la moyenne et l’écart-type de la variable édiée.
x<- sum(na.omit(Age))/length(na.omit(Age))
x
#Calcul pour la variance
var<- sum((na.omit(Age)-x)^2)/(length(na.omit(Age))-1)
var
#Calcul pour l'ecartype
rt<-sqrt(var)
rt
# moyenne d'ages de passager sur le titanic est 29.70 passager avec une dispersion de 14.53 de passagers



