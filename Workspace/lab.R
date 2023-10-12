##
## Lab 1
##

smp1 <- read.csv("C:/Users/Xondy/Desktop/UEspoir/Fun-Mooc/smp1.csv")
smp2 <- read.csv("C:/Users/Xondy/Desktop/UEspoir/Fun-Mooc/smp2.csv")
rm(list=ls())

subset(smp2, prof == 'agriculteur', age) ## 6 valeurs d'age dont la profession de l'individu es agriculteur
smp2$age[which(smp2$prof == 'agriculteur')]
table(smp2$prof == 'agriculteur')

## Si on veut sélectionner plus d'une variable
subset(smp2, prof == 'agriculteur', 1:5)

names(smp2)[1:5]
subset(smp2, prof == 'agriculteur' & n.enfant > 2, c(age, duree, discip, n.enfant))
subset(smp2, prof == 'agriculteur' & n.enfant > 2 & complete.cases(duree), c(age, duree, discip, n.enfant))

head(subset(smp1, prof == 'sans emploi' | prof == 'prof.intermediaire' | prof == 'cadre', c(age, n.enfant, prof)))

sousens <- subset(smp1, prof == 'sans emploi' | prof == 'prof.intermediaire' | prof == 'cadre', c(age, n.enfant, prof))
smp$prof <- factor(sousens$prof)
summary(sousens) 
table(smp1$prof)
table(sousens$prof)

# Si on souhaite résumer le nombre d'enfants moyens en fonction de la profession
aggregate(n.enfant ~ prof, sousens, mean)
boxplot(n.enfant ~ prof, sousens) # On a pour chacune des modalités la distribution du nombre d'enfants
m <- lm(n.enfant ~ prof, sousens) # si On souhaite réaliser une Anova ou une regression linéaire
drop1(m, test = "F") # On obtient un tableau d'analyse de variance avec la variable explicative prof
m2 <- lm(n.enfant ~ age, sousens)  
summary(m2)  

m3 <- lm(n.enfant ~ age, smp1, subset = prof == 'sans emploi' | prof == 'prof.intermediaire' | prof == 'cadre')  
summary(m3)
coef(m3)
coef(m3)[2] 
coef(m3)['age'] 

confint(m3) # Pour trouver l'intervalle de confiance
#               2.5 %      97.5 %
#  (Intercept) -1.52604619 -0.49199700
# age          0.05597581  0.08100076  ici, l'intervalle de confiance est 95% par défaut

# On peut obtenir un tableau d'analyse d'anova associer à la regression
anova(m3)
# Lorsqu'on souhaite réaliser des prédictions
predict(m3, data.frame(age=c(20, 30, 40)), interval = 'confidence') # age est la variable esplicative, et les valeurs de prédictions

# On prend une variable binaire: si le nombre d'enfants est supérieur à 2 on met 1, sinon 0
smp$n.enfant.bin <- ifelse(smp1$n.enfant > 2, 1, 0)

table(smp1$n.enfant)
table(smp$n.enfant.bin)

# r = b x [écart-type(age)/écart-type(durée entretien)]

# r = sum(Z_x * Z_y)/n avec Z_x = (X - X.bar)/écart-type(x)
# Z_y = (y - y.bar)/écart-type(y)

summary(smp$dur.interv)
head(smp, 1)
str(smp$ecole)

min(smp)

View(smp)

names(smp)

str(smp)

summary(smp)

summary(smp$age)

smp$age

smp$age[1]

smp$age[1:10]

min(smp$age)


help(min)

min(smp$age, na.rm=TRUE)

smp$abus[1:10]

unique(smp$abus)

head(smp$abus, n=10)

length(smp$abus)

nrow(smp)

table(smp$abus)

table(smp$abus,useNA="always")

summary(smp$abus)

head(smp$abus)

head(factor(smp$abus))

abus <- factor(smp$abus,levels=c(0,1),labels=c("Non","Oui"))

sum(table(abus))

names(smp)

head(smp$n.enfant)

summary(smp$n.enfant)

table(smp$n.enfant)

table(smp$n.enfant>4)

smp$n.enfant.cat <- factor(smp$n.enfant)

table(smp$n.enfant)

levels(smp$n.enfant.cat)

nlevels(smp$n.enfant.cat)

levels(smp$n.enfant.cat)[6:13] <- "5+"

table(smp$n.enfant.cat)

save(smp,file="smp_v1.rda")

savehistory("commandes.R")

#===============================================================================

smp1 <- read.csv2("C:/Users/Xondy/Desktop/UEspoir/Fun-Mooc/smp1.csv")
head(smp1, 1)
q3 <- summary(smp1)

# Create a vector as input.
data <- c("East","West","East","North","North","East","West","West","West","East","North")

print(data)
print(is.factor(data))

# Apply the factor function.
factor_data <- factor(data)

print(factor_data)
print(is.factor(factor_data))

summary(factor_data)
table(factor_data)

# Create the vectors for data frame.
height <- c(132,151,162,139,166,147,122)
weight <- c(48,49,66,53,67,52,40)
gender <- c("male","male","female","female","male","female","male")

# Create the data frame.
input_data <- data.frame(height,weight,gender)
print(input_data)

# Test if the gender column is a factor.
print(is.factor(input_data$gender))

# Print the gender column so see the levels.
print(input_data$gender)

#Important
# Pour avoir les totaux des effectifs, on applique la fonction addmargins()
addmargins(tb)
prop(tb) # Pour avoir les pourcentages
tb <- table(t$Sex, t$Pclass)

# Les fonctions by() et tapply() sont tres importantes pour comparer des groupes
# Permettent d'appliquer une fonction sur une variable quantitative (1ere), selon
# les modalites d'une variable categorielle (2eme)
by(Titanic$Age, Titanic$Sex, mean, na.rm=TRUE)

# La fonction tapply() va aussi donner la meme chose
tapply(Titanic$Age, Titanic$Sex, mean, na.rm=TRUE)

#========================= Stat Bidimensionnelle ========================
bd <- read.csv("Revenus.csv")
attach(bd)

plot(x1, y1)

# lm() pour les variables quantitatives
m <- lm(y1 ~ x1, data = bd)
summary(m)

# Pour trouver l'intervalle de confiance
confint(m)

# x1 est la variable explicative, et les valeurs de prédictions
predict(m, data.frame(x1=c(7, 10)), interval = 'confidence')

# Calcul du coef de corrélation r
cor(y1, x1, use = "complete.obs")

# Calcul du r-carré
library(rsq)
rsq(m)

plot(x1, y1, main="Les revenus", xlab= "Age des enfants", ylab = "Le niveau de GAG dans l'urine", col.main="purple", col = rgb(0, 0,1))
abline(m, lwd=2, col = "green")

summary(mod)    # Ensemble des résultat détaillés
coef(mod)       # Coefficients et erreurs standards
residuals(m)    # Résidus
confint(mod)    # Intervalles de confiance
fitted(m)       # Valeurs ajustées
anova(mod)      # Appliquer analyse de variance sur modèle
predict(mod)    # Calculer des valeurs predictes à partir d'un modèle
plot(mod)       # Et nombreuses autres fonctions graphiques

residuals(mod)
fitted(mod)

# Il y a 10% de la variance du taux de chomage est expliqué par le taux de décrochage scolaire.


model <- lm(crim$CrimIndex ~ crim$X1 + crim$X2 + crim$X3 + crim$X4, data = crim)

# dplyr facilite le codage des manipulations de données. 
# Il facilite également la lecture du code que vous écrivez pour manipuler les données

#1. Pour cet exercice, vous devez créer un document Colab et télécharger la base de 
# données depuis GitHub.

# Un chercheur s'intéresse aux conditions qui influencent le statut socioéconomique. 
# Vous êtes engagés-es par ce chercheur pour effectuer des analyses statistiques. La base de
# données comprend cinq variables mesurées auprès de 2904 individus. Les données comportent de
# l'information sur le statut socioéconomique, le niveau de scolarité, le genre, l'expérience 
# professionnelle et la taille du réseau professionnel.

# Id : Identificateur unique des individus
# SES : Statut socioéconomique – intervalle/ratio (min=17,1; max=97,2)
# Scolarite : Plus haut niveau de scolarité atteint – ordinale
# (0 = Secondaire non-terminé; 1 = Secondaire; 2 = Collégial; 3 = Universitaire 1er cycle; 
# 4 = Universitaire études supérieures)
# Genre : Genre de l'individu – nominale (1=Homme; 2=Femme)
# Experience : Nombre d'années de vie active – intervalle/ratio (min=1; max=41)
# Reseau : Taille du réseau professionnel – intervalle/ratio (min=0; max=32)
# Il vous demande d'effectuer une régression simple* en testant l'hypothèse que le nombre 
# d'années de vie active (experience) est relié au statut socioéconomique.

# Les histogrammes montrent une forte asymétrie à gauche.
# Les histogrammes montrent une forte asymétrie à droite.
# Les histogrammes suivent partiellement la forme d’une courbe normale, malgré une certaine 
# asymétrie à droite.
# Les histogrammes suivent bien la forme d’une courbe normale.

# https://youtu.be/CHcGZJbQIXo 
# https://youtu.be/McYUlSIAOwM 

#2. Cette capsule concerne la consommation calorique dans différents pays du monde. 
# La variable dépendante est donc le nombre de calories qui ont été consommées en moyenne
# par jour, par habitant, dans un pays. Cette variable est donc une variable continue. 
# Quant à la variable indépendante, c'est une variable catégorielle à 6 niveaux qui divise 
# l'ensemble des pays en six régions du monde.
# https://youtu.be/0QY7z7onH4o
# https://youtu.be/vZRnJHtjvrY


#3. Pour cet exemple, nous utiliserons un base qui est composée de données 
# concernant les États-Américains. On s'intéresse précisément au taux de chomage
# et au taux de décrochage scolaire au sein de chacun des états. Nous essaierons 
# de voir s'il y a une relation entre ces deux variables.

# Voici les variables à l'étude
# jbs170: taux de chomage par état en pourcentage
# scs134: Heure de décrochage scolaire par état en pourcentage

# Le nuage de points représente la relation entre le taux de décrochage scolaire, la variable 
# indépendante sur l'axe des X et le taux de chomage, la variable dépendante sur l'axe des
# Y. On aperçoit une tendance positive, où le taux de chomage augmente avec le taux de dé
# crochage scolaire. Donc plus le taux de décrochage est élevé, plus le taux de chomage l'est
# également et vice-versa. Quelques points semblent éloignés sont des données extremes.

#4. Faire la régression linéaire 
# (General Linear Model: glm)--> qui permet de faire des régressions linéaires simples
# multiples, des regressions logistiques et plusieurs autres analyses

reglineaire <- glm(bd$jbs170 ~ bd$scs134, data = bd, family = "gaussian")
summary(reglineaire)

"Le coefficient de la variable indépendante nous prouve l'existence
d'une relation positive, de sorte que le taux de chomage augmente environ 0.21% pour chaque
augmentation d'une unité du taux de décrochage scolaire. La décision est donc faible mais
positive, et surtout significative (P<0.05). Ces résultats son cohérents avec ce qu'on a 
observé dans le nuage de points."

# Calcul du r-carré
rsq(reglineaire)
# Il y a 10% de la variance du taux de chomage est expliqué par le taux de décrochage scolaire.
# https://youtu.be/McYUlSIAOwM

#========================= Stat Bidimensionnelle ========================

bd <- read.csv("salaries.csv")

View(bd)

library(MASS)
data(package="MASS")

View(MASS)
View(survey)
View(wtloss)
View(Insurance)

library(MASS)
library(rsq)
data("GAGurine")
View(GAGurine)
gag <- GAGurine

attach(gag)
plot(Age, GAG, main="Nuage de points du niveau de GAG dans l’urine\n des enfants selon l’âge", 
     col.main="purple", xlab="l'âge des enfants", ylab="Niveau de GAG", col="red")

cor(Age, GAG, use = "complete.obs")
model <- lm(GAG ~ Age)
summary(model)
rsq(model)


plot(Age, GAG, lwd=2, main="La droite de régression linéaire", xlab= "Age des enfants", ylab = "Le niveau de GAG dans l'urine", col.main="purple",
     col = "red")
abline(model, lwd=3, col = "blue")

predict(model, data.frame(Age= c(18, 20, 21)), interval = 'confidence')

#============== Exercice II ==================================

mice <- read.csv("mice_pheno.csv")
attach(mice)
View(mice)

mod <- glm(Bodyweight ~ Sexe)
summary(mod)
m2 <- lm(Bodyweight ~ Sexe)
summary(m2)


crim <- read.csv("TP.csv")
attach(crim)
View(crim)


crim.model <- lm(CrimIndex ~ X1 + X2 + X3 + X4)
summary(crim.model)
