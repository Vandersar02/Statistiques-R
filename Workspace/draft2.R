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
bd <- read.csv("C:/Users/DELL/Desktop/BeluchyTravaille/ex1.csv")
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



