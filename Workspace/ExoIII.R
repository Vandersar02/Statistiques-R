rev <- read.csv("pop.csv")
View(rev)
attach(rev)

library(MASS)
a <- data(package="MASS")
data("mammals")

View(mammals)
attach(mammals)

# 1- Le nuage de points associe a la serie
plot(body, brain, pch=19, main = "Nuage de points", xlab="brain", ylab="body", col=rainbow(2))

# 2
# a) Calculer le coefficient de corrélation linéaire entre x et y
cor( body, brain, use = "complete.obs")

# b) Calculer le coefficient de détermination
rqr()


library(rsq)
# 3 Calculer la droite de régression de Y par rapport à X.
model <- lm(brain ~ body)
summary(model)
rsq(model)

abline(model, lwd=3, col = "blue")

# 4 Calculer la droite de régression de X par rapport à Y
model2 <- lm(body ~ brain)
summary(model2)
rsq(model2)

abline(model2, lwd=3, col = "purple")



