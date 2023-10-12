rev <- read.csv("prix.csv")
View(rev)
attach(rev)

###################################################################

plot(x1, y1, pch=19, main = "Représentation graphique du nuage de points", xlab="Rang de l’année xi", ylab="Montant des recettes Yi", col=rainbow(2))

###################################################################

cor(x1, y1, use = "complete.obs")


###################################################################

model <- lm(y1 ~ x1)
Var <- model$coefficients
B <- Var[1]
A <- Var[2]

X=10

Y = A*X + B
Y

###################################################################
model2 <- lm(x1 ~ y1)
Var2 <- model2$coefficients
B <- Var2[1]
A <- Var2[2]

Xn <- 63000

X = A*Xn + B
X

###################################################################
plot(x1, y1, lwd=2, main="La droite de régression linéaire", xlab= "Rang de l'année", ylab = "Montant des recettes", col.main="blue",
     col=rainbow(2))
abline(model, lwd=3, col = "blue")
###################################################################

###################################################################








