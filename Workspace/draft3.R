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


