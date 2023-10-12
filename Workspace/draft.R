#Jeff Rubensky-Jean Louis
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
tab<-c(5, 2, 1, 3, 4, 4, 3, 2, 1, 2, 5, 3, 4, 2, 1,
       3, 4, 2, 3, 4, 1, 2, 5, 2, 1, 2, 4, 3, 2, 1, 4, 3, 2, 5, 4, 1, 2, 3, 4, 1, 2, 5, 2, 3, 1, 4, 2, 3, 4, 1, 2, 3, 4, 5,
       1, 2, 3, 4, 2, 1, 3, 5, 2, 1, 4, 3, 2, 4, 1, 5, 2, 3, 4, 1, 2, 3, 4, 5, 2, 1)

#nombre d'observation
length(tab)
#2
A<-matrix(c(tab),c(10,10))
B<-(A**2)-(3*A)
#3a
B[4,3]
#3b
B[5,]
#3c
B[,7]
#4-1
c<-prod(A[4,])
#4-2
x<-sum(B[3,])
r<-sum(B[5,])
t<-sum(B[7,])
#4-3
v<-sum(B[,1])
e<-sum(B[,3])
y<-sum(B[,7])
#5
library(questionr)
fr<-freq(tab,cum = TRUE,total = TRUE,exclude = NA,digits = 0)
names(fr)<-c("tab","%frequence simples","%frequence cumule")
#6
x<- sum(tab)/length(tab)
var<- sum((tab-x)^2)/(length(tab)-1)

#---------------------------------------------------------------------------------------------
#Exo3
titan <- read.csv("C:/Users/lstcyr/OneDrive/Documents/Courses/Statistiques R/Workspace/titanic.csv")
#1a
head(titan,3)
#1b
str(titan)
dim(titan)

# 2
names(titan)

# 3
# Quantitative continue
attach(titan)
range(PassengerId)
summary(PassengerId)
table(cut(PassengerId,8))
a <-cut(PassengerId,breaks=8)
data.frame(table(a))
data.frame(table(a))
#r <- cut(PassengerId,c(0.42,10,20,30,40,50,60,70,80),include.lowest = TRUE,right =  FALSE)

r <- cut(Age,c(0.42,10,20,30,40,50,60,70,80),include.lowest = TRUE,right =  FALSE)

data.frame(table(r))
BB2 <-data.frame(table(r))

#BB2 <-data.frame(table(r),cumul=cumsum(Freq),rel=prop.table(Freq))
typeof(titan$PassengerId)

names(BB2) <- c("classe","effectifs")
BB2