# Charger la bibliothèque tidyverse
library(tidyverse)

# Créer un exemple de jeu de données
data <- data.frame(
  Q1 = sample(c("Très satisfait", "Satisfait", "Neutre", "Insatisfait", "Très insatisfait"), size = 100, replace = TRUE),
  Q2 = sample(c("Oui", "Non"), size = 100, replace = TRUE),
  Q3 = sample(c("Très accessibles et adaptées", "Accessibles et adaptées", "Partiellement accessibles et adaptées", "Peu accessibles et adaptées", "Pas du tout accessibles et adaptées"), size = 100, replace = TRUE),
  Q4 = sample(c("Oui, totalement", "Oui, partiellement", "Non, pas du tout"), size = 100, replace = TRUE),
  Q5 = sample(c("Oui, certainement", "Oui, dans une certaine mesure", "Non, pas vraiment"), size = 100, replace = TRUE),
  Q6 = sample(c("Oui", "Non"), size = 100, replace = TRUE),
  Q7 = sample(c("Oui", "Non"), size = 100, replace = TRUE),
  Q8 = sample(c("Très accessible", "Accessible", "Partiellement accessible", "Peu accessible", "Pas du tout accessible"), size = 100, replace = TRUE),
  Q9 = sample(c("Oui, j'ai des commentaires et des suggestions","Non, je n'ai pas de commentaires ou de suggestions particuliers","Je ne sais pas / Je préfère ne pas répondre" ), size = 100, replace = TRUE ),
  Q10 = sample(c("Oui, totalement", "Oui, partiellement", "Non, pas du tout"), size = 100, replace = TRUE)
)

# 1. Tableau de fréquences pour Q1
table_Q1 <- table(data$Q1)
table_Q1

# 2. Tableau de fréquences pour Q2
table_Q2 <- table(data$Q2)
table_Q2

# 3. Tableau de fréquences pour Q3
table_Q3 <- table(data$Q3)
table_Q3

# 4. Tableau de fréquences pour Q4
table_Q4 <- table(data$Q4)
table_Q4

# 5. Tableau de fréquences pour Q5
table_Q5 <- table(data$Q5)
table_Q5

# 6. Tableau de fréquences pour Q6
table_Q6 <- table(data$Q6)
table_Q6

# 7. Tableau de fréquences pour Q7
table_Q7 <- table(data$Q7)
table_Q7

# 8. Tableau de fréquences pour Q8
table_Q8 <- table(data$Q8)
table_Q8

# 9. Tableau de fréquences pour Q9
table_Q9 <- table(data$Q9)
table_Q9

# 10. Tableau de fréquences pour Q10
table_Q10 <- table(data$Q10)
table_Q10

# Commentaire sur chaque interprétation

# 1. Commentaire pour Q1
comment_Q1 <- "Le niveau de satisfaction global des étudiants en situation de handicap par rapport à l'apprentissage en ligne varie. Certains sont très satisfaits, tandis que d'autres sont insatisfaits."

# 2. Commentaire pour Q2
comment_Q2 <- "Certains étudiants en situation de handicap ont rencontré des difficultés d'accès aux ressources en ligne pendant leur parcours d'apprentissage, tandis que d'autres n'ont pas rencontré de difficultés."

# 3. Commentaire pour Q3
comment_Q3 <- "La perception de l'accessibilité et de l'adaptation des ressources en ligne varie parmi les étudiants en situation de handicap. Certains les trouvent très accessibles et adaptées, tandis que d'autres les trouvent peu accessibles et adaptées."

# 4. Commentaire pour Q4
comment_Q4 <- "Certains étudiants en situation de handicap ont bénéficié d'un soutien adéquat de la part de leurs enseignants pour répondre à leurs besoins, tandis que d'autres n'ont pas reçu un soutien suffisant."

# 5. Commentaire pour Q5
comment_Q5 <- "Certains étudiants en situation de handicap ont remarqué une plus grande flexibilité dans leur emploi du temps grâce à l'apprentissage en ligne, tandis que d'autres n'ont pas constaté de changement significatif."

# 6. Commentaire pour Q6
comment_Q6 <- "Certains étudiants en situation de handicap ont rencontré des difficultés à interagir avec leurs enseignants et leurs pairs pendant les sessions en ligne, tandis que d'autres n'ont pas eu de telles difficultés."

# 8. Commentaire pour Q8
comment_Q8 <- "L'évaluation de l'accessibilité des plates-formes d'apprentissage en ligne varie parmi les étudiants en situation de handicap. Certains les trouvent très accessibles, tandis que d'autres les trouvent peu accessibles."

# 10. Commentaire pour Q10
comment_Q10 <- "Certains étudiants en situation de handicap ont reçu des aménagements raisonnables et un soutien spécifique pour leurs besoins en matière d'apprentissage en ligne, tandis que d'autres n'ont pas bénéficié d'un tel soutien."

# Afficher les commentaires
comment_Q1
comment_Q2
comment_Q3
comment_Q4
comment_Q5
comment_Q6
comment_Q8
comment_Q10

# Générer les graphiques à barres
barplot(table_Q1, main = "Niveau de satisfaction global", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q2, main = "Difficultés d'accès aux ressources en ligne", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q3, main = "Accessibilité et adaptation des ressources en ligne", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q4, main = "Soutien des enseignants pour les besoins des étudiants", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q5, main = "Flexibilité de l'emploi du temps", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q6, main = "Difficultés à interagir pendant les sessions en ligne", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q8, main = "Accessibilité des plates-formes d'apprentissage en ligne", xlab = "Réponses", ylab = "Fréquence")
barplot(table_Q10, main = "Aménagements raisonnables et soutien spécifique", xlab = "Réponses", ylab = "Fréquence")



# Données 7
outils <- c("Lecture d'écran", "Logiciels de reconnaissance vocale", "Outils de transcription", "Outils de prise de notes", "Autres")
ni <- c(28, 18, 15, 22, 17)

# Graphique en bâtons
barplot(ni, names.arg = outils, main = "Utilisation des outils d'assistance technologique", xlab = "Outils", ylab = "Fréquence")


# Données 9
commentaires <- c("Oui, j'ai des commentaires et des suggestions", "Non, je n'ai pas de commentaires ou de suggestions particuliers", "Je ne sais pas / Je préfère ne pas répondre")
ni <- c(42, 38, 10)

# Graphique en bâtons
barplot(ni, names.arg = commentaires, main = "Commentaires et suggestions pour améliorer l'apprentissage en ligne", xlab = "Réponses", ylab = "Fréquence")


# Données 10
reponses <- c("Oui, totalement", "Oui, partiellement", "Non, pas du tout")
ni <- c(26, 34, 10)

# Graphique en bâtons
barplot(ni, names.arg = reponses, main = "Reception d'aménagements raisonnables et soutien spécifique", xlab = "Réponses", ylab = "Fréquence")

