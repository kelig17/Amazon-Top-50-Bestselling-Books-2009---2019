#Librairies importantes
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(janitor)
library(stringr)
library(skimr)

#Importer les données
Amazon <- read.csv("C:/Users/kelig/OneDrive/Bureau/Google Analytics/Projet final/
                   Etude Livres Amazon/Books Bestsellers.csv")
head(Amazon)

# Regardons la dimension des données et le type de données
glimpse(Amazon)

# Changer le type de données de genre
Amazon <- Amazon %>%
  mutate(Genre = as.factor(Genre))

## Etape 2 Valeurs manquantes
# Calculons le pourcentage de valeurs vides et remplies pour toutes les colonnes
for (col in names(Amazon)) {
  pct_missing <- mean(is.na(Amazon[[col]]))
  cat(col, "-", sprintf("%.1f%%", pct_missing * 100), "\n")
}
#CONCLUSION: Les données n’ont pas de valeurs manquantes, donc aucune autre
#transformation n’est nécessaire

#Doublons
# 1. Sélectionner les colonnes non numériques
col_name_without_numeric_data <- names(Amazon)[!sapply(Amazon, is.numeric)]

# 2. Boucle pour détecter les doublons
for (col in col_name_without_numeric_data) {
  if (any(duplicated(Amazon[[col]]))) {
    cat("Column", col, "contains duplicates.\n")
  } else {
    cat("Column", col, "does not contain duplicates.\n")
  }
}

for (col in col_name_without_numeric_data) {
  before <- length(unique(Amazon[[col]]))
  after <- length(unique(str_to_title(str_trim(Amazon[[col]]))))
  cat("Before", col, ":", before, "After", col, ":", after, "\n")
}

Amazon$Name <- str_to_title(str_trim(Amazon$Name))
Amazon$Name

# Vérifions s’il y a les mêmes noms d’auteurs mais avec des orthographes différentes
authors <- sort(unique(Amazon$Author))
authors

#Correction des erreurs
Amazon$Author <- gsub("George R R Martin", "George RR Martin", Amazon$Author)
Amazon$Author <- gsub("J K Rowling", "JK Rowling", Amazon$Author)

Amazon_2 <- Amazon[!duplicated(Amazon$Name, fromLast = TRUE), ]

Amazon_2 <- Amazon_2 %>% select(-Year)

glimpse(Amazon_2)
 

# CONCLUSION: Ainsi, les données contiennent 350 livres différents écrits par 247 auteurs 
# Tous les livres sont présentés dans deux catégories (Non Fiction, Fiction

#Données catégorielles
# Auteurs avec la meilleure note moyenne
top_13_authors <- Amazon_2 %>%
  group_by(Author) %>%
  summarise(User_Rating = mean(`User.Rating`, na.rm = TRUE)) %>%
  arrange(desc(User_Rating)) %>%
  slice_head(n = 13)
top_13_authors

#ou graphiquement
ggplot(top_13_authors, aes(x = reorder(Author, User_Rating), y = User_Rating)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 auteurs par note moyenne",
    x = "Auteur",
    y = "Note moyenne"
  ) +
  theme_minimal()

# Auteurs ayant écrit le plus de bestsellers
top_authors <- Amazon_2 %>%
  group_by(Author) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 10)
top_authors

ggplot(top_authors, aes(x = reorder(Author, Count), y = Count)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 10 auteurs par nombre de bestsellers",
    x = "Auteur",
    y = "Nombre de livres"
  ) +
  theme_minimal()

# Livres avec le plus grand nombre de critiques
top_reviewed_books <- Amazon_2 %>%
  arrange(desc(Reviews)) %>%
  slice_head(n = 10)
top_reviewed_books

ggplot(top_reviewed_books, aes(x = reorder(Name, Reviews), y = Reviews)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Top 10 livres par nombre de critiques",
    x = "Titre du livre",
    y = "Nombre de critiques"
  ) +
  theme_minimal()

# Nombre de livres par genre
genre_count <- Amazon_2 %>%
  count(Genre)
genre_count

ggplot(genre_count, aes(x = Genre, y = n, fill = Genre)) +
  geom_col() +
  labs(
    title = "Nombre de livres par genre",
    x = "Genre",
    y = "Nombre de livres"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Fiction" = "skyblue", "Non Fiction" = "salmon"))

#valeur numérique
summary(Amazon_2)

#box plot
#Notes
ggplot(Amazon_2, aes(x = User.Rating, y = "")) +
  geom_boxplot(fill = "salmon", color = "black") +
  labs(title = "User Rating", y = "User Rating", x = "") +
  theme_minimal(base_family = "Arial", base_size = 12)
#Reviews
ggplot(Amazon_2, aes(x = Reviews, y = )) +
  geom_boxplot(fill = "salmon", color = "black") +
  labs(title = "Reviews", y = "Reviews", x = "") +
  theme_minimal(base_family = "Arial", base_size = 12)
#Prix
ggplot(Amazon_2, aes(x = Price, y ="" )) +
  geom_boxplot(fill = "salmon", color = "black") +
  labs(title = "Price", y = "Price", x = "") +
  theme_minimal(base_family = "Arial", base_size = 12)

#Corrélation variable numérique
cor_matrix <- cor(Amazon_2[, sapply(Amazon_2, is.numeric)], use = "complete.obs")
print(cor_matrix)

#user rating VS reviews
ggplot(Amazon_2, aes(x = `User.Rating`, y = Reviews)) +
  geom_point(color = "salmon", alpha = 0.7) +
  labs(title = "User Rating vs Reviews",
       x = "User Rating",
       y = "Reviews") +
  theme_minimal(base_family = "Arial", base_size = 12)

#User rating VS price
ggplot(Amazon_2, aes(x = `User.Rating`, y = Price)) +
  geom_point(color = "salmon", alpha = 0.7) +
  labs(title = "User Rating vs Price",
       x = "User Rating",
       y = "Price") +
  theme_minimal(base_family = "Arial", base_size = 12)

#Price VS reviews
ggplot(Amazon_2, aes(x = `Price`, y = Reviews)) +
  geom_point(color = "salmon", alpha = 0.7) +
  labs(title = "User Rating vs Price",
       x = "Price",
       y = "Reviews") +
  theme_minimal(base_family = "Arial", base_size = 12)

#III Test d'hypothèse
#2 Vérification de la distribution pour la normalité
# En utilisant le test de normalité de Shapiro Wilk
# Définir le seuil alpha
alpha <- 0.05

# Appliquer le test de Shapiro-Wilk sur la variable User Rating
result <- shapiro.test(Amazon_2$`User.Rating`)

# Afficher les résultats
cat("Statistic:", round(result$statistic, 3), "\n")
cat("P-Value:", format(result$p.value, digits = 20), "\n")

# Vérifier l'hypothèse nulle
if (result$p.value > alpha) {
  cat("Accept H0 - Les données suivent une distribution normale.\n")
} else {
  cat("Reject H0 - Les données ne suivent pas une distribution normale.\n")
}


ggplot(Amazon_2, aes(x = `User.Rating`)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "darkblue", size = 1) +
  labs(title = "Histogramme de User Rating avec densité",
       x = "User Rating",
       y = "Fréquence") +
  theme_minimal(base_family = "Arial", base_size = 12)

# Créer les groupes
non_fiction <- Amazon_2$`User.Rating`[Amazon_2$Genre == "Non Fiction"]
fiction <- Amazon_2$`User.Rating`[Amazon_2$Genre == "Fiction"]

# Définir le niveau alpha
alpha <- 0.05

# Effectuer le test de Wilcoxon (équivalent Mann-Whitney)
test_result <- wilcox.test(non_fiction, fiction)

# Afficher les résultats
cat("Statistic:", round(test_result$statistic, 3), "\n")
cat("P-Value:", format(test_result$p.value, digits = 20), "\n")

# Vérifier la condition pour H0
if (test_result$p.value > alpha) {
  cat("Accept H0 - Il n'y a pas de différence significative entre les groupes.\n")
} else {
  cat("Reject H0 - Il y a des différences significatives entre les groupes.\n")
}
# Créer le graphique
ggplot(Amazon_2, aes(x = `User.Rating`, fill = Genre)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Fiction" = "gray", "Non Fiction" = "salmon")) +
  labs(title = "Comparaison de la densité de distribution entre Fiction et Non Fiction",
       x = "User Rating",
       y = "Densité",
       fill = "Genre") +
  theme_minimal(base_family = "Arial", base_size = 12)
       