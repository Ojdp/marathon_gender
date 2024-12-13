#Stats descriptives
load("~/GitHub/marathon_gender/merged_results.rda")
nombre_de_femmes <- sum(merged_df$gender == "F")
nombre_de_femmes

age_moyen <- mean(merged_df$age)
age_moyen

age_moyen_homme <- mean(merged_df$age[merged_df$gender == "M"], na.rm = TRUE)
age_moyen_femme <- mean(merged_df$age[merged_df$gender == "F"], na.rm = TRUE)


merged_hommes <- merged_df %>% filter(gender == "M")
merged_femmes <- merged_df %>% filter(gender == "F")

# Trouver le prénom le plus fréquent chez les hommes
prenom_hommes_frequence <- table(merged_hommes$firstName)
prenom_plus_frequent_hommes <- names(prenom_hommes_frequence)[which.max(prenom_hommes_frequence)]

# Trouver le prénom le plus fréquent chez les femmes
prenom_femmes_frequence <- table(merged_femmes$firstName)
prenom_plus_frequent_femmes <- names(prenom_femmes_frequence)[which.max(prenom_femmes_frequence)]

# Afficher les résultats
cat("Le prénom masculin le plus fréquent est :", prenom_plus_frequent_hommes, 
    "avec", max(prenom_hommes_frequence), "occurrences.\n")

cat("Le prénom féminin le plus fréquent est :", prenom_plus_frequent_femmes, 
    "avec", max(prenom_femmes_frequence), "occurrences.\n")


age_max <- max(merged_df$age, na.rm = TRUE)
age_min <- min(merged_df$age, na.rm = TRUE)

cat("Âge maximal :", age_max, "\n")
cat("Âge minimal :", age_min, "\n")


femmes_temps_inferieur_3h <- merged_df %>%
  filter(gender == "F" & chipTimeResult < "03:00:00")

# Compter le nombre de femmes concernées
nombre_femmes <- nrow(femmes_temps_inferieur_3h)

# Afficher le résultat
cat("Nombre de femmes ayant un temps inférieur à 3 heures :", nombre_femmes, "\n")

hommes_temps_inferieur_3h <- merged_df %>%
  filter(gender == "M" & chipTimeResult < "03:00:00")

# Compter le nombre d'hommes concernés
nombre_hommes <- nrow(hommes_temps_inferieur_3h)

# Afficher le résultat
cat("Nombre d'hommes ayant un temps inférieur à 3 heures :", nombre_hommes, "\n")

distribution_age_sexe_pct <- merged_df %>%
  group_by(gender, age) %>%
  summarise(count = n()) %>%
  group_by(gender) %>%
  mutate(pct = (count / sum(count)) * 100) %>%  # Calculer le pourcentage au sein de chaque sous-groupe
  arrange(gender, age)  # Trier par genre puis par âge


ggplot(distribution_age_sexe_pct, aes(x = age, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = TRUE) +
  labs(title = "Distribution des âges par sexe en pourcentage", x = "Âge", y = "Pourcentage") +
  scale_fill_manual(values = c("blue", "pink")) +  # Choisir des couleurs pour chaque genre
  theme_minimal()