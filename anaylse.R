#Stats descriptives
load("~/GitHub/marathon_gender/merged_results.rda")

merged_df <- merged_df %>%
  mutate(
    chipTime_interval = cut(
      as.POSIXct(chipTimeResult, format = "%H:%M:%S"),
      breaks = seq(
        as.POSIXct("00:00:00", format = "%H:%M:%S"),
        as.POSIXct("06:00:00", format = "%H:%M:%S"), # Exemple pour 6 heures max
        by = "15 min"
      ),
      include.lowest = TRUE,
      labels = FALSE
    )
  )


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

#Distribution par âge
distribution_age_sexe_pct <- merged_df %>%
  filter(age >= 18) %>%  # Filtrer les âges inférieurs à 18 ans
  group_by(gender, age) %>%
  summarise(count = n(), .groups = "drop") %>%  # Compter les participants par âge et genre
  group_by(age) %>%
  mutate(total_count = sum(count),  # Compter le total des participants pour chaque âge
         pct = (count / total_count) * 100) %>%  # Calculer le pourcentage pour chaque genre
  ungroup() %>%
  arrange(age)


ggplot(distribution_age_sexe_pct, aes(x = age, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = TRUE) +
  labs(title = "Distribution des âges par sexe en pourcentage", x = "Âge", y = "Pourcentage") +
  scale_fill_manual(values = c("darkgreen", "orange"))+  # Choisir des couleurs pour chaque genre
  theme_minimal()


# Visualisation avec ggplot
ggplot(distribution_age_sexe_pct, aes(x = age, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "stack", show.legend = TRUE) +
  labs(title = "Distribution des âges par sexe en pourcentage de l'échantillon total", 
       x = "Âge", y = "Pourcentage dans l'échantillon total") +
  scale_fill_manual(values = c("darkgreen", "orange")) +  # Choisir des couleurs pour chaque genre
  theme_minimal()

# Effet du premier enfant sur l'inscription des femmes au marathon
ggplot(distribution_age_sexe_pct, aes(x = age, y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  labs(
    title = "Distribution des âges par sexe en pourcentage",
    x = "Âge",
    y = "Pourcentage"
  ) +
  scale_fill_manual(values = c("darkgreen", "orange")) +  # Choisir des couleurs
  theme_minimal() +
  facet_wrap(~gender, ncol = 1)

# Côté performance 
# Création de l'histogramme avec des intervalles de 15 minutes (900 secondes)
ggplot(merged_df, aes(x = as.factor(chipTime_interval))) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution des temps réalisés par intervalles de 15 minutes",
    x = "Intervalles de temps (15 minutes)",
    y = "Nombre de participants"
  ) +
  scale_x_discrete(
    labels = function(x) {
      start_time <- as.POSIXct("00:00:00", format = "%H:%M:%S") + (as.numeric(x) - 1) * 15 * 60
      end_time <- start_time + 15 * 60
      paste(format(start_time, "%H:%M:%S"), "-", format(end_time, "%H:%M:%S"))
    }
  ) +
  geom_vline(xintercept = which(levels(as.factor(merged_df$chipTime_interval)) == "16"), 
             linetype = "dashed", color = "red", size = 1)+
  theme_minimal()


# Créer un graphique avec les 10 déciles 

merged_df$chipTime_seconds <- as.numeric(hms(merged_df$chipTimeResult))

# Obtenir les bornes des temps réalisés
min_time <- min(merged_df$chipTime_seconds, na.rm = TRUE)
max_time <- max(merged_df$chipTime_seconds, na.rm = TRUE)

# Diviser l'intervalle total en 10 déciles
decile_breaks <- seq(min_time, max_time, length.out = 11)

# Créer une colonne pour assigner chaque participant à un décile fixe
merged_df$chipTime_decile <- cut(
  merged_df$chipTime_seconds,
  breaks = decile_breaks,
  include.lowest = TRUE,
  labels = paste0("Décile ", 1:10)
)

ggplot(merged_df, aes(x = chipTime_decile)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution des temps réalisés par déciles fixes",
    x = "Déciles de temps (égaux en durée)",
    y = "Nombre de participants"
  ) +
  theme_minimal()

median_time <- median(merged_df$chipTimeResult, na.rm = TRUE)

# Graphique par genre

# Calculer les déciles pour les hommes (seulement les lignes des hommes)
decile_breaks_men <- seq(min(filtered_df$chipTime_seconds[filtered_df$gender == "M"], na.rm = TRUE), 
                         max(filtered_df$chipTime_seconds[filtered_df$gender == "M"], na.rm = TRUE), 
                         length.out = 11)

# Calculer les déciles pour les femmes (seulement les lignes des femmes)
decile_breaks_women <- seq(min(filtered_df$chipTime_seconds[filtered_df$gender == "F"], na.rm = TRUE), 
                           max(filtered_df$chipTime_seconds[filtered_df$gender == "F"], na.rm = TRUE), 
                           length.out = 11)

# Appliquer les déciles respectifs sur chaque sous-groupe
filtered_df$chipTime_decile <- NA  # Créer une nouvelle colonne vide

# Pour les hommes
filtered_df$chipTime_decile[filtered_df$gender == "M"] <- cut(
  filtered_df$chipTime_seconds[filtered_df$gender == "M"],
  breaks = decile_breaks_men,
  include.lowest = TRUE,
  labels = paste0("Décile ", 1:10)
)

# Pour les femmes
filtered_df$chipTime_decile[filtered_df$gender == "F"] <- cut(
  filtered_df$chipTime_seconds[filtered_df$gender == "F"],
  breaks = decile_breaks_women,
  include.lowest = TRUE,
  labels = paste0("Décile ", 1:10)
)


ggplot(filtered_df, aes(x = chipTime_decile, fill = gender)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(
    title = "Distribution des temps réalisés par déciles fixes",
    x = "Déciles de temps (égaux en durée)",
    y = "Nombre de participants"
  ) +
  scale_fill_manual(values = c("darkblue", "purple")) +  # Choisir des couleurs plus neutres
  facet_wrap(~gender, ncol = 1, scales = "free_y") +  # Séparer les graphiques par genre et autoriser des échelles libres
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),  # Ajuster les ticks sur l'axe y
    labels = scales::comma  # Ajouter des séparateurs de milliers
  ) +
  theme_minimal()



#10 déciles mais limite de 6h

# Étape 2 : Définir la limite et filtrer
max_time <- 21600  # 6 heures en secondes
filtered_df <- merged_df %>%
  filter(chipTime_seconds <= max_time)

# Calcul des déciles sur filtered_df
min_time <- min(filtered_df$chipTime_seconds, na.rm = TRUE)
max_time <- max(filtered_df$chipTime_seconds, na.rm = TRUE)

# Diviser l'intervalle total en 10 déciles
decile_breaks <- seq(min_time, max_time, length.out = 11)

# Ajouter une colonne pour assigner chaque participant à un décile fixe dans filtered_df
filtered_df$chipTime_decile <- cut(
  filtered_df$chipTime_seconds,
  breaks = decile_breaks,
  include.lowest = TRUE,
  labels = paste0("Décile ", 1:10)
)

# Graphique de la distribution des temps réalisés par déciles fixes
ggplot(filtered_df, aes(x = chipTime_decile)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution des temps réalisés par déciles fixes (<= 6 heures)",
    x = "Déciles de temps (égaux en durée)",
    y = "Nombre de participants"
  ) +
  theme_minimal()


median_time_filter <- median(filtered_df$chipTimeResult, na.rm = TRUE)
