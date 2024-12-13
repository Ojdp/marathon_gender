
# Installer et charger les bibliothèques nécessaires
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)



# Une fois que vous avez récupéré toutes les données, vous pouvez les convertir en dataframe


get_results <- function(base_url, page_limit = 100, max_pages = 600) {
  all_results <- list()  # Liste pour stocker les résultats de chaque page
  
  # Boucle sur les pages
  for (page in 1:max_pages) {
    offset <- (page - 1) * page_limit  # Calcul de l'offset pour chaque page
    url <- paste0(base_url, "&offset=", offset, "&limit=", page_limit)
    cat("URL requêtée :", url, "\n")
    
    response <- GET(url)  # Effectuer la requête HTTP
    
    # Vérifier si la requête a réussi
    if (status_code(response) == 200) {
      data_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      # Si des résultats sont retournés, les ajouter à la liste pour cette page
      if (!is.null(data_json$items) && length(data_json$items) > 0) {
        all_results[[page]] <- data_json$items  # Stocker uniquement les résultats de la page courante
      } else {
        cat("Fin des données atteinte : moins de 100 résultats retournés.\n")
        break  # Si la page ne contient pas de résultats, on termine la boucle
      }
    } else {
      cat("Erreur HTTP :", status_code(response), "\n")
      break  # Si erreur, on arrête la boucle
    }
  }
  
  return(all_results)
}

# URL de base pour l'API
base_url <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2024/participants?groupId=1027941&routeId=177244"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url, page_limit = 100, max_pages = 600)

final_results_list <- lapply(results, function(result) result$finalResult)
final_results_df <- do.call(rbind, lapply(final_results_list, as.data.frame))

person_list <- lapply(results, function(result) result$person)
person_df <- do.call(rbind, lapply(person_list, as.data.frame))

# Fusionner les deux dataframes
merged_df <- cbind(final_results_df, person_df) %>%
  select(participantId, gunTimeResult, chipTimeResult, averageSpeed, disqualified, firstName, lastName, gender, age) %>%
  mutate(
    # Conversion de gunTimeResult en durée et formatage en HH:MM:SS
    gunTimeResult = as.duration(gunTimeResult),  # Convertir en durée si nécessaire
    gunTimeResult = sprintf(
      "%02d:%02d:%02d",
      as.integer(gunTimeResult) %/% 3600,        # Heures
      (as.integer(gunTimeResult) %% 3600) %/% 60, # Minutes
      as.integer(gunTimeResult) %% 60            # Secondes
    ),
    # Conversion de chipTimeResult en durée et formatage en HH:MM:SS
    chipTimeResult = as.duration(chipTimeResult),
    chipTimeResult = sprintf(
      "%02d:%02d:%02d",
      as.integer(chipTimeResult) %/% 3600,        # Heures
      (as.integer(chipTimeResult) %% 3600) %/% 60, # Minutes
      as.integer(chipTimeResult) %% 60            # Secondes
    )
  )

save(merged_df, file = "merged_results.rda")

#Stats descriptives
    
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

