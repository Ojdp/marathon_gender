
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
    ))

save(merged_df, file = "merged_results.rda")




