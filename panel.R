#On charge pour les autres années

#2023

base_url2 <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2023/participants?groupId=1005392&routeId=175225"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url2, page_limit = 100, max_pages = 600)
final_results_list2 <- lapply(results, function(result) result$finalResult)
final_results_df2 <- do.call(rbind, lapply(final_results_list2, as.data.frame))

person_list2<- lapply(results, function(result) result$person)
person_df2 <- do.call(rbind, lapply(person_list2, as.data.frame))

# Fusionner les deux dataframes
merged_df2023 <- cbind(final_results_df2, person_df2) %>%
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
    ),
    year = 2023,
  )

save(merged_df, file = "merged_results2023.rda")


#2022

base_url3 <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2022/participants?groupId=947610&routeId=170632"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url3, page_limit = 100, max_pages = 600)
final_results_list3 <- lapply(results, function(result) result$finalResult)
final_results_df3 <- do.call(rbind, lapply(final_results_list3, as.data.frame))

person_list3<- lapply(results, function(result) result$person)
person_df3<- do.call(rbind, lapply(person_list3, as.data.frame))

# Fusionner les deux dataframes
merged_df2022 <- cbind(final_results_df3, person_df3) %>%
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
    ),
    year = 2022,
  )

save(merged_df, file = "merged_results2022.rda")


#2021

base_url5 <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2020/participants?groupId=939600&routeId=170068"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url5, page_limit = 100, max_pages = 600)
final_results_list5 <- lapply(results, function(result) result$finalResult)
final_results_df5 <- do.call(rbind, lapply(final_results_list5, as.data.frame))

person_list5<- lapply(results, function(result) result$person)
person_df5<- do.call(rbind, lapply(person_list5, as.data.frame))

# Fusionner les deux dataframes
merged_df2021 <- cbind(final_results_df5, person_df5) %>%
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
    ),
    year = 2021,
  )

save(merged_df, file = "merged_results2021.rda")

#2019

base_url8 <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2019/participants?groupId=828580&routeId=155986"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url5, page_limit = 100, max_pages = 600)
final_results_list8 <- lapply(results, function(result) result$finalResult)
final_results_df8 <- do.call(rbind, lapply(final_results_list8, as.data.frame))

person_list8<- lapply(results, function(result) result$person)
person_df8<- do.call(rbind, lapply(person_list8, as.data.frame))

# Fusionner les deux dataframes
merged_df2019 <- cbind(final_results_df8, person_df8) %>%
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
    ),
    year = 2019,
  )

save(merged_df, file = "merged_results2019.rda")

#2018

base_url4 <- "https://resultscui.active.com/api/results/events/schneiderelectricmarathondeparis2018/participants?groupId=210269&routeId=101035"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url4, page_limit = 100, max_pages = 600)
final_results_list4 <- lapply(results, function(result) result$finalResult)
final_results_df4 <- do.call(rbind, lapply(final_results_list4, as.data.frame))

person_list4<- lapply(results, function(result) result$person)
person_df4 <- bind_rows(lapply(results, function(result) result$person))
# Fusionner les deux dataframes
merged_df2018 <- cbind(final_results_df4, person_df4) %>%
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
    ),
    year = 2018,
  )

save(merged_df, file = "merged_results2018.rda")


#2016

base_url6 <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2016/participants?groupId=816890&routeId=154787"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url6, page_limit = 100, max_pages = 600)
final_results_list6 <- lapply(results, function(result) result$finalResult)
final_results_df6<- do.call(rbind, lapply(final_results_list6, as.data.frame))

person_list6<- lapply(results, function(result) result$person)
person_df6<- bind_rows(lapply(results, function(result) result$person))

# Fusionner les deux dataframes
merged_df2016 <- cbind(final_results_df6, person_df6) %>%
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
    ),
    year = 2016,
  )

save(merged_df, file = "merged_results2016.rda")

#2015

base_url7 <- "https://resultscui.active.com/api/results/events/SchneiderElectricMarathondeParis2015/participants?groupId=817421&routeId=154844"

# Récupérer les résultats page par page, jusqu'à 15 pages
results <- get_results(base_url7, page_limit = 100, max_pages = 600)
final_results_list7 <- lapply(results, function(result) result$finalResult)
final_results_df7<- do.call(rbind, lapply(final_results_list7, as.data.frame))

person_list7<- lapply(results, function(result) result$person)
person_df7<- do.call(rbind, lapply(person_list7, as.data.frame))

# Fusionner les deux dataframes
merged_df2015 <- cbind(final_results_df7, person_df7) %>%
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
    ),
    year = 2015,
  )

save(merged_df, file = "merged_results2015.rda")


#autre site de scrapping 

#2017

get_results_athlinks <- function(url, offset = 0, limit = 50) {
  full_url <- paste0(url, "&from=", offset, "&limit=", limit)
  response <- GET(full_url)
  if (status_code(response) != 200) {
    stop("Erreur : ", status_code(response))
  }
  content <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(content)
  
  return(json_data)
}

# Exemple d'utilisation
base_url <- "https://results.athlinks.com/event/961505?eventCourseId=2023697&divisionId=&intervalId="
all_results <- list()

# Boucle pour récupérer les pages par incréments de 50
for (offset in seq(0, 1000, by = 50)) {
  print(paste("Téléchargement des résultats à partir de", offset))
  results <- get_results_athlinks(base_url, offset)
  
  # Vérifier si les résultats sont vides
  if (length(results$data) == 0) {
    print("Fin des résultats.")
    break
  }
  
  # Ajouter les résultats à la liste
  all_results <- append(all_results, results$data)
}

# Convertir la liste en data.frame
final_results <- do.call(rbind, lapply(all_results, as.data.frame))

# Sauvegarder les résultats
write.csv(final_results, "athlinks_results.csv", row.names = FALSE)


#Marathon de Valence 
"https://results.athlinks.com/event/72408?eventCourseId=154073&divisionId=&intervalId=&from=50&limit=50"


