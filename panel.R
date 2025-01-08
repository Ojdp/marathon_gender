#On charge pour les autres années
library(tidyr)
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


get_athlinks_results <- function(base_url, page_limit = 100, max_pages = 1000) {
  all_results <- list()  # Liste pour stocker les résultats de chaque page
  
  # Boucle sur les pages
  for (page in 1:max_pages) {
    offset <- (page - 1) * page_limit
    url <- paste0(base_url, "&from=", offset, "&limit=", page_limit)
    cat("URL requêtée :", url, "\n")
    
    response <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
    
    if (status_code(response) == 200) {
      data_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      # Vérification de l'existence des résultats
      if (!is.null(data_json$interval$intervalResults[[1]])) {
        results_page <- data_json$interval$intervalResults[[1]]
        all_results[[page]] <- results_page
      } else {
        cat("Fin des données atteinte : moins de", page_limit, "résultats retournés.\n")
        break
      }
    } else {
      cat("Erreur HTTP :", status_code(response), "\n")
      break
    }
  }
  
  # Combiner tous les résultats dans un seul data frame
  final_results <- bind_rows(lapply(all_results, as.data.frame))
  
  return(final_results)
}

# Exécution
#2017
base_url <- "https://results.athlinks.com/event/961505?eventCourseId=2023697&divisionId=27756548&intervalId="
athlinks_results <- get_athlinks_results(base_url)

merged_df2017 <- athlinks_results %>%
  unnest(time) %>%  # Décompresse la colonne time pour avoir timeInMillis
  select(country, firstName, lastName, gender, age, entryId, primaryBracketRank, timeInMillis) %>%
  mutate(
    # Conversion en durée formatée HH:MM:SS
    raceTime = sprintf(
      "%02d:%02d:%02d",
      as.integer(timeInMillis / 1000) %/% 3600,            # Heures
      (as.integer(timeInMillis / 1000) %% 3600) %/% 60,    # Minutes
      as.integer(timeInMillis / 1000) %% 60                # Secondes
    ),
    
    # Ajout de l'année
    year = 2017
  )

save(merged_df, file = "merged_results2017.rda")

#2014
base_url <- "https://results.athlinks.com/event/373568?eventCourseId=1764601&divisionId=&intervalId=&"
athlinks_results2 <- get_athlinks_results(base_url)

merged_df2014 <- athlinks_results2 %>%
  unnest(time) %>%  # Décompresse la colonne time pour avoir timeInMillis
  select(country, firstName, lastName, gender, age, entryId, primaryBracketRank, timeInMillis) %>%
  mutate(
    # Conversion en durée formatée HH:MM:SS
    raceTime = sprintf(
      "%02d:%02d:%02d",
      as.integer(timeInMillis / 1000) %/% 3600,            # Heures
      (as.integer(timeInMillis / 1000) %% 3600) %/% 60,    # Minutes
      as.integer(timeInMillis / 1000) %% 60                # Secondes
    ),
    
    # Ajout de l'année
    year = 2014
  )

save(merged_df, file = "merged_results2014.rda")

#2013 
base_url <- "https://results.athlinks.com/event/261301?eventCourseId=369551&divisionId=16048108&intervalId=&"
athlinks_results3 <- get_athlinks_results(base_url)

merged_df2013 <- athlinks_results3%>%
  unnest(time) %>%  # Décompresse la colonne time pour avoir timeInMillis
  select(country, firstName, lastName, gender, age, entryId, primaryBracketRank, timeInMillis) %>%
  mutate(
    # Conversion en durée formatée HH:MM:SS
    raceTime = sprintf(
      "%02d:%02d:%02d",
      as.integer(timeInMillis / 1000) %/% 3600,            # Heures
      (as.integer(timeInMillis / 1000) %% 3600) %/% 60,    # Minutes
      as.integer(timeInMillis / 1000) %% 60                # Secondes
    ),
    
    # Ajout de l'année
    year = 2013
  )

save(merged_df, file = "merged_results2013.rda")

#2012
base_url <- "https://results.athlinks.com/event/188714?eventCourseId=261507&divisionId=&intervalId=&"
athlinks_results4 <- get_athlinks_results(base_url)

merged_df2012 <- athlinks_results4%>%
  unnest(time) %>%  # Décompresse la colonne time pour avoir timeInMillis
  select(country, firstName, lastName, gender, age, entryId, primaryBracketRank, timeInMillis) %>%
  mutate(
    # Conversion en durée formatée HH:MM:SS
    raceTime = sprintf(
      "%02d:%02d:%02d",
      as.integer(timeInMillis / 1000) %/% 3600,            # Heures
      (as.integer(timeInMillis / 1000) %% 3600) %/% 60,    # Minutes
      as.integer(timeInMillis / 1000) %% 60                # Secondes
    ),
    
    # Ajout de l'année
    year = 2012
  )

save(merged_df, file = "merged_results2012.rda")

2011 -> "https://results.athlinks.com/event/160138?eventCourseId=199425&divisionId=&intervalId=&"
2010 -> "https://results.athlinks.com/event/121993?eventCourseId=166738&divisionId=&intervalId=&"
2009 -> "https://results.athlinks.com/event/86297?eventCourseId=264050&divisionId=&intervalId=&"
2008 -> "https://results.athlinks.com/event/170851?eventCourseId=235625&divisionId=&intervalId=&"
2007 -> "https://results.athlinks.com/event/163635?eventCourseId=596257&divisionId=&intervalId=&"
2006 -> "https://results.athlinks.com/event/126836?eventCourseId=146231&divisionId=&intervalId=&"
2005 -> "https://results.athlinks.com/event/146352?eventCourseId=215850&divisionId=16066627&intervalId=&"
2004 -> "https://results.athlinks.com/event/174931?eventCourseId=240730&divisionId=16065939&intervalId=&"
2002 -> "https://results.athlinks.com/event/365837?eventCourseId=538412&divisionId=&intervalId=&"
2001 -> "https://results.athlinks.com/event/55150?eventCourseId=383736&divisionId=16085183&intervalId=&"

#marathon de NY


if (!require(RSelenium)) install.packages("RSelenium", dependencies = TRUE)
if (!require(rvest)) install.packages("rvest")
if (!require(dplyr)) install.packages("dplyr")

library(RSelenium)
library(rvest)
library(dplyr)

# Start Selenium server
rD <- rsDriver(browser = "chrome", chromever = "latest", verbose = FALSE)
remDr <- rD$client

# Function to scrape NYRR finishers
scrape_nyrr_results <- function(event, start_page = 1, max_pages = 100) {
  all_runners <- list()  # Initialize list to store results
  
  # Loop through pages
  for (page in start_page:max_pages) {
    url <- paste0("https://results.nyrr.org/event/", event, "/finishers#opf=1&page=", page)
    cat("Scraping page:", page, "\n")
    
    # Navigate to the URL
    remDr$navigate(url)
    Sys.sleep(5)  # Wait for the page to load
    
    # Get page source and parse HTML
    page_source <- remDr$getPageSource()[[1]]
    page_html <- read_html(page_source)
    
    # Extract runner details
    runners <- page_html %>%
      html_nodes(".cmd-finisher") %>%
      lapply(function(runner) {
        name <- runner %>% html_node(".name") %>% html_text(trim = TRUE)
        details <- runner %>% html_node(".details") %>% html_text(trim = TRUE)
        time <- runner %>% html_node(xpath = ".//span[contains(text(), 'Time')]/span") %>% html_text(trim = TRUE)
        pace <- runner %>% html_node(xpath = ".//span[contains(text(), 'Pace')]/span") %>% html_text(trim = TRUE)
        place <- runner %>% html_node(xpath = ".//span[contains(text(), 'Place')]/span") %>% html_text(trim = TRUE)
        
        # Return as a data frame
        data.frame(Name = name, Details = details, Time = time, Pace = pace, Place = place, stringsAsFactors = FALSE)
      })
    
    # Combine runners from current page
    if (length(runners) > 0) {
      all_runners[[page]] <- do.call(rbind, runners)
    } else {
      cat("No more runners found. Stopping.\n")
      break
    }
  }
  
  # Combine all pages into a single data frame
  final_results <- do.call(rbind, all_runners)
  
  return(final_results)
}

# Example usage
event <- "M2024"  # Event code
results <- scrape_nyrr_results(event)

# Save to CSV
write.csv(results, "nyrr_finishers.csv", row.names = FALSE)

# Stop Selenium server
remDr$close()
rD$server$stop()

cat("Data saved to nyrr_finishers.csv")
