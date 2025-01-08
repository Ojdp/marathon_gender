#marathon de Paris
load("~/GitHub/marathon_gender/merged_results.rda")
load("~/GitHub/marathon_gender/merged_results2023.rda")
load("~/GitHub/marathon_gender/merged_results2022.rda")
load("~/GitHub/marathon_gender/merged_results2021.rda")
load("~/GitHub/marathon_gender/merged_results2019.rda")
load("~/GitHub/marathon_gender/merged_results2017.rda")
load("~/GitHub/marathon_gender/merged_results2018.rda")
load("~/GitHub/marathon_gender/merged_results2016.rda")
load("~/GitHub/marathon_gender/merged_results2015.rda")
load("~/GitHub/marathon_gender/merged_results2014.rda")
load("~/GitHub/marathon_gender/merged_results2013.rda")
load("~/GitHub/marathon_gender/merged_results2012.rda")


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
