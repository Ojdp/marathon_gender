library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ofce)
library(cowplot)
library(patchwork)



load("merged_results.rda")
nombre_de_femmes <- sum(merged_df$gender == "F")
nombre_de_femmes

age_moyen <- mean(merged_df$age)
age_moyen_homme <- mean(merged_df$age[merged_df$gender == "M"], na.rm = TRUE)
age_moyen_femme <- mean(merged_df$age[merged_df$gender == "F"], na.rm = TRUE)

speed_moyen <- mean(merged_df$averageSpeed)
speed_moyen_homme <- mean(merged_df$averageSpeed[merged_df$gender == "M"], na.rm = TRUE)
speed_moyen_femme <- mean(merged_df$averageSpeed[merged_df$gender == "F"], na.rm = TRUE)

merged_hommes <- merged_df |>  filter(gender == "M")
merged_femmes <- merged_df |> filter(gender == "F")


merged_df$chip_time<- sapply(merged_df$chipTimeResult, function(x) {
  parts <- strsplit(x, ":")[[1]]
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  (hours * 60 + minutes + seconds / 60)/60
})
time_moyen_homme <- mean(merged_df$chip_time[merged_df$gender == "M"], na.rm = TRUE)
time_moyen_femme <- mean(merged_df$chip_time[merged_df$gender == "F"], na.rm = TRUE)

time_min_homme <- min(merged_df$chip_time[merged_df$gender == "M"], na.rm = TRUE)
time_min_femme <- min(merged_df$chip_time[merged_df$gender == "F"], na.rm = TRUE)

merged_df$gun_time<- sapply(merged_df$gunTimeResult, function(x) {
  parts <- strsplit(x, ":")[[1]]
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  (hours * 60 + minutes + seconds / 60)/60
})

merged_df <- merged_df |> 
  mutate(sas=gun_time - chip_time)


merged_df |> summary()
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


# Double graphique sur la distribution des âges
graph1 <- ggplot(merged_df, aes(x = age, color = gender, fill = gender)) +
  geom_histogram(alpha = 0.9, position = "dodge", binwidth = 2) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  geom_vline(xintercept = c(age_moyen_femme, age_moyen_homme), linetype = c("longdash","dotted")) +
  annotate(c("text"), x = 26, y = 3000,  color= "black" , size=3, label = "Women's average age")+
  annotate(c("text"), x =50, y = 2500,  color= "black" , size=3, label = "Men's average age")+
  labs(x="Age", y="Number")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

graph2 <- ggplot(merged_df, aes(x = age, color = gender, fill = gender)) +
  geom_density(adjust = 1,alpha = 0.001, show.legend = FALSE) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  labs(x="Age", y="Density")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

legend1 <- get_legend(graph1 + theme(legend.box.margin = margin(0, 12)))

GRAPH_A <- graph1 + graph2 +
  plot_annotation(
    title="Age Distribution of Participants in the Paris Marathon by Gender",
    caption = "Field: All participants starting the Paris Marathon.  \n  Source: Marathon de Paris, 2024.",
                  theme=theme(plot.caption = element_text(size=8)))


# Double graphique sur la distribution des vitesses moyennes
graph3 <- ggplot(merged_df, aes(x = averageSpeed, color = gender, fill = gender)) +
  geom_histogram(alpha = 0.9, position = "dodge", binwidth = 0.5) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  geom_vline(xintercept = c(speed_moyen_femme, speed_moyen_homme), linetype = c("longdash","dotted")) +
  annotate(c("text"), x = 7, y = 4000,  color= "black" , size=3, label = "Women's average speed")+
  annotate(c("text"), x =13, y = 4500,  color= "black" , size=3, label = "Men's average speed")+
  labs(x="Average speed in Km/h", y="Number")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

graph4 <- ggplot(merged_df, aes(x = averageSpeed, color = gender, fill = gender)) +
  geom_density(adjust = 1,alpha = 0.001, show.legend = FALSE) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  labs(x="Average speed in Km/h", y="Density")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

legend1 <- get_legend(graph1 + theme(legend.box.margin = margin(0, 12)))

GRAPH_B <- graph3 + graph4 +
  plot_annotation(
    title="Average Speed's Distribution of Participants in the Paris Marathon by Gender",
    caption = "Field: All participants starting the Paris Marathon.  \n  Source: Marathon de Paris, 2024.",
    theme=theme(plot.caption = element_text(size=8)))

# Double graphique sur la distribution les temps 
graph5 <- ggplot(merged_df, aes(x = time, color = gender, fill = gender)) +
  geom_histogram(alpha = 0.9, position = "dodge", binwidth = 0.2) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  geom_vline(xintercept = c(time_moyen_femme, time_moyen_homme), linetype = c("longdash","dotted")) +
  annotate(c("text"), x = 3, y = 5000,  color= "black" , size=3, label = "Men's average time")+
  annotate(c("text"), x =5.5, y = 4500,  color= "black" , size=3, label = "Women's average time")+
  labs(x="Average time in hours", y="Number")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

graph6 <- ggplot(merged_df, aes(x = averageSpeed, color = gender, fill = gender)) +
  geom_density(adjust = 1,alpha = 0.001, show.legend = FALSE) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  labs(x="Average time in hours", y="Density")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

legend1 <- get_legend(graph1 + theme(legend.box.margin = margin(0, 12)))

GRAPH_C <- graph5 + graph6 +
  plot_annotation(
    title="Average Time's Distribution of Participants in the Paris Marathon by Gender",
    caption = "Field: All participants starting the Paris Marathon.  \n  Source: Marathon de Paris, 2024.",
    theme=theme(plot.caption = element_text(size=8)))


ggplot(merged_df) +
  geom_density(aes(x=sas), adjust = 1,alpha = 0.001, show.legend = FALSE) +
  scale_color_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  ) +
  scale_fill_manual(
    name = "", 
    values = c("M" = "#4CAF50FF", "F" = "#FF9800FF"),
    labels = c("M" = "Men", "F" = "Women")
  )  +
  labs(x="Average time in hours", y="Density")+
  theme_ofce(base_size = 9,
             legend.position = "bottom")

