# Financing sources of rebel groups
# Working paper
# Álvaro Amorós Rodrígeuz
# 26/07/2021

# Loading libraries and packages
knitr::opts_chunk$set(echo = TRUE)
pkgs <- c('knitr', 'stargazer')
repmis::LoadandCite(pkgs, file = 'Rpackages.bib')

library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(tidytext)
 library(stargazer)
library(gridExtra)

# Max number of digits
options(digits = 2)

# PREPARING DATA

# Load data sets
GTD_groups <- read_excel("dataset GTD groups nov2020.xlsx") # Data provided by Ignacio Sanchez Cuenca on groups with territorial control
GTD_attacks <- read_csv("globalterrorismdb_0718dist.csv") # Global Terrorism Database

# keep only groups that are in both datasets
GTD_attacks <- GTD_attacks %>%
  filter(gname %in% GTD_groups$gname,
         !is.na(gname))

# The same terrorist group appears twice, so we eliminate one of the entries
GTD_groups<- GTD_groups %>%
  filter(gname %in% GTD_attacks$gname,
         !is.na(gname))
GTD_groups <- GTD_groups[-314,]


# Merge the dataset by name
GTD <- merge(GTD_attacks, GTD_groups, by = "gname")

# Drop cases without description
GTD <- GTD %>%
  filter(!is.na(summary),
         !is.na(control),
         iyear > 1997)

# DESCRIPTIVE STATISTICS

# Total n of incidents registered in GTD
total_incidents <- nrow(GTD)

# N of groups with territorial control
GTD %>%
  filter(control == 1) %>%
  distinct(gname) %>%
  nrow()

# N of groups without territorial control
GTD %>%
  filter(control == 0) %>%
  distinct(gname) %>%
  nrow()

# N of events carried out by groups with territorial control
GTD %>%
  filter(control == 1) %>%
  distinct(eventid) %>%
  nrow()

# N of events carried out by groups without territorial control
GTD %>%
  filter(control == 0) %>%
  distinct(eventid) %>%
  nrow()

# GRAFICAL ANALYSIS

# N of attacks by year
graph_1 <- GTD %>%
  mutate(control = as.character(control)) %>%
  count(control, iyear) %>%
  group_by(control) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(iyear, percent, color = control)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  ggtitle("Graph 1: By year") +
  theme_minimal() +
  labs(x = "Year", y = "% of attacks", color = "")  

# N of attacks by region
graph_2 <- GTD %>%
  group_by(region_txt) %>%
  mutate(n = n()) %>%
  filter(n > 100) %>%
  ungroup() %>%
  mutate(control = as.character(control)) %>%
  count(control, region_txt) %>%
  group_by(control) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(x = reorder(region_txt, percent), y =percent, fill = control)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  ggtitle("Graph 2: By region") +
  labs(x = "Region", y = "Attacks", color = "")  +
  theme_minimal() +
  coord_flip()

# Grid joining both graphs
grid.arrange(graph_1, graph_2, top= "Incidence of attacks", ncol =2)

# TEXT ANALYSIS

# Establish regexp patterns to try to ignore irrelevant data like dates or numbers
date.pattern <- ' ?(0|1)?[1-9]/([0-9]{4}|[0-9]{2}) ?'
number.pattern <- '[\\d-]'
single.pattern <- "(^| ).( |$)" # Pattern to delete single characters

# Create a data set with all the words as rows
summary_words <- GTD %>%
  mutate(summary = str_replace_all(summary, date.pattern , ""))  %>% # First we delete all the words present in the 
  mutate(summary = str_replace_all(summary, number.pattern , ""))  %>% # previously defined patterns
  mutate(summary = str_replace_all(summary, single.pattern , ""))  %>%
  unnest_tokens(word, summary, token = "words") # "unnest_tokens" takes each single word in the "description" column and turns it in
                                                 # its own specific row

# Manually filter out some common words in the text, that do not add any valuable information to our project
summary_words <- summary_words %>%
  filter(word != "west",
       word != "north",
       word != "pakistan",
       word != "Liberation",
       word != "liberation",
       word != "united") 

# Create a dummy variable for words that where in descriptions of groups with territorial control
summary_words <- summary_words %>%
  mutate(terrotory = ifelse(control == 1, "Territorial_con", "No_territroail_con"))

# Create a new data frame with the odds ratio of each word as well as the dummy indicating if it is part of a des cription of a group
# with territorial control or without 
control_or <- summary_words %>%
  count(word, terrotory) %>%
  pivot_wider(names_from = "terrotory", values_from = "n", values_fill = 0) %>%
  mutate(or = (No_territroail_con + 0.5) / (sum(No_territroail_con) - No_territroail_con + 0.5) / 
           ( (Territorial_con + 0.5) / (sum(Territorial_con) - Territorial_con + 0.5)))

# Five words with the highest odds ratios
control_or %>% filter(No_territroail_con + Territorial_con > 1000 & No_territroail_con > 100 & Territorial_con > 100) %>%
  arrange(desc(or))%>% head(5) %>%  knitr::kable(caption = "Highest ratio. Groups without territorial control", align = "l", col.names  = c("Word", "N. Control", "N. No Control","Odds Ratio"))

# Five words with the lowest odds ratios
control_or %>% filter(No_territroail_con + Territorial_con > 1000 & No_territroail_con >100 & Territorial_con > 100) %>%
arrange((or)) %>%  head(5) %>%  knitr::kable(caption = "Lowest ratio. Groups without territorial control", align = "l", col.names  = c("Word", "N. Control", "N. No Control","Odds Ratio"))

# Words related to economic activities

# Create a vector including words related to different types of economic activities
all_words <- c("bank", "robbery", "theft", "break-in", "burglary", "heist", "looting", "larceny", "extortion", "shakedown", "money", "payment","kidnap", "kidnapping", "capture", "hijack", "abducted", "abduction")

# To avoid words with very low frequency in the data set, but extreme odds rations, 
# i will restrict the analysis to those words that appear at least 30 times in the data set
economic_activities <- control_or %>% filter(word %in% all_words) %>%
  filter( (Territorial_con + No_territroail_con) > 30) %>% 
  arrange(desc(or))                                        

# Show results in a tabble
rbind(x) %>%
  knitr::kable(caption = "Economic Incidents", align = "l", col.names  = c("Word", "Territorial control", "No territorial control", "Odds ratio."))

# REGRESSION ANALYSIS
# Preparing data for regression

# To safe memory I will drop some of the data used to calculate the odds rations
rm(summary_words,control_or)

# I will add a dummy variable to the GTD, coded as 1 if the event was described as a robbery and 0 otherwise. For that I will check for specific words in the description
t <- grep("bank", GTD$summary, value = FALSE) # Generate a vector with the indexes of the events that include the word "bank" in their descriptions
GTD$robbery <- 0 # Dummy for robbery coded as 0
GTD$robbery[t] <- 1 # Transform to 1 for the events that are in index "t"
t <- grep("robbery", GTD$summary, value = FALSE) # Ill repeat this process with the rest of the words related to robberies...
GTD$robbery[t] <- 1
t <- grep("theft", GTD$summary, value = FALSE) 
GTD$robbery[t] <- 1
t <- grep("burglary", GTD$summary, value = FALSE) 
GTD$robbery[t] <- 1
t <- grep("heist", GTD$summary, value = FALSE) 
GTD$robbery[t] <- 1
t <- grep("holdup", GTD$summary, value = FALSE) 
GTD$robbery[t] <- 1
t <- grep("looting", GTD$summary, value = FALSE) 
GTD$robbery[t] <- 1

# I repeat the same process for extortion
t <- grep("extortion", GTD$summary, value = FALSE) 
GTD$extortion <- 0
GTD$extortion[t] <- 1

t <- grep("extort", GTD$summary, value = FALSE) 
GTD$extortion[t] <- 1

t <- grep("shakedown", GTD$summary, value = FALSE) 
GTD$extortion[t] <- 1

t <- grep("payment", GTD$summary, value = FALSE) 
GTD$extortion[t] <- 1

t <- grep("money", GTD$summary, value = FALSE) 
GTD$extortion[t] <- 1

# As many description in which the previous words appear describe extortion conducted trough kidnapping (and here I want to code regular extortion,
# without kidnappings) I will ad a second restriction, this is, that the event is not codded as a kidnap in the variable "attacktype1"
GTD <- GTD %>%
  mutate(extortion = ifelse(GTD$attacktype1 == 6, 0, GTD$extortion))

# Repeat the process a third time for to code kidnappings with economic aims
t <- grep("extortion", GTD$summary, value = FALSE) 
GTD$kidnap <- 0
GTD$kidnap[t] <- 1

t <- grep("payment", GTD$summary, value = FALSE) 
GTD$kidnap[t] <- 1

t <- grep("money", GTD$summary, value = FALSE) 
GTD$kidnap[t] <- 1

t <- grep("ransom", GTD$summary, value = FALSE) 
GTD$kidnap[t] <- 1

t <- grep("rescue", GTD$summary, value = FALSE) 
GTD$kidnap[t] <- 1

t <- grep("extort", GTD$summary, value = FALSE) 
GTD$kidnap[t] <- 1

GTD <- GTD %>%
  mutate(kidnap = ifelse(GTD$attacktype1 != 6, 0, GTD$kidnap ))

# Regressions
# Regression whee territorial control is the dependent variable and the regressors are the three dummies for robbery extortion and kidnapping
model_1 <- GTD %>%
  mutate(year_factor = as.factor(iyear),
         country_factor = as.factor(country.x),
         control = as.factor(control)) %>%
  glm(control ~ robbery  + extortion + kidnap, family = "binomial", data = .)

# I add the variable country and year as factor variables to the regression. They will act as control variables for country and year fixed efects,
model_2 <- GTD %>%
  mutate(year_factor = as.factor(iyear),
         country_factor = as.factor(country.x),
         control = as.factor(control)) %>%
  glm(control ~ robbery  + extortion + kidnap + year_factor + country_factor, family = "binomial", data = .)

# To finish I output the regression results in latex
stargazer::stargazer(model_1, model_2,
                     digits = 3, type = 'latex', omit = c("year_factor", "country_factor"))