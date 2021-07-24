# Terrorims financing
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(writexl)
library(tidytext)


# Load data sets
GTD_groups <- read_excel("dataset GTD groups nov2020.xlsx")
GTD_attacks <- read_csv("globalterrorismdb_0718dist.csv")

# PREPARING DATASETS

# keep only groups that are in both datasets
GTD_attacks <- GTD_attacks %>%
  filter(gname %in% GTD_groups$gname,
         !is.na(gname))


GTD_groups<- GTD_groups %>%
  filter(gname %in% GTD_attacks$gname,
         !is.na(gname))
GTD_groups <- GTD_groups[-314,] # The same terrorist group appears twice.


# Merge the dataset by name
GTD <- merge(GTD_attacks, GTD_groups, by = "gname")

# Drop cases withput description
GTD <- GTD %>%
  filter(!is.na(summary),
         !is.na(control))

# DESCRIPTIVE ANALYSIS


GTD %>%
  mutate(Attacks = n()) %>%
  distinct(country_txt, .keep_all = TRUE) %>%
  mutate(Countries = n()) %>%
  distinct(gname, .keep_all = TRUE) %>%
  mutate(Groups = n()) %>%
  distinct(Attacks, .keep_all = TRUE) %>%
  summarise(Attacks, Groups, Countries)

# Number of groups with control vs without
GTD %>%
  group_by(gname) %>%
  mutate(n = n(),
         control = as.character(control)) %>%
  ggplot(aes(control)) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels=percent_format())



# Save descriptions


# Text mining 

GTD %>%
  filter(control == 1) %>%
  distinct(eventid) %>%
  nrow()

GTD %>%
  filter(control == 0) %>%
  distinct(eventid) %>%
  nrow()


GTD %>% 
  filter(control == 1) %>%
  count(gname) %>% arrange(desc(n)) %>% head (10) 

GTD %>% 
  filter(control == 0) %>%
  count(gname) %>% arrange(desc(n)) %>% head (10)


GTD %>%
  mutate(control = as.character(control)) %>%
  count(control, iyear) %>%
  group_by(control) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(iyear, percent, color = control)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Year", y = "% of attacks", color = "")


GTD[100,] %>% 
  unnest_tokens(word, summary) %>%
  pull(word)


date.pattern <- ' ?(0|1)?[1-9]/([0-9]{4}|[0-9]{2}) ?'

GTD[320,] %>% 
  mutate(summary = str_replace_all(summary, date.pattern, ""))  %>%
  unnest_tokens(word, summary, token = "words") %>%
  pull(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets")

summary_words <- GTD %>%
  mutate(summary = str_replace_all(summary, date.pattern , ""))  %>%
  unnest_tokens(word, summary, token = "words")

summary_words %>% 
  count(word) %>%
  arrange(desc(n)) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", "")) %>%
  head(30)

summary_words <- summary_words %>%
  mutate(terrotory = ifelse(control == 1, "Territorial_co", "No_territroail_con"))


control_or <- summary_words %>%
  count(word, terrotory) %>%
  pivot_wider(names_from = "terrotory", values_from = "n", values_fill = 0) %>%
  mutate(or = (No_territroail_con + 0.5) / (sum(No_territroail_con) - No_territroail_con + 0.5) / 
           ( (Territorial_co + 0.5) / (sum(Territorial_co) - Territorial_co + 0.5)))

## Or for groups with no territorial control
control_or %>% arrange(desc(or))

## Or for groups with  territorial control
control_or %>% arrange(or)

## Or for groups with no territorial control
control_or %>% filter(No_territroail_con+Territorial_co > 1000) %>%
  arrange(desc(or))

## Or for groups with  territorial control
control_or %>% filter( No_territroail_con > 200 ) %>%
  arrange((or))


# Odds ration stelaing
stealing <- c("bank","robbery", "theft", "break-in", "burglary", "heist", "holdup", "looting", "larceny")

control_or %>% filter(word %in% stealing ) %>%
  arrange(desc(or))


# Odds ration extortion
extortion <- c("extortion", "shakedown", "theft")

control_or %>% filter(word %in% extortion ) %>%
  arrange(desc(or))

# Odds Kidnaping
kidnap <- c("kidnap", "kidnaping", "capture", "hijack")
control_or %>% filter(word %in% kidnap ) %>%
  arrange(desc(or))

smuggling

control_or %>% filter(word %in% "trafficking" ) %>%
  arrange(desc(or))

