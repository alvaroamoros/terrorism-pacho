# Financing sources of rebel groups. Predicting territorial control useing classification trees
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
library(caret)
library(tree)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

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

# Cleaning irrelevant columns
GTD <- GTD %>%
  select(-resolution, -approxdate, -region, -gname, -country.x, -country_pacho, -provstate, -latitude, -longitude, -location, -summary, 
         -attacktype1, -attacktype2, -attacktype3, -targsubtype1, -targsubtype2, -targsubtype3, -targtype2, -targtype2_txt, -targsubtype2,
         -targsubtype2_txt, -targsubtype3, -targsubtype3_txt, -corp2, -corp3, -target2, -natlty2, -natlty2_txt, -targtype3, -targtype3_txt,
         -target3, -natlty3, -natlty3_txt, -gsubname, -gsubname2, -gname3, -gsubname3, -motive, -attacktype2_txt, -attacktype3_txt, -gname2,
         -guncertain2, -guncertain3, -claimmode, -claimmode_txt, -claim2, -claimmode2, -claimmode2_txt, -claim3, -claimmode3, -claimmode3_txt,
         -compclaim, -weaptype2, -weaptype2_txt, -weapsubtype2, -weapsubtype2_txt, -weaptype3, -weaptype3_txt, -weapsubtype3, -weapsubtype3_txt,
         -weaptype4, -weaptype4_txt, -weapsubtype4, -weapsubtype4_txt, -weapdetail, -propextent, -propextent_txt, -propvalue, -propcomment, -nhostkid
         ,-nhostkidus, -nhours, -ndays, -divert, -kidhijcountry, -ransom, -ransomamt, -ransomamtus, -ransompaid, -ransompaidus, -ransomnote, -hostkidoutcome, 
         -hostkidoutcome_txt, -nreleased, -addnotes, -scite1, -scite2, -dbsource, -INT_LOG, -INT_IDEO, -INT_MISC, -INT_ANY, -related, -control_post1997_sandler,
         -firstyear, -lastyear, -var10, -control_gtd1, -country.y, -scite3, -weapsubtype1_txt, -alternative, -alternative_txt, -nperps, -nperpcap, -eventid, -city, -country_txt,
         -corp1, -target1, -natlty1_txt)
GTD <- GTD %>%
  na.omit()

# Drop data we do not need
rm(GTD_attacks)
rm(GTD_groups)

# Add variables based on text analysis
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

# We transform or dependent variable to a factor
GTD$control <- as.factor(ifelse(GTD$control == 1, "Yes", "No"))



# Divide in train and test sets
set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = GTD$control, times = 1, p = 0.1, 
                                  list = FALSE)

train_set <- GTD[-test_index,]
test_set <- GTD[test_index,]

prop.table(table(train_set$control))

# Decision Trees in R using rpart

# We star classifing by using explusivly the variable bank 

fit <- rpart(control~., data = train_set, method = 'class')
rpart.plot(fit)

predict_unseen <-predict(fit, test_set, type = 'class')

table_mat <- table(test_set$control, predict_unseen)
table_mat


accuracy_Test
