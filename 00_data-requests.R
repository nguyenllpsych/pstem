###################
## DATA REQUESTS ##
###################

# Meta ----
library(dplyr)
library(rio)

# 2023-03-27 PSY 499V ----
#setwd("/Users/nguy4006/Library/CloudStorage/Box-Box/P-STEM/Data/Project Data")
full <- readRDS(file = "../PSTEM_Wave_1_SCORED_2023-02-21.RData")
dict <- rio::import(file = "../PSTEM_Data-Dictionary_Wave_1_Target.csv")
demo <- c("participant_id",
          # demo variables from dictionary
          dict$variable[which(dict$variable == "age"):length(dict$variable)],
          # race and gender coded as factors
          "gender_f", "race_f")

## Jinx Acharya ----
# 2023-03-27

# umics education: in-depth exploration and reconsideration of commitment
umics <- dict %>%
  filter(scale %in% c("education in-depth exploration",
                      "education reconsideration of commitment")) %>%
  pull(variable)

# social support form family and friends
ss <- dict %>%
  filter(grepl(pattern = "social support",
               x = scale)) %>%
  pull(variable)

# pull dataframe and associated dictionary
full %>%
  select(all_of(c(demo, umics, ss))) %>%
  rio::export(file = "data_JA.sav")

dict %>%
  filter(variable %in% c(demo, umics, ss)) %>%
  rio::export(file = "dict_JA.csv")

## Thomas Silver ----
# 2023-03-27

# all bfas scales
bfas <- dict %>%
  filter(grepl(pattern = "bfas",
               x = variable)) %>%
  pull(variable)

# ICAR
icar <- dict %>%
  filter(grepl(pattern = "iq_",
               x = variable)) %>%
  pull(variable)

# pull dataframe and associated dictionary
full %>%
  select(all_of(c(demo, bfas, icar))) %>%
  rio::export(file = "data_TS.sav")

dict %>%
  filter(variable %in% c(demo, bfas, icar)) %>%
  rio::export(file = "dict_TS.csv")


## Layssa Pena ----
# 2023-03-27

# meim measure
meim <- dict %>%
  filter(grepl(pattern = "meim_",
               x = variable)) %>%
  pull(variable)

# bfas assertiveness
bfas_a <- dict %>%
  filter(subscale == "assertiveness") %>%
  pull(variable)

# pull dataframe and associated dictionary
full %>%
  select(all_of(c(demo, meim, bfas_a))) %>%
  rio::export(file = "data_LP.sav")

dict %>%
  filter(variable %in% c(demo, meim, bfas_a)) %>%
  rio::export(file = "dict_LP.csv")
