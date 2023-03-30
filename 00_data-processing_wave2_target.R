############################
## P-STEM Cleaning Script ##
## Wave 2 Target          ##
## Linh Nguyen            ##
############################

# Meta ----

# libraries
library(dplyr)
library(codebook)
library(labelled)
library(haven)

# read in data file (working raw)
dat <- read.csv("../Raw Data - DO NOT MODIFY/Wave 2/PSTEM_Wave_2_WORKING_RAW_2023-03-29.csv")
colnames(dat)
head(dat)

# Dictionary ----

# create dictionary from wave 1
dict <- read.csv(file = "../PSTEM_Data-Dictionary_Wave_1_Target.csv") %>%
  # delete variables that are only in wave 1
  filter(variable %in% colnames(dat))

# fix dict encoding
dict[,"label"] <- gsub("<92>", "'", dict[,"label"]) 
dict[,"label"] <- gsub("<93>", "'", dict[,"label"]) 
dict[,"label"] <- gsub("<94>", "'", dict[,"label"]) 

# variables that are only in wave 2
colnames(dat)[which(!colnames(dat) %in% dict$variable)]

# rename Q104 to deviate
dat <- dat %>% 
  rename(deviate = Q104)

# add new variables (narratives) to dictionary
dict[(nrow(dict)+1),] <- c(
  # variable
  "deviate",
  # label
  "Often, others hold certain expectations about the way we should live our 
  lives due to our racial or ethnic background. This can include family, friends,
  teachers, or society in general. Sometimes our lives meet these expectations, 
  and sometimes they don’t. Sometimes we want to meet these expectations, and 
  sometimes we don't. Please now describe a specific experience where your life 
  deviated from the expectations of your racial/ethnic group. Write about a 
  specific experience, as though you are telling the story of the experience to 
  a close friend. Be sure to include details such as: what was the specific 
  expectation you violated? How did you feel when this experience happened? 
  Has this experience changed the way you think about your race/ethnicity, or 
  about race/ethnicity in general? Feel free to include any other details that 
  you think are relevant.",
  # scale, subscale, and keying
  NA, NA, NA,
  # type
  "character",
  #value_labels,
  NA)
dict[(nrow(dict)+1),] <- c(
  # variable
  "proud",
  # label
  "What has been your proudest educational achievement over the past year? 
  When you think back, what are you most excited about?",
  # scale, subscale, and keying
  NA, NA, NA,
  # type
  "character",
  #value_labels,
  NA)

# save dictionary
write.csv(dict, "../PSTEM_Data-Dictionary_Wave_2_Target.csv", row.names = F)  

# Labels ----

## variable labels ----
# make list from two columns in dictionary, apply the dictionary, and save to the data file
labelled::var_label(dat) <- dict %>%
  dplyr::select(variable, label) %>%
  codebook::dict_to_list()

## value labels ----
# need to be done separately for each set of response options

# likert 1-2 no/yes

{
  likert1 <- dict %>% 
    dplyr::filter (value_labels == "1 No 2 Yes") %>%
    dplyr::pull(variable)
  add_likert1 <- function(x) {
    val_labels(x) <- c("No" = 1,
                       "Yes" = 2)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert1, 
                     add_likert1)
  }

#check
dat$consent

# likert 1-5 Not at all-A lot

{
  likert2 <- dict %>% 
    dplyr::filter (value_labels == "1 Not at all 2 A little 3 Somewhat 4 Quite a bit 5 A lot") %>%
    dplyr::pull(variable)
  add_likert2 <- function(x) {
    val_labels(x) <- c("Not at all" = 1,
                       "A little" = 2,
                       "Somewhat" = 3,
                       "Quite a bit" = 4,
                       "A lot" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert2, 
                     add_likert2)
}

#check
dat$stemre_1

# likert 1-5 Confidence

{
  likert3 <- dict %>% 
    dplyr::filter (value_labels == "1 Not at all confident 2 To a small extent 3 To some extent 4 To a large extent 5 Absolutely confident") %>%
    dplyr::pull(variable)
  add_likert3 <- function(x) {
    val_labels(x) <- c("Not at all confident" = 1,
                       "To a small extent" = 2,
                       "To some extent" = 3,
                       "To a large extent" = 4,
                       "Absolutely confident" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert3, 
                     add_likert3)
}

#check
dat$stemse_1

# likert 1-5 Extent

{
  likert4 <- dict %>% 
    dplyr::filter (value_labels == "1 Not at all 2 To a small extent 3 To some extent 4 To a large extent 5 To a very large extent") %>%
    dplyr::pull(variable)
  add_likert4 <- function(x) {
    val_labels(x) <- c("Not at all" = 1,
                       "To a small extent" = 2,
                       "To some extent" = 3,
                       "To a large extent" = 4,
                       "To a very large extent" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert4, 
                     add_likert4)
}

#check
dat$stemment_1

#add value labels likert 1-5 Agree w/ Somewhat, Neutral

{
  likert5 <- dict %>% 
    dplyr::filter (value_labels == "1 Strongly Disagree 2 Somewhat Disagree 3 Neutral 4 Somewhat Agree 5 Strongly Agree") %>%
    dplyr::pull(variable)
  add_likert5 <- function(x) {
    val_labels(x) <- c("Strongly Disagree" = 1,
                       "Somewhat Disagree" = 2,
                       "Neutral" = 3,
                       "Somewhat Agree" = 4,
                       "Strongly Agree" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert5, 
                     add_likert5)
}

#check
dat$stemidco_1

#add value labels likert 1-2 true/false

{
  likert6 <- dict %>% 
    dplyr::filter (value_labels == "1 True 2 False") %>%
    dplyr::pull(variable)
  add_likert6 <- function(x) {
    val_labels(x) <- c("True" = 1,
                       "False" = 2)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert6, 
                     add_likert6)
}

#check
dat$srs_1

#add value labels likert 1-5 Agree w/ Neutral

{
  likert7 <- dict %>% 
    dplyr::filter (value_labels == "1 Strongly Disagree 2 Disagree 3 Neutral 4 Agree 5 Strongly Agree") %>%
    dplyr::pull(variable)
  add_likert7 <- function(x) {
    val_labels(x) <- c("Strongly Disagree" = 1,
                       "Disagree" = 2,
                       "Neutral" = 3,
                       "Agree" = 4,
                       "Strongly Agree" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert7, 
                     add_likert7)
}

#check
dat$epsi_1

#add value labels likert 1-7 Agree w/ Neither

{
  likert8 <- dict %>% 
    dplyr::filter (value_labels == "1 Strongly Disagree 2 Disagree 3 Slightly Disagree 4 Neither Agree nor Disagree 5 Slightly Agree 6 Agree 7 Strongly Agree") %>%
    dplyr::pull(variable)
  add_likert8 <- function(x) {
    val_labels(x) <- c("Strongly Disagree" = 1,
                       "Disagree" = 2,
                       "Slightly Disagree" = 3,
                       "Neither Agree nor Disagree" = 4,
                       "Slightly Agree" = 5,
                       "Agree" = 6,
                       "Strongly Agree" = 7)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert8, 
                     add_likert8)
}

#check
dat$swls_1

#add value labels likert 1-4 Agree w

{
  likert9 <- dict %>% 
    dplyr::filter (value_labels == "1 Strongly Disagree 2 Disagree 3 Agree 4 Strongly Agree") %>%
    dplyr::pull(variable)
  add_likert9 <- function(x) {
    val_labels(x) <- c("Strongly Disagree" = 1,
                       "Disagree" = 2,
                       "Agree" = 3,
                       "Strongly Agree" = 4)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert9, 
                     add_likert9)
}

#check
dat$meim_1

#add value labels likert 1-5 Agree w/ Neither

{
  likert10 <- dict %>% 
    dplyr::filter (value_labels == "1 Strongly Disagree 2 Disagree 3 Neither Agree nor Disagree 4 Agree 5 Strongly Agree") %>%
    dplyr::pull(variable)
  add_likert10 <- function(x) {
    val_labels(x) <- c("Strongly Disagree" = 1,
                       "Disagree" = 2,
                       "Neither Agree nor Disagree" = 3,
                       "Agree" = 4,
                       "Strongly Agree" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert10, 
                     add_likert10)
}

#check
dat$ssc_1

#add value labels likert 1-4 False/True

{
  likert11 <- dict %>% 
    dplyr::filter (value_labels == "1 Very False or Often False 2 Sometimes or Somewhat False 3 Sometimes or Somewhat True 4 Very True or Often True") %>%
    dplyr::pull(variable)
  add_likert11 <- function(x) {
    val_labels(x) <- c("Very False or Often False" = 1,
                       "Sometimes or Somewhat False" = 2,
                       "Sometimes or Somewhat True" = 3,
                       "Very True or Often True" = 4)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert11, 
                     add_likert11)
}

#check
dat$pidbf_1

# now a bunch of idiosycratic ones

val_labels(dat$socialclass) <-
  c("Low-income/poor" = 1, 
    "Working class/Lower middle class" = 2,
    "Middle class" = 3,
    "Upper middle class" = 4,
    "Upper class" = 5)
dat$socialclass  

val_labels(dat$politics) <-
  c("extremely liberal " = 1, 
    "liberal" = 2,
    "slightly liberal" = 3,
    "moderate, middle of the road" = 4,
    "slightly conservative" = 5,
    "conservative" = 6,
    "extremely conservative" = 7)
dat$politics

val_labels(dat$religion) <-
  c("not at all religious" = 1, 
    "a little religious" = 2,
    "fairly religious" = 3,
    "very religious" = 4)
dat$religion

# save this data file for further use
# saving in three formats, csv, RData, and SPSS
# csv file does not store variable/value labels, other two do

write.csv(dat, "../PSTEM_Wave_2_WORKING_SCORED_2023-03-29.csv", row.names = F)  
saveRDS(dat, "../PSTEM_Wave_2_WORKING_SCORED_2023-03-29.RData")
haven::write_sav(dat, "../PSTEM_Wave_2_WORKING_SCORED_2023-03-29.sav")
