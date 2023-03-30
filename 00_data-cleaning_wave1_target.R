##################################################################
##################################################################
### P-STEM Cleaning Script - Wave 1 Target
### Creating Scale Scores
### Cleaning 2020-02-05, by Moin Syed, Linh Nguyen
##################################################################
##################################################################

# I. META ========================

library(dplyr)
library(psych)
library(labelled)
library(codebook) #reverse scoring
#devtools::install_github("ropenscilabs/gendercoder")
library(gendercoder) #gender coding

# data and dictionary
dat <- readRDS("../PSTEM_Wave_1_WORKING_SCORED_2022-02-03.RData")
colnames(dat)
dict <- read.csv(file = "../PSTEM_Data-Dictionary_Wave_1_Target.csv")

# function to reverse score and !update dictionary!
reverse_scale <- function(data, dictionary, scale_variables){
  
  # pull items for current scale
  scale_items <- dictionary %>%
    dplyr::filter(scale %in% scale_variables) %>%
    pull(variable)
  
  # pull reversed items
  scale_r_items <- dictionary %>% 
    dplyr::filter(variable %in% scale_items) %>%
    dplyr::filter(keying == -1) %>%
    pull(variable)
  
  # duplicate data to create reversed scores
  temp_r <- data %>% 
    dplyr::select(all_of(scale_items))
  
  # rename reversed items to end with R
  temp_r <- temp_r %>%
    dplyr::rename_at(scale_r_items, add_R)
  
  # reverse items in duplicated data frame
  temp_r <- temp_r %>%  
    dplyr::mutate_at(vars(matches("\\dR$")),
                     codebook::reverse_labelled_values)

  # reverse item names in dictionary
  variable <- c(dictionary$variable) # create a list of variables
  y <- data.frame(matrix(ncol = length(variable), nrow = 0)) # empty dataframe
  colnames(y) <- variable # name empty dataframe with variable list
  y <- y %>% # rename variable list so reversed items end with R
    rename_at(scale_r_items, add_R)
  dictionary$variable <- colnames(y) # merge renamed reverse items to dictionary
  dict <<- dictionary
  
  # return duplicated data frame
  return(temp_r)
  
} #END reverse_scale FUNC

# function to score scales using average
scale_score <- function(scale_variables, scale_codes,
                        scale_stem, data, sub = FALSE) {
  
  for (var in seq(scale_variables)){
    cat(paste("\n\n========= Cleaning variable:", 
              scale_variables[var], "=========\n\n"))
    
    # list of items and internal consistency
    if(sub == TRUE){
      items <- dict %>% 
        filter(subscale == scale_variables[var]) %>% 
        pull(variable)
    } else {
      items <- dict %>% 
        filter(scale == scale_variables[var]) %>% 
        pull(variable)
    }
    assign("temp",
           items)
    print(psych::alpha(data[, match(temp, names(data))]))

    # create scale scores and descriptives
    temp <- matrix(round(rowMeans(data[ ,temp], na.rm = TRUE), digits = 3),
                         ncol = 1)
    name <- paste0(scale_stem, "_", scale_codes[var])
    data <- cbind(setNames(data, names(data)), 
                    setNames(data.frame(temp), name))
    print(psych::describe(data[, match(name, names(data))]))

    # label scale scores
    labelled::var_label(
      data[ ,match(name, names(data))]) <- paste(
        toupper(scale_stem), scale_variables[var], "- Mean")
  }
  
  # return dataframe with scored scales attached at the end
  return(data)
}

# II. SCALE SCORES ===============

##> STEM Research and Community Involvement - HOLD ----

##> STEM Self-efficacy ----
stemse_items <- c("stemse_1","stemse_2","stemse_3","stemse_4","stemse_5","stemse_6","stemse_7","stemse_8","stemse_9","stemse_10")

dat <- dat %>% dplyr::mutate(stemse = rowMeans(dat[ ,stemse_items], na.rm = TRUE))

labelled::var_label(dat$stemse) <- "STEM Self-Efficacy - Mean"

psych::describe(dat$stemse)

psych::alpha(dplyr::select(dat, "stemse_1","stemse_2","stemse_3","stemse_4","stemse_5","stemse_6","stemse_7","stemse_8","stemse_9","stemse_10"))

##> STEM Mentoring ----

stemment_sm_items <- c("stemment_1","stemment_2","stemment_3","stemment_4","stemment_5","stemment_6","stemment_7")

dat <- dat %>% dplyr::mutate(stemment_sm = rowMeans(dat[ ,stemment_sm_items], na.rm = TRUE))

labelled::var_label(dat$stemment_sm) <- "STEM Socioemotional Mentoring - Mean"

stemment_im_items <- c("stemment_8","stemment_9","stemment_10","stemment_11","stemment_12","stemment_13")

dat <- dat %>% dplyr::mutate(stemment_im = rowMeans(dat[ ,stemment_im_items], na.rm = TRUE))

labelled::var_label(dat$stemment_im) <- "STEM Instrumental Mentoring - Mean"

psych::describe(dat$stemment_sm)
psych::describe(dat$stemment_im)
psych::corr.test(dat$stemment_sm, dat$stemment_im)

psych::alpha(dplyr::select(dat, "stemment_1","stemment_2","stemment_3","stemment_4","stemment_5","stemment_6","stemment_7"))

psych::alpha(dplyr::select(dat, "stemment_8","stemment_9","stemment_10","stemment_11","stemment_12","stemment_13"))

##> STEM Identity and Commitment ----

stemidco_id_items <- c("stemidco_1","stemidco_2","stemidco_3","stemidco_4","stemidco_5","stemidco_6")

dat <- dat %>% dplyr::mutate(stemidco_id = rowMeans(dat[ ,stemidco_id_items], na.rm = TRUE))

labelled::var_label(dat$stemidco_id) <- "STEM Identity - Mean"

stemidco_emi_items <- c("stemidco_7","stemidco_8")

dat <- dat %>% dplyr::mutate(stemidco_emi = rowMeans(dat[ ,stemidco_emi_items], na.rm = TRUE))

labelled::var_label(dat$stemidco_emi) <- "STEM Ethnicity-Major Integration - Mean"

stemidco_co_items <- c("stemidco_9","stemidco_10","stemidco_11","stemidco_12","stemidco_13","stemidco_14","stemidco_15")

dat <- dat %>% dplyr::mutate(stemidco_co = rowMeans(dat[ ,stemidco_co_items], na.rm = TRUE))

labelled::var_label(dat$stemidco_co) <- "STEM Commitment - Mean"

psych::describe(dat$stemidco_id)
psych::describe(dat$stemidco_emi)
psych::describe(dat$stemidco_co)
psych::corr.test(dat$stemidco_id, dat$stemidco_emi)
psych::corr.test(dat$stemidco_id, dat$stemidco_co)
psych::corr.test(dat$stemidco_emi, dat$stemidco_co)

psych::alpha(dplyr::select(dat, "stemment_1","stemment_2","stemment_3","stemment_4","stemment_5","stemment_6","stemment_7"))

psych::alpha(dplyr::select(dat, "stemment_8","stemment_9","stemment_10","stemment_11","stemment_12","stemment_13"))

##> Science Reasoning Scale ----

dat <- dat %>% mutate(srs_1_scored = recode(srs_1, `1` = 1, `2` = 0),
                      srs_2_scored = recode(srs_2, `1` = 0, `2` = 1),
                      srs_3_scored = recode(srs_3, `1` = 1, `2` = 0),
                      srs_4_scored = recode(srs_4, `1` = 0, `2` = 1),
                      srs_5_scored = recode(srs_5, `1` = 0, `2` = 1),
                      srs_6_scored = recode(srs_6, `1` = 0, `2` = 1),
                      srs_7_scored = recode(srs_7, `1` = 1, `2` = 0),
                      srs_8_scored = recode(srs_8, `1` = 0, `2` = 1),
                      srs_9_scored = recode(srs_9, `1` = 0, `2` = 1),
                      srs_10_scored = recode(srs_10, `1` = 0, `2` = 1),
                      srs_11_scored = recode(srs_11, `1` = 0, `2` = 1))

srs_items <- c("srs_1_scored", "srs_2_scored", "srs_3_scored", "srs_4_scored", "srs_5_scored", "srs_6_scored", "srs_7_scored", "srs_8_scored", "srs_9_scored", "srs_10_scored", "srs_11_scored")

dat <- dat %>% dplyr::mutate(srs = rowMeans(dat[ ,srs_items], na.rm = TRUE))

labelled::var_label(dat$srs) <- "Scientific Reasoning Scale - Mean"

psych::describe(dat$srs)

psych::alpha(dplyr::select(dat, "srs_1_scored", "srs_2_scored", "srs_3_scored", "srs_4_scored", "srs_5_scored", "srs_6_scored", "srs_7_scored", "srs_8_scored", "srs_9_scored", "srs_10_scored", "srs_11_scored"))

##> IQ ----

dat <- dat %>% mutate(iq_1_scored = recode(iq_1, `4` = 1, .default = 0),
                      iq_2_scored = recode(iq_2, `6` = 1, .default = 0),
                      iq_3_scored = recode(iq_3, `4` = 1, .default = 0),
                      iq_4_scored = recode(iq_4, `4` = 1, .default = 0),
                      iq_5_scored = recode(iq_5, `6` = 1, .default = 0),
                      iq_6_scored = recode(iq_6, `3` = 1, .default = 0),
                      iq_7_scored = recode(iq_7, `4` = 1, .default = 0),
                      iq_8_scored = recode(iq_8, `4` = 1, .default = 0),
                      iq_9_scored = recode(iq_9, `5` = 1, .default = 0),
                      iq_10_scored = recode(iq_10, `2` = 1, .default = 0),
                      iq_11_scored = recode(iq_11, `2` = 1, .default = 0),
                      iq_12_scored = recode(iq_12, `4` = 1, .default = 0),
                      iq_13_scored = recode(iq_13, `3` = 1, .default = 0),
                      iq_14_scored = recode(iq_14, `2` = 1, .default = 0),
                      iq_15_scored = recode(iq_15, `6` = 1, .default = 0),
                      iq_16_scored = recode(iq_16, `7` = 1, .default = 0))

iq_items <- c("iq_1_scored", "iq_2_scored", "iq_3_scored", "iq_4_scored", "iq_5_scored", "iq_6_scored", "iq_7_scored", "iq_8_scored", "iq_9_scored", "iq_10_scored", "iq_11_scored", "iq_12_scored", "iq_13_scored", "iq_14_scored", "iq_15_scored", "iq_16_scored")

dat <- dat %>% dplyr::mutate(iq = rowMeans(dat[ ,iq_items], na.rm = TRUE))

labelled::var_label(dat$iq) <- "ICAR Cognitive Abilities - Mean"

psych::describe(dat$iq)

psych::alpha(dplyr::select(dat, "iq_1_scored", "iq_2_scored", "iq_3_scored", "iq_4_scored", "iq_5_scored", "iq_6_scored", "iq_7_scored", "iq_8_scored", "iq_9_scored", "iq_10_scored", "iq_11_scored", "iq_12_scored", "iq_13_scored", "iq_14_scored", "iq_15_scored", "iq_16_scored"))

##> EPSI ----

epsi_coh_items <- c("epsi_2","epsi_4","epsi_5","epsi_6","epsi_8","epsi_9")
epsi_con_items <- c("epsi_1","epsi_3","epsi_7","epsi_10","epsi_11","epsi_12")

dat <- dat %>% dplyr::mutate(epsi_coh = rowMeans(dat[ ,epsi_coh_items], na.rm = TRUE),
                             epsi_con = rowMeans(dat[ ,epsi_con_items], na.rm = TRUE))

labelled::var_label(dat$epsi_coh) <- "EPSI Identity Coherence - Mean"
labelled::var_label(dat$epsi_coh) <- "EPSI Identity Confusion - Mean"

psych::describe(dat$epsi_coh)
psych::describe(dat$epsi_con)
psych::corr.test(dat$epsi_coh, dat$epsi_con)

psych::alpha(dplyr::select(dat, "epsi_2","epsi_4","epsi_5","epsi_6","epsi_8","epsi_9"))
psych::alpha(dplyr::select(dat, "epsi_1","epsi_3","epsi_7","epsi_10","epsi_11","epsi_12"))

##> SWLS ----

swls_items <- c("swls_1","swls_2","swls_3","swls_4","swls_5")

dat <- dat %>% dplyr::mutate(swls = rowMeans(dat[ ,swls_items], na.rm = TRUE))

labelled::var_label(dat$swls) <- "Satisfation with Life - Mean"

psych::describe(dat$swls)

psych::alpha(dplyr::select(dat, "swls_1","swls_2","swls_3","swls_4","swls_5"))

##> MEIM ----

meim_ex_items <- c("meim_1", "meim_2", "meim_4", "meim_8", "meim_10")
meim_co_items <- c("meim_3", "meim_5", "meim_6", "meim_7", "meim_9", "meim_11", "meim_12")

dat <- dat %>% dplyr::mutate(meim_ex = rowMeans(dat[ ,meim_ex_items], na.rm = TRUE),
                             meim_co = rowMeans(dat[ ,meim_co_items], na.rm = TRUE))

labelled::var_label(dat$meim_ex) <- "MEIM Ethnic Identity Exploration - Mean"
labelled::var_label(dat$meim_co) <- "MEIM Ethnic Identity Commitment - Mean"

psych::describe(dat$meim_ex)
psych::describe(dat$meim_co)
psych::corr.test(dat$meim_ex, dat$meim_co)

psych::alpha(dplyr::select(dat, "meim_1", "meim_2", "meim_4", "meim_8", "meim_10"))
psych::alpha(dplyr::select(dat, "meim_3", "meim_5", "meim_6", "meim_7", "meim_9", "meim_11", "meim_12"))

##> Self-concept clarity ----

dat <- dat %>% mutate(ssc_1_r = recode(ssc_1, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_2_r = recode(ssc_2, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_3_r = recode(ssc_3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_4_r = recode(ssc_4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_5_r = recode(ssc_5, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_7_r = recode(ssc_7, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_8_r = recode(ssc_8, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_9_r = recode(ssc_9, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_10_r = recode(ssc_10, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                      ssc_12_r = recode(ssc_12, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1))

psych::corr.test(dat$ssc_1, dat$ssc_1_r)
psych::corr.test(dat$ssc_2, dat$ssc_2_r)
psych::corr.test(dat$ssc_3, dat$ssc_3_r)
psych::corr.test(dat$ssc_4, dat$ssc_4_r)
psych::corr.test(dat$ssc_5, dat$ssc_5_r)
psych::corr.test(dat$ssc_7, dat$ssc_7_r)
psych::corr.test(dat$ssc_8, dat$ssc_8_r)
psych::corr.test(dat$ssc_9, dat$ssc_9_r)
psych::corr.test(dat$ssc_10, dat$ssc_10_r)
psych::corr.test(dat$ssc_12, dat$ssc_12_r)

ssc_items <- c("ssc_1_r", "ssc_2_r", "ssc_3_r", "ssc_4_r", "ssc_5_r", "ssc_6", 
                "ssc_7_r", "ssc_8_r", "ssc_9_r", "ssc_10_r", "ssc_11", "ssc_12_r")

dat <- dat %>% dplyr::mutate(ssc = rowMeans(dat[ ,ssc_items], na.rm = TRUE))

labelled::var_label(dat$ssc) <- "Self-Concept Clarity - Mean"

psych::describe(dat$ssc)

psych::alpha(dplyr::select(dat, "ssc_1_r", "ssc_2_r", "ssc_3_r", "ssc_4_r", 
                           "ssc_5_r", "ssc_6", "ssc_7_r", "ssc_8_r", "ssc_9_r", 
                           "ssc_10_r", "ssc_11", "ssc_12_r"))

##> Big Five Traits ----
bfas_variables <- c("agreeableness", "conscientiousness", "extraversion",
                    "neuroticism", "openness",
                    "compassion", "politeness",
                    "industriousness", "orderliness",
                    "assertiveness", "enthusiasm",
                    "volatility", "withdrawal",
                    "intellect", "openness")
bfas_codes     <- c("a", "c", "e", "n", "o",
                    "ac", "ap", "ci", "co", "ea", "ee", "nv", "nw", "oi", "oo")

# reverse scale
bfas_r <- reverse_scale(data = dat, dictionary = dict, 
                        scale_variables = bfas_variables)

# score scales
bfas_r <- scale_score(data = bfas_r, scale_variables = bfas_variables[1:5],
                      scale_codes = bfas_codes[1:5], scale_stem = "bfas")
bfas_r <- scale_score(data = bfas_r, scale_variables = bfas_variables[6:15],
                      scale_codes = bfas_codes[6:15], scale_stem = "bfas",
                      sub = TRUE)

# merge bfas_r back into master dataset
bfas_r <- bfas_r %>% 
  select(all_of(names(bfas_r)[!names(bfas_r) %in% names(dat)]))
dat <- cbind(dat, bfas_r)

##> PID 5 Traits ----
pid_variables <- c("disinhibition", "detachment", "psychoticism", 
                   "negative", "antagonism")
pid_codes     <- c("dis", "det", "psy", "neg", "ant")

# score scales
dat <- scale_score(data = dat, scale_variables = pid_variables,
                   scale_codes = pid_codes, scale_stem = "pid")

##> Twenty Statements Test - NA, text entry no scale scoring ----

##> UMICS Identity Development - Education Domain ----
umics_variables <- c("education commitment", "education in-depth exploration", 
                     "education reconsideration of commitment")
umics_codes     <- c("comm", "depth", "recon")

# score scales
dat <- scale_score(data = dat, scale_variables = umics_variables,
                   scale_codes = umics_codes, scale_stem = "umics")

##> Interest Inventory ----
ii_variables <- c("realistic", "investigative", "artistic", 
                  "social", "enterprising", "conventional")
ii_codes     <- c("real", "inv", "art", "soc", "ent", "con")

# score scales
dat <- scale_score(data = dat, scale_variables = ii_variables,
                   scale_codes = ii_codes, scale_stem = "ii")

##> Theory of Intelligence ----
toi_variables <- c("growth")
toi_codes     <- c("gr")

# reverse scale
toi_r <- reverse_scale(data = dat, dictionary = dict, 
                       scale_variables = toi_variables)

# score scales
toi_r <- scale_score(data = toi_r, scale_variables = toi_variables,
                      scale_codes = toi_codes, scale_stem = "toi")

# merge toi_r back into master dataset
toi_r <- toi_r %>% 
  select(all_of(names(toi_r)[!names(toi_r) %in% names(dat)]))
dat <- cbind(dat, toi_r)

##> Locus of Control ----
loc_variables <- c("internal")
loc_codes     <- c("int")

# score scales
dat <- scale_score(data = dat, scale_variables = loc_variables,
                   scale_codes = loc_codes, scale_stem = "loc")

##> Educational Public Regard ----
edpr_variables <- c("public regard")
edpr_codes     <- c("reg")

# score scales
dat <- scale_score(data = dat, scale_variables = edpr_variables,
                   scale_codes = edpr_codes, scale_stem = "edpr")

##> Social Support from Friends and Family ----
ss_variables <- c("social support from friends", "social support from family")
ss_codes     <- c("fri", "fam")

# reverse scale
ss_r <- reverse_scale(data = dat, dictionary = dict, 
                      scale_variables = ss_variables)

# score scales
ss_r <- scale_score(data = ss_r, scale_variables = ss_variables,
                    scale_codes = ss_codes, scale_stem = "ss")

# merge ss_r back into master dataset
ss_r <- ss_r %>% 
  select(all_of(names(ss_r)[!names(ss_r) %in% names(dat)]))
dat <- cbind(dat, ss_r)

# III. DEMOGRAPHICS ===============

##> Gender
# use gendercoder
dat <- dat %>% 
  mutate(gender_f = recode_gender(gender, dictionary = fewlevels_en, 
                                  retain_unmatched = FALSE))

# manually fix results
dat[c(64, 68, 78, 86, 113, 171, 179), "gender_f"] <- "woman"
dat[108, "gender_f"] <- "man"
summary(as.factor(dat$gender_f))

##> Race
dat <- dat %>%
  mutate(race_f = 
           ifelse(grepl(pattern = "Caucasian from Middle East", x = race),
                  NA,
           ifelse(grepl(pattern = "mix|half|multi|bi|and |;|Asian, Caucasian", 
                        x = race, ignore.case = TRUE),
                  "mixed",
           ifelse(grepl(pattern = "hispa|latin|mexic", x = race, ignore.case = TRUE),
                  "hispanic",
           ifelse(grepl(pattern = "asian|chinese|viet|indian|filip|afghan|hmong|Bengali|Korean|cambodian|Taiwanese", 
                        x = race, ignore.case = TRUE),
                  "asian",
           ifelse(grepl(pattern = "black|africa|afro|kenyan|oromo|somali|ethio", 
                        x = race, ignore.case = TRUE),
                  "black",
                  NA))))))
summary(as.factor(dat$race_f))

# IV. EXPORT ======================
saveRDS(dat, file = paste0("../PSTEM_Wave_1_SCORED_", Sys.Date(), ".RData"))
rio::export(dat, file = paste0("../PSTEM_Wave_1_SCORED_", Sys.Date(), ".csv"))
rio::export(dat, file = paste0("../PSTEM_Wave_1_SCORED_", Sys.Date(), ".sav"))
