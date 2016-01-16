# set up workspace -------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(devtools)
library(stats)
library(knitr)

# clear workspace
rm(list = ls(all = T))
graphics.off()

# choose datasource (manually)
# datasource <- "study 1" # 2015-12-15 (between)
# datasource <- "study 2" # 2016-01-12 (between rep)
# datasource <- "studies 1 & 2" # combine
# datasource <- "study 3" # 2016-01-10 (within)
datasource <- "study 4" # 2016-01-14 (between, 21 characters)

# prepare datasets -------------------------------------------------------------

## load in dataset --------------------------------------------------------

if(datasource == "study 1") {
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v1 (2 conditions between)/GGWmod2_v1_clean.csv")
  d <- d_raw %>%
    mutate(study = "study 1")
  rm(d_raw)
} 

if(datasource == "study 2") {
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v1 (replication)/GGWmod2_v1_replication_clean.csv")
  d <- d_raw %>%
    mutate(study = "study 2")
  rm(d_raw)
} 

if(datasource == "studies 1 & 2") {
  d_raw1 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v1 (2 conditions between)/GGWmod2_v1_clean.csv")
  d_raw2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v1 (replication)/GGWmod2_v1_replication_clean.csv")
  d1 <- d_raw1 %>%
    mutate(yob = as.integer(as.character(yob)),
           study = "study 1")
  d2 <- d_raw2 %>%
    mutate(yob = as.integer(as.character(yob)),
           study = "study 2")
  d <- full_join(d1, d2)
  rm(d_raw1, d_raw2, d1, d2)
}

if(datasource == "study 3") {
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v2 (2 conditions within)/GGWmod2_v2_withinsubjects_clean.csv")
  d <- d_raw %>%
    mutate(study = "study 3")
  rm(d_raw)
}

if(datasource == "study 4") {
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v3 (21 conditions between)/GGWmod2_v3_many_characters_clean.csv")
  d <- d_raw %>%
    mutate(study = "study 4")
  rm(d_raw)
}

## clean up dataset -------------------------------------------------------

if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  # enact exclusionary criteria
  d_clean_1 <- d %>%
    mutate(finished_mod = ifelse(is.na(CATCH), 0,
                                 ifelse(finished == 1, 1,
                                        0.5))) %>%
    filter(CATCH == 1, # exclude Ps who fail catch trials 
           finished_mod != 0) %>% # exclude Ps who did not complete task
    mutate(yob_correct = as.numeric(
      ifelse(as.numeric(as.character(yob)) > 1900 & 
               as.numeric(as.character(yob)) < 2000, 
             as.numeric(as.character(yob)), NA)), # correct formatting in yob
      age_approx = 2015 - yob_correct) %>% # calculate approximate age
    mutate(gender = factor(gender, levels = c(1, 2, 0), 
                           labels = c("m", "f", "other"))) %>%
    filter(age_approx >= 18) # exclude Ps who are younger than 18 years
  
  # recode background and demographic variables
  d_clean <- d_clean_1 %>%
    mutate( # deal with study number
      study = factor(study)) %>%
    mutate( # deal with study duration
      duration = strptime(end_time, "%I:%M:%S") - 
        strptime(start_time, "%I:%M:%S")) %>%
    mutate( # deal with race
      race_asian_east = 
        factor(ifelse(is.na(race_asian_east), "", "asian_east ")),
      race_asian_south = 
        factor(ifelse(is.na(race_asian_south), "", "asian_south ")),
      race_asian_other = 
        factor(ifelse(is.na(race_asian_other), "", "asian_other ")),
      race_black = 
        factor(ifelse(is.na(race_black), "", "black ")),
      race_hispanic = 
        factor(ifelse(is.na(race_hispanic), "", "hispanic ")),
      race_middle_eastern = 
        factor(ifelse(is.na(race_middle_eastern), "", "middle_eastern ")),
      race_native_american = 
        factor(ifelse(is.na(race_native_american), "", "native_american ")),
      race_pac_islander = 
        factor(ifelse(is.na(race_pac_islander), "", "pac_islander ")),
      race_white = 
        factor(ifelse(is.na(race_white), "", "white ")),
      race_other_prefno = 
        factor(ifelse(is.na(race_other_prefno), "", "other_prefno ")),
      race_cat = paste0(race_asian_east, race_asian_south, race_asian_other,
                        race_black, race_hispanic, race_middle_eastern,
                        race_native_american, race_pac_islander, race_white,
                        race_other_prefno),
      race_cat2 = factor(sub(" +$", "", race_cat)),
      race_cat3 = factor(ifelse(grepl(" ", race_cat2) == T, "multiracial",
                                as.character(race_cat2)))) %>%
    select(study, subid:end_time, duration, finished:gender, 
           religion_buddhism:age_approx, race_cat3) %>%
    rename(race_cat = race_cat3) %>%
    mutate( # deal with religion
      religion_buddhism = 
        factor(ifelse(is.na(religion_buddhism), "", "buddhism ")),
      religion_christianity = 
        factor(ifelse(is.na(religion_christianity), "", "christianity ")),
      religion_hinduism = 
        factor(ifelse(is.na(religion_hinduism), "", "hinduism ")),
      religion_islam = 
        factor(ifelse(is.na(religion_islam), "", "islam ")),
      religion_jainism = 
        factor(ifelse(is.na(religion_jainism), "", "jainism ")),
      religion_judaism = 
        factor(ifelse(is.na(religion_judaism), "", "judaism ")),
      religion_sikhism = 
        factor(ifelse(is.na(religion_sikhism), "", "sikhism ")),
      religion_other = 
        factor(ifelse(is.na(religion_other), "", "other ")),
      religion_none = 
        factor(ifelse(is.na(religion_none), "", "none ")),
      religion_prefno = 
        factor(ifelse(is.na(religion_prefno), "", "other_prefno ")),
      religion_cat = paste0(religion_buddhism, religion_christianity, 
                            religion_hinduism, religion_islam, religion_jainism,
                            religion_judaism, religion_sikhism, religion_other, 
                            religion_none, religion_prefno),
      religion_cat2 = factor(sub(" +$", "", religion_cat)),
      religion_cat3 = factor(ifelse(grepl(" ", religion_cat2) == T, 
                                    "multireligious",
                                    as.character(religion_cat2)))) %>%
    select(study:gender, feedback:race_cat, religion_cat3) %>%
    rename(religion_cat = religion_cat3)
  
  # remove extraneous dfs and variables
  rm(d_clean_1)
}

if(datasource == "study 3") {
  # enact exclusionary criteria
  d_clean_1 <- d %>%
    mutate(finished_mod = ifelse(is.na(CATCH..characterL) | 
                                   is.na(CATCH..characterR), 0,
                                 ifelse(finished == 1, 1,
                                        0.5))) %>%
    filter(CATCH..characterL == 5, # exclude Ps who fail catch trials 
           CATCH..characterR == 5,
           finished_mod != 0) %>% # exclude Ps who did not complete task
    mutate(yob_correct = as.numeric(
      ifelse(as.numeric(as.character(yob)) > 1900 & 
               as.numeric(as.character(yob)) < 2000, 
             as.numeric(as.character(yob)), NA)), # correct formatting in yob
      age_approx = 2015 - yob_correct) %>% # calculate approximate age
    mutate(gender = factor(gender, levels = c(1, 2, 0), 
                           labels = c("m", "f", "other"))) %>%
    filter(age_approx >= 18) # exclude Ps who are younger than 18 years
  
  # recode background and demographic variables
  d_clean_2 <- d_clean_1 %>%
    mutate( # deal with study number
      study = factor(study)) %>%
    mutate( # deal with study duration
      duration = strptime(end_time, "%I:%M:%S") - 
        strptime(start_time, "%I:%M:%S")) %>%
    mutate( # deal with race
      race_asian_east = 
        factor(ifelse(is.na(race_asian_east), "", "asian_east ")),
      race_asian_south = 
        factor(ifelse(is.na(race_asian_south), "", "asian_south ")),
      race_asian_other = 
        factor(ifelse(is.na(race_asian_other), "", "asian_other ")),
      race_black = 
        factor(ifelse(is.na(race_black), "", "black ")),
      race_hispanic = 
        factor(ifelse(is.na(race_hispanic), "", "hispanic ")),
      race_middle_eastern = 
        factor(ifelse(is.na(race_middle_eastern), "", "middle_eastern ")),
      race_native_american = 
        factor(ifelse(is.na(race_native_american), "", "native_american ")),
      race_pac_islander = 
        factor(ifelse(is.na(race_pac_islander), "", "pac_islander ")),
      race_white = 
        factor(ifelse(is.na(race_white), "", "white ")),
      race_other_prefno = 
        factor(ifelse(is.na(race_other_prefno), "", "other_prefno ")),
      race_cat = paste0(race_asian_east, race_asian_south, race_asian_other,
                        race_black, race_hispanic, race_middle_eastern,
                        race_native_american, race_pac_islander, race_white,
                        race_other_prefno),
      race_cat2 = factor(sub(" +$", "", race_cat)),
      race_cat3 = factor(ifelse(grepl(" ", race_cat2) == T, "multiracial",
                                as.character(race_cat2)))) %>%
    select(study, subid:end_time, duration, finished:gender, 
           religion_buddhism:age_approx, race_cat3) %>%
    rename(race_cat = race_cat3) %>%
    mutate( # deal with religion
      religion_buddhism = 
        factor(ifelse(is.na(religion_buddhism), "", "buddhism ")),
      religion_christianity = 
        factor(ifelse(is.na(religion_christianity), "", "christianity ")),
      religion_hinduism = 
        factor(ifelse(is.na(religion_hinduism), "", "hinduism ")),
      religion_islam = 
        factor(ifelse(is.na(religion_islam), "", "islam ")),
      religion_jainism = 
        factor(ifelse(is.na(religion_jainism), "", "jainism ")),
      religion_judaism = 
        factor(ifelse(is.na(religion_judaism), "", "judaism ")),
      religion_sikhism = 
        factor(ifelse(is.na(religion_sikhism), "", "sikhism ")),
      religion_other = 
        factor(ifelse(is.na(religion_other), "", "other ")),
      religion_none = 
        factor(ifelse(is.na(religion_none), "", "none ")),
      religion_prefno = 
        factor(ifelse(is.na(religion_prefno), "", "other_prefno ")),
      religion_cat = paste0(religion_buddhism, religion_christianity, 
                            religion_hinduism, religion_islam, religion_jainism,
                            religion_judaism, religion_sikhism, religion_other, 
                            religion_none, religion_prefno),
      religion_cat2 = factor(sub(" +$", "", religion_cat)),
      religion_cat3 = factor(ifelse(grepl(" ", religion_cat2) == T, 
                                    "multireligious",
                                    as.character(religion_cat2)))) %>%
    select(study:gender, feedback:race_cat, religion_cat3) %>%
    rename(religion_cat = religion_cat3)
  
  # rename response variables
  d_clean_3 <- d_clean_2
  names(d_clean_3) <- gsub("get", "", names(d_clean_3))
  names(d_clean_3) <- gsub("\\.", "", names(d_clean_3))
  names(d_clean_3) <- gsub("char", "_char", names(d_clean_3))
  names(d_clean_3)[names(d_clean_3) %in% c("_characterL", "_characterR")] <- c("characterL", "characterR")
  
  # recode response variables (center)
  d_clean_4 <- d_clean_3
  for(i in 11:92) {
    d_clean_4[,i] <- d_clean_4[,i] - 4 # transform from 1 to 7 --> -3 to 3
  }
  
  # recode characterL vs. characterR as beetle vs. robot
  d_clean_5_demo <- d_clean_4 %>%
    select(study:condition, yob:religion_cat)
  
  d_clean_5_characterL <- d_clean_4 %>%
    mutate(target = characterL) %>%
    select(study, subid, target, happy_characterL:CATCH_characterL)
  names(d_clean_5_characterL) <- gsub("_characterL", "", names(d_clean_5_characterL))
  
  d_clean_5_characterR <- d_clean_4 %>%
    mutate(target = characterR) %>%
    select(study, subid, target, happy_characterR:CATCH_characterR)
  names(d_clean_5_characterR) <- gsub("_characterR", "", names(d_clean_5_characterR))
  
  d_clean <- d_clean_5_characterL %>%
    full_join(d_clean_5_characterR) %>%
    full_join(d_clean_5_demo) %>%
    select(study, subid, date:religion_cat, target:CATCH)
  
  # remove extraneous dfs and variables
  rm(d_clean_1, d_clean_2, d_clean_3, d_clean_4, d_clean_5_characterL, 
     d_clean_5_characterR, d_clean_5_demo, i)
}

if(datasource == "study 4") {
  # enact exclusionary criteria
  d_clean_1 <- d %>%
    mutate(finished_mod = ifelse(is.na(CATCH), 0,
                                 ifelse(finished == 1, 1,
                                        0.5))) %>%
    filter(CATCH == 1, # exclude Ps who fail catch trials 
           finished_mod != 0) %>% # exclude Ps who did not complete task
    mutate(yob_correct = as.numeric(
      ifelse(as.numeric(as.character(yob)) > 1900 & 
               as.numeric(as.character(yob)) < 2000, 
             as.numeric(as.character(yob)), NA)), # correct formatting in yob
      age_approx = 2015 - yob_correct) %>% # calculate approximate age
    mutate(gender = factor(gender, levels = c(1, 2, 0), 
                           labels = c("m", "f", "other"))) %>%
    filter(age_approx >= 18) # exclude Ps who are younger than 18 years
  
  # recode one character
  d_clean_2 <- d_clean_1 %>%
    mutate(condition = factor(ifelse(
      grepl("vegetative", as.character(condition)), "pvs",
      ifelse(grepl("blue", as.character(condition)), "bluejay",
             as.character(condition)))))
  
  # recode background and demographic variables
  d_clean <- d_clean_2 %>%
    mutate( # deal with study number
      study = factor(study)) %>%
    mutate( # deal with study duration
      duration = strptime(end_time, "%I:%M:%S") - 
        strptime(start_time, "%I:%M:%S")) %>%
    mutate( # deal with race
      race_asian_east = 
        factor(ifelse(is.na(race_asian_east), "", "asian_east ")),
      race_asian_south = 
        factor(ifelse(is.na(race_asian_south), "", "asian_south ")),
      race_asian_other = 
        factor(ifelse(is.na(race_asian_other), "", "asian_other ")),
      race_black = 
        factor(ifelse(is.na(race_black), "", "black ")),
      race_hispanic = 
        factor(ifelse(is.na(race_hispanic), "", "hispanic ")),
      race_middle_eastern = 
        factor(ifelse(is.na(race_middle_eastern), "", "middle_eastern ")),
      race_native_american = 
        factor(ifelse(is.na(race_native_american), "", "native_american ")),
      race_pac_islander = 
        factor(ifelse(is.na(race_pac_islander), "", "pac_islander ")),
      race_white = 
        factor(ifelse(is.na(race_white), "", "white ")),
      race_other_prefno = 
        factor(ifelse(is.na(race_other_prefno), "", "other_prefno ")),
      race_cat = paste0(race_asian_east, race_asian_south, race_asian_other,
                        race_black, race_hispanic, race_middle_eastern,
                        race_native_american, race_pac_islander, race_white,
                        race_other_prefno),
      race_cat2 = factor(sub(" +$", "", race_cat)),
      race_cat3 = factor(ifelse(grepl(" ", race_cat2) == T, "multiracial",
                                as.character(race_cat2)))) %>%
    select(study, subid:end_time, duration, finished:gender, 
           education:age_approx, race_cat3) %>%
    rename(race_cat = race_cat3)
  
  # remove extraneous dfs and variables
  rm(d_clean_1, d_clean_2)
}

## prepare datasets for dimension reduction analyses ----------------------

if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  # beetle condition
  d_beetle <- d_clean %>%
    filter(condition == "beetle") %>% # filter by condition
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
  d_beetle <- data.frame(d_beetle[,-1], row.names = d_beetle[,1])
  
  # robot condition
  d_robot <- d_clean %>%
    filter(condition == "robot") %>% # filter by condition
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3!
  d_robot <- data.frame(d_robot[,-1], row.names = d_robot[,1])
  
  # collapse across conditions
  d_both <- d_clean %>%
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
  d_both <- data.frame(d_both[,-1], row.names = d_both[,1])
}

if(datasource == "study 3") {
  # beetle target
  d_beetle <- d_clean %>%
    filter(target == "beetle") %>% # filter by target
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
  d_beetle <- data.frame(d_beetle[,-1], row.names = d_beetle[,1])
  
  # robot target
  d_robot <- d_clean %>%
    filter(target == "robot") %>% # filter by target
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3!
  d_robot <- data.frame(d_robot[,-1], row.names = d_robot[,1])
  
  # collapse across targets
  d_both <- d_clean %>%
    mutate(subid = paste(target, subid, sep = "_")) %>%
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
  d_both <- data.frame(d_both[,-1], row.names = d_both[,1])
}

if(datasource == "study 4") {
  # collapse across conditions
  d_all <- d_clean %>%
    select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
  d_all <- data.frame(d_all[,-1], row.names = d_all[,1])
}

# examine demographic variables ------------------------------------------------

# sample size
sample_size <- with(d_clean %>% select(condition, subid) %>% unique(), 
                    table(condition))
kable(d_clean %>% select(condition, subid) %>% unique() %>% count(condition))

# duration
duration <- d_clean %>% 
  group_by(condition) %>%
  summarise(min_duration = min(duration, na.rm = T),
            max_duration = max(duration, na.rm = T),
            median_duration = median(duration, na.rm = T),
            mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration, na.rm = T))

if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  # test for differences in duration across conditions
  duration_diff <- t.test(duration ~ condition,
                          data = d_clean %>% 
                            select(condition, subid, duration) %>%
                            unique()) 
}

# approxiate age
age_approx <- d_clean %>%
  group_by(condition) %>%
  summarise(min_age = min(age_approx, na.rm = T),
            max_age = max(age_approx, na.rm = T),
            median_age = median(age_approx, na.rm = T),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))

if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  # test for differences in age across conditions
  age_diff <- t.test(age_approx ~ condition,
                     data = d_clean %>%
                       select(condition, subid, duration) %>%
                       unique()) 
}

# gender
if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  gender <- with(d_clean %>% select(subid, condition, gender) %>% unique(), 
                 table(condition, gender))
  kable(addmargins(gender))
  summary(gender) # test for difference in gender distribution across conditions
} else {
  gender <- with(d_clean %>% select(subid, gender) %>% unique(),
                 table(gender))
  gender_diff <- chisq.test(gender)
}

# racial/ethnic background
if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  race <- with(d_clean %>% select(subid, condition, race_cat) %>% unique(), 
                 table(condition, race_cat))
  kable(addmargins(race))
  summary(race) # test for difference in race distribution across conditions
} else {
  race <- with(d_clean %>% select(subid, race_cat) %>% unique(),
                 table(race_cat))
  race_diff <- chisq.test(race)
}

# racial/ethnic background
if(datasource %in% c("study 1", "study 2", "studies 1 & 2")) {
  religion <- with(d_clean %>% select(subid, condition, religion_cat) %>% unique(), 
               table(condition, religion_cat))
  kable(addmargins(religion))
  summary(religion) # test for difference in religion distribution across conditions
} else if(datasource == "study 3") {
  religion <- with(d_clean %>% select(subid, religion_cat) %>% unique(),
               table(religion_cat))
  religion_diff <- chisq.test(religion)
}

# STILL NEED TO DEAL WITH EDUCATION FOR STUDY 4


