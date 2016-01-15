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

# choose datasource: simulated or real data (manually)
datasource <- "simulated"
# datasource <- "study 4" # 2016-01-14 (between, 21 characters)

# prepare datasets -------------------------------------------------------------

## simulate or load in dataset -----------------------------------------------

if(datasource == "simulated") { # simulate data!
  
  # randomly generate participation variables
  subid <- factor(paste0(rep("s", 420, replace = T), 1:420))
  date <- factor("1/14/2016")
  start_time <- factor("11:30:30 AM")
  end_time <- factor("11:34:50 AM")
  finished <- as.integer(sample(c(0, 1), 420, replace = T))
  mturkcode <- array()
  for(i in 1:420) {mturkcode[i] <- paste0(sample(0:9, 9, replace = T), collapse = "")}
  mturkcode <- factor(mturkcode)
  condition <- factor(sample(c("adult", "bear", "beetle", "bluejay",
                               "car", "child", "chimpanzee", "computer",
                               "dog", "dolphin", "elephant", "fetus",
                               "fish", "frog", "goat", "infant",
                               "microbe", "mouse", "pvs", "robot",
                               "stapler"), 420, replace = T))
  
  # randomly generate demographic variables
  yob <- as.numeric(sample(1940:1997, 420, replace = T))
  gender <- factor(sample(c(1, 2, 0), 420, replace = T))
  race_asian_east <- factor(sample(c(0, 1), 420, replace = T))
  race_asian_south <- factor(sample(c(0, 1), 420, replace = T))
  race_asian_other <- factor(sample(c(0, 1), 420, replace = T))
  race_black <- factor(sample(c(0, 1), 420, replace = T))
  race_hispanic <- factor(sample(c(0, 1), 420, replace = T))
  race_middle_eastern <- factor(sample(c(0, 1), 420, replace = T))
  race_native_american <- factor(sample(c(0, 1), 420, replace = T))
  race_pac_islander <- factor(sample(c(0, 1), 420, replace = T))
  race_white <- factor(sample(c(0, 1), 420, replace = T))
  race_other_prefno <- factor(sample(c(0, 1), 420, replace = T))
  religion_buddhism <- factor(sample(c(0, 1), 420, replace = T))
  religion_christianity <- factor(sample(c(0, 1), 420, replace = T))
  religion_hinduism <- factor(sample(c(0, 1), 420, replace = T))
  religion_islam <- factor(sample(c(0, 1), 420, replace = T))
  religion_jainism <- factor(sample(c(0, 1), 420, replace = T))
  religion_judaism <- factor(sample(c(0, 1), 420, replace = T))
  religion_sikhism <- factor(sample(c(0, 1), 420, replace = T))
  religion_other <- factor(sample(c(0, 1), 420, replace = T))
  religion_none <- factor(sample(c(0, 1), 420, replace = T))
  religion_prefno <- factor(sample(c(0, 1), 420, replace = T))
  education_less <- factor(sample(c(0, 1), 420, replace = T))
  education_high_school <- factor(sample(c(0, 1), 420, replace = T))
  education_some_college <- factor(sample(c(0, 1), 420, replace = T))
  education_associates <- factor(sample(c(0, 1), 420, replace = T))
  education_bachelors <- factor(sample(c(0, 1), 420, replace = T))
  education_some_graduate <- factor(sample(c(0, 1), 420, replace = T))
  education_graduate <- factor(sample(c(0, 1), 420, replace = T))
  education_prefno <- factor(sample(c(0, 1), 420, replace = T))
  feedback <- sample(c("blahblah", "yadayada", ""), 420, replace = T)
  
  # randomly generate responses to test questions
  happy <- as.integer(sample(-3:3, 420, replace = T))
  depressed <- as.integer(sample(-3:3, 420, replace = T))
  fear <- as.integer(sample(-3:3, 420, replace = T))
  angry <- as.integer(sample(-3:3, 420, replace = T))
  calm <- as.integer(sample(-3:3, 420, replace = T))
  sounds <- as.integer(sample(-3:3, 420, replace = T))
  seeing <- as.integer(sample(-3:3, 420, replace = T))
  temperature <- as.integer(sample(-3:3, 420, replace = T))
  odors <- as.integer(sample(-3:3, 420, replace = T))
  depth <- as.integer(sample(-3:3, 420, replace = T))
  computations <- as.integer(sample(-3:3, 420, replace = T))
  thoughts <- as.integer(sample(-3:3, 420, replace = T))
  reasoning <- as.integer(sample(-3:3, 420, replace = T))
  remembering <- as.integer(sample(-3:3, 420, replace = T))
  beliefs <- as.integer(sample(-3:3, 420, replace = T))
  hungry <- as.integer(sample(-3:3, 420, replace = T))
  tired <- as.integer(sample(-3:3, 420, replace = T))
  pain <- as.integer(sample(-3:3, 420, replace = T))
  nauseated <- as.integer(sample(-3:3, 420, replace = T))
  safe <- as.integer(sample(-3:3, 420, replace = T))
  love <- as.integer(sample(-3:3, 420, replace = T))
  recognizing <- as.integer(sample(-3:3, 420, replace = T))
  communicating <- as.integer(sample(-3:3, 420, replace = T))
  guilt <- as.integer(sample(-3:3, 420, replace = T))
  disrespected <- as.integer(sample(-3:3, 420, replace = T))
  free_will <- as.integer(sample(-3:3, 420, replace = T))
  choices <- as.integer(sample(-3:3, 420, replace = T))
  self_restraint <- as.integer(sample(-3:3, 420, replace = T))
  intentions <- as.integer(sample(-3:3, 420, replace = T))
  goal <- as.integer(sample(-3:3, 420, replace = T))
  conscious <- as.integer(sample(-3:3, 420, replace = T))
  self_aware <- as.integer(sample(-3:3, 420, replace = T))
  desires <- as.integer(sample(-3:3, 420, replace = T))
  embarrassed <- as.integer(sample(-3:3, 420, replace = T))
  emo_recog <- as.integer(sample(-3:3, 420, replace = T))
  joy <- as.integer(sample(-3:3, 420, replace = T))
  morality <- as.integer(sample(-3:3, 420, replace = T))
  personality <- as.integer(sample(-3:3, 420, replace = T))
  pleasure <- as.integer(sample(-3:3, 420, replace = T))
  pride <- as.integer(sample(-3:3, 420, replace = T))
  CATCH <- as.integer(sample(-3:3, 420, replace = T))
  
  # bind into one dataframe
  d_sim <- data.frame(subid, date, start_time, end_time, finished, mturkcode, 
                      condition, happy, depressed, fear, angry, calm, sounds, 
                      seeing, temperature, odors, depth, computations, 
                      thoughts, reasoning, remembering, beliefs, hungry, tired, 
                      pain, nauseated, safe, love, recognizing, communicating, 
                      guilt, disrespected, free_will, choices, self_restraint, 
                      intentions, goal, conscious, self_aware, desires, 
                      embarrassed, emo_recog, joy, morality, personality, 
                      pleasure, pride, CATCH, yob, gender, race_asian_east, 
                      race_asian_south, race_asian_other, race_black, 
                      race_hispanic, race_middle_eastern, race_native_american, 
                      race_pac_islander, race_white, race_other_prefno, 
                      religion_buddhism, religion_christianity, 
                      religion_hinduism, religion_islam, religion_jainism, 
                      religion_judaism, religion_sikhism, religion_other, 
                      religion_none, religion_prefno, education_less,
                      education_high_school, education_some_college,
                      education_associates, education_bachelors,
                      education_some_graduate, education_graduate,
                      education_prefno, feedback)
  d <- d_sim
  
} else if(datasource == "study 4") { # load in real data from study 1
  
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v3 (21 conditions between)/GGWmod2_v3_clean.csv")
  d <- d_raw
  
}

## clean up dataset ----------------------------------------------------------

# enact exclusionary criteria
d_clean_1 <- d %>%
  mutate(finished_mod = ifelse(is.na(CATCH), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH == 1, # exclude participants who fail catch trials 
         finished_mod != 0) %>% # exclude participants who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 & 
             as.numeric(as.character(yob)) < 2000, 
           as.numeric(as.character(yob)), NA)), # correct formatting in yob
    age_approx = 2015 - yob_correct) %>% # calculate approximate age
  mutate(gender = factor(gender, levels = c(1, 2, 0), 
                         labels = c("m", "f", "other"))) %>%
  filter(age_approx >= 18) # exclude participants who are younger than 18 years

# recode background and demographic variables
d_clean <- d_clean_1 %>%
  mutate( # deal with study duration
    duration = strptime(end_time, "%I:%M:%S") - strptime(start_time, "%I:%M:%S")) %>%
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
  select(subid:end_time, duration, finished:gender, 
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
  # still need to deal with education ***
  select(subid:gender, feedback:race_cat, religion_cat3) %>%
  rename(religion_cat = religion_cat3)

## prepare datasets for PCA --------------------------------------------------

# separate conditions and remove identifier variables
d_all <- d_clean %>%
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
d_all <- data.frame(d_all[,-1], row.names = d_all[,1])

# examine demographic variables ------------------------------------------------

# sample size
sample_size <- with(d_clean, table(condition))
kable(d_clean %>% count(condition))

# duration
duration <- d_clean %>% 
  group_by(condition) %>%
  summarise(min_duration = min(duration, na.rm = T),
            max_duration = max(duration, na.rm = T),
            median_duration = median(duration, na.rm = T),
            mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration, na.rm = T))
summary(lm(duration ~ condition, data = d_clean)) # test for differences in duration across conditions

# approxiate age
age_approx <- d_clean %>%
  group_by(condition) %>%
  summarise(min_age = min(age_approx, na.rm = T),
            max_age = max(age_approx, na.rm = T),
            median_age = median(age_approx, na.rm = T),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))
summary(lm(age_approx ~ condition, data = d_clean)) # test for differences in age across conditions

# gender
gender <- with(d_clean, table(condition, gender))
kable(addmargins(gender))
summary(gender) # test for difference in gender distribution across conditions

# racial/ethnic background
race_ethn <- with(d_clean, table(condition, race_cat))
kable(addmargins(race_ethn))
summary(race_ethn) # test for difference in race/ethnicity distribution across conditions

# religious background
religion <- with(d_clean, table(condition, religion_cat))
kable(addmargins(religion))
summary(religion) # test for difference in religion distribution across conditions

# need to deal with education ***


