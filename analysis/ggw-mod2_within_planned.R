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
# datasource <- "simulated"
datasource <- "real"

# prepare datasets -------------------------------------------------------------

## simulate or load in dataset -----------------------------------------------

if(datasource == "simulated") { # simulate data!
  
  # randomly generate participation variables
  subid <- factor(paste0(rep("s", 800, replace = T), 1:800))
  date <- factor("1/1/2016")
  start_time <- factor("11:30:30 AM")
  end_time <- factor("11:34:50 AM")
  finished <- as.integer(sample(c(0, 1), 800, replace = T))
  mturkcode <- array()
  for(i in 1:800) {mturkcode[i] <- paste0(sample(0:9, 9, replace = T), collapse = "")}
  mturkcode <- factor(mturkcode)
  condition <- factor(sample(c("beetle_robot", "robot_beetle"), 800, replace = T))
  characterL <- factor(sample(c("beetle", "robot"), 800, replace = T))

  # randomly generate demographic variables
  yob <- as.numeric(sample(1940:1997, 800, replace = T))
  gender <- factor(sample(c(1, 2, 0), 800, replace = T))
  race_asian_east <- factor(sample(c(0, 1), 800, replace = T))
  race_asian_south <- factor(sample(c(0, 1), 800, replace = T))
  race_asian_other <- factor(sample(c(0, 1), 800, replace = T))
  race_black <- factor(sample(c(0, 1), 800, replace = T))
  race_hispanic <- factor(sample(c(0, 1), 800, replace = T))
  race_middle_eastern <- factor(sample(c(0, 1), 800, replace = T))
  race_native_american <- factor(sample(c(0, 1), 800, replace = T))
  race_pac_islander <- factor(sample(c(0, 1), 800, replace = T))
  race_white <- factor(sample(c(0, 1), 800, replace = T))
  race_other_prefno <- factor(sample(c(0, 1), 800, replace = T))
  religion_buddhism <- factor(sample(c(0, 1), 800, replace = T))
  religion_christianity <- factor(sample(c(0, 1), 800, replace = T))
  religion_hinduism <- factor(sample(c(0, 1), 800, replace = T))
  religion_islam <- factor(sample(c(0, 1), 800, replace = T))
  religion_jainism <- factor(sample(c(0, 1), 800, replace = T))
  religion_judaism <- factor(sample(c(0, 1), 800, replace = T))
  religion_sikhism <- factor(sample(c(0, 1), 800, replace = T))
  religion_other <- factor(sample(c(0, 1), 800, replace = T))
  religion_none <- factor(sample(c(0, 1), 800, replace = T))
  religion_prefno <- factor(sample(c(0, 1), 800, replace = T))
  feedback <- sample(c("blahblah", "yadayada", ""), 800, replace = T)
  
  # randomly generate responses to test questions (characterL)
  "happy: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "depressed: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "fear: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "angry: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "calm: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "sounds: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "seeing: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "temperature: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "odors: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "depth: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "computations: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "thoughts: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "reasoning: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "remembering: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "beliefs: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "hungry: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "tired: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "pain: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "nauseated: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "safe: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "love: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "recognizing: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "communicating: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "guilt: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "disrespected: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "free_will: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "choices: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "self_restraint: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "intentions: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "goal: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "conscious: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "self_aware: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "desires: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "embarrassed: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "emo_recog: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "joy: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "morality: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "personality: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "pleasure: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "pride: characterL" <- as.integer(sample(0:6, 800, replace = T))
  "CATCH: characterL" <- as.integer(sample(0:6, 800, replace = T))
  
  # randomly generate responses to test questions (characterR)
  "happy: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "depressed: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "fear: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "angry: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "calm: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "sounds: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "seeing: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "temperature: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "odors: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "depth: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "computations: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "thoughts: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "reasoning: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "remembering: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "beliefs: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "hungry: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "tired: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "pain: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "nauseated: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "safe: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "love: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "recognizing: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "communicating: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "guilt: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "disrespected: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "free_will: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "choices: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "self_restraint: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "intentions: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "goal: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "conscious: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "self_aware: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "desires: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "embarrassed: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "emo_recog: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "joy: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "morality: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "personality: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "pleasure: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "pride: characterR" <- as.integer(sample(0:6, 800, replace = T))
  "CATCH: characterR" <- as.integer(sample(0:6, 800, replace = T))
  
  # bind into one dataframe
  d_sim <- data.frame(subid, date, start_time, end_time, finished, mturkcode, 
                      condition, characterL, 
                      get("happy: characterL"), get("depressed: characterL"), 
                      get("fear: characterL"), get("angry: characterL"),
                      get("calm: characterL"), get("sounds: characterL"), 
                      get("seeing: characterL"), get("temperature: characterL"), 
                      get("odors: characterL"), get("depth: characterL"), 
                      get("computations: characterL"), get("thoughts: characterL"), 
                      get("reasoning: characterL"), get("remembering: characterL"), 
                      get("beliefs: characterL"), get("hungry: characterL"), 
                      get("tired: characterL"), get("pain: characterL"), 
                      get("nauseated: characterL"), get("safe: characterL"), 
                      get("love: characterL"), get("recognizing: characterL"), 
                      get("communicating: characterL"), get("guilt: characterL"), 
                      get("disrespected: characterL"), get("free_will: characterL"), 
                      get("choices: characterL"), get("self_restraint: characterL"), 
                      get("intentions: characterL"), get("goal: characterL"), 
                      get("conscious: characterL"), get("self_aware: characterL"), 
                      get("desires: characterL"), get("embarrassed: characterL"), 
                      get("emo_recog: characterL"), get("joy: characterL"), 
                      get("morality: characterL"), get("personality: characterL"), 
                      get("pleasure: characterL"), get("pride: characterL"), 
                      get("CATCH: characterL"), 
                      get("happy: characterR"), get("depressed: characterR"), 
                      get("fear: characterR"), get("angry: characterR"),
                      get("calm: characterR"), get("sounds: characterR"), 
                      get("seeing: characterR"), get("temperature: characterR"), 
                      get("odors: characterR"), get("depth: characterR"), 
                      get("computations: characterR"), get("thoughts: characterR"), 
                      get("reasoning: characterR"), get("remembering: characterR"), 
                      get("beliefs: characterR"), get("hungry: characterR"), 
                      get("tired: characterR"), get("pain: characterR"), 
                      get("nauseated: characterR"), get("safe: characterR"), 
                      get("love: characterR"), get("recognizing: characterR"), 
                      get("communicating: characterR"), get("guilt: characterR"), 
                      get("disrespected: characterR"), get("free_will: characterR"), 
                      get("choices: characterR"), get("self_restraint: characterR"), 
                      get("intentions: characterR"), get("goal: characterR"), 
                      get("conscious: characterR"), get("self_aware: characterR"), 
                      get("desires: characterR"), get("embarrassed: characterR"), 
                      get("emo_recog: characterR"), get("joy: characterR"), 
                      get("morality: characterR"), get("personality: characterR"), 
                      get("pleasure: characterR"), get("pride: characterR"), 
                      get("CATCH: characterR"), 
                      yob, gender,
                      race_asian_east, race_asian_south, race_asian_other, race_black, 
                      race_hispanic, race_middle_eastern, race_native_american, 
                      race_pac_islander, race_white, race_other_prefno, 
                      religion_buddhism, religion_christianity, 
                      religion_hinduism, religion_islam, religion_jainism, 
                      religion_judaism, religion_sikhism, religion_other, 
                      religion_none, religion_prefno, feedback) %>%
    mutate(characterR = 
             factor(ifelse(characterL == "beetle", "robot",
                           ifelse(characterL == "robot", "beetle", NA))))
  d <- d_sim
  
} else if(datasource == "real") { # load in real data
  
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v2 (2 conditions within)/GGWmod2_v2_withinsubjects_clean.csv")
  d <- d_raw
  
}

## clean up dataset ----------------------------------------------------------

# enact exclusionary criteria
d_clean_1 <- d %>%
  mutate(finished_mod = ifelse(is.na(CATCH..characterL) | 
                                 is.na(CATCH..characterR), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH..characterL == 5, # exclude participants who fail catch trials 
         CATCH..characterR == 5,
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
d_clean_2 <- d_clean_1 %>%
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
  select(subid:gender, feedback:race_cat, religion_cat3) %>%
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
  select(subid:condition, yob:religion_cat)

d_clean_5_characterL <- d_clean_4 %>%
  mutate(target = characterL) %>%
  select(subid, target, happy_characterL:CATCH_characterL)
names(d_clean_5_characterL) <- gsub("_characterL", "", names(d_clean_5_characterL))

d_clean_5_characterR <- d_clean_4 %>%
  mutate(target = characterR) %>%
  select(subid, target, happy_characterR:CATCH_characterR)
names(d_clean_5_characterR) <- gsub("_characterR", "", names(d_clean_5_characterR))

d_clean <- d_clean_5_characterL %>%
  full_join(d_clean_5_characterR) %>%
  full_join(d_clean_5_demo) %>%
  select(subid, date:religion_cat, target:CATCH)

## prepare datasets for PCA --------------------------------------------------

# separate conditions and remove identifier variables
d_beetle <- d_clean %>%
  filter(target == "beetle") %>% # filter by character
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
d_beetle <- data.frame(d_beetle[,-1], row.names = d_beetle[,1])

d_robot <- d_clean %>%
  filter(target == "robot") %>% # filter by condition
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3!
d_robot <- data.frame(d_robot[,-1], row.names = d_robot[,1])

# examine demographic variables ------------------------------------------------

# sample size
sample_size <- with(d_clean %>% select(condition, subid) %>% unique(), table(condition))
kable(d_clean %>% select(condition, subid) %>% unique() %>% count(condition))

# duration
duration <- d_clean %>% 
  group_by(condition) %>%
  summarise(min_duration = min(duration, na.rm = T),
            max_duration = max(duration, na.rm = T),
            median_duration = median(duration, na.rm = T),
            mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration, na.rm = T))
t.test(duration ~ condition, 
       data = d_clean %>% select(condition, subid, duration) %>% unique()) # test for differences in duration across conditions

# approxiate age
age_approx <- d_clean %>%
  group_by(condition) %>%
  summarise(min_age = min(age_approx, na.rm = T),
            max_age = max(age_approx, na.rm = T),
            median_age = median(age_approx, na.rm = T),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))
t.test(age_approx ~ condition, 
       data = d_clean %>% select(condition, subid, age_approx) %>% unique()) # test for differences in age across conditions

# gender
gender <- with(d_clean %>% select(subid, condition, gender) %>% unique(), 
               table(condition, gender))
kable(addmargins(gender))
summary(gender) # test for difference in gender distribution across conditions

# racial/ethnic background
race_ethn <- with(d_clean %>% select(subid, condition, race_cat) %>% unique(), 
                  table(condition, race_cat))
kable(addmargins(race_ethn))
summary(race_ethn) # test for difference in race/ethnicity distribution across conditions

# religious background
religion <- with(d_clean %>% select(subid, condition, religion_cat) %>% unique(), 
                 table(condition, religion_cat))
kable(addmargins(religion))
summary(religion) # test for difference in religion distribution across conditions

# PCA: BEETLE condition --------------------------------------------------------

## step 1: determine how many dimensions to extract --------------------------

# use "very simple structure" criterion
# VSS(d_beetle, n = 39)
VSS.scree(d_beetle)

# run unrotated pca with maximum number of dimensions
pca_beetle_unrotated <- principal(d_beetle, nfactors = 39, rotate = "none")
pca_beetle_unrotated
pca_beetle_unrotated$values # examine eignenvalues, consider retaining iff > 1.00

# set number of dimensions to extract (manually)
nfactors_beetle <- 3

## step 2: run pca without rotation with N dimensions ------------------------

# run unrotated pca with n dimensions
pca_beetle_unrotatedN <- principal(d_beetle, nfactors = nfactors_beetle, rotate = "none")
pca_beetle_unrotatedN

# plot mental capacities in first two dimensions
pca_beetle_unrotatedN_loadings <- 
  data.frame(pca_beetle_unrotatedN$loadings[1:40, 1:nfactors_beetle],
             row.names = rownames(pca_beetle_unrotatedN$loadings[1:40, 1:nfactors_beetle]))

# code a priori mental capacity categories
pca_beetle_unrotatedN_loadings[c("hungry", "tired", "pain", 
                                 "nauseated", "safe"),
                               "mc_cat"] <- "biological"
pca_beetle_unrotatedN_loadings[c("happy", "depressed", "fear", 
                                 "angry", "calm", "joy"),
                               "mc_cat"] <- "affective"
pca_beetle_unrotatedN_loadings[c("sounds", "seeing", "temperature", 
                                 "odors", "depth"),
                               "mc_cat"] <- "perceptual"
pca_beetle_unrotatedN_loadings[c("computations", "thoughts", "reasoning", 
                                 "remembering", "beliefs"),
                               "mc_cat"] <- "cognitive"
pca_beetle_unrotatedN_loadings[c("free_will", "choices", "self_restraint", 
                                 "intentions", "goal"),
                               "mc_cat"] <- "autonomous"
pca_beetle_unrotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                                 "disrespected", "embarrassed", "emo_recog"),
                               "mc_cat"] <- "social"
pca_beetle_unrotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                                 "desires", "morality", "personality", "pride"),
                               "mc_cat"] <- "other"

pca_beetle_unrotatedN_loadings$mc_cat <- factor(pca_beetle_unrotatedN_loadings$mc_cat)

pca_beetle_unrotatedN_plot12 <- 
  ggplot(pca_beetle_unrotatedN_loadings,
         aes(x = PC1, y = PC2,
             label = rownames(pca_beetle_unrotatedN_loadings),
             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BEETLE: factor loadings (first 2 unrotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_beetle_unrotatedN_plot12

# examine loadings
mc_beetle_unrotatedN = rownames(pca_beetle_unrotatedN_loadings)

# ... for PC1
pca_beetle_unrotatedN_pc1 <- pca_beetle_unrotatedN_loadings %>%
  mutate(mc = mc_beetle_unrotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_beetle_unrotatedN_pc1

# ... for PC2
pca_beetle_unrotatedN_pc2 <- pca_beetle_unrotatedN_loadings %>%
  mutate(mc = mc_beetle_unrotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_beetle_unrotatedN_pc2

# ... for PC3
pca_beetle_unrotatedN_pc3 <- pca_beetle_unrotatedN_loadings %>%
  mutate(mc = mc_beetle_unrotatedN) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_beetle_unrotatedN_pc3

## step 3: run pca with varimax rotation with N dimensions -------------------

# run pca with n dimensions with varimax rotation
pca_beetle_rotatedN <- principal(d_beetle, nfactors = nfactors_beetle, 
                                 rotate = "varimax")
pca_beetle_rotatedN

# plot mental capacities in first two dimensions
pca_beetle_rotatedN_loadings <- 
  data.frame(pca_beetle_rotatedN$loadings[1:40, 1:nfactors_beetle],
             row.names = rownames(pca_beetle_rotatedN$loadings[1:40, 1:nfactors_beetle]))

# code a priori mental capacity categories
pca_beetle_rotatedN_loadings[c("hungry", "tired", "pain", 
                               "nauseated", "safe"),
                             "mc_cat"] <- "biological"
pca_beetle_rotatedN_loadings[c("happy", "depressed", "fear", 
                               "angry", "calm", "joy"),
                             "mc_cat"] <- "affective"
pca_beetle_rotatedN_loadings[c("sounds", "seeing", "temperature", 
                               "odors", "depth"),
                             "mc_cat"] <- "perceptual"
pca_beetle_rotatedN_loadings[c("computations", "thoughts", "reasoning", 
                               "remembering", "beliefs"),
                             "mc_cat"] <- "cognitive"
pca_beetle_rotatedN_loadings[c("free_will", "choices", "self_restraint", 
                               "intentions", "goal"),
                             "mc_cat"] <- "autonomous"
pca_beetle_rotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                               "disrespected", "embarrassed", "emo_recog"),
                             "mc_cat"] <- "social"
pca_beetle_rotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                               "desires", "morality", "personality", "pride"),
                             "mc_cat"] <- "other"

pca_beetle_rotatedN_loadings$mc_cat <- factor(pca_beetle_rotatedN_loadings$mc_cat)

pca_beetle_rotatedN_plot12 <- 
  ggplot(pca_beetle_rotatedN_loadings,
         aes(x = PC1, y = PC2,
             label = rownames(pca_beetle_rotatedN_loadings),
             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BEETLE: factor loadings (first 2 rotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_beetle_rotatedN_plot12

# examine loadings
mc_beetle_rotatedN = rownames(pca_beetle_rotatedN_loadings)

# ... for PC1
pca_beetle_rotatedN_pc1 <- pca_beetle_rotatedN_loadings %>%
  mutate(mc = mc_beetle_rotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_beetle_rotatedN_pc1

# ... for PC2
pca_beetle_rotatedN_pc2 <- pca_beetle_rotatedN_loadings %>%
  mutate(mc = mc_beetle_rotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_beetle_rotatedN_pc2

# ... for PC3
pca_beetle_rotatedN_pc3 <- pca_beetle_rotatedN_loadings %>%
  mutate(mc = mc_beetle_rotatedN) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_beetle_rotatedN_pc3

# PCA: ROBOT condition ---------------------------------------------------------

## step 1: determine how many dimensions to extract --------------------------

# use "very simple structure" criterion
# VSS(d_robot, n = 39)
VSS.scree(d_robot)

# run unrotated pca with maximum number of dimensions
pca_robot_unrotated <- principal(d_robot, nfactors = 39, rotate = "none")
pca_robot_unrotated
pca_robot_unrotated$values # examine eignenvalues, consider retaining iff > 1.00

# set number of dimensions to extract (manually)
nfactors_robot <- 3

## step 2: run pca without rotation with N dimensions ------------------------

# run unrotated pca with n dimensions
pca_robot_unrotatedN <- principal(d_robot, nfactors = nfactors_robot, rotate = "none")
pca_robot_unrotatedN

# plot mental capacities in first two dimensions
pca_robot_unrotatedN_loadings <- 
  data.frame(pca_robot_unrotatedN$loadings[1:40, 1:nfactors_robot],
             row.names = rownames(pca_robot_unrotatedN$loadings[1:40, 1:nfactors_robot]))

# code a priori mental capacity categories
pca_robot_unrotatedN_loadings[c("hungry", "tired", "pain", 
                                "nauseated", "safe"),
                              "mc_cat"] <- "biological"
pca_robot_unrotatedN_loadings[c("happy", "depressed", "fear", 
                                "angry", "calm", "joy"),
                              "mc_cat"] <- "affective"
pca_robot_unrotatedN_loadings[c("sounds", "seeing", "temperature", 
                                "odors", "depth"),
                              "mc_cat"] <- "perceptual"
pca_robot_unrotatedN_loadings[c("computations", "thoughts", "reasoning", 
                                "remembering", "beliefs"),
                              "mc_cat"] <- "cognitive"
pca_robot_unrotatedN_loadings[c("free_will", "choices", "self_restraint", 
                                "intentions", "goal"),
                              "mc_cat"] <- "autonomous"
pca_robot_unrotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                                "disrespected", "embarrassed", "emo_recog"),
                              "mc_cat"] <- "social"
pca_robot_unrotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                                "desires", "morality", "personality", "pride"),
                              "mc_cat"] <- "other"

pca_robot_unrotatedN_loadings$mc_cat <- factor(pca_robot_unrotatedN_loadings$mc_cat)

pca_robot_unrotatedN_plot12 <- 
  ggplot(pca_robot_unrotatedN_loadings,
         aes(x = PC1, y = PC2,
             label = rownames(pca_robot_unrotatedN_loadings),
             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "ROBOT: factor loadings (first 2 unrotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_robot_unrotatedN_plot12

# examine loadings
mc_robot_unrotatedN = rownames(pca_robot_unrotatedN_loadings)

# ... for PC1
pca_robot_unrotatedN_pc1 <- pca_robot_unrotatedN_loadings %>%
  mutate(mc = mc_robot_unrotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_robot_unrotatedN_pc1

# ... for PC2
pca_robot_unrotatedN_pc2 <- pca_robot_unrotatedN_loadings %>%
  mutate(mc = mc_robot_unrotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_robot_unrotatedN_pc2

# ... for PC3
pca_robot_unrotatedN_pc3 <- pca_robot_unrotatedN_loadings %>%
  mutate(mc = mc_robot_unrotatedN) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_robot_unrotatedN_pc3

## step 3: run pca with varimax rotation with N dimensions -------------------

# run pca with n dimensions with varimax rotation
pca_robot_rotatedN <- principal(d_robot, nfactors = nfactors_robot, 
                                rotate = "varimax")
pca_robot_rotatedN

# plot mental capacities in first two dimensions
pca_robot_rotatedN_loadings <- 
  data.frame(pca_robot_rotatedN$loadings[1:40, 1:nfactors_robot],
             row.names = rownames(pca_robot_rotatedN$loadings[1:40, 1:nfactors_robot]))

# code a priori mental capacity categories
pca_robot_rotatedN_loadings[c("hungry", "tired", "pain", 
                              "nauseated", "safe"),
                            "mc_cat"] <- "biological"
pca_robot_rotatedN_loadings[c("happy", "depressed", "fear", 
                              "angry", "calm", "joy"),
                            "mc_cat"] <- "affective"
pca_robot_rotatedN_loadings[c("sounds", "seeing", "temperature", 
                              "odors", "depth"),
                            "mc_cat"] <- "perceptual"
pca_robot_rotatedN_loadings[c("computations", "thoughts", "reasoning", 
                              "remembering", "beliefs"),
                            "mc_cat"] <- "cognitive"
pca_robot_rotatedN_loadings[c("free_will", "choices", "self_restraint", 
                              "intentions", "goal"),
                            "mc_cat"] <- "autonomous"
pca_robot_rotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                              "disrespected", "embarrassed", "emo_recog"),
                            "mc_cat"] <- "social"
pca_robot_rotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                              "desires", "morality", "personality", "pride"),
                            "mc_cat"] <- "other"

pca_robot_rotatedN_loadings$mc_cat <- factor(pca_robot_rotatedN_loadings$mc_cat)

pca_robot_rotatedN_plot12 <- 
  ggplot(pca_robot_rotatedN_loadings,
         aes(x = PC1, y = PC2,
             label = rownames(pca_robot_rotatedN_loadings),
             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "ROBOT: factor loadings (first 2 rotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_robot_rotatedN_plot12

# examine loadings
mc_robot_rotatedN = rownames(pca_robot_rotatedN_loadings)

# ... for PC1
pca_robot_rotatedN_pc1 <- pca_robot_rotatedN_loadings %>%
  mutate(mc = mc_robot_rotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_robot_rotatedN_pc1

# ... for PC2
pca_robot_rotatedN_pc2 <- pca_robot_rotatedN_loadings %>%
  mutate(mc = mc_robot_rotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_robot_rotatedN_pc2

# ... for PC3
pca_robot_rotatedN_pc3 <- pca_robot_rotatedN_loadings %>%
  mutate(mc = mc_robot_rotatedN) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_robot_rotatedN_pc3

# compare dimensions across conditions -----------------------------------------

## look for common factors in UNROTATED solutions ----------------------------

# get top 10 factor loadings by conditions and dimension (absolute value)
beetle_unrotatedN_top10_pc1 <- pca_beetle_unrotatedN_pc1 %>%
  mutate(abs_PC1 = abs(PC1),
         valence = factor(ifelse(PC1 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC1)

beetle_unrotatedN_top10_pc2 <- pca_beetle_unrotatedN_pc2 %>%
  mutate(abs_PC2 = abs(PC2),
         valence = factor(ifelse(PC2 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC2)

beetle_unrotatedN_top10_pc3 <- pca_beetle_unrotatedN_pc3 %>%
  mutate(abs_PC3 = abs(PC3),
         valence = factor(ifelse(PC3 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC3)

robot_unrotatedN_top10_pc1 <- pca_robot_unrotatedN_pc1 %>%
  mutate(abs_PC1 = abs(PC1),
         valence = factor(ifelse(PC1 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC1)

robot_unrotatedN_top10_pc2 <- pca_robot_unrotatedN_pc2 %>%
  mutate(abs_PC2 = abs(PC2),
         valence = factor(ifelse(PC2 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC2)

robot_unrotatedN_top10_pc3 <- pca_robot_unrotatedN_pc3 %>%
  mutate(abs_PC3 = abs(PC3),
         valence = factor(ifelse(PC3 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC3)

# compare all possible combinations (across conditions)
match_unrotated_beetlePC1_robotPC1 <-
  beetle_unrotatedN_top10_pc1 %>%
  mutate(comparison = "beetlePC1_robotPC1") %>%
  filter(beetle_unrotatedN_top10_pc1$mc %in% robot_unrotatedN_top10_pc1$mc)

match_unrotated_beetlePC1_robotPC2 <-
  beetle_unrotatedN_top10_pc1 %>%
  mutate(comparison = "beetlePC1_robotPC2") %>%
  filter(beetle_unrotatedN_top10_pc1$mc %in% robot_unrotatedN_top10_pc2$mc)

match_unrotated_beetlePC1_robotPC3 <-
  beetle_unrotatedN_top10_pc1 %>%
  mutate(comparison = "beetlePC1_robotPC3") %>%
  filter(beetle_unrotatedN_top10_pc1$mc %in% robot_unrotatedN_top10_pc3$mc)

match_unrotated_beetlePC2_robotPC1 <-
  beetle_unrotatedN_top10_pc2 %>%
  mutate(comparison = "beetlePC2_robotPC1") %>%
  filter(beetle_unrotatedN_top10_pc2$mc %in% robot_unrotatedN_top10_pc1$mc)

match_unrotated_beetlePC2_robotPC2 <-
  beetle_unrotatedN_top10_pc2 %>%
  mutate(comparison = "beetlePC2_robotPC2") %>%
  filter(beetle_unrotatedN_top10_pc2$mc %in% robot_unrotatedN_top10_pc2$mc)

match_unrotated_beetlePC2_robotPC3 <-
  beetle_unrotatedN_top10_pc2 %>%
  mutate(comparison = "beetlePC2_robotPC3") %>%
  filter(beetle_unrotatedN_top10_pc2$mc %in% robot_unrotatedN_top10_pc3$mc)

match_unrotated_beetlePC3_robotPC1 <-
  beetle_unrotatedN_top10_pc3 %>%
  mutate(comparison = "beetlePC3_robotPC1") %>%
  filter(beetle_unrotatedN_top10_pc3$mc %in% robot_unrotatedN_top10_pc1$mc)

match_unrotated_beetlePC3_robotPC2 <-
  beetle_unrotatedN_top10_pc3 %>%
  mutate(comparison = "beetlePC3_robotPC2") %>%
  filter(beetle_unrotatedN_top10_pc3$mc %in% robot_unrotatedN_top10_pc2$mc)

match_unrotated_beetlePC3_robotPC3 <-
  beetle_unrotatedN_top10_pc3 %>%
  mutate(comparison = "beetlePC3_robotPC3") %>%
  filter(beetle_unrotatedN_top10_pc3$mc %in% robot_unrotatedN_top10_pc3$mc)

match_unrotated <- match_unrotated_beetlePC1_robotPC1 %>%
  full_join(match_unrotated_beetlePC1_robotPC2) %>%
  full_join(match_unrotated_beetlePC1_robotPC3) %>%
  full_join(match_unrotated_beetlePC2_robotPC1) %>%
  full_join(match_unrotated_beetlePC2_robotPC2) %>%
  full_join(match_unrotated_beetlePC2_robotPC3) %>%
  full_join(match_unrotated_beetlePC3_robotPC1) %>%
  full_join(match_unrotated_beetlePC3_robotPC2) %>%
  full_join(match_unrotated_beetlePC3_robotPC3)

top_match_unrotated <- match_unrotated %>%
  count(comparison) %>%
  arrange(desc(n)) %>%
  mutate(rotation = "unrotated") %>%
  left_join(match_unrotated) %>%
  select(comparison, n, rotation, mc)

kable(top_match_unrotated)

## look for common factors in ROTATED solutions ------------------------------

# get top 10 factor loadings by conditions and dimension (absolute value)
beetle_rotatedN_top10_pc1 <- pca_beetle_rotatedN_pc1 %>%
  mutate(abs_PC1 = abs(PC1),
         valence = factor(ifelse(PC1 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC1)

beetle_rotatedN_top10_pc2 <- pca_beetle_rotatedN_pc2 %>%
  mutate(abs_PC2 = abs(PC2),
         valence = factor(ifelse(PC2 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC2)

beetle_rotatedN_top10_pc3 <- pca_beetle_rotatedN_pc3 %>%
  mutate(abs_PC3 = abs(PC3),
         valence = factor(ifelse(PC3 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC3)

robot_rotatedN_top10_pc1 <- pca_robot_rotatedN_pc1 %>%
  mutate(abs_PC1 = abs(PC1),
         valence = factor(ifelse(PC1 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC1)

robot_rotatedN_top10_pc2 <- pca_robot_rotatedN_pc2 %>%
  mutate(abs_PC2 = abs(PC2),
         valence = factor(ifelse(PC2 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC2)

robot_rotatedN_top10_pc3 <- pca_robot_rotatedN_pc3 %>%
  mutate(abs_PC3 = abs(PC3),
         valence = factor(ifelse(PC3 < 0, "neg", "pos"))) %>%
  top_n(10, abs_PC3)

# compare all possible combinations (across conditions)
match_rotated_beetlePC1_robotPC1 <-
  beetle_rotatedN_top10_pc1 %>%
  mutate(comparison = "beetlePC1_robotPC1") %>%
  filter(beetle_rotatedN_top10_pc1$mc %in% robot_rotatedN_top10_pc1$mc)

match_rotated_beetlePC1_robotPC2 <-
  beetle_rotatedN_top10_pc1 %>%
  mutate(comparison = "beetlePC1_robotPC2") %>%
  filter(beetle_rotatedN_top10_pc1$mc %in% robot_rotatedN_top10_pc2$mc)

match_rotated_beetlePC1_robotPC3 <-
  beetle_rotatedN_top10_pc1 %>%
  mutate(comparison = "beetlePC1_robotPC3") %>%
  filter(beetle_rotatedN_top10_pc1$mc %in% robot_rotatedN_top10_pc3$mc)

match_rotated_beetlePC2_robotPC1 <-
  beetle_rotatedN_top10_pc2 %>%
  mutate(comparison = "beetlePC2_robotPC1") %>%
  filter(beetle_rotatedN_top10_pc2$mc %in% robot_rotatedN_top10_pc1$mc)

match_rotated_beetlePC2_robotPC2 <-
  beetle_rotatedN_top10_pc2 %>%
  mutate(comparison = "beetlePC2_robotPC2") %>%
  filter(beetle_rotatedN_top10_pc2$mc %in% robot_rotatedN_top10_pc2$mc)

match_rotated_beetlePC2_robotPC3 <-
  beetle_rotatedN_top10_pc2 %>%
  mutate(comparison = "beetlePC2_robotPC3") %>%
  filter(beetle_rotatedN_top10_pc2$mc %in% robot_rotatedN_top10_pc3$mc)

match_rotated_beetlePC3_robotPC1 <-
  beetle_rotatedN_top10_pc3 %>%
  mutate(comparison = "beetlePC3_robotPC1") %>%
  filter(beetle_rotatedN_top10_pc3$mc %in% robot_rotatedN_top10_pc1$mc)

match_rotated_beetlePC3_robotPC2 <-
  beetle_rotatedN_top10_pc3 %>%
  mutate(comparison = "beetlePC3_robotPC2") %>%
  filter(beetle_rotatedN_top10_pc3$mc %in% robot_rotatedN_top10_pc2$mc)

match_rotated_beetlePC3_robotPC3 <-
  beetle_rotatedN_top10_pc3 %>%
  mutate(comparison = "beetlePC3_robotPC3") %>%
  filter(beetle_rotatedN_top10_pc3$mc %in% robot_rotatedN_top10_pc3$mc)

match_rotated <- match_rotated_beetlePC1_robotPC1 %>%
  full_join(match_rotated_beetlePC1_robotPC2) %>%
  full_join(match_rotated_beetlePC1_robotPC3) %>%
  full_join(match_rotated_beetlePC2_robotPC1) %>%
  full_join(match_rotated_beetlePC2_robotPC2) %>%
  full_join(match_rotated_beetlePC2_robotPC3) %>%
  full_join(match_rotated_beetlePC3_robotPC1) %>%
  full_join(match_rotated_beetlePC3_robotPC2) %>%
  full_join(match_rotated_beetlePC3_robotPC3)

top_match_rotated <- match_rotated %>%
  count(comparison) %>%
  arrange(desc(n)) %>%
  mutate(rotation = "rotated") %>%
  left_join(match_rotated) %>%
  select(comparison, n, rotation, mc)

kable(top_match_rotated)
