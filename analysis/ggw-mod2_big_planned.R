# set up workspace -------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(devtools)
library(stats)
library(knitr)
library(ggfortify)

# clear workspace
rm(list = ls(all = T))
graphics.off()

# choose datasource: simulated or real data (manually)
# datasource <- "simulated"
datasource <- "study 4" # 2016-01-14 (between, 21 characters)

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
  education <- as.integer(sample(0:8, 420, replace = T))
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
                      education, feedback)
  d <- d_sim
  
} else if(datasource == "study 4") { # load in real data from study 1
  
  d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/v3 (21 conditions between)/GGWmod2_v3_many_characters_clean.csv")
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
         education:age_approx, race_cat3) %>%
  rename(race_cat = race_cat3)

## prepare datasets for PCA --------------------------------------------------

# separate conditions and remove identifier variables
d_all <- d_clean %>%
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
d_all <- data.frame(d_all[,-1], row.names = d_all[,1])

# examine demographic variables ------------------------------------------------

# sample size
sample_size <- with(d_clean, table(condition))
kable(d_clean %>% count(condition))
total_n <- as.numeric(count(d_clean))

# duration
duration <- d_clean %>% 
  # group_by(condition) %>%
  summarise(min_duration = min(duration, na.rm = T),
            max_duration = max(duration, na.rm = T),
            median_duration = median(duration, na.rm = T),
            mean_duration = mean(duration, na.rm = T),
            sd_duration = sd(duration, na.rm = T))
duration
summary(lm(as.numeric(duration) ~ condition, data = d_clean)) # test for differences in duration across conditions

# approxiate age
age_approx <- d_clean %>%
  # group_by(condition) %>%
  summarise(min_age = min(age_approx, na.rm = T),
            max_age = max(age_approx, na.rm = T),
            median_age = median(age_approx, na.rm = T),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))
age_approx
summary(lm(age_approx ~ condition, data = d_clean)) # test for differences in age across conditions

# gender
table(d_clean$gender)
# gender <- with(d_clean, table(condition, gender))
# kable(addmargins(gender))
summary(gender) # test for difference in gender distribution across conditions

# racial/ethnic background
table(d_clean$race_cat)
# race_ethn <- with(d_clean, table(condition, race_cat))
# kable(addmargins(race_ethn))
summary(race_ethn) # test for difference in race/ethnicity distribution across conditions

# education
education <- d_clean %>%
  # group_by(condition) %>%
  summarise(min_edu = min(education, na.rm = T),
            max_edu = max(education, na.rm = T),
            median_edu = median(education, na.rm = T),
            mean_edu = mean(education, na.rm = T),
            sd_edu = sd(education, na.rm = T))
education
summary(lm(education ~ condition, data = d_clean)) # test for differences in age across conditions

# PCA: ALL condition --------------------------------------------------------

## step 1: determine how many dimensions to extract --------------------------

# use "very simple structure" criterion
# VSS(d_all, n = 39, rotate = "none") # unrotated
# VSS(d_all, n = 39, rotate = "varimax") # rotated
VSS.scree(d_all) # scree plot

# run unrotated pca with maximum number of dimensions
pca_all_unrotated <- principal(d_all, nfactors = 39, rotate = "none")
pca_all_unrotated
pca_all_unrotated$values[pca_all_unrotated$values > 1] # examine eignenvalues > 1

# run roated pca with maximum number of dimensions
pca_all_rotated <- principal(d_all, nfactors = 39, rotate = "varimax")
pca_all_rotated
pca_all_rotated$values[pca_all_rotated$values > 1] # examine eignenvalues > 1

# set number of dimensions to extract (manually)
nfactors_all <- 4

## step 2: run pca without rotation with N dimensions ------------------------

# run unrotated pca with n dimensions
pca_all_unrotatedN <- principal(d_all, nfactors = nfactors_all, rotate = "none")
pca_all_unrotatedN

# plot mental capacities in first two dimensions
pca_all_unrotatedN_loadings <- 
  data.frame(pca_all_unrotatedN$loadings[1:40, 1:nfactors_all],
             row.names = rownames(pca_all_unrotatedN$loadings[1:40, 1:nfactors_all]))

# code a priori mental capacity categories
pca_all_unrotatedN_loadings[c("hungry", "tired", "pain", 
                                 "nauseated", "safe"),
                               "mc_cat"] <- "biological"
pca_all_unrotatedN_loadings[c("happy", "depressed", "fear", 
                                 "angry", "calm", "joy"),
                               "mc_cat"] <- "affective"
pca_all_unrotatedN_loadings[c("sounds", "seeing", "temperature", 
                                 "odors", "depth"),
                               "mc_cat"] <- "perceptual"
pca_all_unrotatedN_loadings[c("computations", "thoughts", "reasoning", 
                                 "remembering", "beliefs"),
                               "mc_cat"] <- "cognitive"
pca_all_unrotatedN_loadings[c("free_will", "choices", "self_restraint", 
                                 "intentions", "goal"),
                               "mc_cat"] <- "autonomous"
pca_all_unrotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                                 "disrespected", "embarrassed", "emo_recog"),
                               "mc_cat"] <- "social"
pca_all_unrotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                                 "desires", "morality", "personality", "pride"),
                               "mc_cat"] <- "other"

pca_all_unrotatedN_loadings$mc_cat <- factor(pca_all_unrotatedN_loadings$mc_cat)

# pca_all_unrotatedN_plot12 <- 
#   ggplot(pca_all_unrotatedN_loadings,
#          aes(x = PC1, y = PC2,
#              label = rownames(pca_all_unrotatedN_loadings),
#              color = mc_cat)) +
#   geom_text(hjust = 0.5, vjust = 0.5) +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_color_brewer(type = "qual", palette = 2) +
#   labs(title = "ALL: factor loadings (first 2 unrotated components)\n",
#        x = "\nPrincipal Component 1",
#        y = "Principal Component 2\n")
# pca_all_unrotatedN_plot12

# examine loadings
mc_all_unrotatedN = rownames(pca_all_unrotatedN_loadings)

# ... for PC1
pca_all_unrotatedN_pc1 <- pca_all_unrotatedN_loadings %>%
  mutate(mc = mc_all_unrotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_all_unrotatedN_pc1

# ... for PC2
pca_all_unrotatedN_pc2 <- pca_all_unrotatedN_loadings %>%
  mutate(mc = mc_all_unrotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_all_unrotatedN_pc2

# ... for PC3
if(nfactors_all > 2) {
  pca_all_unrotatedN_pc3 <- pca_all_unrotatedN_loadings %>%
    mutate(mc = mc_all_unrotatedN) %>%
    arrange(desc(PC3)) %>%
    select(PC3, mc, mc_cat)
  pca_all_unrotatedN_pc3
}

# ... for PC4
if(nfactors_all > 3) {
  pca_all_unrotatedN_pc4 <- pca_all_unrotatedN_loadings %>%
    mutate(mc = mc_all_unrotatedN) %>%
    arrange(desc(PC4)) %>%
    select(PC4, mc, mc_cat)
  pca_all_unrotatedN_pc4
}

## step 3: run pca with varimax rotation with N dimensions -------------------

# run pca with n dimensions with varimax rotation
pca_all_rotatedN <- principal(d_all, nfactors = nfactors_all, 
                                 rotate = "varimax")
pca_all_rotatedN

# plot mental capacities in first two dimensions
pca_all_rotatedN_loadings <- 
  data.frame(pca_all_rotatedN$loadings[1:40, 1:nfactors_all],
             row.names = rownames(pca_all_rotatedN$loadings[1:40, 1:nfactors_all]))

# code a priori mental capacity categories
pca_all_rotatedN_loadings[c("hungry", "tired", "pain", 
                               "nauseated", "safe"),
                             "mc_cat"] <- "biological"
pca_all_rotatedN_loadings[c("happy", "depressed", "fear", 
                               "angry", "calm", "joy"),
                             "mc_cat"] <- "affective"
pca_all_rotatedN_loadings[c("sounds", "seeing", "temperature", 
                               "odors", "depth"),
                             "mc_cat"] <- "perceptual"
pca_all_rotatedN_loadings[c("computations", "thoughts", "reasoning", 
                               "remembering", "beliefs"),
                             "mc_cat"] <- "cognitive"
pca_all_rotatedN_loadings[c("free_will", "choices", "self_restraint", 
                               "intentions", "goal"),
                             "mc_cat"] <- "autonomous"
pca_all_rotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                               "disrespected", "embarrassed", "emo_recog"),
                             "mc_cat"] <- "social"
pca_all_rotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                               "desires", "morality", "personality", "pride"),
                             "mc_cat"] <- "other"

pca_all_rotatedN_loadings$mc_cat <- factor(pca_all_rotatedN_loadings$mc_cat)

# pca_all_rotatedN_plot12 <- 
#   ggplot(pca_all_rotatedN_loadings,
#          aes(x = PC1, y = PC2,
#              label = rownames(pca_all_rotatedN_loadings),
#              color = mc_cat)) +
#   geom_text(hjust = 0.5, vjust = 0.5) +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_color_brewer(type = "qual", palette = 2) +
#   labs(title = "ALL: factor loadings (first 2 rotated components)\n",
#        x = "\nPrincipal Component 1",
#        y = "Principal Component 2\n")
# pca_all_rotatedN_plot12

# examine loadings
mc_all_rotatedN = rownames(pca_all_rotatedN_loadings)

# ... for PC1
pca_all_rotatedN_pc1 <- pca_all_rotatedN_loadings %>%
  mutate(mc = mc_all_rotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_all_rotatedN_pc1

# ... for PC2
pca_all_rotatedN_pc2 <- pca_all_rotatedN_loadings %>%
  mutate(mc = mc_all_rotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_all_rotatedN_pc2

# ... for PC3
if(nfactors_all > 2) {
  pca_all_rotatedN_pc3 <- pca_all_rotatedN_loadings %>%
    mutate(mc = mc_all_rotatedN) %>%
    arrange(desc(PC3)) %>%
    select(PC3, mc, mc_cat)
  pca_all_rotatedN_pc3
}

# ... for PC4
if(nfactors_all > 3) {
  pca_all_rotatedN_pc4 <- pca_all_rotatedN_loadings %>%
    mutate(mc = mc_all_rotatedN) %>%
    arrange(desc(PC4)) %>%
    select(PC4, mc, mc_cat)
  pca_all_rotatedN_pc4
}

# PCA: ALL condition, plot participants by component scores -----------------

# get condition by subject
condition_subid <- d_clean %>%
  select(subid, condition)

pca_all_rotatedN_scores <- pca_all_rotatedN$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  left_join(condition_subid, by = "subid") %>%
  gather(dimension, score, starts_with("PC"))

if(nfactors_all == 3) {
  pca_all_rotatedN_scores <- pca_all_rotatedN_scores %>%
    mutate(dimension = factor(dimension,
                              levels = c("PC1", "PC2", "PC3"),
                              labels = c("PC1 (ALL)", "PC2 (ALL)", 
                                         "PC3 (ALL)")))
} else if(nfactors_all == 4) {
  pca_all_rotatedN_scores <- pca_all_rotatedN_scores %>%
    mutate(dimension = factor(dimension,
                              levels = c("PC1", "PC2", "PC3", "PC4"),
                              labels = c("PC1 (ALL)", "PC2 (ALL)", 
                                         "PC3 (ALL)", "PC4 (ALL)")))
}

p_all_rotatedN_scores <- ggplot(aes(x = dimension, y = score, 
                                     color = dimension),
                                 data = pca_all_rotatedN_scores) +
  facet_wrap(~ condition, ncol = 7) +
  geom_hline(y = 0, lty = 3) +
  geom_jitter(size = 2) +
  geom_boxplot(width = 0.5, color = "black", alpha = 0,
               outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "ALL Scores (from rotated PCA)\n",
       x = "\nDimension",
       y = "Score\n",
       color = "Dimension\n")
p_all_rotatedN_scores

# EXPLORATORY: hand-calculate "scores" on dimensions for each condition? ---------

d_long <- d_clean %>%
  gather(mc, rating, happy:pride) %>%
  mutate(mc_cat = 
           factor(
             ifelse(mc %in% 
                      c("hungry", "tired", "pain", "nauseated", "safe"),
                    "biological",
                    ifelse(mc %in% 
                             c("happy", "depressed", "fear", "angry", 
                               "calm", "joy"),
                           "affective",
                           ifelse(mc %in% 
                                    c("sounds", "seeing", "temperature",
                                      "odors", "depth"),
                                  "perceptual",
                                  ifelse(mc %in% 
                                           c("computations", "thoughts", 
                                             "reasoning", "remembering", 
                                             "beliefs"),
                                         "cognitive",
                                         ifelse(mc %in% 
                                                  c("free_will", "choices", 
                                                    "self_restraint",
                                                    "intentions", "goal"),
                                                "autonomous",
                                                ifelse(mc %in% c("love", 
                                                                 "recognizing", 
                                                                 "communicating", 
                                                                 "guilt",
                                                                 "disrespected", 
                                                                 "embarrassed", 
                                                                 "emo_recog"),
                                                       "social",
                                                       "other"))))))))

# summarize ratings by condition and mc
rating_sum <- d_long %>%
  group_by(condition, mc) %>%
  summarise(min = min(rating),
            max = max(rating),
            median = median(rating),
            mean = mean(rating),
            sd = sd(rating))
rating_sum

# extract factor loadings
all_PC <- cbind(mc = rownames(pca_all_rotatedN_loadings),
                 pca_all_rotatedN_loadings)

# combine!
d_scoring_pre <- full_join(rating_sum, all_PC)

if(nfactors_all == 4) {
  # score!
  d_scored <- d_scoring_pre %>%
    mutate(PC1_score = PC1 * mean,
           PC2_score = PC2 * mean,
           PC3_score = PC3 * mean,
           PC4_score = PC4 * mean) %>%
    group_by(condition) %>%
    summarise(PC1_sum = sum(PC1_score),
              PC2_sum = sum(PC2_score),
              PC3_sum = sum(PC3_score),
              PC4_sum = sum(PC4_score)) %>%
    data.frame()
  d_scored
  
  # plot scores
  
  d_scored_long <- d_scored %>%
    gather(PC, score, -condition) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum", "PC3_sum", "PC4_sum"),
                              labels = c("PC1", "PC2", "PC3", "PC4")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = condition, fill = condition), 
                     data = d_scored_long) +
    facet_wrap(~ condition, ncol = 7) +
    geom_bar(stat = "identity", width = 0.8,
             position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
         x = "\nDimension",
         y = "Score\n")
  p_scored
} else if(nfactors_all == 3) {
  
  # score!
  d_scored <- d_scoring_pre %>%
    mutate(PC1_score = PC1 * mean,
           PC2_score = PC2 * mean,
           PC3_score = PC3 * mean) %>%
    group_by(condition) %>%
    summarise(PC1_sum = sum(PC1_score),
              PC2_sum = sum(PC2_score),
              PC3_sum = sum(PC3_score)) %>%
    data.frame()
  d_scored
  
  # plot scores
  
  d_scored_long <- d_scored %>%
    gather(PC, score, -condition) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum", "PC3_sum"),
                              labels = c("PC1", "PC2", "PC3")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = condition, fill = condition), 
                     data = d_scored_long) +
    facet_wrap(~ condition, ncol = 7) +
    geom_bar(stat = "identity", width = 0.8,
             position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
         x = "\nDimension",
         y = "Score\n")
  p_scored
}

# EXPLORATORY: hierarchical cluster analysis -----------------------------------

dist_all <- dist(d_all)
hclust_all <- hclust(dist_all)
ggdendrogram(hclust_all)
autoplot(clara(d_all, k = 3), frame = TRUE, frame.type = 'norm')
autoplot(fanny(d_all, k = 3), frame = TRUE, frame.type = 'norm')


