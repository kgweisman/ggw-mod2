# load libraries --------------------------------------------------------------
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

# upload dataset ------------------------------------------------------------

d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/GGWmod2_v1_clean.csv")

# clean up dataset ------------------------------------------------------------

d_clean_1 <- d_raw %>%
  mutate(finished_mod = ifelse(is.na(CATCH), 0,
                               ifelse(finished == 1, 1,
                                      0.5))) %>%
  filter(CATCH == 1, # exclude participants who fail catch trials 
         finished_mod != 0) %>% # exclude participants who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 & 
             as.numeric(as.character(yob)) < 2000, 
           as.numeric(as.character(yob)), NA)), # correct formatting issues in yob
         age_approx = 2015 - yob_correct) %>% # calculate approximate age
  mutate(gender = factor(gender, levels = c(1, 2, 0), labels = c("m", "f", "other"))) %>%
  filter(age_approx >= 18) # exclude participants who are younger than 18 years old

# recode demographic variables
d_clean <- d_clean_1 %>%
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
  select(subid:gender, religion_buddhism:age_approx, race_cat3) %>%
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

# prepare datasets for PCA ----------------------------------------------------

# separate conditions and remove identifier variables
d_beetle <- d_clean %>%
  filter(condition == "beetle") %>% # filter by condition
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3!
d_beetle <- data.frame(d_beetle[,-1], row.names = d_beetle[,1])

d_robot <- d_clean %>%
  filter(condition == "robot") %>% # filter by condition
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3!
d_robot <- data.frame(d_robot[,-1], row.names = d_robot[,1])

d_both <- d_clean %>%
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3!
d_both <- data.frame(d_both[,-1], row.names = d_both[,1])

# examine demographic variables -----------------------------------------------

# sample size
sample_size <- with(d_clean, table(condition))
kable(d_clean %>% count(condition))

# approxiate age
age_approx <- d_clean %>%
  group_by(condition) %>%
  summarise(min_age = min(age_approx, na.rm = T),
            max_age = max(age_approx, na.rm = T),
            median_age = median(age_approx, na.rm = T),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))
t.test(age_approx ~ condition, data = d_clean) # test for differences in age across conditions

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

# PCA: BEETLE condition --------------------------------------------------------

# use "very simple structure" criterion to examine how many factors to extract
# VSS(d_beetle, n = 39)
VSS.scree(d_beetle)

# run unrotated pca with maximum number of factors
pca_beetle_unrotated <- principal(d_beetle, nfactors = 39, rotate = "none")
pca_beetle_unrotated
pca_beetle_unrotated$values # examine eignenvalues, retain iff > 1.00

# set number of factors to extract (manually)
nfactors_beetle <- 3

# run pca without rotation with n factors (as determined above)
pca_beetle_unrotated3 <- principal(d_beetle, nfactors = nfactors_beetle, rotate = "none")
pca_beetle_unrotated3
pca_beetle_unrotated3$loadings

# plot mental capacities in first two dimensions
pca_beetle_unrotated3_loadings <- 
  data.frame(pca_beetle_unrotated3$loadings[1:40, 1:nfactors_beetle],
             row.names = rownames(pca_beetle_unrotated3$loadings[1:40, 1:nfactors_beetle]))

# code a priori mental capacity categories
pca_beetle_unrotated3_loadings[c("hungry", "tired", "pain", 
                             "nauseated", "safe"),
                           "mc_cat"] <- "biological"
pca_beetle_unrotated3_loadings[c("happy", "depressed", "fear", 
                             "angry", "calm", "joy"),
                           "mc_cat"] <- "affective"
pca_beetle_unrotated3_loadings[c("sounds", "seeing", "temperature", 
                             "odors", "depth"),
                           "mc_cat"] <- "perceptual"
pca_beetle_unrotated3_loadings[c("computations", "thoughts", "reasoning", 
                             "remembering", "beliefs"),
                           "mc_cat"] <- "cognitive"
pca_beetle_unrotated3_loadings[c("free_will", "choices", "self_restraint", 
                             "intentions", "goal"),
                           "mc_cat"] <- "autonomous"
pca_beetle_unrotated3_loadings[c("love", "recognizing", "communicating", "guilt", 
                             "disrespected", "embarrassed", "emo_recog"),
                           "mc_cat"] <- "social"
pca_beetle_unrotated3_loadings[c("conscious", "self_aware", "pleasure", 
                             "desires", "morality", "personality", "pride"),
                           "mc_cat"] <- "other"

pca_beetle_unrotated3_loadings$mc_cat <- factor(pca_beetle_unrotated3_loadings$mc_cat)

pca_beetle_plot <- ggplot(pca_beetle_unrotated3_loadings,
                         aes(x = PC1, y = PC2,
                             label = rownames(pca_beetle_unrotated3_loadings),
                             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  # geom_point() +
  # geom_dl(method = "smart.grid")+
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BEETLE: factor loadings (first 2 unrotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_beetle_plot

# examine loadings
mc_beetle = rownames(pca_beetle_unrotated3_loadings)

# ... for PC1
pca_beetle_unrotated3_pc1 <- pca_beetle_unrotated3_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_beetle_unrotated3_pc1

# ... for PC2
pca_beetle_unrotated3_pc2 <- pca_beetle_unrotated3_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_beetle_unrotated3_pc2

# ... for PC3
pca_beetle_unrotated3_pc3 <- pca_beetle_unrotated3_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_beetle_unrotated3_pc3

# run pca with varimax rotation with n factors (as determined above)
pca_beetle_varimax <- principal(d_beetle, nfactors = nfactors_beetle, rotate = "varimax")
pca_beetle_varimax
pca_beetle_varimax$loadings

# plot mental capacities in first two dimensions
pca_beetle_varimax_loadings <- 
  data.frame(pca_beetle_varimax$loadings[1:40, 1:nfactors_beetle],
             row.names = rownames(pca_beetle_varimax$loadings[1:40, 1:nfactors_beetle]))

# code a priori mental capacity categories
pca_beetle_varimax_loadings[c("hungry", "tired", "pain", 
                             "nauseated", "safe"),
                           "mc_cat"] <- "biological"
pca_beetle_varimax_loadings[c("happy", "depressed", "fear", 
                             "angry", "calm", "joy"),
                           "mc_cat"] <- "affective"
pca_beetle_varimax_loadings[c("sounds", "seeing", "temperature", 
                             "odors", "depth"),
                           "mc_cat"] <- "perceptual"
pca_beetle_varimax_loadings[c("computations", "thoughts", "reasoning", 
                             "remembering", "beliefs"),
                           "mc_cat"] <- "cognitive"
pca_beetle_varimax_loadings[c("free_will", "choices", "self_restraint", 
                             "intentions", "goal"),
                           "mc_cat"] <- "autonomous"
pca_beetle_varimax_loadings[c("love", "recognizing", "communicating", "guilt", 
                             "disrespected", "embarrassed", "emo_recog"),
                           "mc_cat"] <- "social"
pca_beetle_varimax_loadings[c("conscious", "self_aware", "pleasure", 
                             "desires", "morality", "personality", "pride"),
                           "mc_cat"] <- "other"

pca_beetle_varimax_loadings$mc_cat <- factor(pca_beetle_varimax_loadings$mc_cat)

pca_beetle_plot <- ggplot(pca_beetle_varimax_loadings,
                         aes(x = PC1, y = PC2,
                             label = rownames(pca_beetle_varimax_loadings),
                             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  # geom_point() +
  # geom_dl(method = "smart.grid")+
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BEETLE: factor loadings (first 2 rotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_beetle_plot

# examine loadings
mc_beetle = rownames(pca_beetle_varimax_loadings)

# ... for PC1
pca_beetle_varimax_pc1 <- pca_beetle_varimax_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_beetle_varimax_pc1

# ... for PC2
pca_beetle_varimax_pc2 <- pca_beetle_varimax_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_beetle_varimax_pc2

# ... for PC3
pca_beetle_varimax_pc3 <- pca_beetle_varimax_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_beetle_varimax_pc3

# PCA: ROBOT condition --------------------------------------------------------

# use "very simple structure" criterion to examine how many factors to extract
# VSS(d_robot, n = 39)
VSS.scree(d_robot)

# run unrotated pca with maximum number of factors
pca_robot_unrotated <- principal(d_robot, nfactors = 39, rotate = "none")
pca_robot_unrotated
pca_robot_unrotated$values # examine eignenvalues, retain iff > 1.00

# set number of factors to extract (manually)
nfactors_robot <- 3

# run pca without rotation with n factors (as determined above)
pca_robot_unrotated3 <- principal(d_robot, nfactors = nfactors_robot, rotate = "none")
pca_robot_unrotated3
pca_robot_unrotated3$loadings

# plot mental capacities in first two dimensions
pca_robot_unrotated3_loadings <- 
  data.frame(pca_robot_unrotated3$loadings[1:40, 1:nfactors_robot],
             row.names = rownames(pca_robot_unrotated3$loadings[1:40, 1:nfactors_robot]))

# code a priori mental capacity categories
pca_robot_unrotated3_loadings[c("hungry", "tired", "pain", 
                                "nauseated", "safe"),
                              "mc_cat"] <- "biological"
pca_robot_unrotated3_loadings[c("happy", "depressed", "fear", 
                                "angry", "calm", "joy"),
                              "mc_cat"] <- "affective"
pca_robot_unrotated3_loadings[c("sounds", "seeing", "temperature", 
                                "odors", "depth"),
                              "mc_cat"] <- "perceptual"
pca_robot_unrotated3_loadings[c("computations", "thoughts", "reasoning", 
                                "remembering", "beliefs"),
                              "mc_cat"] <- "cognitive"
pca_robot_unrotated3_loadings[c("free_will", "choices", "self_restraint", 
                                "intentions", "goal"),
                              "mc_cat"] <- "autonomous"
pca_robot_unrotated3_loadings[c("love", "recognizing", "communicating", "guilt", 
                                "disrespected", "embarrassed", "emo_recog"),
                              "mc_cat"] <- "social"
pca_robot_unrotated3_loadings[c("conscious", "self_aware", "pleasure", 
                                "desires", "morality", "personality", "pride"),
                              "mc_cat"] <- "other"

pca_robot_unrotated3_loadings$mc_cat <- factor(pca_robot_unrotated3_loadings$mc_cat)

pca_robot_plot <- ggplot(pca_robot_unrotated3_loadings,
                         aes(x = PC1, y = PC2,
                             label = rownames(pca_robot_unrotated3_loadings),
                             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  # geom_point() +
  # geom_dl(method = "smart.grid")+
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "ROBOT: factor loadings (first 2 unrotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_robot_plot

# examine loadings
mc_robot = rownames(pca_robot_unrotated3_loadings)

# ... for PC1
pca_robot_unrotated3_pc1 <- pca_robot_unrotated3_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_robot_unrotated3_pc1

# ... for PC2
pca_robot_unrotated3_pc2 <- pca_robot_unrotated3_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_robot_unrotated3_pc2

# ... for PC3
pca_robot_unrotated3_pc3 <- pca_robot_unrotated3_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_robot_unrotated3_pc3

# run pca with varimax rotation with n factors (as determined above)
pca_robot_varimax <- principal(d_robot, nfactors = nfactors_robot, rotate = "varimax")
pca_robot_varimax
pca_robot_varimax$loadings

# plot mental capacities in first two dimensions
pca_robot_varimax_loadings <- 
  data.frame(pca_robot_varimax$loadings[1:40, 1:nfactors_robot],
             row.names = rownames(pca_robot_varimax$loadings[1:40, 1:nfactors_robot]))

# code a priori mental capacity categories
pca_robot_varimax_loadings[c("hungry", "tired", "pain", 
                             "nauseated", "safe"),
                           "mc_cat"] <- "biological"
pca_robot_varimax_loadings[c("happy", "depressed", "fear", 
                             "angry", "calm", "joy"),
                           "mc_cat"] <- "affective"
pca_robot_varimax_loadings[c("sounds", "seeing", "temperature", 
                             "odors", "depth"),
                           "mc_cat"] <- "perceptual"
pca_robot_varimax_loadings[c("computations", "thoughts", "reasoning", 
                             "remembering", "beliefs"),
                           "mc_cat"] <- "cognitive"
pca_robot_varimax_loadings[c("free_will", "choices", "self_restraint", 
                             "intentions", "goal"),
                           "mc_cat"] <- "autonomous"
pca_robot_varimax_loadings[c("love", "recognizing", "communicating", "guilt", 
                             "disrespected", "embarrassed", "emo_recog"),
                           "mc_cat"] <- "social"
pca_robot_varimax_loadings[c("conscious", "self_aware", "pleasure", 
                             "desires", "morality", "personality", "pride"),
                           "mc_cat"] <- "other"

pca_robot_varimax_loadings$mc_cat <- factor(pca_robot_varimax_loadings$mc_cat)

pca_robot_plot <- ggplot(pca_robot_varimax_loadings,
                         aes(x = PC1, y = PC2,
                             label = rownames(pca_robot_varimax_loadings),
                             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  # geom_point() +
  # geom_dl(method = "smart.grid")+
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "ROBOT: factor loadings (first 2 rotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_robot_plot

# examine loadings
mc_robot = rownames(pca_robot_varimax_loadings)

# ... for PC1
pca_robot_varimax_pc1 <- pca_robot_varimax_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_robot_varimax_pc1

# ... for PC2
pca_robot_varimax_pc2 <- pca_robot_varimax_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_robot_varimax_pc2

# ... for PC3
pca_robot_varimax_pc3 <- pca_robot_varimax_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_robot_varimax_pc3

# PCA: BOTH condition --------------------------------------------------------

# use "very simple structure" criterion to examine how many factors to extract
# VSS(d_both, n = 39)
VSS.scree(d_both)

# run unrotated pca with maximum number of factors
pca_both_unrotated <- principal(d_both, nfactors = 39, rotate = "none")
pca_both_unrotated
pca_both_unrotated$values # examine eignenvalues, retain iff > 1.00

# set number of factors to extract (manually)
nfactors_both <- 4

# run pca without rotation with n factors (as determined above)
pca_both_unrotated3 <- principal(d_both, nfactors = nfactors_both, rotate = "none")
pca_both_unrotated3
pca_both_unrotated3$loadings

# plot mental capacities in first two dimensions
pca_both_unrotated3_loadings <- 
  data.frame(pca_both_unrotated3$loadings[1:40, 1:nfactors_both],
             row.names = rownames(pca_both_unrotated3$loadings[1:40, 1:nfactors_both]))

# code a priori mental capacity categories
pca_both_unrotated3_loadings[c("hungry", "tired", "pain", 
                                "nauseated", "safe"),
                              "mc_cat"] <- "biological"
pca_both_unrotated3_loadings[c("happy", "depressed", "fear", 
                                "angry", "calm", "joy"),
                              "mc_cat"] <- "affective"
pca_both_unrotated3_loadings[c("sounds", "seeing", "temperature", 
                                "odors", "depth"),
                              "mc_cat"] <- "perceptual"
pca_both_unrotated3_loadings[c("computations", "thoughts", "reasoning", 
                                "remembering", "beliefs"),
                              "mc_cat"] <- "cognitive"
pca_both_unrotated3_loadings[c("free_will", "choices", "self_restraint", 
                                "intentions", "goal"),
                              "mc_cat"] <- "autonomous"
pca_both_unrotated3_loadings[c("love", "recognizing", "communicating", "guilt", 
                                "disrespected", "embarrassed", "emo_recog"),
                              "mc_cat"] <- "social"
pca_both_unrotated3_loadings[c("conscious", "self_aware", "pleasure", 
                                "desires", "morality", "personality", "pride"),
                              "mc_cat"] <- "other"

pca_both_unrotated3_loadings$mc_cat <- factor(pca_both_unrotated3_loadings$mc_cat)

pca_both_plot <- ggplot(pca_both_unrotated3_loadings,
                         aes(x = PC1, y = PC2,
                             label = rownames(pca_both_unrotated3_loadings),
                             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  # geom_point() +
  # geom_dl(method = "smart.grid")+
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BOTH: factor loadings (first 2 unrotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_both_plot

# examine loadings
mc_both = rownames(pca_both_unrotated3_loadings)

# ... for PC1
pca_both_unrotated3_pc1 <- pca_both_unrotated3_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_both_unrotated3_pc1

# ... for PC2
pca_both_unrotated3_pc2 <- pca_both_unrotated3_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_both_unrotated3_pc2

# ... for PC3
pca_both_unrotated3_pc3 <- pca_both_unrotated3_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_both_unrotated3_pc3

# ... for PC4
pca_both_unrotated3_pc4 <- pca_both_unrotated3_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC4)) %>%
  select(PC4, mc, mc_cat)
pca_both_unrotated3_pc4

# run pca with varimax rotation with n factors (as determined above)
pca_both_varimax <- principal(d_both, nfactors = nfactors_both, rotate = "varimax")
pca_both_varimax
pca_both_varimax$loadings

# plot mental capacities in first two dimensions
pca_both_varimax_loadings <- 
  data.frame(pca_both_varimax$loadings[1:40, 1:nfactors_both],
             row.names = rownames(pca_both_varimax$loadings[1:40, 1:nfactors_both]))

# code a priori mental capacity categories
pca_both_varimax_loadings[c("hungry", "tired", "pain", 
                             "nauseated", "safe"),
                           "mc_cat"] <- "biological"
pca_both_varimax_loadings[c("happy", "depressed", "fear", 
                             "angry", "calm", "joy"),
                           "mc_cat"] <- "affective"
pca_both_varimax_loadings[c("sounds", "seeing", "temperature", 
                             "odors", "depth"),
                           "mc_cat"] <- "perceptual"
pca_both_varimax_loadings[c("computations", "thoughts", "reasoning", 
                             "remembering", "beliefs"),
                           "mc_cat"] <- "cognitive"
pca_both_varimax_loadings[c("free_will", "choices", "self_restraint", 
                             "intentions", "goal"),
                           "mc_cat"] <- "autonomous"
pca_both_varimax_loadings[c("love", "recognizing", "communicating", "guilt", 
                             "disrespected", "embarrassed", "emo_recog"),
                           "mc_cat"] <- "social"
pca_both_varimax_loadings[c("conscious", "self_aware", "pleasure", 
                             "desires", "morality", "personality", "pride"),
                           "mc_cat"] <- "other"

pca_both_varimax_loadings$mc_cat <- factor(pca_both_varimax_loadings$mc_cat)

pca_both_plot <- ggplot(pca_both_varimax_loadings,
                         aes(x = PC1, y = PC2,
                             label = rownames(pca_both_varimax_loadings),
                             color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  # geom_point() +
  # geom_dl(method = "smart.grid")+
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BOTH: factor loadings (first 2 rotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_both_plot

# examine loadings
mc_both = rownames(pca_both_varimax_loadings)

# ... for PC1
pca_both_varimax_pc1 <- pca_both_varimax_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_both_varimax_pc1

# ... for PC2
pca_both_varimax_pc2 <- pca_both_varimax_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_both_varimax_pc2

# ... for PC3
pca_both_varimax_pc3 <- pca_both_varimax_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_both_varimax_pc3

# ... for PC4
pca_both_varimax_pc4 <- pca_both_varimax_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC4)) %>%
  select(PC4, mc, mc_cat)
pca_both_varimax_pc4

# plot participants by component scores ---------------------------------------

# need a way to include condition information!
# plot(pca_both_varimax$scores[,c("PC1", "PC2")])
# plot(pca_both_varimax$scores[,c("PC1", "PC3")])
# plot(pca_both_varimax$scores[,c("PC1", "PC4")])
# plot(pca_both_varimax$scores[,c("PC2", "PC3")])
# plot(pca_both_varimax$scores[,c("PC2", "PC4")])
# plot(pca_both_varimax$scores[,c("PC3", "PC4")])

# get condition by subject
condition_subid <- d_clean %>%
  select(subid, condition)

pca_both_varimax_scores <- pca_both_varimax$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  full_join(condition_subid)

p_both_varimax_scores_12 <- ggplot(aes(x = PC1, y = PC2, color = condition),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC1 vs. PC2 (from rotated PCA)\n",
       x = "\nPC1 (social/emotional)",
       y = "PC2 (cognitive/agentic)\n")

p_both_varimax_scores_13 <- ggplot(aes(x = PC1, y = PC3, color = condition),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC1 vs. PC3 (from rotated PCA)\n",
       x = "\nPC1 (social/emotional)",
       y = "PC3 (perceptual)\n")

p_both_varimax_scores_14 <- ggplot(aes(x = PC1, y = PC4, color = condition),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC1 vs. PC4 (from rotated PCA)\n",
       x = "\nPC1 (social/emotional)",
       y = "PC4 (biological)\n")

p_both_varimax_scores_23 <- ggplot(aes(x = PC2, y = PC3, color = condition),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC2 vs. PC3 (from rotated PCA)\n",
       x = "\nPC2 (cognitive/agentic)",
       y = "PC3 (perceptual)\n")

p_both_varimax_scores_24 <- ggplot(aes(x = PC2, y = PC4, color = condition),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC2 vs. PC4 (from rotated PCA)\n",
       x = "\nPC2 (cognitive/agentic)",
       y = "PC4 (biological)\n")

p_both_varimax_scores_34 <- ggplot(aes(x = PC3, y = PC4, color = condition),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC3 vs. PC4 (from rotated PCA)\n",
       x = "\nPC3 (perceptual)",
       y = "PC4 (biological)\n")

p_both_varimax_scores_12
p_both_varimax_scores_13
p_both_varimax_scores_14
p_both_varimax_scores_23
p_both_varimax_scores_24
p_both_varimax_scores_34

# EXPLORATORY: hand-calculate "scores" on dimensions for each target? ---------

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
both_PC <- cbind(mc = rownames(pca_both_varimax_loadings),
                 pca_both_varimax_loadings)

# combine!
d_scoring_pre <- full_join(rating_sum, both_PC)

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
                            labels = c("social_emotional", "cognitive_agentic", 
                                       "perceptual", "biological")))

p_scored <- ggplot(aes(x = dimension, y = score,
                       group = condition, fill = condition), 
                   data = d_scored_long) +
  geom_bar(stat = "identity", width = 0.8,
           position = position_dodge(width = 0.9)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
       x = "\nDimension",
       y = "Score\n")
p_scored

# # EXPLORATORY: normality tests (both conditions) --------------------------------------------
# 
# # make function for conducting shapiro-wilk tests for all distributions
# sw_multi <- function(data) {
#   mental_caps <- names(data)
#   sw_W <- data.frame(NULL)
#   sw_p <- data.frame(NULL)
#   for (i in 1:length(mental_caps)) {
#     temp <- shapiro.test(unlist(data[mental_caps[i]], use.names = F))
#     sw_W[1, mental_caps[i]] <- temp$statistic
#     sw_p[1, mental_caps[i]] <- temp$p.value
#   }
#   results <- data.frame(NULL)
#   attr(results, "W") <- sw_W
#   attr(results, "p.value") <- sw_p
#   return(results)
# }
# 
# # run sw tests on ROBOT condition
# sw_robot <- sw_multi(d_robot)
# attr(sw_robot, "W") # look at D-values
# attr(sw_robot, "p.value") # look at p-values
# 
# p.adjust(as.matrix(attr(sw_robot, "p.value")), method = "bonferroni") # auto bonferroni
# p.adjust(as.matrix(attr(sw_robot, "p.value")), method = "holm") # auto false discovery
# 
# # run sw tests on BEETLE condition
# sw_beetle <- sw_multi(d_beetle)
# attr(sw_beetle, "W") # look at D-values
# attr(sw_beetle, "p.value") # look at p-values
# 
# p.adjust(as.matrix(attr(sw_beetle, "p.value")), method = "bonferroni") # auto bonferroni
# p.adjust(as.matrix(attr(sw_beetle, "p.value")), method = "holm") # auto false discovery

# # EXPLORATORY: bimodality comparisons (both conditions) -------------------------------------
# 
# # set corrected critical p-value for multiple comparisons (crude bonferroni method)
# n_comparisons <- choose(40, 2) # determine how many comparisons (choose 2 from 40 MCs)
# p_crit <- 0.05/n_comparisons # divide alpha by number of comparisons
# 
# # make function for conducting kolmogorov-smirnov tests for all possible comparisons
# ks_multi <- function(data) {
#   mental_caps <- names(data)
#   ks_D <- data.frame(NULL)
#   ks_p <- data.frame(NULL)
#   ks_signif <- data.frame(NULL)
#   for (i in 1:length(mental_caps)) {
#     for (j in 2:length(mental_caps)) {
#       temp <- ks.test(unlist(data[mental_caps[i]], use.names = F), 
#                       unlist(data[mental_caps[j]], use.names = F))
#       ks_D[mental_caps[i], mental_caps[j]] <- temp$statistic
#       ks_p[mental_caps[i], mental_caps[j]] <- temp$p.value
#     }
#   }
#   results <- data.frame(NULL)
#   attr(results, "D") <- ks_D
#   attr(results, "p.value") <- ks_p
#   attr(results, "signif") <- ks_signif
#   return(results)
# }
# 
# # run ks tests on ROBOT condition
# ks_robot <- ks_multi(d_robot)
# attr(ks_robot, "D") # look at D-values
# attr(ks_robot, "p.value") # look at p-values
# 
# p.adjust(as.matrix(attr(ks_robot, "p.value")), method = "bonferroni") # auto bonferroni
# p.adjust(as.matrix(attr(ks_robot, "p.value")), method = "holm") # auto false discovery
# 
# # run ks tests on BEETLE condition
# ks_beetle <- ks_multi(d_beetle)
# attr(ks_beetle, "D") # look at D-values
# attr(ks_beetle, "p.value") # look at p-values
# 
# p.adjust(as.matrix(attr(ks_beetle, "p.value")), method = "bonferroni") # auto bonferroni
# p.adjust(as.matrix(attr(ks_beetle, "p.value")), method = "holm") # auto false discovery

# # EXPLORATORY: look at response distributions across mental capacities ------
# dist_plot_affective <- ggplot(d_long %>% filter(mc_cat == "affective"), 
#                               aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_affective
# 
# dist_plot_perceptual <- ggplot(d_long %>% filter(mc_cat == "perceptual"),
#                                aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_perceptual
# 
# dist_plot_autonomous <- ggplot(d_long %>% filter(mc_cat == "autonomous"), 
#                                aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_autonomous
# 
# dist_plot_biological <- ggplot(d_long %>% filter(mc_cat == "biological"), 
#                                aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_biological
# 
# dist_plot_cognitive <- ggplot(d_long %>% filter(mc_cat == "cognitive"), 
#                               aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_cognitive
# 
# dist_plot_social <- ggplot(d_long %>% filter(mc_cat == "social"), 
#                            aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_social
# 
# dist_plot_other <- ggplot(d_long %>% filter(mc_cat == "other"), 
#                           aes(x = rating)) +
#   facet_grid(mc ~ condition) +
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_x_continuous(breaks = -3:3)
# dist_plot_other