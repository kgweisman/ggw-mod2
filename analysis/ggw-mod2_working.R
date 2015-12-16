# load libraries --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(devtools)
library(stats)
library(directlabels)

# clear workspace
rm(list = ls(all = T))
graphics.off()

# upload dataset ------------------------------------------------------------

d_raw <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod2/mturk/GGWmod2_v1_clean.csv")

# clean up dataset ------------------------------------------------------------

d_clean <- d_raw %>%
  filter(CATCH == 1, # exclude participants who fail catch trials 
         finished != 0) %>% # exclude participants who did not complete task
  mutate(yob_correct = as.numeric(
    ifelse(as.numeric(as.character(yob)) > 1900 & 
             as.numeric(as.character(yob)) < 2000, 
           as.numeric(as.character(yob)), NA)), # correct formatting issues in yob
         age_approx = 2015 - yob_correct) %>% # calculate approximate age
  mutate(gender = factor(gender, levels = c(1, 2, 0), labels = c("m", "f", "other"))) %>%
  filter(age_approx >= 18) # exclude participants who are younger than 18 years old

# examine demographic variables -----------------------------------------------

# sample size
sample_size <- with(d_clean, table(condition)) %>% addmargins(); sample_size

# approxiate age
age_approx <- d_clean %>%
  group_by(condition) %>%
  summarise(min_age = min(age_approx, na.rm = T),
            max_age = max(age_approx, na.rm = T),
            median_age = median(age_approx, na.rm = T),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))
age_approx
t.test(age_approx ~ condition, data = d_clean) # test for differences in age across conditions

# gender
gender <- with(d_clean, table(condition, gender)); addmargins(gender)
summary(gender) # test for difference in gender distribution across conditions

# # racial/ethnic background (NEED TO ADJUST TO ALLOW FOR MULTIPLE ANSWERS)
# race_ethn <- with(d_clean, table(condition, race)); addmargins(race_ethn) 
# summary(race_ethn) # test for difference in race/ethnicity distribution across conditions
# 
# # religious background (NEED TO ADJUST TO ALLOW FOR MULTIPLE ANSWERS)
# religion <- with(d_clean, table(condition, religion)); addmargins(religion) 
# summary(religion) # test for difference in religion distribution across conditions

# prepare datasets for PCA ----------------------------------------------------

# separate conditions and remove identifier variables
d_robot <- d_clean %>%
  filter(condition == "robot") %>% # filter by condition
  select(happy:pride) # NOTE: make sure to check that responses are scored as -3:3!

d_beetle <- d_clean %>%
  filter(condition == "beetle") %>% # filter by condition
  select(happy:pride) # NOTE: make sure to check that responses are scored as -3:3!

# PCA: ROBOT condition --------------------------------------------------------

# use "very simple structure" criterion to examine how many factors to extract
VSS(d_robot, n = 39)
VSS.scree(d_robot)

# run unrotated pca with maximum number of factors
pca_robot_unrotated <- principal(d_robot, nfactors = 39, rotate = "none")
pca_robot_unrotated
pca_robot_unrotated$values # examine eignenvalues, retain iff > 1.00

# set number of factors to extract (manually)
nfactors_robot <- 3

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
  select(PC1, mc, mc_cat); pca_robot_varimax_pc1

# ... for PC2
pca_robot_varimax_pc2 <- pca_robot_varimax_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat); pca_robot_varimax_pc2

# ... for PC3
pca_robot_varimax_pc3 <- pca_robot_varimax_loadings %>%
  mutate(mc = mc_robot) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat); pca_robot_varimax_pc3

# PCA: BEETLE condition -------------------------------------------------------

# use "very simple structure" criterion to examine how many factors to extract
VSS(d_beetle, n = 39)
VSS.scree(d_beetle)

# run unrotated pca with maximum number of factors
pca_beetle_unrotated <- principal(d_beetle, nfactors = 39, rotate = "none")
pca_beetle_unrotated
pca_beetle_unrotated$values # examine eignenvalues, retain iff > 1.00

# set number of factors to extract (manually)
nfactors_beetle <- 3

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
  select(PC1, mc, mc_cat); pca_beetle_varimax_pc1

# ... for PC2
pca_beetle_varimax_pc2 <- pca_beetle_varimax_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat); pca_beetle_varimax_pc2

# ... for PC3
pca_beetle_varimax_pc3 <- pca_beetle_varimax_loadings %>%
  mutate(mc = mc_beetle) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat); pca_beetle_varimax_pc3

# normality tests: both conditions --------------------------------------------

# make function for conducting shapiro-wilk tests for all distributions
sw_multi <- function(data) {
  mental_caps <- names(data)
  sw_W <- data.frame(NULL)
  sw_p <- data.frame(NULL)
  for (i in 1:length(mental_caps)) {
    temp <- shapiro.test(unlist(data[mental_caps[i]], use.names = F))
    sw_W[1, mental_caps[i]] <- temp$statistic
    sw_p[1, mental_caps[i]] <- temp$p.value
  }
  results <- data.frame(NULL)
  attr(results, "W") <- sw_W
  attr(results, "p.value") <- sw_p
  return(results)
}

# run sw tests on ROBOT condition
sw_robot <- sw_multi(d_robot)
attr(sw_robot, "W") # look at D-values
attr(sw_robot, "p.value") # look at p-values

p.adjust(as.matrix(attr(sw_robot, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(sw_robot, "p.value")), method = "holm") # auto false discovery

# run sw tests on BEETLE condition
sw_beetle <- sw_multi(d_beetle)
attr(sw_beetle, "W") # look at D-values
attr(sw_beetle, "p.value") # look at p-values

p.adjust(as.matrix(attr(sw_beetle, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(sw_beetle, "p.value")), method = "holm") # auto false discovery

# bimodality comparisons: both conditions -------------------------------------

# set corrected critical p-value for multiple comparisons (crude bonferroni method)
n_comparisons <- choose(40, 2) # determine how many comparisons (choose 2 from 40 MCs)
p_crit <- 0.05/n_comparisons # divide alpha by number of comparisons

# make function for conducting kolmogorov-smirnov tests for all possible comparisons
ks_multi <- function(data) {
  mental_caps <- names(data)
  ks_D <- data.frame(NULL)
  ks_p <- data.frame(NULL)
  ks_signif <- data.frame(NULL)
  for (i in 1:length(mental_caps)) {
    for (j in 2:length(mental_caps)) {
      temp <- ks.test(unlist(data[mental_caps[i]], use.names = F), 
                      unlist(data[mental_caps[j]], use.names = F))
      ks_D[mental_caps[i], mental_caps[j]] <- temp$statistic
      ks_p[mental_caps[i], mental_caps[j]] <- temp$p.value
    }
  }
  results <- data.frame(NULL)
  attr(results, "D") <- ks_D
  attr(results, "p.value") <- ks_p
  attr(results, "signif") <- ks_signif
  return(results)
}

# run ks tests on ROBOT condition
ks_robot <- ks_multi(d_robot)
attr(ks_robot, "D") # look at D-values
attr(ks_robot, "p.value") # look at p-values

p.adjust(as.matrix(attr(ks_robot, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(ks_robot, "p.value")), method = "holm") # auto false discovery

# run ks tests on BEETLE condition
ks_beetle <- ks_multi(d_beetle)
attr(ks_beetle, "D") # look at D-values
attr(ks_beetle, "p.value") # look at p-values

p.adjust(as.matrix(attr(ks_beetle, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(ks_beetle, "p.value")), method = "holm") # auto false discovery

# EXPLORATORY: look at response distributions across mental capacities ------

d_merge <- d_clean %>%
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

ggplot(d_merge %>% filter(mc_cat == "affective"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)

ggplot(d_merge %>% filter(mc_cat == "perceptual"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)

ggplot(d_merge %>% filter(mc_cat == "autonomous"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)

ggplot(d_merge %>% filter(mc_cat == "biological"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)

ggplot(d_merge %>% filter(mc_cat == "cognitive"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)

ggplot(d_merge %>% filter(mc_cat == "social"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)

ggplot(d_merge %>% filter(mc_cat == "other"), aes(x = rating)) +
  facet_grid(mc ~ condition) +
  geom_histogram() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = -3:3)
