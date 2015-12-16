# load libraries --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(devtools)
library(stats)

# clear workspace
rm(list = ls(all = T))
graphics.off()

# simulate dataset ------------------------------------------------------------

# randomly generate demographics and other background variables
subid <- factor(paste0(rep("s", 400, replace = T), 1:400))
condition <- factor(sample(c("beetle", "robot"), 400, replace = T))
yob <- as.numeric(sample(1940:1997, 400, replace = T))
gender <- factor(sample(c("m", "f", "na"), 400, replace = T))
race <- factor(sample(c("asian_east", "asian_south", "asian_other",
                        "black", "hispanic", "middle_eastern",
                        "native_american", "pac_islander", "white",
                        "other_prefno"), 400, replace = T))
religion <- factor(sample(c("buddhism", "christianity", "hinduism", "islam",
                            "jainism", "judaism", "sikhism", "other_religious",
                            "not_religious", "prefno"), 400, replace = T))
feedback <- sample(c("blahblah", "yadayada", ""), 400, replace = T)

# randomly generate responses to test questions
happy <- as.numeric(sample(0:6, 400, replace = T))
depressed <- as.numeric(sample(0:6, 400, replace = T))
fear <- as.numeric(sample(0:6, 400, replace = T))
angry <- as.numeric(sample(0:6, 400, replace = T))
calm <- as.numeric(sample(0:6, 400, replace = T))
sounds <- as.numeric(sample(0:6, 400, replace = T))
seeing <- as.numeric(sample(0:6, 400, replace = T))
temperature <- as.numeric(sample(0:6, 400, replace = T))
odors <- as.numeric(sample(0:6, 400, replace = T))
depth <- as.numeric(sample(0:6, 400, replace = T))
computations <- as.numeric(sample(0:6, 400, replace = T))
thoughts <- as.numeric(sample(0:6, 400, replace = T))
reasoning <- as.numeric(sample(0:6, 400, replace = T))
remembering <- as.numeric(sample(0:6, 400, replace = T))
beliefs <- as.numeric(sample(0:6, 400, replace = T))
hungry <- as.numeric(sample(0:6, 400, replace = T))
tired <- as.numeric(sample(0:6, 400, replace = T))
pain <- as.numeric(sample(0:6, 400, replace = T))
nauseated <- as.numeric(sample(0:6, 400, replace = T))
safe <- as.numeric(sample(0:6, 400, replace = T))
love <- as.numeric(sample(0:6, 400, replace = T))
recognizing <- as.numeric(sample(0:6, 400, replace = T))
communicating <- as.numeric(sample(0:6, 400, replace = T))
guilt <- as.numeric(sample(0:6, 400, replace = T))
disrespected <- as.numeric(sample(0:6, 400, replace = T))
free_will <- as.numeric(sample(0:6, 400, replace = T))
choices <- as.numeric(sample(0:6, 400, replace = T))
self_restraint <- as.numeric(sample(0:6, 400, replace = T))
intentions <- as.numeric(sample(0:6, 400, replace = T))
goal <- as.numeric(sample(0:6, 400, replace = T))
conscious <- as.numeric(sample(0:6, 400, replace = T))
self_aware <- as.numeric(sample(0:6, 400, replace = T))
desires <- as.numeric(sample(0:6, 400, replace = T))
embarrassed <- as.numeric(sample(0:6, 400, replace = T))
emo_recog <- as.numeric(sample(0:6, 400, replace = T))
joy <- as.numeric(sample(0:6, 400, replace = T))
morality <- as.numeric(sample(0:6, 400, replace = T))
personality <- as.numeric(sample(0:6, 400, replace = T))
pleasure <- as.numeric(sample(0:6, 400, replace = T))
pride <- as.numeric(sample(0:6, 400, replace = T))
CATCH <- as.numeric(sample(0:6, 400, replace = T))

# bind into one dataframe
dsim <- data_frame(subid, condition, yob, gender, race, religion, feedback, happy, depressed, fear, angry, calm, sounds, seeing, temperature, odors, depth, computations, thoughts, reasoning, remembering, beliefs, hungry, tired, pain, nauseated, safe, love, recognizing, communicating, guilt, disrespected, free_will, choices, self_restraint, intentions, goal, conscious, self_aware, desires, embarrassed, emo_recog, joy, morality, personality, pleasure, pride, CATCH)

# clean up dataset ------------------------------------------------------------

dsim_clean <- dsim %>%
  filter(CATCH == 1) %>% # exclude participants who fail catch trials 
  mutate(age_approx = 2015 - yob) %>% # calculate approximate age
  filter(age_approx >= 18) # exclude participants who are younger than 18 years old

# examine demographic variables -----------------------------------------------

# sample size
sample_size <- with(dsim_clean, table(condition)) %>% addmargins(); sample_size

# approxiate age
age_approx <- dsim_clean %>%
  group_by(condition) %>%
  summarise(min_age = min(age_approx),
            max_age = max(age_approx),
            mean_age = mean(age_approx, na.rm = T),
            sd_age = sd(age_approx, na.rm = T))
age_approx
t.test(age_approx ~ condition, data = dsim_clean) # test for differences in age across conditions

# gender
gender <- with(dsim_clean, table(condition, gender)); addmargins(gender)
summary(gender) # test for difference in gender distribution across conditions

# racial/ethnic background (NEED TO ADJUST TO ALLOW FOR MULTIPLE ANSWERS)
race_ethn <- with(dsim_clean, table(condition, race)); addmargins(race_ethn) 
summary(race_ethn) # test for difference in race/ethnicity distribution across conditions

# religious background (NEED TO ADJUST TO ALLOW FOR MULTIPLE ANSWERS)
religion <- with(dsim_clean, table(condition, religion)); addmargins(religion) 
summary(religion) # test for difference in religion distribution across conditions

# prepare datasets for PCA ----------------------------------------------------

# separate conditions and remove identifier variables
d_robot <- dsim_clean %>%
  filter(condition == "robot") %>% # filter by condition
  select(happy:pride) # NOTE: make sure to check that responses are scored as -3:3!

d_beetle <- dsim_clean %>%
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
nfactors_robot <- 12

# run pca with varimax rotation with n factors (as determined above)
pca_robot_varimax <- principal(d_robot, nfactors = nfactors_robot, rotate = "none")
pca_robot_varimax
pca_robot_varimax$loadings

# plot mental capacities in first two dimensions
ggplot(data.frame(pca_robot_varimax$loadings[1:40, 1:nfactors_robot]), aes(x = PC1, y = PC2, label = rownames(data.frame(pca_robot_varimax$loadings[1:40, 1:nfactors_robot])))) +
  geom_text() +
  theme_bw() +
  labs(title = "ROBOT: factor loadings (first 2 components)\n",
       x = "\nPrincipal Component 2",
       y = "Principal Component 1\n")

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
pca_beetle_varimax <- principal(d_beetle, nfactors = nfactors_beetle, rotate = "none")
pca_beetle_varimax

# plot mental capacities in first two dimensions
ggplot(data.frame(pca_beetle_varimax$loadings[1:40, 1:nfactors_beetle]), aes(x = PC1, y = PC2, label = rownames(data.frame(pca_beetle_varimax$loadings[1:40, 1:nfactors_beetle])))) +
  geom_text() +
  theme_bw() +
  labs(title = "BEETLE: factor loadings (first 2 components)\n",
       x = "\nPrincipal Component 2",
       y = "Principal Component 1\n")

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

# run ks tests on ROBOT condition
sw_robot <- sw_multi(d_robot)
attr(sw_robot, "W") # look at D-values
attr(sw_robot, "p.value") # look at p-values

p.adjust(as.matrix(attr(sw_robot, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(sw_robot, "p.value")), method = "holm") # auto false discovery

# run ks tests on BEETLE condition
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
      ks_signif[mental_caps[i], mental_caps[j]] <- ifelse(temp$p.value < p_crit, round(p.value, 3), "ns")
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
attr(ks_robot, "signif") # identify significant comparisons according to new p-crit

p.adjust(as.matrix(attr(ks_robot, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(ks_robot, "p.value")), method = "holm") # auto false discovery

# run ks tests on BEETLE condition
ks_beetle <- ks_multi(d_beetle)
attr(ks_beetle, "D") # look at D-values
attr(ks_beetle, "p.value") # look at p-values
attr(ks_beetle, "signif") # identify significant comparisons according to new p-crit

p.adjust(as.matrix(attr(ks_beetle, "p.value")), method = "bonferroni") # auto bonferroni
p.adjust(as.matrix(attr(ks_beetle, "p.value")), method = "holm") # auto false discovery