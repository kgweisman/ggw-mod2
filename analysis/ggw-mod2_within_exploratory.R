# PCA: BOTH condition --------------------------------------------------------

# make dataset for both conditions
d_both <- d_clean %>%
  mutate(subid = paste(target, subid, sep = "_")) %>%
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
d_both <- data.frame(d_both[,-1], row.names = d_both[,1])

# use "very simple structure" criterion to examine how many factors to extract
# VSS(d_both, n = 39)
VSS.scree(d_both)

# run unrotated pca with maximum number of factors
pca_both_unrotated <- principal(d_both, nfactors = 39, rotate = "none")
pca_both_unrotated
pca_both_unrotated$values # examine eignenvalues, retain iff > 1.00

# set number of factors to extract (manually)
nfactors_both <- 3

# run pca without rotation with n factors (as determined above)
pca_both_unrotatedN <- principal(d_both, nfactors = nfactors_both, rotate = "none")
pca_both_unrotatedN

# plot mental capacities in first two dimensions
pca_both_unrotatedN_loadings <- 
  data.frame(pca_both_unrotatedN$loadings[1:40, 1:nfactors_both],
             row.names = rownames(pca_both_unrotatedN$loadings[1:40, 1:nfactors_both]))

# code a priori mental capacity categories
pca_both_unrotatedN_loadings[c("hungry", "tired", "pain", 
                               "nauseated", "safe"),
                             "mc_cat"] <- "biological"
pca_both_unrotatedN_loadings[c("happy", "depressed", "fear", 
                               "angry", "calm", "joy"),
                             "mc_cat"] <- "affective"
pca_both_unrotatedN_loadings[c("sounds", "seeing", "temperature", 
                               "odors", "depth"),
                             "mc_cat"] <- "perceptual"
pca_both_unrotatedN_loadings[c("computations", "thoughts", "reasoning", 
                               "remembering", "beliefs"),
                             "mc_cat"] <- "cognitive"
pca_both_unrotatedN_loadings[c("free_will", "choices", "self_restraint", 
                               "intentions", "goal"),
                             "mc_cat"] <- "autonomous"
pca_both_unrotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                               "disrespected", "embarrassed", "emo_recog"),
                             "mc_cat"] <- "social"
pca_both_unrotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                               "desires", "morality", "personality", "pride"),
                             "mc_cat"] <- "other"

pca_both_unrotatedN_loadings$mc_cat <- factor(pca_both_unrotatedN_loadings$mc_cat)

pca_both_plot <- ggplot(pca_both_unrotatedN_loadings,
                        aes(x = PC1, y = PC2,
                            label = rownames(pca_both_unrotatedN_loadings),
                            color = mc_cat)) +
  geom_text(hjust = 0.5, vjust = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_brewer(type = "qual", palette = 2) +
  labs(title = "BOTH: factor loadings (first 2 unrotated components)\n",
       x = "\nPrincipal Component 1",
       y = "Principal Component 2\n")
pca_both_plot

# examine loadings
mc_both = rownames(pca_both_unrotatedN_loadings)

# ... for PC1
pca_both_unrotatedN_pc1 <- pca_both_unrotatedN_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_both_unrotatedN_pc1

# ... for PC2
pca_both_unrotatedN_pc2 <- pca_both_unrotatedN_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_both_unrotatedN_pc2

# ... for PC3
pca_both_unrotatedN_pc3 <- pca_both_unrotatedN_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC3)) %>%
  select(PC3, mc, mc_cat)
pca_both_unrotatedN_pc3

# ... for PC4
pca_both_unrotatedN_pc4 <- pca_both_unrotatedN_loadings %>%
  mutate(mc = mc_both) %>%
  arrange(desc(PC4)) %>%
  select(PC4, mc, mc_cat)
pca_both_unrotatedN_pc4

# run pca with varimax rotation with n factors (as determined above)
pca_both_varimax <- principal(d_both, nfactors = nfactors_both, rotate = "varimax")
pca_both_varimax

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

# get condition by subject
pca_both_varimax_scores <- pca_both_varimax$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  mutate(target = factor(ifelse(grepl("robot", subid), "robot", "beetle")))

p_both_varimax_scores_12 <- ggplot(aes(x = PC1, y = PC2, color = target),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC1 vs. PC2 (from rotated PCA)\n",
       x = "\nPC1",
       y = "PC2\n")

p_both_varimax_scores_13 <- ggplot(aes(x = PC1, y = PC3, color = target),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC1 vs. PC3 (from rotated PCA)\n",
       x = "\nPC1",
       y = "PC3\n")

p_both_varimax_scores_14 <- ggplot(aes(x = PC1, y = PC4, color = target),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC1 vs. PC4 (from rotated PCA)\n",
       x = "\nPC1",
       y = "PC4\n")

p_both_varimax_scores_23 <- ggplot(aes(x = PC2, y = PC3, color = target),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC2 vs. PC3 (from rotated PCA)\n",
       x = "\nPC2",
       y = "PC3\n")

p_both_varimax_scores_24 <- ggplot(aes(x = PC2, y = PC4, color = target),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC2 vs. PC4 (from rotated PCA)\n",
       x = "\nPC2",
       y = "PC4\n")

p_both_varimax_scores_34 <- ggplot(aes(x = PC3, y = PC4, color = target),
                                   data = pca_both_varimax_scores) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Scores: PC3 vs. PC4 (from rotated PCA)\n",
       x = "\nPC3",
       y = "PC4\n")

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

# summarize ratings by target and mc
rating_sum <- d_long %>%
  group_by(target, mc) %>%
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

if(nfactors_both == 4) {
  # score!
  d_scored <- d_scoring_pre %>%
    mutate(PC1_score = PC1 * mean,
           PC2_score = PC2 * mean,
           PC3_score = PC3 * mean,
           PC4_score = PC4 * mean) %>%
    group_by(target) %>%
    summarise(PC1_sum = sum(PC1_score),
              PC2_sum = sum(PC2_score),
              PC3_sum = sum(PC3_score),
              PC4_sum = sum(PC4_score)) %>%
    data.frame()
  d_scored
  
  # plot scores
  
  d_scored_long <- d_scored %>%
    gather(PC, score, -target) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum", "PC3_sum", "PC4_sum"),
                              labels = c("PC1", "PC2", "PC3", "PC4")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = target, fill = target), 
                     data = d_scored_long) +
    geom_bar(stat = "identity", width = 0.8,
             position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
         x = "\nDimension",
         y = "Score\n")
  p_scored
} else if(nfactors_both == 3) {
  
  # score!
  d_scored <- d_scoring_pre %>%
    mutate(PC1_score = PC1 * mean,
           PC2_score = PC2 * mean,
           PC3_score = PC3 * mean) %>%
    group_by(target) %>%
    summarise(PC1_sum = sum(PC1_score),
              PC2_sum = sum(PC2_score),
              PC3_sum = sum(PC3_score)) %>%
    data.frame()
  d_scored
  
  # plot scores
  
  d_scored_long <- d_scored %>%
    gather(PC, score, -target) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum", "PC3_sum"),
                              labels = c("PC1", "PC2", "PC3")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = target, fill = target), 
                     data = d_scored_long) +
    geom_bar(stat = "identity", width = 0.8,
             position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
         x = "\nDimension",
         y = "Score\n")
  p_scored
}

