# PCA: ANIMATE condition ----------------------------------------------------

d_animate <- d_clean %>%
  filter(condition != "stapler",
         condition != "computer",
         condition != "car",
         condition != "robot") %>%
  select(subid, happy:pride) # NOTE: make sure responses are scored as -3:3! 
d_animate <- data.frame(d_animate[,-1], row.names = d_animate[,1])

## step 1: determine how many dimensions to extract --------------------------

# use "very simple structure" criterion
# VSS(d_animate, n = 39, rotate = "none") # unrotated
# VSS(d_animate, n = 39, rotate = "varimax") # rotated
VSS.scree(d_animate) # scree plot

# run unrotated pca with maximum number of dimensions
pca_animate_unrotated <- principal(d_animate, nfactors = 39, rotate = "none")
pca_animate_unrotated
pca_animate_unrotated$values[pca_animate_unrotated$values > 1] # examine eignenvalues > 1

# run roated pca with maximum number of dimensions
pca_animate_rotated <- principal(d_animate, nfactors = 39, rotate = "varimax")
pca_animate_rotated
pca_animate_rotated$values[pca_animate_rotated$values > 1] # examine eignenvalues > 1

# set number of dimensions to extract (manuanimatey)
nfactors_animate <- 2

## step 2: run pca without rotation with N dimensions ------------------------

# run unrotated pca with n dimensions
pca_animate_unrotatedN <- principal(d_animate, nfactors = nfactors_animate, rotate = "none")
pca_animate_unrotatedN

# plot mental capacities in first two dimensions
pca_animate_unrotatedN_loadings <- 
  data.frame(pca_animate_unrotatedN$loadings[1:40, 1:nfactors_animate],
             row.names = rownames(pca_animate_unrotatedN$loadings[1:40, 1:nfactors_animate]))

# code a priori mental capacity categories
pca_animate_unrotatedN_loadings[c("hungry", "tired", "pain", 
                              "nauseated", "safe"),
                            "mc_cat"] <- "biological"
pca_animate_unrotatedN_loadings[c("happy", "depressed", "fear", 
                              "angry", "calm", "joy"),
                            "mc_cat"] <- "affective"
pca_animate_unrotatedN_loadings[c("sounds", "seeing", "temperature", 
                              "odors", "depth"),
                            "mc_cat"] <- "perceptual"
pca_animate_unrotatedN_loadings[c("computations", "thoughts", "reasoning", 
                              "remembering", "beliefs"),
                            "mc_cat"] <- "cognitive"
pca_animate_unrotatedN_loadings[c("free_will", "choices", "self_restraint", 
                              "intentions", "goal"),
                            "mc_cat"] <- "autonomous"
pca_animate_unrotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                              "disrespected", "embarrassed", "emo_recog"),
                            "mc_cat"] <- "social"
pca_animate_unrotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                              "desires", "morality", "personality", "pride"),
                            "mc_cat"] <- "other"

pca_animate_unrotatedN_loadings$mc_cat <- factor(pca_animate_unrotatedN_loadings$mc_cat)

# pca_animate_unrotatedN_plot12 <- 
#   ggplot(pca_animate_unrotatedN_loadings,
#          aes(x = PC1, y = PC2,
#              label = rownames(pca_animate_unrotatedN_loadings),
#              color = mc_cat)) +
#   geom_text(hjust = 0.5, vjust = 0.5) +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_color_brewer(type = "qual", palette = 2) +
#   labs(title = "ANIMATE: factor loadings (first 2 unrotated components)\n",
#        x = "\nPrincipal Component 1",
#        y = "Principal Component 2\n")
# pca_animate_unrotatedN_plot12

# examine loadings
mc_animate_unrotatedN = rownames(pca_animate_unrotatedN_loadings)

# ... for PC1
pca_animate_unrotatedN_pc1 <- pca_animate_unrotatedN_loadings %>%
  mutate(mc = mc_animate_unrotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_animate_unrotatedN_pc1

# ... for PC2
pca_animate_unrotatedN_pc2 <- pca_animate_unrotatedN_loadings %>%
  mutate(mc = mc_animate_unrotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_animate_unrotatedN_pc2

# ... for PC3
if(nfactors_animate > 2) {
  pca_animate_unrotatedN_pc3 <- pca_animate_unrotatedN_loadings %>%
    mutate(mc = mc_animate_unrotatedN) %>%
    arrange(desc(PC3)) %>%
    select(PC3, mc, mc_cat)
  pca_animate_unrotatedN_pc3
}

# ... for PC4
if(nfactors_animate > 3) {
  pca_animate_unrotatedN_pc4 <- pca_animate_unrotatedN_loadings %>%
    mutate(mc = mc_animate_unrotatedN) %>%
    arrange(desc(PC4)) %>%
    select(PC4, mc, mc_cat)
  pca_animate_unrotatedN_pc4
}

## step 3: run pca with varimax rotation with N dimensions -------------------

# run pca with n dimensions with varimax rotation
pca_animate_rotatedN <- principal(d_animate, nfactors = nfactors_animate, 
                              rotate = "varimax")
pca_animate_rotatedN

# plot mental capacities in first two dimensions
pca_animate_rotatedN_loadings <- 
  data.frame(pca_animate_rotatedN$loadings[1:40, 1:nfactors_animate],
             row.names = rownames(pca_animate_rotatedN$loadings[1:40, 1:nfactors_animate]))

# code a priori mental capacity categories
pca_animate_rotatedN_loadings[c("hungry", "tired", "pain", 
                            "nauseated", "safe"),
                          "mc_cat"] <- "biological"
pca_animate_rotatedN_loadings[c("happy", "depressed", "fear", 
                            "angry", "calm", "joy"),
                          "mc_cat"] <- "affective"
pca_animate_rotatedN_loadings[c("sounds", "seeing", "temperature", 
                            "odors", "depth"),
                          "mc_cat"] <- "perceptual"
pca_animate_rotatedN_loadings[c("computations", "thoughts", "reasoning", 
                            "remembering", "beliefs"),
                          "mc_cat"] <- "cognitive"
pca_animate_rotatedN_loadings[c("free_will", "choices", "self_restraint", 
                            "intentions", "goal"),
                          "mc_cat"] <- "autonomous"
pca_animate_rotatedN_loadings[c("love", "recognizing", "communicating", "guilt", 
                            "disrespected", "embarrassed", "emo_recog"),
                          "mc_cat"] <- "social"
pca_animate_rotatedN_loadings[c("conscious", "self_aware", "pleasure", 
                            "desires", "morality", "personality", "pride"),
                          "mc_cat"] <- "other"

pca_animate_rotatedN_loadings$mc_cat <- factor(pca_animate_rotatedN_loadings$mc_cat)

# pca_animate_rotatedN_plot12 <- 
#   ggplot(pca_animate_rotatedN_loadings,
#          aes(x = PC1, y = PC2,
#              label = rownames(pca_animate_rotatedN_loadings),
#              color = mc_cat)) +
#   geom_text(hjust = 0.5, vjust = 0.5) +
#   theme_bw() +
#   theme(text = element_text(size = 12)) +
#   scale_color_brewer(type = "qual", palette = 2) +
#   labs(title = "ANIMATE: factor loadings (first 2 rotated components)\n",
#        x = "\nPrincipal Component 1",
#        y = "Principal Component 2\n")
# pca_animate_rotatedN_plot12

# examine loadings
mc_animate_rotatedN = rownames(pca_animate_rotatedN_loadings)

# ... for PC1
pca_animate_rotatedN_pc1 <- pca_animate_rotatedN_loadings %>%
  mutate(mc = mc_animate_rotatedN) %>%
  arrange(desc(PC1)) %>%
  select(PC1, mc, mc_cat)
pca_animate_rotatedN_pc1

# ... for PC2
pca_animate_rotatedN_pc2 <- pca_animate_rotatedN_loadings %>%
  mutate(mc = mc_animate_rotatedN) %>%
  arrange(desc(PC2)) %>%
  select(PC2, mc, mc_cat)
pca_animate_rotatedN_pc2

# ... for PC3
if(nfactors_animate > 2) {
  pca_animate_rotatedN_pc3 <- pca_animate_rotatedN_loadings %>%
    mutate(mc = mc_animate_rotatedN) %>%
    arrange(desc(PC3)) %>%
    select(PC3, mc, mc_cat)
  pca_animate_rotatedN_pc3
}

# ... for PC4
if(nfactors_animate > 3) {
  pca_animate_rotatedN_pc4 <- pca_animate_rotatedN_loadings %>%
    mutate(mc = mc_animate_rotatedN) %>%
    arrange(desc(PC4)) %>%
    select(PC4, mc, mc_cat)
  pca_animate_rotatedN_pc4
}

# PCA: ANIMATE condition, plot participants by component scores -----------------

# get condition by subject
condition_subid <- d_clean %>%
  select(subid, condition)

pca_animate_rotatedN_scores <- pca_animate_rotatedN$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  left_join(condition_subid, by = "subid") %>%
  gather(dimension, score, starts_with("PC"))

if(nfactors_animate == 3) {
  pca_animate_rotatedN_scores <- pca_animate_rotatedN_scores %>%
    mutate(dimension = factor(dimension,
                              levels = c("PC1", "PC2", "PC3"),
                              labels = c("PC1 (ANIMATE)", "PC2 (ANIMATE)", 
                                         "PC3 (ANIMATE)")))
} else if(nfactors_animate == 4) {
  pca_animate_rotatedN_scores <- pca_animate_rotatedN_scores %>%
    mutate(dimension = factor(dimension,
                              levels = c("PC1", "PC2", "PC3", "PC4"),
                              labels = c("PC1 (ANIMATE)", "PC2 (ANIMATE)", 
                                         "PC3 (ANIMATE)", "PC4 (ANIMATE)")))
}

p_animate_rotatedN_scores <- ggplot(aes(x = dimension, y = score, 
                                    color = dimension),
                                data = pca_animate_rotatedN_scores) +
  facet_wrap(~ condition, ncol = 7) +
  geom_hline(y = 0, lty = 3) +
  geom_jitter(size = 2) +
  geom_boxplot(width = 0.5, color = "black", alpha = 0,
               outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "ANIMATE Scores (from rotated PCA)\n",
       x = "\nDimension",
       y = "Score\n",
       color = "Dimension\n")
p_animate_rotatedN_scores

# EXPLORATORY: hand-calculate "scores" on dimensions for each condition? ---------

d_long_animate <- d_clean %>%
  filter(condition != "stapler",
         condition != "computer",
         condition != "car",
         condition != "robot") %>%
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
rating_sum_animate <- d_long_animate %>%
  mutate(rating = as.numeric(rating)) %>%
  group_by(condition, mc) %>%
  summarise(min = min(rating),
            max = max(rating),
            median = median(rating),
            mean = mean(rating),
            sd = sd(rating))
rating_sum_animate

# extract factor loadings
animate_PC <- cbind(mc = rownames(pca_animate_rotatedN_loadings),
                pca_animate_rotatedN_loadings)

# combine!
d_scoring_pre_animate <- full_join(rating_sum_animate, animate_PC)

if(nfactors_animate == 4) {
  # score!
  d_scored <- d_scoring_pre_animate %>%
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
  
  d_scored_long_animate <- d_scored %>%
    gather(PC, score, -condition) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum", "PC3_sum", "PC4_sum"),
                              labels = c("PC1", "PC2", "PC3", "PC4")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = condition, fill = dimension), 
                     data = d_scored_long_animate) +
    facet_wrap(~ condition, ncol = 7) +
    geom_bar(stat = "identity", width = 0.8,
             position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
         x = "\nDimension",
         y = "Score\n")
  p_scored
} else if(nfactors_animate == 3) {
  
  # score!
  d_scored <- d_scoring_pre_animate %>%
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
  
  d_scored_long_animate <- d_scored %>%
    gather(PC, score, -condition) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum", "PC3_sum"),
                              labels = c("PC1", "PC2", "PC3")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = condition, fill = dimension), 
                     data = d_scored_long_animate) +
    facet_wrap(~ condition, ncol = 7) +
    geom_bar(stat = "identity", width = 0.8,
             position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(title = "Hand-calculated scores by character\nfrom large PCA with varimax rotation\n",
         x = "\nDimension",
         y = "Score\n")
  p_scored
} else if(nfactors_animate == 2) {
  
  # score!
  d_scored <- d_scoring_pre_animate %>%
    mutate(PC1_score = PC1 * mean,
           PC2_score = PC2 * mean) %>%
    group_by(condition) %>%
    summarise(PC1_sum = sum(PC1_score),
              PC2_sum = sum(PC2_score)) %>%
    data.frame()
  d_scored
  
  # plot scores
  
  d_scored_long_animate <- d_scored %>%
    gather(PC, score, -condition) %>%
    mutate(dimension = factor(PC,
                              levels = c("PC1_sum", "PC2_sum"),
                              labels = c("PC1", "PC2")))
  
  p_scored <- ggplot(aes(x = dimension, y = score,
                         group = condition, fill = dimension), 
                     data = d_scored_long_animate) +
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

dist_animate <- dist(d_animate)
hclust_animate <- hclust(dist_animate)
ggdendrogram(hclust_animate)
autoplot(clara(d_animate, k = 3), frame = TRUE, frame.type = 'norm')
autoplot(fanny(d_animate, k = 3), frame = TRUE, frame.type = 'norm')


