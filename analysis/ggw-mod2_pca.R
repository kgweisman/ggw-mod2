# choose datasource
# d <- d1_beetle
# d <- d1_robot
# d <- d1_all
# d <- d2_beetle
# d <- d2_robot
# d <- d2_all
# d <- d3_beetle
# d <- d3_robot
# d <- d3_all
d <- d4_all

# # standardize?
# d_std <- d %>%
#   add_rownames(var = "subid") %>%
#   gather(mc, score, -subid) %>%
#   mutate(score = scale(score)) %>%
#   spread(mc, score)
# d_std <- data.frame(d_std[,-1], row.names = d_std[,1])
# 
# d <- d_std

# PCA

# run PCA without rotation with maximal factors
pca_d_PCA_unrotated <- principal(d, 39, rotate = "none", cor = "poly")
print(pca_d_PCA_unrotated)

# run PCA without rotation with N factors
pca_d_PCA_rotatedN <- principal(d, 3, rotate = "varimax", cor = "poly")
print(pca_d_PCA_rotatedN)

# get loadings for each factor
pca_d_PCA_rotatedN_loadings <- getFactorLoadings(pca_d_PCA_rotatedN)
print(pca_d_PCA_rotatedN_loadings)

# plot in first 2 dimensions

pca_d_PCA_rotatedN_scores <- pca_d_PCA_rotatedN$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  full_join(d4 %>% select(condition, subid))

pca_d_PCA_rotatedN_PC1_meanscores <- 
  multi_boot(pca_d_PCA_rotatedN_scores,
             column = "PC1",
             summary_groups = "condition",
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

pca_d_PCA_rotatedN_PC2_meanscores <- 
  multi_boot(pca_d_PCA_rotatedN_scores,
             column = "PC2",
             summary_groups = "condition",
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

pca_d_PCA_rotatedN_PC3_meanscores <- 
  multi_boot(pca_d_PCA_rotatedN_scores,
             column = "PC3",
             summary_groups = "condition",
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

pca_d_PCA_rotatedN_meanscores <-
  full_join(pca_d_PCA_rotatedN_PC1_meanscores %>%
              rename(PC1_ci_lower = ci_lower,
                     PC1_mean = mean,
                     PC1_ci_upper = ci_upper),
            pca_d_PCA_rotatedN_PC2_meanscores %>%
              rename(PC2_ci_lower = ci_lower,
                     PC2_mean = mean,
                     PC2_ci_upper = ci_upper)) %>%
  full_join(pca_d_PCA_rotatedN_PC3_meanscores %>%
              rename(PC3_ci_lower = ci_lower,
                     PC3_mean = mean,
                     PC3_ci_upper = ci_upper))
  
ggplot(aes(x = PC1_mean, y = PC2_mean, colour = condition, label = condition), 
       data = pca_d_PCA_rotatedN_meanscores) +
  geom_errorbar(aes(ymin = PC2_ci_lower, ymax = PC2_ci_upper),
                width = 0.01, alpha = 0.3) +
  geom_errorbarh(aes(xmin = PC1_ci_lower, xmax = PC1_ci_upper),
                 height = 0.01, alpha = 0.3) +
  geom_point(size = 4) +
  # geom_dl(aes(label = condition), method = "smart.grid") +
  geom_text(aes(y = PC2_mean + 0.5)) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  # scale_x_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  # scale_y_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  labs(title = "Study 4: Average factor scores by target character (PC1 vs. PC2)\n",
       x = "\nAverage score: PC1 (Physiological)",
       y = "Average score: PC2 (Social-emotional)\n")

ggplot(aes(x = PC1_mean, y = PC3_mean, colour = condition, label = condition), 
       data = pca_d_PCA_rotatedN_meanscores) +
  geom_errorbar(aes(ymin = PC3_ci_lower, ymax = PC3_ci_upper),
                width = 0.01, alpha = 0.3) +
  geom_errorbarh(aes(xmin = PC1_ci_lower, xmax = PC1_ci_upper),
                 height = 0.01, alpha = 0.3) +
  geom_point(size = 4) +
  # geom_dl(aes(label = condition), method = "smart.grid") +
  geom_text(aes(y = PC3_mean + 0.5)) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  # scale_x_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  # scale_y_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  labs(title = "Study 4: Average factor scores by target character (PC1 vs. PC3)\n",
       x = "\nAverage score: PC1 (Physiological)",
       y = "Average score: PC3 (Perceptual)\n")

ggplot(aes(x = PC2_mean, y = PC3_mean, colour = condition, label = condition), 
       data = pca_d_PCA_rotatedN_meanscores) +
  geom_errorbar(aes(ymin = PC3_ci_lower, ymax = PC3_ci_upper),
                width = 0.01, alpha = 0.3) +
  geom_errorbarh(aes(xmin = PC2_ci_lower, xmax = PC2_ci_upper),
                 height = 0.01, alpha = 0.3) +
  geom_point(size = 4) +
  # geom_dl(aes(label = condition), method = "smart.grid") +
  geom_text(aes(y = PC3_mean + 0.5)) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  # scale_x_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  # scale_y_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  labs(title = "Study 4: Average factor scores by target character (PC2 vs. PC3)\n",
       x = "\nAverage score: PC2 (Social-emotional)",
       y = "Average score: PC3 (Perceptual)\n")






#############################
# OTHER ANALYSIS OPTIONS

# # PCA
# 
# # run PCA without rotation with maximal factors
# pca_d_PCA_unrotated <- principal(d, 39, rotate = "none", cor = "cor")
# print(pca_d_PCA_unrotated)
# 
# # run PCA without rotation with N factors
# pca_d_PCA_rotatedN <- principal(d, 4, rotate = "varimax", cor = "cor")
# print(pca_d_PCA_rotatedN)
# 
# # get loadings for each factor
# pca_d_PCA_rotatedN_loadings <- getFactorLoadings(pca_d_PCA_rotatedN)
# print(pca_d_PCA_rotatedN_loadings)
# 
# # EFA (Pearson correlation)
# 
# # run EFA without rotation with maximal factors
# pca_d_EFA_unrotated <- fa(d, 39, rotate = "none", cor = "cor")
# print(pca_d_EFA_unrotated)
# 
# # run EFA without rotation with N factors
# pca_d_EFA_rotatedN <- fa(d, 4, rotate = "optimin", cor = "cor")
# print(pca_d_EFA_rotatedN)
# 
# # get loadings for each factor
# pca_d_EFA_rotatedN_loadings <- getFactorLoadings(pca_d_EFA_rotatedN)
# print(pca_d_EFA_rotatedN_loadings)
# 
# # EFA (Polychoric correlation)
# 
# # run EFA without rotation with maximal factors
# pca_d_EFA_unrotated <- fa(d, 39, rotate = "none", cor = "poly")
# print(pca_d_EFA_unrotated)
# 
# # run EFA without rotation with N factors
# pca_d_EFA_rotatedN <- fa(d, 4, rotate = "varimax", cor = "poly")
# print(pca_d_EFA_rotatedN)
# 
# # get loadings for each factor
# pca_d_EFA_rotatedN_loadings <- getFactorLoadings(pca_d_EFA_rotatedN)
# print(pca_d_EFA_rotatedN_loadings)
