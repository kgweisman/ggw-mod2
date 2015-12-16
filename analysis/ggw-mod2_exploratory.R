# library(devtools)
# install_github("ggbiplot", "vqv")
library(ggbiplot)
library(psych)

# ROBOT ----------

# include gender
d_robot2 <- d_clean %>%
  filter(condition == "robot") %>% # filter by condition
  mutate(race_white_cat = 
           factor(ifelse(race_cat == "white", "white", "non-white")),
         religion_jc_cat = 
           factor(ifelse(religion_cat %in% c("christianity", "judaism"), 
                         "judeo-christian", 
                         ifelse(religion_cat == "none", 
                                "non_religious", "other_religious"))),
         age_cat =
           factor(ifelse(age_approx < 30, "young",
                         ifelse(age_approx >= 30, "old", NA))))

# run pca
pca_robot2 <- prcomp(d_robot2 %>% select(happy:pride),
                     center = T, scale = T)
pca_robot2
plot(pca_robot2, type = "l")
summary(pca_robot2)

# biplot!
g <- ggbiplot(pca_robot2, obs.scale = 1, var.scale = 1, 
              # groups = d_robot2$race_white_cat,
              # groups = d_robot2$religion_jc_cat,
              # groups = d_robot2$age_cat,
              ellipse = TRUE, 
              circle = TRUE) +
  theme_bw() +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  labs(title = "ROBOT: factor loadings (first 2 unrotated components)\n")
print(g)

# unit circle!
theta <- seq(0, 2*pi, length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle, aes(x, y)) + geom_path()

loadings <- data.frame(pca_robot2$rotation,
                       .names = row.names(pca_robot2$rotation))
p + geom_text(data = loadings, 
              mapping = aes(x = PC1, y = PC2, label = .names, colour = .names),
              size = 4) +
  coord_fixed(ratio = 1) +
  labs(x = "PC1", y = "PC2")

# BEETLE ----------

# include gender
d_beetle2 <- d_clean %>%
  filter(condition == "beetle") %>% # filter by condition
  mutate(race_white_cat = 
           factor(ifelse(race_cat == "white", "white", "non-white")),
         religion_jc_cat = 
           factor(ifelse(religion_cat %in% c("christianity", "judaism"), 
                         "judeo-christian", 
                         ifelse(religion_cat == "none", 
                                "non_religious", "other_religious"))),
         age_cat =
           factor(ifelse(age_approx < 30, "young",
                         ifelse(age_approx >= 30, "old", NA))))

# run pca
pca_beetle2 <- prcomp(d_beetle2 %>% select(happy:pride),
                     center = T, scale = T)
pca_beetle2
plot(pca_beetle2, type = "l")
summary(pca_beetle2)

# biplot!
g <- ggbiplot(pca_beetle2, obs.scale = 1, var.scale = 1, 
              # groups = d_beetle2$race_white_cat,
              # groups = d_beetle2$religion_jc_cat,
              # groups = d_beetle2$age_cat,
              ellipse = TRUE, 
              circle = TRUE) +
  theme_bw() +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  labs(title = "BEETLE: factor loadings (first 2 unrotated components)\n")
print(g)

# unit circle!
theta <- seq(0, 2*pi, length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle, aes(x, y)) + geom_path()

loadings <- data.frame(pca_beetle2$rotation,
                       .names = row.names(pca_beetle2$rotation))
p + geom_text(data = loadings, 
              mapping = aes(x = PC1, y = PC2, label = .names, colour = .names),
              size = 4) +
  coord_fixed(ratio = 1) +
  labs(x = "PC1", y = "PC2")
