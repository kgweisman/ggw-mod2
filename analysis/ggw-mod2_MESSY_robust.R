temp <- princomp(d4_all, covmat = MASS::cov.rob(d4_all))
summary(temp)
plot(temp)

temp_loadings <- temp$loadings[] %>%
  data.frame() %>%
  select(Comp.1:Comp.3) %>%
  add_rownames(var = "condition")

temp_Comp.1 <- temp_loadings %>%
  select(condition, Comp.1) %>%
  arrange(desc(Comp.1))

temp_Comp.2 <- temp_loadings %>%
  select(condition, Comp.2) %>%
  arrange(desc(Comp.2))

temp_Comp.3 <- temp_loadings %>%
  select(condition, Comp.3) %>%
  arrange(desc(Comp.3))

temp_loadings_sorted <- list(temp_Comp.1, temp_Comp.2, temp_Comp.3)
temp_loadings_sorted

temp_scores <- temp$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  full_join(d4 %>% select(condition, subid))

temp_meanscores_Comp.1 <- 
  multi_boot(temp_scores,
             column = "Comp.1",
             summary_groups = "condition",
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

temp_meanscores_Comp.2 <- 
  multi_boot(temp_scores,
             column = "Comp.2",
             summary_groups = "condition",
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

temp_meanscores_Comp.3 <- 
  multi_boot(temp_scores,
             column = "Comp.3",
             summary_groups = "condition",
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

temp_meanscores <-
  full_join(temp_meanscores_Comp.1 %>%
              rename(Comp.1_ci_lower = ci_lower,
                     Comp.1_mean = mean,
                     Comp.1_ci_upper = ci_upper),
            temp_meanscores_Comp.2 %>%
              rename(Comp.2_ci_lower = ci_lower,
                     Comp.2_mean = mean,
                     Comp.2_ci_upper = ci_upper)) %>%
  full_join(temp_meanscores_Comp.3 %>%
              rename(Comp.3_ci_lower = ci_lower,
                     Comp.3_mean = mean,
                     Comp.3_ci_upper = ci_upper))

ggplot(aes(x = Comp.1_mean, y = Comp.2_mean, colour = condition, label = condition), 
       data = temp_meanscores) +
  scale_x_reverse() + # reverse-code Comp.1
  geom_errorbar(aes(ymin = Comp.2_ci_lower, ymax = Comp.2_ci_upper),
                width = 0.01, alpha = 0.3) +
  geom_errorbarh(aes(xmin = Comp.1_ci_lower, xmax = Comp.1_ci_upper),
                 height = 0.01, alpha = 0.3) +
  geom_point(size = 4) +
  # geom_dl(aes(label = condition), method = "smart.grid") +
  geom_text(aes(y = Comp.2_mean + 0.05)) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  # scale_x_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  # scale_y_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  labs(title = "Study 4: Average factor scores by target character (Comp.1 vs. Comp.2)\n",
       x = "\nAverage score: Comp.1",
       y = "Average score: Comp.2\n")

ggplot(aes(x = Comp.1_mean, y = Comp.3_mean, colour = condition, label = condition), 
       data = temp_meanscores) +
  scale_x_reverse() + # reverse-code Comp.1
  geom_errorbar(aes(ymin = Comp.3_ci_lower, ymax = Comp.3_ci_upper),
                width = 0.01, alpha = 0.3) +
  geom_errorbarh(aes(xmin = Comp.1_ci_lower, xmax = Comp.1_ci_upper),
                 height = 0.01, alpha = 0.3) +
  geom_point(size = 4) +
  # geom_dl(aes(label = condition), method = "smart.grid") +
  geom_text(aes(y = Comp.3_mean + 0.05)) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  # scale_x_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  # scale_y_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  labs(title = "Study 4: Average factor scores by target character (Comp.1 vs. Comp.3)\n",
       x = "\nAverage score: Comp.1",
       y = "Average score: Comp.3\n")

ggplot(aes(x = Comp.2_mean, y = Comp.3_mean, colour = condition, label = condition), 
       data = temp_meanscores) +
  geom_errorbar(aes(ymin = Comp.3_ci_lower, ymax = Comp.3_ci_upper),
                width = 0.01, alpha = 0.3) +
  geom_errorbarh(aes(xmin = Comp.2_ci_lower, xmax = Comp.2_ci_upper),
                 height = 0.01, alpha = 0.3) +
  geom_point(size = 4) +
  # geom_dl(aes(label = condition), method = "smart.grid") +
  geom_text(aes(y = Comp.3_mean + 0.05)) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  # scale_x_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  # scale_y_continuous(limits = c(-3.6, 3.6), breaks = seq(-3, 3, 0.5)) +
  labs(title = "Study 4: Average factor scores by target character (Comp.2 vs. Comp.3)\n",
       x = "\nAverage score: Comp.2",
       y = "Average score: Comp.3\n")