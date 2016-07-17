# need to run gww-mod2.Rmd to get d4_all

# scores: regression ----------------------------------------------------------
efa_d4_all_rotatedN_regression <- 
  fa(d4_all, nfactors = 3, rotate = "varimax", cor = chosenCorType,
     fm = "minres", scores = "regression")

# make dataframe for plotting
efa_d4_all_rotatedN_regression_scores <- efa_d4_all_rotatedN_regression$scores %>%
  data.frame() %>%
  tibble::rownames_to_column(var = "subid") %>%
  full_join(d4 %>% dplyr::select(subid, condition)) %>%
  dplyr::select(condition, subid, MR1, MR2, MR3) #%>%
# mutate(MR1_rescale = scales::rescale(MR1, to = c(0, 1)),
#        MR2_rescale = scales::rescale(MR2, to = c(0, 1)),
#        MR3_rescale = scales::rescale(MR3, to = c(0, 1)))

efa_d4_all_rotatedN_regression_scores_F1 <- efa_d4_all_rotatedN_regression_scores %>%
  multi_boot(column = "MR1",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F1_ci_lower = ci_lower_na, F1_mean = mean_na, F1_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_regression_scores_F2 <- efa_d4_all_rotatedN_regression_scores %>%
  multi_boot(column = "MR2",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F2_ci_lower = ci_lower_na, F2_mean = mean_na, F2_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_regression_scores_F3 <- efa_d4_all_rotatedN_regression_scores %>%
  multi_boot(column = "MR3",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F3_ci_lower = ci_lower_na, F3_mean = mean_na, F3_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_regression_scores <- efa_d4_all_rotatedN_regression_scores_F1 %>%
  full_join(efa_d4_all_rotatedN_regression_scores_F2) %>%
  full_join(efa_d4_all_rotatedN_regression_scores_F3)

# remove extraneous variables
rm(efa_d4_all_rotatedN_regression_scores_F1, efa_d4_all_rotatedN_regression_scores_F2, 
   efa_d4_all_rotatedN_regression_scores_F3)

d4_F1F2_regression <- 
  ggplot(aes(x = F1_mean, y = F2_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_regression_scores) +
  geom_errorbar(aes(ymin = F2_ci_lower, ymax = F2_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: regression")
d4_F1F2_regression

d4_F1F3_regression <- 
  ggplot(aes(x = F1_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_regression_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: regression")
d4_F1F3_regression

d4_F2F3_regression <- 
  ggplot(aes(x = F2_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_regression_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F2_ci_lower, xmax = F2_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: regression")
d4_F2F3_regression

# scores: Thurstone ----------------------------------------------------------
efa_d4_all_rotatedN_Thurstone <- 
  fa(d4_all, nfactors = 3, rotate = "varimax", cor = chosenCorType,
     fm = "minres", scores = "Thurstone")

# make dataframe for plotting
efa_d4_all_rotatedN_Thurstone_scores <- efa_d4_all_rotatedN_Thurstone$scores %>%
  data.frame() %>%
  tibble::rownames_to_column(var = "subid") %>%
  full_join(d4 %>% dplyr::select(subid, condition)) %>%
  dplyr::select(condition, subid, MR1, MR2, MR3) #%>%
# mutate(MR1_rescale = scales::rescale(MR1, to = c(0, 1)),
#        MR2_rescale = scales::rescale(MR2, to = c(0, 1)),
#        MR3_rescale = scales::rescale(MR3, to = c(0, 1)))

efa_d4_all_rotatedN_Thurstone_scores_F1 <- efa_d4_all_rotatedN_Thurstone_scores %>%
  multi_boot(column = "MR1",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F1_ci_lower = ci_lower_na, F1_mean = mean_na, F1_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Thurstone_scores_F2 <- efa_d4_all_rotatedN_Thurstone_scores %>%
  multi_boot(column = "MR2",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F2_ci_lower = ci_lower_na, F2_mean = mean_na, F2_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Thurstone_scores_F3 <- efa_d4_all_rotatedN_Thurstone_scores %>%
  multi_boot(column = "MR3",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F3_ci_lower = ci_lower_na, F3_mean = mean_na, F3_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Thurstone_scores <- efa_d4_all_rotatedN_Thurstone_scores_F1 %>%
  full_join(efa_d4_all_rotatedN_Thurstone_scores_F2) %>%
  full_join(efa_d4_all_rotatedN_Thurstone_scores_F3)

# remove extraneous variables
rm(efa_d4_all_rotatedN_Thurstone_scores_F1, efa_d4_all_rotatedN_Thurstone_scores_F2, 
   efa_d4_all_rotatedN_Thurstone_scores_F3)

d4_F1F2_Thurstone <- 
  ggplot(aes(x = F1_mean, y = F2_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Thurstone_scores) +
  geom_errorbar(aes(ymin = F2_ci_lower, ymax = F2_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Thurstone")
d4_F1F2_Thurstone

d4_F1F3_Thurstone <- 
  ggplot(aes(x = F1_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Thurstone_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Thurstone")
d4_F1F3_Thurstone

d4_F2F3_Thurstone <- 
  ggplot(aes(x = F2_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Thurstone_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F2_ci_lower, xmax = F2_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Thurstone")
d4_F2F3_Thurstone

# scores: tenBerge ----------------------------------------------------------
efa_d4_all_rotatedN_tenBerge <- 
  fa(d4_all, nfactors = 3, rotate = "varimax", cor = chosenCorType,
     fm = "minres", scores = "tenBerge")

# make dataframe for plotting
efa_d4_all_rotatedN_tenBerge_scores <- efa_d4_all_rotatedN_tenBerge$scores %>%
  data.frame() %>%
  tibble::rownames_to_column(var = "subid") %>%
  full_join(d4 %>% dplyr::select(subid, condition)) %>%
  dplyr::select(condition, subid, MR1, MR2, MR3) #%>%
# mutate(MR1_rescale = scales::rescale(MR1, to = c(0, 1)),
#        MR2_rescale = scales::rescale(MR2, to = c(0, 1)),
#        MR3_rescale = scales::rescale(MR3, to = c(0, 1)))

efa_d4_all_rotatedN_tenBerge_scores_F1 <- efa_d4_all_rotatedN_tenBerge_scores %>%
  multi_boot(column = "MR1",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F1_ci_lower = ci_lower_na, F1_mean = mean_na, F1_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_tenBerge_scores_F2 <- efa_d4_all_rotatedN_tenBerge_scores %>%
  multi_boot(column = "MR2",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F2_ci_lower = ci_lower_na, F2_mean = mean_na, F2_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_tenBerge_scores_F3 <- efa_d4_all_rotatedN_tenBerge_scores %>%
  multi_boot(column = "MR3",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F3_ci_lower = ci_lower_na, F3_mean = mean_na, F3_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_tenBerge_scores <- efa_d4_all_rotatedN_tenBerge_scores_F1 %>%
  full_join(efa_d4_all_rotatedN_tenBerge_scores_F2) %>%
  full_join(efa_d4_all_rotatedN_tenBerge_scores_F3)

# remove extraneous variables
rm(efa_d4_all_rotatedN_tenBerge_scores_F1, efa_d4_all_rotatedN_tenBerge_scores_F2, 
   efa_d4_all_rotatedN_tenBerge_scores_F3)

d4_F1F2_tenBerge <- 
  ggplot(aes(x = F1_mean, y = F2_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_tenBerge_scores) +
  geom_errorbar(aes(ymin = F2_ci_lower, ymax = F2_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: tenBerge")
d4_F1F2_tenBerge

d4_F1F3_tenBerge <- 
  ggplot(aes(x = F1_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_tenBerge_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: tenBerge")
d4_F1F3_tenBerge

d4_F2F3_tenBerge <- 
  ggplot(aes(x = F2_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_tenBerge_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F2_ci_lower, xmax = F2_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: tenBerge")
d4_F2F3_tenBerge

# scores: Anderson ----------------------------------------------------------
efa_d4_all_rotatedN_Anderson <- 
  fa(d4_all, nfactors = 3, rotate = "varimax", cor = chosenCorType,
     fm = "minres", scores = "Anderson")

# make dataframe for plotting
efa_d4_all_rotatedN_Anderson_scores <- efa_d4_all_rotatedN_Anderson$scores %>%
  data.frame() %>%
  tibble::rownames_to_column(var = "subid") %>%
  full_join(d4 %>% dplyr::select(subid, condition)) %>%
  dplyr::select(condition, subid, MR1, MR2, MR3) #%>%
# mutate(MR1_rescale = scales::rescale(MR1, to = c(0, 1)),
#        MR2_rescale = scales::rescale(MR2, to = c(0, 1)),
#        MR3_rescale = scales::rescale(MR3, to = c(0, 1)))

efa_d4_all_rotatedN_Anderson_scores_F1 <- efa_d4_all_rotatedN_Anderson_scores %>%
  multi_boot(column = "MR1",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F1_ci_lower = ci_lower_na, F1_mean = mean_na, F1_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Anderson_scores_F2 <- efa_d4_all_rotatedN_Anderson_scores %>%
  multi_boot(column = "MR2",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F2_ci_lower = ci_lower_na, F2_mean = mean_na, F2_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Anderson_scores_F3 <- efa_d4_all_rotatedN_Anderson_scores %>%
  multi_boot(column = "MR3",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F3_ci_lower = ci_lower_na, F3_mean = mean_na, F3_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Anderson_scores <- efa_d4_all_rotatedN_Anderson_scores_F1 %>%
  full_join(efa_d4_all_rotatedN_Anderson_scores_F2) %>%
  full_join(efa_d4_all_rotatedN_Anderson_scores_F3)

# remove extraneous variables
rm(efa_d4_all_rotatedN_Anderson_scores_F1, efa_d4_all_rotatedN_Anderson_scores_F2, 
   efa_d4_all_rotatedN_Anderson_scores_F3)

d4_F1F2_Anderson <- 
  ggplot(aes(x = F1_mean, y = F2_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Anderson_scores) +
  geom_errorbar(aes(ymin = F2_ci_lower, ymax = F2_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Anderson")
d4_F1F2_Anderson

d4_F1F3_Anderson <- 
  ggplot(aes(x = F1_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Anderson_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Anderson")
d4_F1F3_Anderson

d4_F2F3_Anderson <- 
  ggplot(aes(x = F2_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Anderson_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F2_ci_lower, xmax = F2_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Anderson")
d4_F2F3_Anderson

# scores: Bartlett ----------------------------------------------------------
efa_d4_all_rotatedN_Bartlett <- 
  fa(d4_all, nfactors = 3, rotate = "varimax", cor = chosenCorType,
     fm = "minres", scores = "Bartlett")

# make dataframe for plotting
efa_d4_all_rotatedN_Bartlett_scores <- efa_d4_all_rotatedN_Bartlett$scores %>%
  data.frame() %>%
  tibble::rownames_to_column(var = "subid") %>%
  full_join(d4 %>% dplyr::select(subid, condition)) %>%
  dplyr::select(condition, subid, MR1, MR2, MR3) #%>%
# mutate(MR1_rescale = scales::rescale(MR1, to = c(0, 1)),
#        MR2_rescale = scales::rescale(MR2, to = c(0, 1)),
#        MR3_rescale = scales::rescale(MR3, to = c(0, 1)))

efa_d4_all_rotatedN_Bartlett_scores_F1 <- efa_d4_all_rotatedN_Bartlett_scores %>%
  multi_boot(column = "MR1",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F1_ci_lower = ci_lower_na, F1_mean = mean_na, F1_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Bartlett_scores_F2 <- efa_d4_all_rotatedN_Bartlett_scores %>%
  multi_boot(column = "MR2",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F2_ci_lower = ci_lower_na, F2_mean = mean_na, F2_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Bartlett_scores_F3 <- efa_d4_all_rotatedN_Bartlett_scores %>%
  multi_boot(column = "MR3",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(F3_ci_lower = ci_lower_na, F3_mean = mean_na, F3_ci_upper = ci_upper_na)

efa_d4_all_rotatedN_Bartlett_scores <- efa_d4_all_rotatedN_Bartlett_scores_F1 %>%
  full_join(efa_d4_all_rotatedN_Bartlett_scores_F2) %>%
  full_join(efa_d4_all_rotatedN_Bartlett_scores_F3)

# remove extraneous variables
rm(efa_d4_all_rotatedN_Bartlett_scores_F1, efa_d4_all_rotatedN_Bartlett_scores_F2, 
   efa_d4_all_rotatedN_Bartlett_scores_F3)

d4_F1F2_Bartlett <- 
  ggplot(aes(x = F1_mean, y = F2_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Bartlett_scores) +
  geom_errorbar(aes(ymin = F2_ci_lower, ymax = F2_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Bartlett")
d4_F1F2_Bartlett

d4_F1F3_Bartlett <- 
  ggplot(aes(x = F1_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Bartlett_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F1_ci_lower, xmax = F1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Bartlett")
d4_F1F3_Bartlett

d4_F2F3_Bartlett <- 
  ggplot(aes(x = F2_mean, y = F3_mean,
             label = condition, colour = condition),
         data = efa_d4_all_rotatedN_Bartlett_scores) +
  geom_errorbar(aes(ymin = F3_ci_lower, ymax = F3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = F2_ci_lower, xmax = F2_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Factor scores: Bartlett")
d4_F2F3_Bartlett

# scores: PCA regression ------------------------------------------------------
pca_d4_all_rotatedN_regression <- 
  principal(d4_all, nfactors = 3, rotate = "varimax", method = "regression")

# make dataframe for plotting
pca_d4_all_rotatedN_regression_scores <- pca_d4_all_rotatedN_regression$scores %>%
  data.frame() %>%
  tibble::rownames_to_column(var = "subid") %>%
  full_join(d4 %>% dplyr::select(subid, condition)) %>%
  dplyr::select(condition, subid, RC1, RC2, RC3) #%>%
# mutate(RC1_rescale = scales::rescale(RC1, to = c(0, 1)),
#        RC2_rescale = scales::rescale(RC2, to = c(0, 1)),
#        RC3_rescale = scales::rescale(RC3, to = c(0, 1)))

pca_d4_all_rotatedN_regression_scores_C1 <- pca_d4_all_rotatedN_regression_scores %>%
  multi_boot(column = "RC1",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(C1_ci_lower = ci_lower_na, C1_mean = mean_na, C1_ci_upper = ci_upper_na)

pca_d4_all_rotatedN_regression_scores_C2 <- pca_d4_all_rotatedN_regression_scores %>%
  multi_boot(column = "RC2",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(C2_ci_lower = ci_lower_na, C2_mean = mean_na, C2_ci_upper = ci_upper_na)

pca_d4_all_rotatedN_regression_scores_C3 <- pca_d4_all_rotatedN_regression_scores %>%
  multi_boot(column = "RC3",
             summary_groups = "condition",
             summary_function = "mean_na",
             statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na")) %>%
  rename(C3_ci_lower = ci_lower_na, C3_mean = mean_na, C3_ci_upper = ci_upper_na)

pca_d4_all_rotatedN_regression_scores <- pca_d4_all_rotatedN_regression_scores_C1 %>%
  full_join(pca_d4_all_rotatedN_regression_scores_C2) %>%
  full_join(pca_d4_all_rotatedN_regression_scores_C3)

# remove extraneous variables
rm(pca_d4_all_rotatedN_regression_scores_C1, pca_d4_all_rotatedN_regression_scores_C2, 
   pca_d4_all_rotatedN_regression_scores_C3)

d4_pca_C1C2_regression <- 
  ggplot(aes(x = C1_mean, y = C2_mean,
             label = condition, colour = condition),
         data = pca_d4_all_rotatedN_regression_scores) +
  geom_errorbar(aes(ymin = C2_ci_lower, ymax = C2_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = C1_ci_lower, xmax = C1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Component scores: regression")
d4_pca_C1C2_regression

d4_pca_C1C3_regression <- 
  ggplot(aes(x = C1_mean, y = C3_mean,
             label = condition, colour = condition),
         data = pca_d4_all_rotatedN_regression_scores) +
  geom_errorbar(aes(ymin = C3_ci_lower, ymax = C3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = C1_ci_lower, xmax = C1_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Component scores: regression")
d4_pca_C1C3_regression

d4_pca_C2C3_regression <- 
  ggplot(aes(x = C2_mean, y = C3_mean,
             label = condition, colour = condition),
         data = pca_d4_all_rotatedN_regression_scores) +
  geom_errorbar(aes(ymin = C3_ci_lower, ymax = C3_ci_upper), 
                width = 0.01, alpha = 0.3, size = 0.5) +
  geom_errorbarh(aes(xmin = C2_ci_lower, xmax = C2_ci_upper),
                 height = 0.01, alpha = 0.3, size = 0.5) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = condition), size = 6, segment.size = 0) +
  theme_bw() +
  scale_color_manual(values = tol21rainbow) +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Component scores: regression")
d4_pca_C2C3_regression
