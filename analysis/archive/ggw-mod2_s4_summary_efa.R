d4_targets <- d4 %>%
  select(condition, happy:pride) %>%
  group_by(condition) %>%
  summarise_each(funs(mean)) %>%
  ungroup()

d4_targets <- data.frame(d4_targets[,-1], row.names = d4_targets$condition)

# FA
d4_targets_fa_N <- fa(d4_targets, nfactors = 10, rotate = "none")
d4_targets_fa_N

d4_targets_fa_3 <- fa(d4_targets, nfactors = 3, rotate = "varimax")
d4_targets_fa_3

d4_targets_fa_3_loadings <- loadings(d4_targets_fa_3)[] %>%
  data.frame() %>% 
  add_rownames(var = "mc")

kable(data.frame(loadings(fa.sort(d4_targets_fa_3))[]), digits = 2)

# PCA
d4_targets_pca_N <- principal(d4_targets, nfactors = 10, rotate = "none")
d4_targets_pca_3 <- principal(d4_targets, nfactors = 3, rotate = "varimax")

d4_targets_pca_3_loadings <- loadings(d4_targets_pca_3)[] %>%
  data.frame() %>% 
  add_rownames(var = "mc")

kable(data.frame(loadings(fa.sort(d4_targets_pca_3))[]), digits = 2)
