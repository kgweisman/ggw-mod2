# first run bodymindsoul_analysis.Rmd

# STUDY 1 ------
# select only items from GGW2007
d1_ggw <- d1_all %>%
  select(hungry, fear, pain, pleasure, angry, desires,
         personality, conscious, pride, embarrassed, joy,
         self_restraint, morality, remembering, emo_recog, intentions,
         communicating, thoughts)

# examine scree plot
scree(d1_ggw)

# run EFA without rotation with N factors
d1_ggw_unrotated <- fa(d1_ggw, 6, rotate = "none",
                     cor = chosenCorType, fm = "minres")
print(fa.sort(efa_d1_all_unrotated))

d1_ggw_nfactors <- 3

# run EFA with rotation with N factors
d1_ggw_rotated <- fa(d1_ggw, d1_ggw_nfactors, rotate = "varimax",
                   cor = chosenCorType, fm = "minres")
print(fa.sort(d1_ggw_rotated))

# STUDY 2 ------
# select only items from GGW2007
d2_ggw <- d2_all %>%
  select(hungry, fear, pain, pleasure, angry, desires,
         personality, conscious, pride, embarrassed, joy,
         self_restraint, morality, remembering, emo_recog, intentions,
         communicating, thoughts)

# examine scree plot
scree(d2_ggw)

# run EFA without rotation with N factors
d2_ggw_unrotated <- fa(d2_ggw, 6, rotate = "none",
                       cor = chosenCorType, fm = "minres")
print(fa.sort(efa_d2_all_unrotated))

d2_ggw_nfactors <- 3

# run EFA with rotation with N factors
d2_ggw_rotated <- fa(d2_ggw, d2_ggw_nfactors, rotate = "varimax",
                     cor = chosenCorType, fm = "minres")
print(fa.sort(d2_ggw_rotated))

# STUDY 3 ------
# select only items from GGW2007
d3_ggw <- d3_all %>%
  select(hungry, fear, pain, pleasure, angry, desires,
         personality, conscious, pride, embarrassed, joy,
         self_restraint, morality, remembering, emo_recog, intentions,
         communicating, thoughts)

# examine scree plot
scree(d3_ggw)

# run EFA without rotation with N factors
d3_ggw_unrotated <- fa(d3_ggw, 6, rotate = "none",
                       cor = chosenCorType, fm = "minres")
print(fa.sort(efa_d3_all_unrotated))

d3_ggw_nfactors <- 3

# run EFA with rotation with N factors
d3_ggw_rotated <- fa(d3_ggw, d3_ggw_nfactors, rotate = "varimax",
                     cor = chosenCorType, fm = "minres")
print(fa.sort(d3_ggw_rotated))

# STUDY 4 ------
# select only items from GGW2007
d4_ggw <- d4_all %>%
  select(hungry, fear, pain, pleasure, angry, desires,
         personality, conscious, pride, embarrassed, joy,
         self_restraint, morality, remembering, emo_recog, intentions,
         communicating, thoughts)

# examine scree plot
scree(d4_ggw)

# run EFA without rotation with N factors
d4_ggw_unrotated <- fa(d4_ggw, 6, rotate = "none",
                       cor = chosenCorType, fm = "minres")
print(fa.sort(efa_d4_all_unrotated))

d4_ggw_nfactors <- 3

# run EFA with rotation with N factors
d4_ggw_rotated <- fa(d4_ggw, d4_ggw_nfactors, rotate = "varimax",
                     cor = chosenCorType, fm = "minres")
print(fa.sort(d4_ggw_rotated))



