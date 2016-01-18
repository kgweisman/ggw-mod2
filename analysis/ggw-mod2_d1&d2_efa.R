temp <- full_join(d1_all, d2_all)

howManyFactors(temp)
nfactors_temp <- 3

# run EFA without rotation with N factors
efa_temp_unrotatedN <- fa(temp, nfactors_temp, 
                               rotate = "none", cor = "cor", fm = "minres")
print(efa_temp_unrotatedN)

# get loadings for each factor
efa_temp_unrotatedN_loadings <- getFactorLoadings(efa_temp_unrotatedN)

# run EFA with rotation with N factors
efa_temp_rotatedN <- fa(temp, nfactors_temp, 
                             rotate = "oblimin", cor = "cor", fm = "minres")
print(efa_temp_rotatedN)

# get loadings for each factor
efa_temp_rotatedN_loadings <- getFactorLoadings(efa_temp_rotatedN)

# examine factor 1
kable(efa_temp_rotatedN_loadings[[1]], 
      align = "l", caption = "Study 1, BEETLE condition: Rotated Factor 1")

# examine factor 2
kable(efa_temp_rotatedN_loadings[[2]], 
      align = "l", caption = "Study 1, BEETLE condition: Rotated Factor 2")

# examine factor 3
kable(efa_temp_rotatedN_loadings[[3]], 
      align = "l", caption = "Study 1, BEETLE condition: Rotated Factor 3")
