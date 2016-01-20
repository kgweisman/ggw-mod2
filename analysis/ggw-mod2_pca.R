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
# d <- d4_all

# PCA

# run PCA without rotation with maximal factors
pca_d_PCA_unrotated <- principal(d, 39, rotate = "none", cor = "cor")
print(pca_d_PCA_unrotated)

# run PCA without rotation with N factors
pca_d_PCA_rotatedN <- principal(d, 2, rotate = "varimax", cor = "cor")
print(pca_d_PCA_rotatedN)

# get loadings for each factor
pca_d_PCA_rotatedN_loadings <- getFactorLoadings(pca_d_PCA_rotatedN)
print(pca_d_PCA_rotatedN_loadings)

# PCA

# run PCA without rotation with maximal factors
pca_d_PCA_unrotated <- principal(d, 39, rotate = "none", cor = "poly")
print(pca_d_PCA_unrotated)

# run PCA without rotation with N factors
pca_d_PCA_rotatedN <- principal(d, 4, rotate = "varimax", cor = "poly")
print(pca_d_PCA_rotatedN)

# get loadings for each factor
pca_d_PCA_rotatedN_loadings <- getFactorLoadings(pca_d_PCA_rotatedN)
print(pca_d_PCA_rotatedN_loadings)

# EFA (Pearson correlation)

# run EFA without rotation with maximal factors
pca_d_EFA_unrotated <- fa(d, 39, rotate = "none", cor = "cor")
print(pca_d_EFA_unrotated)

# run EFA without rotation with N factors
pca_d_EFA_rotatedN <- fa(d, 4, rotate = "optimin", cor = "cor")
print(pca_d_EFA_rotatedN)

# get loadings for each factor
pca_d_EFA_rotatedN_loadings <- getFactorLoadings(pca_d_EFA_rotatedN)
print(pca_d_EFA_rotatedN_loadings)

# EFA (Polychoric correlation)

# run EFA without rotation with maximal factors
pca_d_EFA_unrotated <- fa(d, 39, rotate = "none", cor = "poly")
print(pca_d_EFA_unrotated)

# run EFA without rotation with N factors
pca_d_EFA_rotatedN <- fa(d, 4, rotate = "varimax", cor = "poly")
print(pca_d_EFA_rotatedN)

# get loadings for each factor
pca_d_EFA_rotatedN_loadings <- getFactorLoadings(pca_d_EFA_rotatedN)
print(pca_d_EFA_rotatedN_loadings)
