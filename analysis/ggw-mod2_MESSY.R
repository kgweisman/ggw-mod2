# playing with factor analysis ----------------------------------

library(GPArotation)
# fa(d_all, nfactors = 3, n.iter = 1000)
fa <- fa(d_beetle, nfactors = 3, rotate = "varimax")

F1 <- data.frame(fa$loadings[,1]) %>%
  rename(F1 = fa.loadings...1.) %>%
  add_rownames() %>%
  arrange(desc(F1))
F1

F2 <- data.frame(fa$loadings[,2]) %>%
  rename(F2 = fa.loadings...2.) %>%
  add_rownames() %>%
  arrange(desc(F2))
F2 

F3 <- data.frame(fa$loadings[,3]) %>%
  rename(F3 = fa.loadings...3.) %>%
  add_rownames() %>%
  arrange(desc(F3))
F3

# playing with rank correlations and permutations -------------------------

compareItemOrders <- function(pc1, pc2, method = c("spearman", "kendall")) {
  pc1i <- as.integer(as.factor(pc1))
  pc2i <- as.integer(as.factor(pc2))
  
  if(method == "spearman") {
    Spearman(pc1i, pc2i, length(pc1i), length(pc2i), 40)
  } else if(method == "kendall") {
    cor.test(pc1i, pc2i, method == "kendall")
  }
  
}

footruleDist <- c(# beetle PC1 vs. robot PCs 1-4
  B1R1 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc1$mc, 
                                      pca_robot_rotatedN_pc1$mc, "spearman")),
  B1R2 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc1$mc, 
                                      pca_robot_rotatedN_pc2$mc, "spearman")),
  B1R3 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc1$mc, 
                                      pca_robot_rotatedN_pc3$mc, "spearman")),
  B1R4 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc1$mc, 
                                      pca_robot_rotatedN_pc4$mc, "spearman")),
  
  # beetle PC2 vs. robot PCs 1-4
  B2R1 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc2$mc, 
                                      pca_robot_rotatedN_pc1$mc, "spearman")),
  B2R2 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc2$mc, 
                                      pca_robot_rotatedN_pc2$mc, "spearman")),
  B2R3 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc2$mc, 
                                      pca_robot_rotatedN_pc3$mc, "spearman")),
  B2R4 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc2$mc, 
                                      pca_robot_rotatedN_pc4$mc, "spearman")),
  
  # beetle PC3 vs. robot PCs 1-4
  B3R1 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc3$mc, 
                                      pca_robot_rotatedN_pc1$mc, "spearman")),
  B3R2 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc3$mc, 
                                      pca_robot_rotatedN_pc2$mc, "spearman")),
  B3R3 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc3$mc, 
                                      pca_robot_rotatedN_pc3$mc, "spearman")),
  B3R4 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc3$mc, 
                                      pca_robot_rotatedN_pc4$mc, "spearman")),
  
  # beetle PC4 vs. robot PCs 1-4
  B4R1 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc4$mc, 
                                      pca_robot_rotatedN_pc1$mc, "spearman")),
  B4R2 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc4$mc, 
                                      pca_robot_rotatedN_pc2$mc, "spearman")),
  B4R3 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc4$mc, 
                                      pca_robot_rotatedN_pc3$mc, "spearman")),
  B4R4 = as.numeric(compareItemOrders(pca_beetle_rotatedN_pc4$mc, 
                                      pca_robot_rotatedN_pc4$mc, "spearman")))

sort(footruleDist)

tauDist <- c(
  B1R1 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc1$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc1$mc)), 
                             method = "kendall")$p.value),
  B1R2 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc1$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc2$mc)), 
                             method = "kendall")$p.value),
  B1R3 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc1$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc3$mc)), 
                             method = "kendall")$p.value),
  B1R4 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc1$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc4$mc)), 
                             method = "kendall")$p.value),
  
  B2R1 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc2$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc1$mc)), 
                             method = "kendall")$p.value),
  B2R2 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc2$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc2$mc)), 
                             method = "kendall")$p.value),
  B2R3 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc2$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc3$mc)), 
                             method = "kendall")$p.value),
  B2R4 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc2$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc4$mc)), 
                             method = "kendall")$p.value),
  
  B3R1 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc3$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc1$mc)), 
                             method = "kendall")$p.value),
  B3R2 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc3$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc2$mc)), 
                             method = "kendall")$p.value),
  B3R3 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc3$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc3$mc)), 
                             method = "kendall")$p.value),
  B3R4 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc3$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc4$mc)), 
                             method = "kendall")$p.value),
  
  B4R1 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc4$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc1$mc)), 
                             method = "kendall")$p.value),
  B4R2 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc4$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc2$mc)), 
                             method = "kendall")$p.value),
  B4R3 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc4$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc3$mc)), 
                             method = "kendall")$p.value),
  B4R4 = as.numeric(cor.test(as.numeric(factor(pca_beetle_rotatedN_pc4$mc)),
                             as.numeric(factor(pca_robot_rotatedN_pc4$mc)), 
                             method = "kendall")$p.value)
)

sort(abs(tauDist))

# nipals ---------------------------------------------

library(mixOmics)

temp <- nipals(d_beetle, ncomp = 20)

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
X.na <- X <- hilbert(9)[, 1:6]

## Hilbert matrix with missing data
idx.na <- matrix(sample(c(0, 1, 1, 1, 1), 36, replace = TRUE), ncol = 6)
X.na[idx.na == 0] <- NA
X.rec <- nipals(X.na, reconst = TRUE)$rec
round(X, 2)
round(X.rec, 2)



