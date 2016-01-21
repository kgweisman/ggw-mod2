# library(plot3D)
# library(RColorBrewer)
# with(efa_d4_all_rotatedN_scores,
#      scatter3D(F1_mean, F2_mean, F3_mean,
#      # colvar = NULL,
#      pch = 19,
#      bty = "g",
#      clab = "Condition",
#      col.var = as.integer(efa_d4_all_rotatedN_scores$condition)),
#      col = tol21rainbow)

# # Add small dots on basal plane and on the depth plane
# scatter3D_fancy <- function(x, y, z,..., colvar = z)
# {
#   panelfirst <- function(pmat) {
#     XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
#     scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
#               cex = 2, add = TRUE, colkey = FALSE)
#     
#     XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
#     scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
#               cex = 2, add = TRUE, colkey = FALSE)
#   }
#   scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
#             colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
# }

scatter3D(x = efa_d4_all_rotatedN_scores$F1_mean, 
          y = efa_d4_all_rotatedN_scores$F2_mean, 
          z = efa_d4_all_rotatedN_scores$F3_mean, 
          bty = "g", 
          pch = 19, 
          # phi = 0,
          col.var = as.integer(efa_d4_all_rotatedN_scores$condition), 
          col = tol21rainbow,
          main = "Study 4: Factor scores", 
          xlab = "\n\nF1: Social-emotional",
          ylab ="\n\nF2: Physiological", 
          zlab = "\n\nF3: Perceptual",
          # colkey = FALSE,
          ticktype = "detailed",
          type = "h")

text3D(x = efa_d4_all_rotatedN_scores$F1_mean, 
       y = efa_d4_all_rotatedN_scores$F2_mean, 
       z = efa_d4_all_rotatedN_scores$F3_mean, 
       labels = efa_d4_all_rotatedN_scores$condition,
       col.var = as.integer(efa_d4_all_rotatedN_scores$condition), 
       add = TRUE, 
       colkey = FALSE, 
       cex = 1)