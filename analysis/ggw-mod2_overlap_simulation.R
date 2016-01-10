# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

n_common <- array()
items <- 1:40
for(i in 1:100000) {
  sample1 <- sample(items, 40, replace = F)
  sample2 <- sample(items, 40, replace = F)
  s1_top10 <- sample1[1:10]
  s2_top10 <- sample2[1:10]
  n_common[i] <- length(s1_top10[s1_top10 %in% s2_top10])
}

min(n_common)
max(n_common)
Mode(n_common)
median(n_common)
mean(n_common)
sd(n_common)
qplot(n_common)

p_values <- array()
for(i in 1:8) {
  p_values[i] <- pnorm(i, mean = mean(n_common), sd = sd(n_common),
                       lower.tail = ifelse(i < mean(n_common), T, F))
}
round(p_values, 3)