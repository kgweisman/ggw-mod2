# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

n_iter <- 5000
n_common <- array()
items <- 1:40
for(i in 1:n_iter) {
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
qplot(n_common) + 
  geom_line(aes(x = n_common, y = dpois(n_common, mean(n_common))*n_iter), color = "red")
shapiro.test(n_common)

p_values <- array()
for(i in 1:8) {
  p_values[i] <- ppois(i, lambda = mean(n_common),
                       lower.tail = ifelse(i < mean(n_common), T, F))
}
round(p_values, 3)