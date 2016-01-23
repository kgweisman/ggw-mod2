# library(ca)
# data("wg93")
# mjca(wg93[,1:4])
# 
# mjca(wg93[,1:4], subsetcol = (1:20)[-seq(3,18,5)])
# mjca(wg93, supcol = 5:7)
# 
# 
# temp <- mjca(d4_all)
# temp$rowpcoord
# plot(mjca(d4_all))
# summary(temp)
# print(temp)



library(FactoMineR)
temp <- d4 %>%
  select(condition, subid, happy:pride) %>%
  gather(mc, response, happy:pride) %>%
  mutate(response = ifelse(as.numeric(response) > 2, TRUE, FALSE)) %>%
  filter(response == TRUE) %>%
  select(-subid, -response) %>%
  mutate(condition = factor(as.character(condition)),
         mc = factor(as.character(mc)))

temp_tab <- table(temp)

temp_ca <- CA(temp_tab)
temp_ca$col
temp_plot12 <- plot(temp_ca)
temp_plot13 <- plot(temp_ca, axes = c("Dim 1", "Dim 3"))
temp_plot23 <- plot(temp_ca, axes = c("Dim 2", "Dim 3"))
