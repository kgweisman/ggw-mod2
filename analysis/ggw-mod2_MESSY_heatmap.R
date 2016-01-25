# temp <- d4_all %>%
#   add_rownames(var = "subid") %>%
#   gather(mc, score, -subid) %>%
#   full_join(d4 %>% select(subid, condition))

# ggplot(temp, aes(x = mc, y = subid, fill = score)) +
#   geom_tile() +
#   scale_fill_gradient()

library(RColorBrewer)
library(gplots)
hm_all <- heatmap.2(as.matrix(d1_all),
                       col = brewer.pal(7, "Blues"),
                       Rowv = T, Colv = T,
                       dendrogram = "both",
                       trace = "none",
                       main = "Study 1: Beetle",
                       labRow = d1$condition,
                       cexRow = 1.5,
                       cexCol = 1.5)


temp_beetle <- d1 %>% 
  filter(condition == "beetle") %>%
  select(subid, happy:pride) %>%
  gather(mc, score, -subid) %>%
  mutate(score = as.integer(score) + 3) %>%
  spread(mc, score) %>%
  select(-subid) %>%
  as.matrix()
hm_beetle <- heatmap.2(temp_beetle,
                       col = brewer.pal(7, "Blues"),
                       Rowv = T, Colv = T,
                       dendrogram = "none",
                       trace = "none",
                       main = "Study 1: Beetle",
                       labRow = d1$condition,
                       cexRow = 1.5,
                       cexCol = 1.5)

temp_robot <- d1 %>% 
  filter(condition == "robot") %>%
  select(subid, happy:pride) %>%
  gather(mc, score, -subid) %>%
  mutate(score = as.integer(score) + 3) %>%
  spread(mc, score) %>%
  select(-subid) %>%
  as.matrix()
hm_robot <- heatmap.2(temp_robot,
                       col = brewer.pal(7, "Blues"),
                       Rowv = T, Colv = T,
                       dendrogram = "none",
                       trace = "none",
                       main = "Study 1: Robot",
                       labRow = d1$condition,
                       cexRow = 1.5,
                       cexCol = 1.5)

carpet_beetle <- data.frame(hm_beetle$carpet) %>%
  add_rownames(var = "mc") %>%
  gather(sub_index, response, -mc) %>%
  add_rownames(var = "order") %>%
  mutate(sub_index = factor(sub_index, labels = as.character(1:200)),
         condition = "beetle")

carpet_robot <- data.frame(hm_robot$carpet) %>%
  add_rownames(var = "mc") %>%
  gather(sub_index, response, -mc) %>%
  add_rownames(var = "order") %>%
  mutate(sub_index = factor(sub_index, labels = as.character(201:405)),
         condition = "robot")

carpet <- full_join(carpet_beetle, carpet_robot) %>%
  arrange(condition, desc(order)) %>%
  mutate(overall_order = 1:16200,
         sub_index = reorder(sub_index, overall_order))

ggplot(carpet, aes(x = mc, y = sub_index, fill = response)) + 
  geom_tile() +
  scale_fill_continuous()


#### WITH GGPLOT2

# d4_subid_order <- data.frame(hclust(dist(d4_all), method = "ward.D")[c("labels", "order")]) %>%
#   rename(subid = labels, subid_order = order)
# 
# d4_mc_order <- data.frame(hclust(dist(t(d4_all)), method = "ward.D")[c("labels", "order")]) %>%
#   rename(mc = labels, mc_order = order)
# 
# d4_ordered <- d4_all %>%
#   add_rownames(var = "subid") %>%
#   gather(mc, score, -subid) %>%
#   full_join(d4_subid_order) %>%
#   mutate(subid_ordered = reorder(subid, subid_order)) %>%
#   full_join(d4_mc_order) %>%
#   mutate(mc_ordered = reorder(mc, mc_order)) %>%
#   full_join(d4 %>% select(subid, condition), by = "subid")
# 
# ggplot(d4_ordered, aes(x = mc_ordered, y = subid_ordered, fill = factor(score))) +
#   geom_tile() +
#   scale_fill_brewer(type = "div") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 



