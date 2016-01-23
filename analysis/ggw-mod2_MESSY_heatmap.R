# temp <- d4_all %>%
#   add_rownames(var = "subid") %>%
#   gather(mc, score, -subid) %>%
#   full_join(d4 %>% select(subid, condition))

# ggplot(temp, aes(x = mc, y = subid, fill = score)) +
#   geom_tile() +
#   scale_fill_gradient()

library(RColorBrewer)
library(gplots)
heatmap.2(as.matrix(d4 %>% select(happy:pride)), 
          col = brewer.pal(7, "RdYlBu"), 
          Rowv = T, Colv = T, 
          dendrogram = "none", 
          trace = "none",
          main = "Study 4",
          labRow = d4$condition,
          cexRow = 1.5,
          cexCol = 1.5)


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



