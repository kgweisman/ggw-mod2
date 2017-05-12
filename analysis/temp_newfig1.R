library(ggrepel)

# # top 5 dominant by factor
# scatter_plotting2 <- scatter_plotting %>%
#   rownames_to_column(var = "item_num") %>%
#   select(-short) %>%
#   full_join(scatter_plotting %>%
#               group_by(dominant) %>%
#               top_n(5, size) %>%
#               select(item, short)) %>%
#   mutate(short = ifelse(is.na(short), "", short))

# chosen by hand
scatter_plotting2 <- scatter_plotting %>%
  rownames_to_column(var = "item_num") %>%
  mutate(short = ifelse(short %in% c("embarrassment", "love", "guilt", "disrespect",
                                     "happiness", "safety", "desire", "pleasure",
                                     "goal", "sound", "personality", "recognition", "thought",
                                     "consciousness"), "", short))

fig1 <- plot_ly(scatter_plotting2, x = SOUL, y = BODY, z = MIND,
                type = "scatter3d",
                color = dominant, colors = c("#4daf4a", "#e41a1c", "#377eb8"),
                marker = list(size = 6),
                text = short,
                # text = item_num,
                textfont = list(size = 16),
                mode = "text+markers",
                showlegend = TRUE)
fig1



# fig1 +
#   geom_text(x = 1, y = 2, z = 3,  label = "test")
#   geom_text_repel(aes(x = scatter_plotting2$SOUL, 
#                       y = scatter_plotting2$BODY, 
#                       # z = scatter_plotting2$MIND, 
#                       label = scatter_plotting2$short))
# 
# t <- scatter_plotting2 %>%
#   select(item_num, short) %>%
#   mutate(item_num = as.numeric(item_num)) %>%
#   arrange(item_num)
# 
# fig1 +
#   annotate("text", x = 0, y = 0, label = t)
