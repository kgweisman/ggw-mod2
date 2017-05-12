# make corplot functions
make_corplot_df <- function(d, cor = c("cor", "poly"),
                            rot = c("varimax", "oblimin")) {

  # set variables
  data_set <- d
  cor_type <- cor
  rot_type <- rot
  
  # make dataframe
  factors_df <- fa.sort(fa(data_set, nfactors = 3, rotate = rot_type)$loadings[]) %>% 
    data.frame() %>%
    rownames_to_column(var = "item") %>%
    left_join(cap_key) %>%
    mutate(short = gsub("_", " ", wording)) %>%
    select(short, MR1, MR2, MR3) %>%
    rename(Factor1 = MR1, Factor2 = MR2, Factor3 = MR3) %>%
    rownames_to_column(var = "order") %>%
    mutate(order = as.numeric(order))
  
  factors_df_long <- factors_df %>%
    gather(factor, loading, -short, -order) %>%
    mutate(factor = factor(gsub("Factor", "Factor ", factor)),
           face = ifelse(abs(loading) > 0.6, "bold", "plain")) %>%
    arrange(order, factor)
  
  return(factors_df_long)
}
 
make_corplot_plot <- function(d) {
  
  # make plot
  p <- ggplot(d, aes(x = factor,
                     y = reorder(short, desc(order)), fill = loading)) +
    geom_tile(color = "black") +
    geom_text(aes(label = format(round(loading, 2), nsmall = 2),
                  fontface = face)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), breaks = c(-1, 0, 1),
                         guide = guide_colorbar(title = element_blank(),
                                                barheight = 20)) +
    scale_x_discrete(position = "top") +
    # geom_rect(aes(xmin = 0.51, xmax = 1.49, ymin = 14.55, ymax = 20.45),
    #           alpha = 0, color = "black", size = .5) +
    # geom_rect(aes(xmin = 1.51, xmax = 2.49, ymin = 6.55, ymax = 14.45),
    #           alpha = 0, color = "black", size = .5) +
    # geom_rect(aes(xmin = 2.51, xmax = 3.49, ymin = 0.55, ymax = 6.45),
    #           alpha = 0, color = "black", size = .5) +
    # theme_bw() +
    theme_minimal() +
    theme(text = element_text(size = 18),
          axis.title = element_blank(),
          panel.grid = element_blank()) # 1000 by 1000 
  
  # return plot 
  return(p)
}

# make individual plots per study
corplot_d1_all <- make_corplot_df(d1_all, "cor", "varimax")
corplot_d2_all <- make_corplot_df(d2_all, "cor", "varimax")
corplot_d3_all <- make_corplot_df(d3_all, "cor", "varimax")
corplot_d4_all <- make_corplot_df(d4_all, "cor", "varimax")
corplot_all_studies <- corplot_d1_all %>%
  select(short, loading, face, factor) %>%
  mutate(study = "Study 1") %>%
  full_join(corplot_d2_all %>% select(short, loading, face, factor) %>% mutate(study = "Study 2")) %>%
  full_join(corplot_d3_all %>% select(short, loading, face, factor) %>% mutate(study = "Study 3")) %>%
  full_join(corplot_d4_all %>% select(short, loading, face, factor) %>% mutate(study = "Study 4")) %>%
  full_join(corplot_d1_all %>% select(short, order)) %>%
  mutate(factor = paste0(factor, " (", study, ")"))

# make corplots... and one for whole study
# make_corplot_plot(corplot_d1_all)
# make_corplot_plot(corplot_d2_all)
# make_corplot_plot(corplot_d3_all)
# make_corplot_plot(corplot_d4_all)
make_corplot_plot(corplot_all_studies) +
  scale_x_discrete(labels = rep(c("Study 1", "Study 2", "Study 3", "Study 4"), 3), 
                   position = "top")


# correlations by participant
# cor.ci(fa(data_set, nfactors = 3, rotate = rot_type, cor = cor_type, scores = score_type)$scores %>% data.frame())
