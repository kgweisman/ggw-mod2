# bootstrap 95% CIs for ratings by character (nonparametric)
char_ratings <- d1 %>% 
  select(condition, subid, happy:pride) %>%
  gather(mc, response, -subid, -condition) %>%
  mutate(response = as.numeric(response)) %>%
  multi_boot(column = "response",
             summary_function = "mean",
             summary_groups = c("condition", "mc"),
             statistics_functions = c("ci_lower", "mean", "ci_upper"))

# elaborate and merge with sample sizes
char_plotting <- char_ratings %>%
  ungroup() %>%
  mutate(wording = factor(
    recode(mc,
           hungry = "getting hungry", pain = "experiencing pain",
           tired = "feeling tired", fear = "experiencing fear",
           computations = "doing computations", pleasure = "experiencing pleasure",
           conscious = "being conscious", free_will = "having free will",
           safe = "feeling safe", desires = "having desires",
           calm = "feeling calm", nauseated = "feeling nauseated",
           angry = "getting angry", intentions = "having intentions",
           self_aware = "being self-aware", odors = "detecting odors",
           embarrassed = "feeling embarrassed", pride = "experiencing pride",
           love = "feeling love", guilt = "experiencing guilt",
           depressed = "feeling depressed", disrespected = "feeling disrespected",
           beliefs = "holding beliefs", emo_recog = "understanding ... feeling",
           joy = "experiencing joy", personality = "having a personality",
           happy = "feeling happy", morality = "telling right from wrong",
           thoughts = "having thoughts", self_restraint = "exercising self-restraint",
           remembering = "remembering things", recognizing = "recognizing others",
           temperatures = "sensing temperatures", communicating = "communicating ...",
           goal = "working toward a goal", depth = "perceiving depth",
           sounds = "detecting sounds", seeing = "seeing things",
           choices = "making choices", reasoning = "reasoning about things"))) %>%
  full_join(demoSampleSize("study 4") %>% filter(condition != "all")) %>%
  mutate(condition = factor(condition,
                            levels = c("beetle", "robot")))

# merge with loadings, orderings, and dominant factors from each study 
char_plotting <- char_plotting %>%
  full_join(order_s1 %>%
              mutate(s1_MR1_abs = abs(s1_MR1),
                     s1_MR2_abs = abs(s1_MR2),
                     s1_MR3_abs = abs(s1_MR3),
                     s1_factor = 
                       ifelse(s1_MR1_abs > s1_MR2_abs &
                                s1_MR1_abs > s1_MR3_abs, "BODY",
                              ifelse(s1_MR2_abs > s1_MR1_abs &
                                       s1_MR2_abs > s1_MR3_abs, "SOUL",
                                     ifelse(s1_MR3_abs > s1_MR1_abs &
                                              s1_MR3_abs > s1_MR2_abs, "MIND",
                                            NA))),
                     s1_color = recode(s1_factor,
                                       "BODY" = "#377EB8",
                                       "SOUL" = "#4DAF4A",
                                       "MIND" = "#E41A1C"),
                     s1_order = as.numeric(order1)) %>%
              select(-s1_MR1_abs, -s1_MR2_abs, -s1_MR3_abs)) %>%
  full_join(order_s2 %>%
              data.frame() %>%
              mutate(s2_MR1_abs = abs(s2_MR1),
                     s2_MR2_abs = abs(s2_MR2),
                     s2_MR3_abs = abs(s2_MR3),
                     s2_factor = 
                       ifelse(s2_MR1_abs > s2_MR2_abs &
                                s2_MR1_abs > s2_MR3_abs, "BODY",
                              ifelse(s2_MR2_abs > s2_MR1_abs &
                                       s2_MR2_abs > s2_MR3_abs, "SOUL",
                                     ifelse(s2_MR3_abs > s2_MR1_abs &
                                              s2_MR3_abs > s2_MR2_abs, "MIND",
                                            NA))),
                     s2_color = recode(s2_factor,
                                       "BODY" = "#377EB8",
                                       "SOUL" = "#4DAF4A",
                                       "MIND" = "#E41A1C")) %>%
              rownames_to_column(var = "s2_order") %>%
              mutate(s2_order = as.numeric(s2_order)) %>%
              select(-s2_MR1_abs, -s2_MR2_abs, -s2_MR3_abs)) %>%
  full_join(order_s3 %>%
              mutate(s3_MR1_abs = abs(s3_MR1),
                     s3_MR2_abs = abs(s3_MR2),
                     s3_MR3_abs = abs(s3_MR3),
                     s3_factor = 
                       ifelse(s3_MR1_abs > s3_MR2_abs &
                                s3_MR1_abs > s3_MR3_abs, "BODY",
                              ifelse(s3_MR2_abs > s3_MR1_abs &
                                       s3_MR2_abs > s3_MR3_abs, "SOUL",
                                     ifelse(s3_MR3_abs > s3_MR1_abs &
                                              s3_MR3_abs > s3_MR2_abs, "MIND",
                                            NA))),
                     s3_color = recode(s3_factor,
                                       "BODY" = "#377EB8",
                                       "SOUL" = "#4DAF4A",
                                       "MIND" = "#E41A1C")) %>%
              rownames_to_column(var = "s3_order") %>%
              mutate(s3_order = as.numeric(s3_order)) %>%
              select(-s3_MR1_abs, -s3_MR2_abs, -s3_MR3_abs)) %>%
  full_join(order_s4 %>%
              mutate(s4_MR1_abs = abs(s4_MR1),
                     s4_MR2_abs = abs(s4_MR2),
                     s4_MR3_abs = abs(s4_MR3),
                     s4_factor = 
                       ifelse(s4_MR1_abs > s4_MR2_abs &
                                s4_MR1_abs > s4_MR3_abs, "BODY",
                              ifelse(s4_MR2_abs > s4_MR1_abs &
                                       s4_MR2_abs > s4_MR3_abs, "SOUL",
                                     ifelse(s4_MR3_abs > s4_MR1_abs &
                                              s4_MR3_abs > s4_MR2_abs, "MIND",
                                            NA))),
                     s4_color = recode(s4_factor,
                                       "BODY" = "#377EB8",
                                       "SOUL" = "#4DAF4A",
                                       "MIND" = "#E41A1C")) %>%
              rownames_to_column(var = "s4_order") %>%
              mutate(s4_order = as.numeric(s4_order)) %>%
              select(-s4_MR1_abs, -s4_MR2_abs, -s4_MR3_abs))

# configure plot settings (labels, palette)
label_df <- char_plotting %>% select(condition, n) %>% unique()
facetLabs <- makeFacetLabs(char_plotting)
myPalette <- brewer.pal(3, "Set1"); names(myPalette) <- c("BODY", "SOUL", "MIND")

# include only 6 conditions
fig2_plotting <- char_plotting %>% filter(condition %in% c("stapler", "robot", "beetle", 
                                                           "goat", "elephant", "adult"))

# make palette based on dominant factors in study 1
fig2_pal <- c(rep(myPalette["MIND"],
                  length(fig2_plotting$s1_factor[fig2_plotting$condition == "adult" &
                                                   fig2_plotting$s1_factor == "MIND"])),
              rep(myPalette["SOUL"],
                  length(fig2_plotting$s1_factor[fig2_plotting$condition == "adult" &
                                                   fig2_plotting$s1_factor == "SOUL"])),
              rep(myPalette["BODY"],
                  length(fig2_plotting$s1_factor[fig2_plotting$condition == "adult" &
                                                   fig2_plotting$s1_factor == "BODY"])))

# plot! (ordered by study 1 factor loadings)
fig2 <- ggplot(fig2_plotting, 
               aes(x = mean, y = reorder(wording, desc(s1_order)), colour = s1_color)) +
  geom_point(stat = "identity", position = "identity", size = 4) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.4) +
  facet_wrap(~ condition, ncol = 2,
             labeller = labeller(condition = facetLabs)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_text(face = "italic",
                                   colour = fig2_pal),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  scale_x_continuous(name = "Mean rating",
                     limits = c(-3, 3),
                     breaks = seq(-3, 3, 1),
                     labels = seq(0, 6, 1)) +
  scale_colour_brewer(name = "Factor:",
                      type = "qual", palette = 6)

fig2
