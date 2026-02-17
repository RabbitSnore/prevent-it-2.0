################################################################################

# Prevent It 2.0, Global Perpetration Prevention -- Additional Analyses

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("lme4", 
              "lmerTest", 
              "ggplot2",
              "psych")

lapply(packages, library, character.only = TRUE)

# Data analysis ----------------------------------------------------------------

# This script is written with the assumption that the data have already been
# wrangled into suitable structures.

## Preliminary note ------------------------------------------------------------

# For all analyses using mixed-effects models, we will not impute missing data.

# This script does not provide exhaustive documentation of all the additional
# and exploratory analyses we will conduct. Rather, it is provided to indicate
# the planned additional analyses prior to data collection.

# Additional research questions ------------------------------------------------

### Is there a change in motivation to commit future abuse/exploitation?

# The SChiMRA+ Part A contains three Likert-type self-report items concerning
# motivation to use CSAM, to socialize with children, and to have sexual
# interactions with children.

# Watch

lmm_watch_motive_linear    <- lmer(schimra_a_watch 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after + (1|id), 
                                   data = gpp_data_main)

lmm_watch_motive_quad      <- lmer(schimra_a_watch 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_watch_motive_quad_2      <- lmer(schimra_a_watch 
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + time_after_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lrt_watch_motive           <- anova(lmm_csam_hours_linear, 
                                    lmm_csam_hours_quad, 
                                    lmm_csam_hours_quad_2, 
                                    test = "LRT")

## Create new data for predictions

pred_df_motivewatch <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq = time^2
  )

## Predictions from retained model

predict_df_motivewatch <- as.data.frame(
  predict(lmm_watch_motive_quad, 
          newdata = pred_df_motivewatch,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_motivewatch <- bind_cols(pred_df_motivewatch, predict_df_motivewatch)

pred_df_motivewatch <- pred_df_motivewatch %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

## Visualization of predicted values

plot_motivewatch_predict <- 
  ggplot(pred_df_motivewatch,
         aes(
           y     = fit,
           x     = time,
           color = group
         )) +
  geom_line(
    linewidth = 1
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_lb,
      x     = time,
      color = group
    )
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_ub,
      x     = time,
      color = group
    )
  ) +
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  scale_x_continuous(
    breaks = 0:9
  ) +
  scale_color_manual(
    labels = c("Prevent It", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted Motvation to Observe Children",
    color = "Group"
  ) +
  theme_classic()

## Effect size calculation

contrast_motivewatch   <- predictions(lmm_watch_motive_quad,
                                      newdata    = pred_df_motivewatch %>% 
                                        filter(time == 9),
                                      hypothesis = "pairwise",
                                      re.form    = NA)

contrast_motivewatch_d <- paste(
  round(contrast_motivewatch$estimate / sigma(lmm_watch_motive_quad), 3),
  " 95% CI [",
  round(contrast_motivewatch$conf.low / sigma(lmm_watch_motive_quad), 3),
  ", ",
  round(contrast_motivewatch$conf.high / sigma(lmm_watch_motive_quad), 3),
  "]",
  sep = ""
)

# Socialize

lmm_social_motive_linear    <- lmer(schimra_a_socialize 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after + (1|id), 
                                   data = gpp_data_main)

lmm_social_motive_quad      <- lmer(schimra_a_socialize 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_social_motive_quad_2      <- lmer(schimra_a_socialize 
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + time_after_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lrt_social_motive           <- anova(lmm_csam_hours_linear, 
                                    lmm_csam_hours_quad, 
                                    lmm_csam_hours_quad_2, 
                                    test = "LRT")

## Create new data for predictions

pred_df_motivesocial <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq = time^2
  )

## Predictions from retained model

predict_df_motivesocial <- as.data.frame(
  predict(lmm_social_motive_quad, 
          newdata = pred_df_motivesocial,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_motivesocial <- bind_cols(pred_df_motivesocial, predict_df_motivesocial)

pred_df_motivesocial <- pred_df_motivesocial %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

## Visualization of predicted values

plot_motivesocial_predict <- 
  ggplot(pred_df_motivesocial,
         aes(
           y     = fit,
           x     = time,
           color = group
         )) +
  geom_line(
    linewidth = 1
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_lb,
      x     = time,
      color = group
    )
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_ub,
      x     = time,
      color = group
    )
  ) +
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  scale_x_continuous(
    breaks = 0:9
  ) +
  scale_color_manual(
    labels = c("Prevent It", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted Motvation to Socialize with Children",
    color = "Group"
  ) +
  theme_classic()

## Effect size calculation

contrast_motivesocial   <- predictions(lmm_social_motive_quad,
                                      newdata    = pred_df_motivesocial %>% 
                                        filter(time == 9),
                                      hypothesis = "pairwise",
                                      re.form    = NA)

contrast_motivesocial_d <- paste(
  round(contrast_motivesocial$estimate / sigma(lmm_social_motive_quad), 3),
  " 95% CI [",
  round(contrast_motivesocial$conf.low / sigma(lmm_social_motive_quad), 3),
  ", ",
  round(contrast_motivesocial$conf.high / sigma(lmm_social_motive_quad), 3),
  "]",
  sep = ""
)

# Interact

lmm_interact_motive_linear    <- lmer(schimra_a_interact 
                                      ~ 1 
                                      + treatment 
                                      + time 
                                      + time_after + (1|id), 
                                      data = gpp_data_main)

lmm_interact_motive_quad      <- lmer(schimra_a_interact 
                                      ~ 1 
                                      + treatment 
                                      + time 
                                      + time_after 
                                      + time_sq
                                      + (1|id), 
                                      data = gpp_data_main)

lmm_interact_motive_quad_2      <- lmer(schimra_a_interact 
                                        ~ 1 
                                        + treatment 
                                        + time 
                                        + time_after 
                                        + time_sq 
                                        + time_after_sq 
                                        + (1|id), 
                                        data = gpp_data_main)

lrt_interact_motive           <- anova(lmm_csam_hours_linear, 
                                       lmm_csam_hours_quad, 
                                       lmm_csam_hours_quad_2, 
                                       test = "LRT")

## Create new data for predictions

pred_df_motiveinteract <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq = time^2
  )

## Predictions from retained model

predict_df_motiveinteract <- as.data.frame(
  predict(lmm_interact_motive_quad, 
          newdata = pred_df_motiveinteract,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_motiveinteract <- bind_cols(pred_df_motiveinteract, predict_df_motiveinteract)

pred_df_motiveinteract <- pred_df_motiveinteract %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

## Visualization of predicted values

plot_motiveinteract_predict <- 
  ggplot(pred_df_motiveinteract,
         aes(
           y     = fit,
           x     = time,
           color = group
         )) +
  geom_line(
    linewidth = 1
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_lb,
      x     = time,
      color = group
    )
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_ub,
      x     = time,
      color = group
    )
  ) +
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  scale_x_continuous(
    breaks = 0:9
  ) +
  scale_color_manual(
    labels = c("Prevent It", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted Motvation to Interact with Children",
    color = "Group"
  ) +
  theme_classic()

## Effect size calculation

contrast_motiveinteract   <- predictions(lmm_interact_motive_quad,
                                       newdata    = pred_df_motiveinteract %>% 
                                         filter(time == 9),
                                       hypothesis = "pairwise",
                                       re.form    = NA)

contrast_motiveinteract_d <- paste(
  round(contrast_motiveinteract$estimate / sigma(lmm_interact_motive_quad), 3),
  " 95% CI [",
  round(contrast_motiveinteract$conf.low / sigma(lmm_interact_motive_quad), 3),
  ", ",
  round(contrast_motiveinteract$conf.high / sigma(lmm_interact_motive_quad), 3),
  "]",
  sep = ""
)

# NEQ-20 wrangling -------------------------------------------------------------

# Item text

neq_20_text <- c(
  "I had more problems with my sleep",
  "I felt like I was under more stress",
  "I experienced more anxiety",
  "I felt more worried",
  "I experienced more hopelessness",
  "I experienced more unpleasant feelings",
  "I felt that the issue I was looking for help with got worse",
  "Unpleasant memories resurfaced",
  "I became afraid that other people would find out about my treatment",
  "I got thoughts that it would be better if I did not exist anymore and that I should take my own life",
  "I started feeling ashamed in front of other people because I was having treatment",
  "I stopped thinking that things could get better",
  "I started thinking that the issue I was seeking help for could not be made any better",
  "I think that I have developed a dependency on my treatment",
  "I did not always understand my treatment",
  "I did not always understand my therapist",
  "I did not have confidence in my treatment",
  "I felt that the treatment did not produce any results",
  "I felt that my expectations for the therapist were not fulfilled",
  "I felt that the treatment was not motivating"
)

# Select NEQ-20 items

neq_20_raw <- gpp_data_main %>% 
  select(starts_with("neq_20"),
         -neq_20_other
  )

# Separate item types

neq_20_indicated <- neq_20_raw %>% 
  select(
    all_of(
      paste("neq_20_",
            str_pad(1:20, width = 2, pad = "0", side = "left"),
            sep = "")
    )
  )

neq_20_quant <- neq_20_raw %>% 
  select(ends_with("a"))

neq_20_cause <- neq_20_raw %>% 
  select(ends_with("b"))

# Negative Experience Indicated

neq_20_summary_ind <-  neq_20_indicated %>% 
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "indicated"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(indicated)) %>% 
  summarise(
    Indicated = sum(indicated)/n(),
    ind_n     = sum(indicated),
    n         = n()
  )

# Negative Experience Caused by Treatment

neq_20_summary_cause <-  neq_20_cause %>% 
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "cause"
  ) %>% 
  extract(
    col   = item,
    into  = "item",
    regex = "(.*)b"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(cause)) %>% 
  summarise(
    caused_n  = sum(cause)
  )

# Join qualitative summary

neq_20_summary_qual <- neq_20_summary_ind %>% 
  left_join(neq_20_summary_cause, by = "item") %>% 
  mutate(
    Caused       = caused_n/ind_n,
    Caused_total = caused_n/n
  ) %>% 
  relocate(
    Caused, Caused_total,
    .after = Indicated
  )

neq_20_summary_qual$item <- neq_20_text

# Quantitative summary

neq_20_summary_quant <- neq_20_quant %>%
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "rating"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(rating)) %>% 
  summarise(
    Mean   = mean(rating),
    SD     = sd(rating),
    Median = median(rating)
  )

neq_20_summary_quant$item <- neq_20_text  

# Join summaries

neq_20_summary <- neq_20_summary_qual %>% 
  left_join(neq_20_summary_quant, by = "item") %>% 
  relocate(
    Mean, SD, Median,
    .after = Caused_total
  )

neq_20_summary_desc <- neq_20_summary %>% 
  arrange(desc(Indicated))

# Rate of negative experiences

neq_20_frequencies <- neq_20_indicated %>% 
  filter(complete.cases(.)) %>% 
  map_df(as.numeric) %>% 
  rowSums()

neq_20_rate <- sum(neq_20_frequencies > 0)/length(neq_20_frequencies)

neq_20_freq_cause <- neq_20_cause %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>% 
  map_df(as.numeric) %>%
  rowSums(na.rm = TRUE)

neq_20_rate_cause <- sum(neq_20_freq_cause > 0)/length(neq_20_frequencies)

# NEQ-20 table

## Function

neq_table <- function(neq_summary, digits = 3, head_n = 20) {
  
  require(flextable)
  
  neq_summary <- head(neq_summary, head_n)
  
  item <- neq_summary$item
  
  indicated <- paste(
    format(round(neq_summary$Indicated, digits) * 100, nsmall = 1), 
    "% (",
    neq_summary$ind_n,
    ")",
    sep = ""
  )
  
  treat_cause <- paste(
    format(round(neq_summary$Caused_total, digits) * 100, nsmall = 1), 
    "% (",
    neq_summary$caused_n,
    ")",
    sep = ""
  )
  
  mean_sd <- paste(
    format(round(neq_summary$Mean, digits), nsmall = digits),
    " (",
    format(round(neq_summary$SD, digits), nsmall = digits),
    ")",
    sep = ""
  )
  
  mean_sd <- str_replace_all(mean_sd, "   NA", "-")
  
  neq_df <- data.frame(
    item,
    indicated,
    treat_cause,
    mean_sd
  )
  
  flextable(neq_df,
            cwidth = c(
              4.5, 1, 1, 1
            )) %>% 
    set_header_labels(
      values = c(
        "Negative Experience",
        "Experience Indicated\n% total (n)",
        "Experience Caused by Treatment\n% total (n)",
        "Severity\nMean (SD)"
      )
    ) %>%
    align(
      align = c(
        "left", 
        "center", 
        "center",
        "center"), 
      part = "all"
    )
  
}

## Tables

neq_20_table_full <- neq_20_summary_desc %>% 
  neq_table()

neq_20_table_10   <- neq_20_summary_desc %>% 
  neq_table(head_n = 10)

neq_20_table_05   <- neq_20_summary_desc %>% 
  neq_table(head_n = 5)

## Table export

save_as_docx("Full"   = neq_20_table_full,
             "Top 10" = neq_20_table_10,
             "Top 5"  = neq_20_table_05,
             path  = "output/gpp_neq-20-tables.docx",
             align = "center")


## Split by assigned group

### Select NEQ-20 items

neq_20_cbt <- gpp_data_main %>% 
  filter(assigned_group == "cbt") %>% 
  select(starts_with("neq_20"),
         -neq_20_other
  )

### Separate item types

neq_20_indicated_cbt <- neq_20_cbt %>% 
  select(
    all_of(
      paste("neq_20_",
            str_pad(1:20, width = 2, pad = "0", side = "left"),
            sep = "")
    )
  )

neq_20_quant_cbt <- neq_20_cbt %>% 
  select(ends_with("a"))

neq_20_cause_cbt <- neq_20_cbt %>% 
  select(ends_with("b"))

### Negative Experience Indicated

neq_20_summary_ind_cbt <-  neq_20_indicated_cbt %>% 
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "indicated"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(indicated)) %>% 
  summarise(
    Indicated = sum(indicated)/n(),
    ind_n     = sum(indicated),
    n         = n()
  )

### Negative Experience Caused by Treatment

neq_20_summary_cause_cbt <-  neq_20_cause_cbt %>% 
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "cause"
  ) %>% 
  extract(
    col   = item,
    into  = "item",
    regex = "(.*)b"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(cause)) %>% 
  summarise(
    caused_n  = sum(cause)
  )

### Join qualitative summary

neq_20_summary_qual_cbt <- neq_20_summary_ind_cbt %>% 
  left_join(neq_20_summary_cause, by = "item") %>% 
  mutate(
    Caused       = caused_n/ind_n,
    Caused_total = caused_n/n
  ) %>% 
  relocate(
    Caused, Caused_total,
    .after = Indicated
  )

neq_20_summary_qual_cbt$item <- neq_20_text

### Quantitative summary

neq_20_summary_quant_cbt <- neq_20_quant_cbt %>%
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "rating"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(rating)) %>% 
  summarise(
    Mean   = mean(rating),
    SD     = sd(rating),
    Median = median(rating)
  )

neq_20_summary_quant_cbt$item <- neq_20_text  

### Join summaries

neq_20_summary_cbt <- neq_20_summary_qual_cbt %>% 
  left_join(neq_20_summary_quant, by = "item") %>% 
  relocate(
    Mean, SD, Median,
    .after = Caused_total
  )

neq_20_summary_desc_cbt <- neq_20_summary_cbt %>% 
  arrange(desc(Indicated))

### Rate of negative experiences

neq_20_frequencies_cbt <- neq_20_indicated_cbt %>% 
  filter(complete.cases(.)) %>% 
  map_df(as.numeric) %>% 
  rowSums()

neq_20_rate_cbt <- sum(neq_20_frequencies_cbt > 0)/length(neq_20_frequencies_cbt)

neq_20_freq_cause_cbt <- neq_20_cause_cbt %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>% 
  map_df(as.numeric) %>%
  rowSums(na.rm = TRUE)

neq_20_rate_cause_cbt <- sum(neq_20_freq_cause_cbt > 0)/length(neq_20_frequencies_cbt)

### Select NEQ-20 items

neq_20_wl <- gpp_data_main %>% 
  filter(assigned_group == "waitlist") %>% 
  select(starts_with("neq_20"),
         -neq_20_other
  )

### Separate item types

neq_20_indicated_wl <- neq_20_wl %>% 
  select(
    all_of(
      paste("neq_20_",
            str_pad(1:20, width = 2, pad = "0", side = "left"),
            sep = "")
    )
  )

neq_20_quant_wl <- neq_20_wl %>% 
  select(ends_with("a"))

neq_20_cause_wl <- neq_20_wl %>% 
  select(ends_with("b"))

### Negative Experience Indicated

neq_20_summary_ind_wl <-  neq_20_indicated_wl %>% 
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "indicated"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(indicated)) %>% 
  summarise(
    Indicated = sum(indicated)/n(),
    ind_n     = sum(indicated),
    n         = n()
  )

### Negative Experience Caused by Treatment

neq_20_summary_cause_wl <-  neq_20_cause_wl %>% 
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "cause"
  ) %>% 
  extract(
    col   = item,
    into  = "item",
    regex = "(.*)b"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(cause)) %>% 
  summarise(
    caused_n  = sum(cause)
  )

### Join qualitative summary

neq_20_summary_qual_wl <- neq_20_summary_ind_wl %>% 
  left_join(neq_20_summary_cause, by = "item") %>% 
  mutate(
    Caused       = caused_n/ind_n,
    Caused_total = caused_n/n
  ) %>% 
  relocate(
    Caused, Caused_total,
    .after = Indicated
  )

neq_20_summary_qual_wl$item <- neq_20_text

### Quantitative summary

neq_20_summary_quant_wl <- neq_20_quant_wl %>%
  map_df(as.numeric) %>% 
  pivot_longer(
    cols      = everything(),
    names_to  = "item",
    values_to = "rating"
  ) %>% 
  group_by(item) %>%
  filter(complete.cases(rating)) %>% 
  summarise(
    Mean   = mean(rating),
    SD     = sd(rating),
    Median = median(rating)
  )

neq_20_summary_quant_wl$item <- neq_20_text  

### Join summaries

neq_20_summary_wl <- neq_20_summary_qual_wl %>% 
  left_join(neq_20_summary_quant, by = "item") %>% 
  relocate(
    Mean, SD, Median,
    .after = Caused_total
  )

neq_20_summary_desc_wl <- neq_20_summary_wl %>% 
  arrange(desc(Indicated))

### Rate of negative experiences

neq_20_frequencies_wl <- neq_20_indicated_wl %>% 
  filter(complete.cases(.)) %>% 
  map_df(as.numeric) %>% 
  rowSums()

neq_20_rate_wl <- sum(neq_20_frequencies_wl > 0)/length(neq_20_frequencies_wl)

neq_20_freq_cause_wl <- neq_20_cause_wl %>%
  filter(if_any(everything(), ~ !is.na(.x))) %>% 
  map_df(as.numeric) %>%
  rowSums(na.rm = TRUE)

neq_20_rate_cause_wl <- sum(neq_20_freq_cause_wl > 0)/length(neq_20_frequencies_wl)

## Group tables

neq_20_table_10_cbt   <- neq_20_summary_desc_cbt %>% 
  neq_table(head_n = 10)

neq_20_table_10_wl   <- neq_20_summary_desc_wl %>% 
  neq_table(head_n = 10)

### Table export

save_as_docx("CBT - Top 10"       = neq_20_table_10_cbt,
             "Waitlist - Top 10"  = neq_20_table_10_wl,
             path  = "output/gpp_neq-20-tables-grouped.docx",
             align = "center")

### To what extent does the treatment produce sustainable changes in sexual urges (follow-up measure)?

# To assess the extent to which the treatment produces a sustainable change in
# problematic sexual urges, using the total SSAS scores, we will estimate the
# change from the post-treatment measure (or in the case of the waitlist, the
# last weekly) to the follow-up measure, controlling for the first (i.e.,
# baseline) measure.
#
# For SSAS scores, we will fit linear mixed effects models, with treatment group
# (treatment vs. waitlist, dummy coded) as a fixed predictor, measurement point
# (last weekly measure vs. follow-up, dummy coded) as a fixed predictor,
# baseline measure as a fixed predictor, and random intercepts for each
# participant. We will fit two models for each variable, one with only main
# effects and one that adds the interaction term for the treatment and
# measurement predictors. These models will be compared using a likelihood ratio
# test.
#
# A significant interaction such that the treatment group's slope from post-
# treatment to followup is smaller (i.e., less positive) than the  waitlist's
# slope would provide support for the sustained effectiveness of the treatment.
# As long as the primary analyses also support the effectiveness of the
# treatment, a non-significant interaction would also support lasting change.

lmm_ssas_total_followup_main <- lmer(ssas_sumscore
                                     ~ 1
                                     + treat_postfollow 
                                     + post_follow
                                     + ssas_sumscore_baseline
                                     + (1|id), 
                                     data = gpp_data_followup)

lmm_ssas_total_followup_int  <- lmer(ssas_sumscore 
                                     ~ 1
                                     + treat_postfollow 
                                     * post_follow
                                     + ssas_sumscore_baseline
                                     + (1|id), 
                                     data = gpp_data_followup)

lrt_ssas_total_followup      <- anova(lmm_ssas_total_followup_main, 
                                      lmm_ssas_total_followup_int, 
                                      test = "LRT")

ssas_followup_desc <- gpp_data_followup %>% 
  filter(complete.cases(treat_postfollow, post_follow)) %>% 
  group_by(treat_postfollow, post_follow) %>% 
  summarise(
    mean  = mean(ssas_sumscore, na.rm = TRUE),
    sd    = sd(ssas_sumscore, na.rm = TRUE),
    se    = sd/sqrt(n()),
    ci_lb = mean - se*qnorm(.975), 
    ci_ub = mean + se*qnorm(.975) 
  )

ggplot(ssas_followup_desc,
       aes(
         x     = as.factor(post_follow),
         color = as.factor(treat_postfollow),
         group = as.factor(treat_postfollow),
         y     = mean,
         ymax  = ci_ub,
         ymin  = ci_lb
       )) +
  geom_point(
    size = 1
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    width     = .33,
    linewidth = 1
  ) +
  scale_color_manual(
    labels = c("Waitlist", "Prevent It"),
    values = c(
      "#003F91",
      "#ED254E"
    )
  ) +
  scale_x_discrete(
    labels = c("Post", "Follow-up")
  ) +
  scale_y_continuous(
    limits = c(0, 48),
    breaks = seq(0, 48, 6)
  ) +
  labs(
    x     = "Time",
    y     = "Mean SSAS Sum Score",
    color = "Group"
  ) +
  theme_classic()

### How severe are the side-effects of the treatment?

# Negative side effects are measured by the NEQ-20, which is administered to
# participants post-treatment.

#### Present trial

# We will examine participants responses on the NEQ-20 descriptively, both the
# total sum score as well as the subscales.

#### Compared to Prevent It 1.0

# To compare the negative side effects of the treatment for Prevent It 1.0 vs. 
# 2.0, we will compare the means on the NEQ-20 for each trial, using all valid
# post-treatment data from both trials, using a Welch t-test. This analysis will
# use the total sum score on the NEQ-20 (min = 0, max = 80).

neq20_comparison <- t.test(neq20 ~ trial, data = pi1_pi2_neq20_data)

### Attrition compared to Prevent It 1.0

# We will create a binary indicator for whether a participant has dropped out at
# each measurement point. The indicator will be coded 0 if the participant
# remains in the trial, and it will be coded 1 if the participant has dropped
# out of the trial. A participant will be said to have dropped out if they
# provide no data at the measurement point in question and no further data. A
# participant will not be coded as a dropout if they have successfully completed
# the treatment early.

# This binary indicator will serve as the primary dependent measure for our
# analyses of attrition and will also be used to calculate the simple rate of
# attrition.

# This indicator will be computed both for the Prevent It 2.0 data and the prior
# Prevent It 1.0 data. Those data sets will be combined for this analysis.

#### Analysis of final attrition rate

# We will calculate the dropout rate at the end of the first treatment wave
# (i.e., at the post-treatment measurement, prior to the follow up), for both
# the current trial as well as the PI1 trial. These two rates will be compared
# using a chi-square test.

attrition_rate_chisq <- prop.test(x = c(att_rate_pi2, att_rate_pi1),
                                  n = c(n_pi2, n_pi1),
                                  alternative = "two.sided",
                                  correct = FALSE)

### To what extent does the treatment affect the participants' quality of life?

# Quality of life will be assessed by the EQ-5D, which is administered to
# participants prior to treatment, after treatment, and at the follow-up.
# Specifically, we will use the visual analogue scale (value range 1 to 100) as
# a measure of self-reported quality of life.

# To examine changes in participants' quality of life, we will fit a series of
# linear mixed effects models predicting VAS ratings from whether they have been
# provided with the treatment and the measurement point (0 = pre, 1 = post). In
# the first model, we will add each of these predictors, with random intercepts
# for each participant. In the second model, we will add the interaction term.
# We will compare the models with a likelihood ratio test.

# We will also fit a similar set of models examining sustained change from post
# treatment to followup. For this, the measurement variable will be coded as 0 =
# post, 1 = follow up. We will add the pre-treatment baseline measure as a
# covariate in each model, and we will compare the models with a likelihood
# ratio test.

#### Effect of treatment

lmm_eq5d_main <- lmer(eq5d_vas 
                      ~ 1 
                      + treat_prepost 
                      + pre_post 
                      + (1|id), 
                      data = gpp_data_main)

lmm_eq5d_int  <- lmer(eq5d_vas 
                      ~ 1 
                      + treat_prepost 
                      * pre_post 
                      + (1|id), 
                      data = gpp_data_main)

lrt_eq5d      <- anova(lmm_eq5d_main, 
                       lmm_eq5d_int, 
                       test = "LRT")

#### Sustained change

lmm_eq5d_followup_main <- lmer(eq5d_vas ~ treatment + measurement + baseline + (1|id), data = pi_data_eq5d_followup)
lmm_eq5d_followup_int  <- lmer(eq5d_vas ~ treatment * measurement + baseline + (1|id), data = pi_data_eq5d_followup)
lrt_eq5d_followup      <- anova(lmm_eq5d_followup_main, lmm_eq5d_followup_int, test = "LRT")

### How does Prevent It 2.0 compare to the first version of Prevent It?                   

# To assess the extent to which Prevent It 2.0 effectively reduces problematic
# sexual behavior compared to Prevent It 1.0, we will fit models predicting the
# variables measured by the SChiMRA+ Part B, pretreatment and posttreatment. The
# treatment indicator variable will be expanded such that

# 0 = waitlist (PI2), 1 = treatment (PI2), 2 = placebo (PI1), 3 = treatment
# (PI1), coded with treatment contrasts such that the waitlist is the reference
# group.

#### CSAM

##### Hours (daily average)

lmm_p1p2_csam_hours_main     <- lmer(schimra_b_csam_hours_avg ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_csam_hours_int      <- lmer(schimra_b_csam_hours_avg ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_csam_hours          <- anova(lmm_p1p2_csam_hours_main, lmm_p1p2_csam_hours_int, test = "LRT")

##### COPINE severity

lmm_p1p2_csam_copine_main    <- lmer(schimra_b_csam_copine_max ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_csam_copine_int     <- lmer(schimra_b_csam_copine_max ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_csam_copine         <- anova(lmm_p1p2_csam_copine_main, lmm_p1p2_csam_copine_int, test = "LRT")

##### Youngest age

lmm_p1p2_csam_age_main       <- lmer(schimra_b_csam_age_min ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_csam_age_int        <- lmer(schimra_b_csam_age_min ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_csam_age            <- anova(lmm_p1p2_csam_age_main, lmm_p1p2_csam_age_int, test = "LRT")

#### Socialization

##### Hours (daily average)

lmm_p1p2_social_hours_main   <- lmer(schimra_b_social_hours_avg ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_social_hours_int    <- lmer(schimra_b_social_hours_avg ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_social_hours        <- anova(lmm_p1p2_social_hours_main, lmm_p1p2_social_hours_int, test = "LRT")

##### Youngest age

lmm_p1p2_social_age_main     <- lmer(schimra_b_social_age_min ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_social_age_int      <- lmer(schimra_b_social_age_min ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_social_age          <- anova(lmm_p1p2_social_age_main, lmm_p1p2_social_age_int, test = "LRT")

#### Sexual interactions

##### Hours (daily average)

lmm_p1p2_interact_hours_main <- lmer(schimra_b_interact_hours_avg ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_interact_hours_int  <- lmer(schimra_b_interact_hours_avg ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_interact_hours      <- anova(lmm_p1p2_interact_hours_main, lmm_p1p2_interact_hours_int, test = "LRT")

##### Youngest age

lmm_p1p2_interact_age_main   <- lmer(schimra_b_interact_age_min ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_interact_age_int    <- lmer(schimra_b_interact_age_min ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_interact_age        <- anova(lmm_p1p2_interact_age_main, lmm_p1p2_interact_age_int, test = "LRT")

#### Other behaviors

##### Hours (daily average)

lmm_p1p2_other_hours_main     <- lmer(schimra_b_other_hours_avg ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_other_hours_int       <- lmer(schimra_b_other_hours_avg ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_other_hours            <- anova(lmm_p1p2_other_hours_main, lmm_p1p2_other_hours_int, test = "LRT")

##### Youngest age

lmm_p1p2_other_age_main       <- lmer(schimra_b_other_age_min ~ treatment + measurement + (1|id), data = pi1_pi2_data_long)
lmm_p1p2_other_age_int         <- lmer(schimra_b_other_age_min ~ treatment * measurement + (1|id), data = pi1_pi2_data_long)
lrt_p1p2_other_age              <- anova(lmm_p1p2_other_age_main, lmm_p1p2_other_age_int, test = "LRT")

# Post Hoc Analyses ------------------------------------------------------------

# HBI-19

lmm_hbi_19_main <- lmer(hbi_19_sumscore
                      ~ 1 
                      + treat_prepost 
                      + pre_post 
                      + (1|id), 
                      data = gpp_data_main)

lmm_hbi_19_int  <- lmer(hbi_19_sumscore 
                      ~ 1 
                      + treat_prepost 
                      * pre_post 
                      + (1|id), 
                      data = gpp_data_main)

lrt_hbi_19      <- anova(lmm_hbi_19_main, 
                       lmm_hbi_19_int, 
                       test = "LRT")

## Visualization

hbi_19_summary <- gpp_data_main %>% 
  group_by(treat_prepost, pre_post) %>% 
  summarise(
    mean_hbi_19 = mean(hbi_19_sumscore, na.rm = TRUE),
    sd_hbi_19   = sd(hbi_19_sumscore, na.rm = TRUE),
    se_hbi_19   = sd_hbi_19/sqrt(n()),
    ci_lb     = mean_hbi_19 - se_hbi_19*qnorm(.975),
    ci_ub     = mean_hbi_19 + se_hbi_19*qnorm(.975),
    n         = n()
  ) %>% 
  filter(!is.na(treat_prepost))

hbi_19_summary$treat_prepost <- factor(hbi_19_summary$treat_prepost,
                                       levels = c(1, 0))

plot_hbi_19_means <- 
ggplot(hbi_19_summary,
       aes(
         y     = mean_hbi_19,
         x     = as.factor(pre_post),
         color = treat_prepost,
         group = treat_prepost,
         ymax  = ci_ub,
         ymin  = ci_lb
       )) + 
  geom_point(
    size = 1
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    width     = .33,
    linewidth = 1
  ) +
  scale_color_manual(
    labels = c("CBT", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  scale_x_discrete(
    labels = c("Pre", "Post")
  ) +
  scale_y_continuous(
    limits = c(5, 95),
    breaks = seq(5, 95, 10)
  ) +
  labs(
    x     = "Time",
    y     = "Mean hypersexuality (HBI-19 total score)",
    color = "Group"
  ) +
  theme_classic()

# ACUTE-2007 dynamic risk

lmm_acute_linear      <- lmer(acute_sum 
                              ~ 1 
                              + treatment 
                              + time 
                              + time_after 
                              + (1|id), 
                              data = gpp_data_main)

lmm_acute_quad        <- lmer(acute_sum 
                              ~ 1 
                              + treatment 
                              + time 
                              + time_after 
                              + time_sq
                              + (1|id), 
                              data = gpp_data_main)

lrt_acute             <- anova(lmm_acute_linear, 
                               lmm_acute_quad, 
                               test = "LRT")

## Predicted ACUTE-2007 values

### Create new data for predictions

pred_df_acute <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq       = time^2
  )

### Predictions from retained model

predict_df_acute <- as.data.frame(
  predict(lmm_acute_quad, 
          newdata = pred_df_acute,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_acute <- bind_cols(pred_df_acute, predict_df_acute)

pred_df_acute <- pred_df_acute %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_acute_predict <- 
  ggplot(pred_df_acute,
         aes(
           y     = fit,
           x     = time,
           color = group
         )) +
  geom_line(
    linewidth = 1
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_lb,
      x     = time,
      color = group
    )
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_ub,
      x     = time,
      color = group
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 21, 3),
    limits = c(0, 21)
  ) +
  scale_x_continuous(
    breaks = 0:10
  ) +
  scale_color_manual(
    labels = c("Prevent It", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted ACUTE-2007 Sum Score",
    color = "Group"
  ) +
  theme_classic()

# Effect size calculation

contrast_acute   <- predictions(lmm_acute_quad,
                               newdata    = pred_df_acute %>% 
                                 filter(time == 9),
                               hypothesis = "pairwise",
                               re.form    = NA)

contrast_acute_d <- paste(
  round(contrast_acute$estimate / sigma(lmm_acute_quad), 3),
  " 95% CI [",
  round(contrast_acute$conf.low / sigma(lmm_acute_quad), 3),
  ", ",
  round(contrast_acute$conf.high / sigma(lmm_acute_quad), 3),
  "]",
  sep = ""
)

# Mixed Effects Model Without Crossover (Primary Outcome, SSAS)

# This can be considered a sensitivity analysis that only looks at the data
# without the waitlist participants who crossed over into the treatment.

lmm_ssas_nc_linear      <- lmer(ssas_sumscore 
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                + (1|id), 
                                data = gpp_data_main %>% 
                                  filter(
                                    !(assigned_group == "waitlist" & time > 10)
                                  ))

lmm_ssas_nc_quad        <- lmer(ssas_sumscore 
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                + time_sq
                                + (1|id), 
                                data = gpp_data_main %>% 
                                  filter(
                                    !(assigned_group == "waitlist" & time > 10)
                                  ))

lmm_ssas_nc_quad_2      <- lmer(ssas_sumscore 
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                + time_sq 
                                + time_after_sq 
                                + (1|id), 
                                data = gpp_data_main %>% 
                                  filter(
                                    !(assigned_group == "waitlist" & time > 10)
                                  ))

lrt_ssas_nc             <- anova(lmm_ssas_nc_linear, 
                                 lmm_ssas_nc_quad, 
                                 lmm_ssas_nc_quad_2, 
                                 test = "LRT")


### Predicted SSAS values

#### Create new data for predictions

pred_df_ssas_nc <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq       = time^2,
    time_after_sq = time_after^2
  )

#### Predictions from retained model

predict_df_nc_ssas <- as.data.frame(
  predict(lmm_ssas_nc_quad, 
          newdata = pred_df_ssas_nc,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_ssas_nc <- bind_cols(pred_df_ssas_nc, predict_df_nc_ssas)

pred_df_ssas_nc <- pred_df_ssas_nc %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_ssas_nc_predict <- 
  ggplot(pred_df_ssas_nc,
         aes(
           y     = fit,
           x     = time,
           color = group
         )) +
  geom_line(
    linewidth = 1
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_lb,
      x     = time,
      color = group
    )
  ) +
  geom_line(
    linetype = "dashed",
    aes(
      y     = ci_ub,
      x     = time,
      color = group
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 48, 6),
    limits = c(0, 48)
  ) +
  scale_x_continuous(
    breaks = 0:10
  ) +
  scale_color_manual(
    labels = c("Prevent It", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted SSAS Sum Score",
    color = "Group"
  ) +
  theme_classic()

### Effect size calculation

contrast_ssas_nc   <- predictions(lmm_ssas_nc_quad,
                               newdata    = pred_df_ssas_nc %>% 
                                 filter(time == 9),
                               hypothesis = "pairwise",
                               re.form    = NA)

contrast_ssas_nc_d <- paste(
  round(contrast_ssas_nc$estimate / sigma(lmm_ssas_nc_quad), 3),
  " 95% CI [",
  round(contrast_ssas_nc$conf.low / sigma(lmm_ssas_nc_quad), 3),
  ", ",
  round(contrast_ssas_nc$conf.high / sigma(lmm_ssas_nc_quad), 3),
  "]",
  sep = ""
)

# Pre-Post Comparison Without Crossover (Primary Outcome, SSAS)

# This can be considered a sensitivity analysis that only looks at the data
# without the waitlist participants who crossed over into the treatment.

lmm_ssas_prepost_main <- lmer(ssas_sumscore 
                              ~ 1 
                              + treat_prepost 
                              + pre_post 
                              + (1|id), 
                              data = gpp_data_main %>% 
                                filter(
                                  !(assigned_group == "waitlist" & time > 10)
                                  ))

lmm_ssas_prepost_int  <- lmer(ssas_sumscore 
                              ~ 1 
                              + treat_prepost 
                              * pre_post 
                              + (1|id), 
                              data = gpp_data_main %>% 
                                filter(
                                  !(assigned_group == "waitlist" & time > 10)
                                ))

lrt_ssas_prepost      <- anova(lmm_ssas_prepost_main, 
                               lmm_ssas_prepost_int, 
                               test = "LRT")

# Sensitivity analysis (HBI-19 baseline)

lmm_ssas_quad_hbi        <- lmer(ssas_sumscore 
                                 ~ 1 
                                 + treatment 
                                 + time 
                                 + time_after 
                                 + time_sq
                                 + scale(hbi_sumscore_baseline, scale = FALSE)
                                 + (1|id), 
                                 data = gpp_data_main)

lmm_ssas_rs_hbi          <- lmer(ssas_sumscore 
                                 ~ 1 
                                 + treatment 
                                 + time 
                                 + time_after 
                                 + time_sq
                                 + scale(hbi_sumscore_baseline, scale = FALSE)
                                 + (1 + time + time_after|id), 
                                 control = lmerControl(
                                   optimizer = "bobyqa"
                                 ), 
                                 data = gpp_data_main)

# Sensitivity analysis (assigned group)

# This analysis controls for which group the participants were assigned to,
# which in effect helps account for baseline differences in SSAS score.

gpp_data_main$assigned_group <- factor(gpp_data_main$assigned_group,
                                       levels = c("waitlist", "cbt"))

lmm_ssas_quad_arm        <- lmer(ssas_sumscore 
                                 ~ 1 
                                 + treatment 
                                 + time 
                                 + time_after 
                                 + time_sq
                                 + assigned_group
                                 + (1|id), 
                                 data = gpp_data_main)

lmm_ssas_rs_arm          <- lmer(ssas_sumscore 
                                 ~ 1 
                                 + treatment 
                                 + time 
                                 + time_after 
                                 + time_sq
                                 + assigned_group
                                 + (1 + time + time_after|id), 
                                 control = lmerControl(
                                   optimizer = "bobyqa"
                                 ), 
                                 data = gpp_data_main)

## Interaction between assigned group and treatment effect over time

lmm_ssas_int_arm         <- lmer(ssas_sumscore 
                                 ~ 1 
                                 + treatment 
                                 + time 
                                 + time_after
                                 * assigned_group
                                 + time_sq
                                 + (1|id), 
                                 data = gpp_data_main)

lmm_ssas_int_rs_arm      <- lmer(ssas_sumscore 
                                 ~ 1 
                                 + treatment 
                                 + time 
                                 + time_after
                                 * assigned_group
                                 + time_sq
                                 + (1 + time + time_after|id), 
                                 control = lmerControl(
                                   optimizer = "bobyqa"
                                 ), 
                                 data = gpp_data_main)


# Reliability analysis ---------------------------------------------------------

ssas_reliability <- omega(ssas_df, fm = "ml")

# Visualization export ---------------------------------------------------------

# Model predictions

predict_motive_grid <- plot_grid(plot_motivewatch_predict,
                                 plot_motivesocial_predict,
                                 plot_motiveinteract_predict,
                                 plot_hbi_19_means,
                                 nrow = 2)

save_plot("figures/gpp_schimra-a-prediction.png",
          predict_motive_grid,
          base_width = 10, base_height = 8)

# Main figure

predict_main_grid <- plot_grid(plot_ssas_predict,
                               plot_csam_predict,
                               plot_copine_predict,
                               plot_motivewatch_predict,
                               plot_motivesocial_predict,
                               plot_motiveinteract_predict,
                               nrow = 3)

save_plot("figures/gpp_primary-secondary-outcome-prediction.png",
          predict_main_grid,
          base_width = 10, base_height = 12)


# Model tables -----------------------------------------------------------------

## Basic table function

lmm_table <- function(model, 
                      dig          = 3,
                      fixed_names  = c(
                        "Intercept",
                        "Treatment start",
                        "Time (weeks)",
                        "Time in treatment",
                        "Time (quadratic)",
                        "Time in treatment (quadratic)"
                      ),
                      random_groups = c(
                        "Participant",
                        "Residual"
                      ),
                      random_terms  = c(
                        "Intercept SD",
                        "SD"
                      ) ) {
  
  # Fixed effects 
  
  fixed_effects  <- summary(model)$coefficients
  
  colnames(fixed_effects) <- c(
    "Estimate",
    "SE",
    "df",
    "t statistic",
    "p-value"
  )
  
  fixed_effects <- fixed_effects %>% 
    round(dig) %>% 
    as.data.frame() %>% 
    mutate(
      `p-value` = case_when(
        `p-value` != 0 ~ as.character(`p-value`),
        `p-value` == 0 ~ "<.001"
      )
    )
  
  fixed_effects$`p-value` <- str_replace(fixed_effects$`p-value`, "0\\.", "\\.")
  
  fixed_effects$Term <- fixed_names
  
  fixed_effects <- fixed_effects %>% 
    relocate(Term, .before = "Estimate")
  
  fixed_effects$Effect <- "Fixed"
  
  # Random Effects
  
  random_effects <- as.data.frame(summary(model)$varcor)
  
  colnames(random_effects) <- c(
    "Groups",
    "Term",
    "x",
    "Variance",
    "Estimate"
  )
  
  random_effects <- random_effects %>% 
    select(-x, -Variance)
  
  random_effects$Groups <- random_groups
  
  random_effects$Term <- random_terms
  
  random_effects$Estimate <- round(random_effects$Estimate, dig)
  
  random_effects$Effect <- "Random"
  
  effects <- bind_rows(fixed_effects, random_effects) %>% 
    relocate(Groups, .before = "Term")
  
  effects %>% 
    as_grouped_data(groups = "Effect") %>% 
    flextable() %>% 
    align(align = c("left",
                    "left",
                    "left",
                    "right",
                    "right",
                    "right",
                    "right",
                    "right"))
  
}

# Results tables

table_watch <- lmm_watch_motive_quad %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Time (quadratic)"
    )
  ) %>% 
  autofit()

table_social <- lmm_social_motive_quad %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Time (quadratic)"
    )
  ) %>% 
  autofit()

table_interact <- lmm_interact_motive_quad %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Time (quadratic)"
    )
  ) %>% 
  autofit()

## Sensitivity analyses

table_sens_hbi <- lmm_ssas_quad_hbi %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Time (quadratic)",
      "Baseline HBI-19 score"
    )
  ) %>% 
  autofit()

table_sens_arm <- lmm_ssas_quad_arm %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Time (quadratic)",
      "Assigned Group (CBT)"
    )
  ) %>% 
  autofit()

table_sens_int_arm <- lmm_ssas_int_arm %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Assigned Group (CBT)",
      "Time (quadratic)",
      "Time in treatment x Group"
    )
  ) %>% 
  autofit()

# Export tables

## Primary and secondary outcomes

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

save_as_docx("Watch"     = table_watch,
             "Socialize" = table_social,
             "Interact"  = table_interact,
             path  = "output/gpp_posthoc-tables.docx",
             align = "center")

save_as_docx("Baseline HBI-19"             = table_sens_hbi,
             "Assigned Group"              = table_sens_arm,
             "Treatment x Assigned Group"  = table_sens_int_arm,
             path  = "output/gpp_baseline-difference-tables.docx",
             align = "center")

