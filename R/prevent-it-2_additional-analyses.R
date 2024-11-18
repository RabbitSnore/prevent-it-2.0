################################################################################

# Prevent It 2.0, Global Perpetration Prevention -- Additional Analyses

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("lme4", 
              "lmerTest", 
              "ggplot")

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
    labels = c("CBT", "Waitlist"),
    values = c(
      "#ED254E",
      "#003F91"
    )
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted Motvation to Watch CSAM",
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
    labels = c("CBT", "Waitlist"),
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
    labels = c("CBT", "Waitlist"),
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
# As long as the primary analyses also support the effectiveness of the treatment,
# a non-significant 

lmm_ssas_total_followup_main <- lmer(ssas_total ~ treatment + measurement + baseline + (1|id), data = pi_data_csam_hours_sust)
lmm_ssas_total_followup_int  <- lmer(ssas_total ~ treatment * measurement + baseline + (1|id), data = pi_data_csam_hours_sust)
lrt_ssas_total_followup      <- anova(lmm_ssas_total_followup_main, lmm_ssas_total_followup_int, test = "LRT")

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

# Visualization export ---------------------------------------------------------

# Model predictions

predict_motive_grid <- plot_grid(plot_motivewatch_predict ,
                                 plot_motivesocial_predict ,
                                 plot_motiveinteract_predict ,
                                 nrow = 2)

save_plot("figures/gpp_schimra-a-prediction.png",
          predict_motive_grid,
          base_width = 10, base_height = 8)


