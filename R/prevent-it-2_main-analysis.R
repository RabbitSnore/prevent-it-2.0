################################################################################

# Prevent It 2.0 -- Main Analysis

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("lme4", 
              "lmerTest", 
              "ggplot2",
              "cowplot",
              "flextable",
              "marginaleffects",
              "boot")

lapply(packages, library, character.only = TRUE)

# Functions --------------------------------------------------------------------

post_prediction <- function(newdata) {
  
  function(x) {
    
    pred <- predict(x, 
                    newdata = newdata, 
                    re.form = NA)
    
    pred[[1]] - pred[[2]]
    
  }
  
}

# Data analysis ----------------------------------------------------------------

# This script is written with the assumption that the data have already been
# wrangled into suitable structures.

## Preliminary note ------------------------------------------------------------

# For all analyses using mixed-effects models, we will not impute missing data.

## Primary research question ---------------------------------------------------

# Is there a decrease in problematic sexual urges as a function of the treatment?

# Note: This analysis corresponds to the primary outcome measure in the
# ClinicalTrials preregistration.

# The primary outcome is a measure of problematic sexual urges, assessed through
# the SSAS. We will use the sum score from the 12 items on the SSAS, for a total
# score that varies from 0 to 48 (each item uses a 0 to 4 scale). For the
# present study, we will modify items 3, 6, and 8 on the SSAS. These three items
# prompt the participant to provide an estimate of how much time they have been
# spent in the last week engaged in problematic sexual thoughts or behaviors. In
# the original measure, participants are asked to indicate an estimate given
# pre-specified intervals. Here, we will prompt them to provide an estimate with
# a drop down menu. We will then convert these values into a 0-4 scale. For each
# item, an estimate of 0 hours will be coded as 0. Using the non-zero values
# (across all measurement points), we will calculate quartiles, and these
# quartiles will form the basis of the scores of 1 through 4 (1 = >0% and <25%,
# 2 = >=25% and <50%, 3 = >=50% and <75%, 4 = >=75%). Any responses using the
# option "more than 24 hours" will not be used to calculate the quartiles and
# will be coded as 4.

# For this composite outcome variable we will fit a linear mixed effects
# model, with a dummy coded treatment predictor (0 = waitlist, 1 = Prevent It),
# indicating whether treatment has commenced, a time predictor (starting at 0,
# and counting each weekly measurement point), and a time-since-treatment
# predictor (starting at 0, and counting up at each measurement point after
# treatment begins). We will also fit another model adding quadratic terms for
# the time and time-since-treatment predictors. We will compare these two models
# using a likelihood ratio test and retain the better performing model for
# interpretation. We will use a significance threshold of .05 for all tests.

# For significance tests for the coefficients in the linear mixed effects
# models, we will use the Satterthwaite method for calculating degrees of
# freedom.

# Some participants will be assigned to receive treatment immediately, and
# others will be assigned to a waitlist during that treatment period (and will
# receive treatment thereafter).
#
# Treatment begins with a baseline measurement, followed by nine weeks of
# treatment, with a measurement taken at each week. The waitlist is measured on
# the same schedule as those who begin treatment immediately, and then a
# baseline measure is taken in four weeks.
#
# This schedule can be represented schematically as follows:
#
# TREATMENT: 
# time:       00 01 02 03 04 05 06 07 08 09 -- -- -- -- -- -- -- -- -- -- -- -- --
# treatment:  00 01 01 01 01 01 01 01 01 01 -- -- -- -- -- -- -- -- -- -- -- -- --
# time_after: 00 01 02 03 04 05 06 07 08 09 -- -- -- -- -- -- -- -- -- -- -- -- -- 
#
# WAITLIST:  
# time        00 01 02 03 04 05 06 07 08 09 -- -- -- 13 14 15 16 17 18 19 20 21 22
# treatment:  00 00 00 00 00 00 00 00 00 00 -- -- -- 00 01 01 01 01 01 01 01 01 01
# time_after: 00 00 00 00 00 00 00 00 00 00 -- -- -- 00 01 02 03 04 05 06 07 08 09 
#
# Each double-digit number represents a measurement point, noted with its 
# corresponding value for each predictor variable.
#
# This schematic diagram does not include the "follow-up" measurement for the
# Treatment group and the waitlist group (after treatment). This follow-up 
# measure point will not be included in the primary analysis.

lmm_ssas_linear      <- lmer(ssas_sumscore 
                             ~ 1 
                             + treatment 
                             + time 
                             + time_after 
                             + (1|id), 
                             data = gpp_data_main)

lmm_ssas_quad        <- lmer(ssas_sumscore 
                             ~ 1 
                             + treatment 
                             + time 
                             + time_after 
                             + time_sq
                             + (1|id), 
                             data = gpp_data_main)

lmm_ssas_quad_2      <- lmer(ssas_sumscore 
                             ~ 1 
                             + treatment 
                             + time 
                             + time_after 
                             + time_sq 
                             + time_after_sq 
                             + (1|id), 
                             data = gpp_data_main)

lrt_ssas             <- anova(lmm_ssas_linear, 
                              lmm_ssas_quad, 
                              lmm_ssas_quad_2, 
                              test = "LRT")

## Visualization of mean SSAS scores

ssas_time_arm <- gpp_data_main %>% 
  group_by(assigned_group, time) %>% 
  summarise(
    mean_ssas = mean(ssas_sumscore, na.rm = TRUE),
    sd_ssas   = sd(ssas_sumscore, na.rm = TRUE),
    se_ssas   = sd_ssas/sqrt(n()),
    ci_lb     = mean_ssas - se_ssas*qnorm(.975),
    ci_ub     = mean_ssas + se_ssas*qnorm(.975),
    n         = n()
  )

plot_ssas_time <- 
ggplot(ssas_time_arm,
       aes(
         x     = time, 
         y     = mean_ssas,
         ymax  = ci_ub,
         ymin  = ci_lb,
         group = assigned_group,
         color = assigned_group
       )) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    linewidth = 1,
    width = .33
  ) +
  scale_x_continuous(
    breaks = 0:max(ssas_time_arm$time, na.rm = TRUE)
  ) +
  scale_y_continuous(
    breaks = 0:48
  ) +
  labs(
    x = "Time",
    y = "Mean SSAS Score",
    color = "Group"
  ) +
  theme_classic()

plot_ssas_time_arm <- 
ggplot(ssas_time_arm %>% 
         filter(n > 10),
       aes(
         x     = time, 
         y     = mean_ssas,
         ymax  = ci_ub,
         ymin  = ci_lb,
         group = assigned_group
       )) +
  facet_wrap(~ assigned_group,
             nrow = 2) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    linewidth = 1,
    width = .33
  ) +
  scale_x_continuous(
    breaks = 0:max(ssas_time_arm$time, na.rm = TRUE)
  ) +
  scale_y_continuous(
    breaks = 0:48
  ) +
  geom_line(
    alpha     = .20,
    linewidth = 1,
    data = gpp_data_main,
    inherit.aes = FALSE,
    aes(
      x = time,
      y = ssas_sumscore,
      group = id,
      color = as.factor(treatment)
    )
  ) +
  labs(
    x = "Time",
    y = "Mean SSAS Score",
    color = "Group"
  ) +
  theme_classic()

## Predicted SSAS values

### Create new data for predictions

pred_df_ssas <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq       = time^2,
    time_after_sq = time_after^2
  )

### Predictions from retained model

predict_df_ssas <- as.data.frame(
  predict(lmm_ssas_quad, 
          newdata = pred_df_ssas,
          re.form = NA, 
          se.fit  = TRUE)
  )

pred_df_ssas <- bind_cols(pred_df_ssas, predict_df_ssas)

pred_df_ssas <- pred_df_ssas %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_ssas_predict <- 
ggplot(pred_df_ssas,
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

# Effect size calculation

contrast_ssas   <- predictions(lmm_ssas_quad,
                               newdata    = pred_df_ssas %>% 
                                 filter(time == 9),
                               hypothesis = "pairwise",
                               re.form    = NA)

contrast_ssas_d <- paste(
  round(contrast_ssas$estimate / sigma(lmm_ssas_quad), 3),
  " 95% CI [",
  round(contrast_ssas$conf.low / sigma(lmm_ssas_quad), 3),
  ", ",
  round(contrast_ssas$conf.high / sigma(lmm_ssas_quad), 3),
  "]",
  sep = ""
)

## Bootstrapped effect size

bootstrap_ssas   <- bootMer(lmm_ssas_quad,
                            post_prediction(pred_df_ssas %>% 
                                              filter(time == 9)),
                            type = "parametric",
                            nsim = 5000)

boot_ci_ssas     <- boot.ci(bootstrap_ssas, type = "perc") 

bootstrap_ssas_d <- paste(
  round(bootstrap_ssas$t0 / sigma(lmm_ssas_quad), 3),
  " 95% CI [",
  round(boot_ci_ssas$percent[4] / sigma(lmm_ssas_quad), 3),
  ", ",
  round(boot_ci_ssas$percent[5] / sigma(lmm_ssas_quad), 3),
  "]",
  sep = ""
)

# Sensitivity analyses/Robustness checks

## Selection model

# We will use a Heckman-type selection modeling approach as a sensitivity
# analysis. First we will predict missingness in the SSAS data using a probit
# model. We will then use the predicted values from the probit model to
# calculate the inverse Mills ratio, which will then be added as a predictor in
# the model predicting the outcome. The probit model will use the same
# predictors as the outcome model, with additional predictors expected to be
# associated with missingness.

### Missingness model (probit)

# This model will be modified to incorporate predictors that are deemed
# plausible to predict dropouts/missingness, based on clinical experience and
# exploration of the data. For now, "ADDITIONAL_PREDICTORS" is included in the
# model formula as a placeholder.

# Time points between the end of the waitlist period and the beginning of
# treatment will be missing by design, so they will treated as missing in the
# variable indicating (planned) missingness.

gpp_heckman_data <- gpp_data_main %>% 
  filter(
    complete.cases(lassie_pedophilia_01_baseline,
                   swch_sumscore_baseline,
                   raads_14_sumscore_baseline,
                   cecwc_sumscore_baseline,
                   phq_sumscore_baseline)
  )

miss_ssas              <- glmer(ssas_missing 
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                # + time_sq
                                + lassie_pedophilia_01_baseline
                                + cecwc_sumscore_baseline
                                + swch_sumscore_baseline
                                + raads_14_sumscore_baseline
                                + phq_sumscore_baseline
                                + (1|id), 
                                data = gpp_heckman_data, 
                                family = binomial(link = "probit"),
                                control = glmerControl(
                                  optimizer = "bobyqa",
                                  optCtrl   = list(
                                    maxfun = 100000
                                  )
                                ))

gpp_heckman_data$inv_mills <- 
  dnorm( predict(miss_ssas) ) / pnorm( predict(miss_ssas) )

### Outcome model

lmm_sass_sens_linear      <- lmer(ssas_sumscore 
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + inv_mills 
                                  + (1|id), 
                                  data = gpp_heckman_data)

lmm_sass_sens_quad        <- lmer(ssas_sumscore 
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + time_sq 
                                  + inv_mills 
                                  + (1|id), 
                                  data = gpp_heckman_data)

lrt_sass_sens             <- anova(lmm_sass_sens_linear, 
                                   lmm_sass_sens_quad, 
                                   test = "LRT")

# Random slopes model

lmm_ssas_quad_rs        <- lmer(ssas_sumscore 
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                + time_sq 
                                + (1 + time + time_after|id), 
                                control = lmerControl(
                                  optimizer = "bobyqa"
                                ),
                                data = gpp_data_main)

## Secondary research questions ------------------------------------------------

### Is there a change in abuse behavior?

# Note: This analysis corresponds to the secondary outcome measure in the
# ClinicalTrials preregistration.

# This secondary research question concerns the effect of the Prevent It 2.0
# treatment, compared to a waitlist control group, on the time spend using CSAM
# to socializing with children, having sexual interactions with children, and
# other behaviors related to sexual interest in children.
#
# These behaviors will be assessed using Part B of the SChiMRA+ (4 items)

# Each of the four items on Part B of the SChiMRA+ takes a measure of the number
# of days in which the participant engaged in the behavior of interest, time
# spent in hours engaging in the behavior for each active day of the last seven
# days, as well as a measure of either the age of the youngest child involved in
# the activities of each day and/or the severity of the material (using the
# COPINE scale). For each seven day period, each participant's time measures
# will be summed (in hours) and divided by 7, to create a daily average measure
# for that week. For the youngest age and COPINE severity measure, we will take
# the lowest reported age for that week and the highest reported COPINE severity
# for that week, and these will serve as dependent measures.

# We will address this research question with models highly similar to those
# used for the primary research question.

#### CSAM

##### Hours (weekly average) -- Active user subgroup

lmm_csam_hours_linear      <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_active_csam)

lmm_csam_hours_quad        <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq
                                   + (1|id), 
                                   data = gpp_data_active_csam)

lmm_csam_hours_quad_2      <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_active_csam)

lrt_csam_hours             <- anova(lmm_csam_hours_linear, 
                                    lmm_csam_hours_quad, 
                                    lmm_csam_hours_quad_2, 
                                    test = "LRT")

###### Visualization of weekly average CSAM use

csam_time_arm <- gpp_data_active_csam %>% 
  group_by(assigned_group, time) %>% 
  summarise(
    mean_csam = mean(schimra_b_csam_hours_avg, na.rm = TRUE),
    sd_csam   = sd(schimra_b_csam_hours_avg, na.rm = TRUE),
    se_csam   = sd_csam/sqrt(n()),
    ci_lb     = mean_csam - se_csam*qnorm(.975),
    ci_ub     = mean_csam + se_csam*qnorm(.975),
    n         = n()
  )

plot_csam_time <- 
ggplot(csam_time_arm,
       aes(
         x     = time, 
         y     = mean_csam,
         ymax  = ci_ub,
         ymin  = ci_lb,
         group = assigned_group,
         color = assigned_group
       )) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    linewidth = 1,
    width = .33
  ) +
  scale_x_continuous(
    breaks = 0:max(csam_time_arm$time)
  ) +
  labs(
    x = "Time",
    y = "Weekly Average CSAM Use",
    color = "Group"
  ) +
  theme_classic()


## Predicted CSAM use values

### Create new data for predictions

pred_df_csam <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq       = time^2,
    time_after_sq = time_after^2
  )

### Predictions from retained model

predict_df_csam <- as.data.frame(
  predict(lmm_csam_hours_quad, 
          newdata = pred_df_csam,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_csam <- bind_cols(pred_df_csam, predict_df_csam)

pred_df_csam <- pred_df_csam %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_csam_predict_active <- 
ggplot(pred_df_csam,
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
    breaks = seq(0, 1.5, .25),
    limits = c(-.25, 1.5)
  ) +
  scale_x_continuous(
    breaks = 0:9
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted Weekly Average CSAM Use (Hours)",
    color = "Group"
  ) +
  theme_classic()

##### Hours (weekly average) -- All participants

lmm_csam_hours_all_linear  <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_hours_all_quad    <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_hours_all_quad_2  <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_csam_hours_all         <- anova(lmm_csam_hours_all_linear, 
                                    lmm_csam_hours_all_quad, 
                                    lmm_csam_hours_all_quad_2,
                                    test = "LRT")

csam_time_arm_all <- gpp_data_main %>% 
  filter(!is.na(time)) %>% 
  group_by(assigned_group, time) %>% 
  summarise(
    mean_csam = mean(schimra_b_csam_hours_avg, na.rm = TRUE),
    sd_csam   = sd(schimra_b_csam_hours_avg, na.rm = TRUE),
    se_csam   = sd_csam/sqrt(n()),
    ci_lb     = mean_csam - se_csam*qnorm(.975),
    ci_ub     = mean_csam + se_csam*qnorm(.975),
    n         = n()
  )

plot_csam_time_all <- 
  ggplot(csam_time_arm_all,
         aes(
           x     = time, 
           y     = mean_csam,
           ymax  = ci_ub,
           ymin  = ci_lb,
           group = assigned_group,
           color = assigned_group
         )) +
  geom_line(
    linewidth = 1
  ) +
  geom_errorbar(
    linewidth = 1,
    width = .33
  ) +
  scale_x_continuous(
    breaks = 0:max(csam_time_arm_all$time)
  ) +
  labs(
    x = "Time",
    y = "Weekly Average CSAM Use",
    color = "Group"
  ) +
  theme_classic()

## Predicted CSAM use values

### Create new data for predictions

pred_df_csam_all <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq       = time^2,
    time_after_sq = time_after^2
  )

### Predictions from retained model

predict_df_csam_all <- as.data.frame(
  predict(lmm_csam_hours_all_quad, 
          newdata = pred_df_csam_all,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_csam_all <- bind_cols(pred_df_csam_all, predict_df_csam_all)

pred_df_csam_all <- pred_df_csam_all %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_csam_predict <- 
ggplot(pred_df_csam_all,
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
    breaks = seq(0, 8.0, 1),
    limits = c(0, 8.0)
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
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  labs(
    x     = "Time (Weeks)",
    y     = "Predicted Weekly Hours Observing Children",
    color = "Group"
  ) +
  theme_classic()

# Effect size calculation

contrast_csam   <- predictions(lmm_csam_hours_all_quad,
                               newdata    = pred_df_csam_all %>% 
                                 filter(time == 9),
                               hypothesis = "pairwise",
                               re.form    = NA)

contrast_csam_d <- paste(
  round(contrast_csam$estimate / sigma(lmm_csam_hours_all_quad), 3),
  " 95% CI [",
  round(contrast_csam$conf.low / sigma(lmm_csam_hours_all_quad), 3),
  ", ",
  round(contrast_csam$conf.high / sigma(lmm_csam_hours_all_quad), 3),
  "]",
  sep = ""
)

##### COPINE severity

lmm_csam_copine_linear     <- lmer(schimra_b_csam_copine_max 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_copine_quad       <- lmer(schimra_b_csam_copine_max
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_copine_quad_2       <- lmer(schimra_b_csam_copine_max
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + time_after_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lrt_csam_copine            <- anova(lmm_csam_copine_linear, 
                                    lmm_csam_copine_quad, 
                                    lmm_csam_copine_quad_2, 
                                    test = "LRT")

## Predicted COPINE severity value

### Create new data for predictions

pred_df_copine <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
)

### Predictions from retained model

predict_df_copine <- as.data.frame(
  predict(lmm_csam_copine_linear, 
          newdata = pred_df_copine,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_copine <- bind_cols(pred_df_copine, predict_df_copine)

pred_df_copine <- pred_df_copine %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_copine_predict <- 
  ggplot(pred_df_copine,
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
    breaks = 1:10,
    limits = c(1, 10)
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
    y     = "Predicted Maximum COPINE Severity",
    color = "Group"
  ) +
  theme_classic()

# Effect size calculation

contrast_copine   <- predictions(lmm_csam_copine_linear,
                               newdata    = pred_df_copine %>% 
                                 filter(time == 9),
                               hypothesis = "pairwise",
                               re.form    = NA)

contrast_copine_d <- paste(
  round(contrast_copine$estimate / sigma(lmm_csam_copine_linear), 3),
  " 95% CI [",
  round(contrast_copine$conf.low / sigma(lmm_csam_copine_linear), 3),
  ", ",
  round(contrast_copine$conf.high / sigma(lmm_csam_copine_linear), 3),
  "]",
  sep = ""
)

##### Youngest age

lmm_csam_age_linear        <- lmer(schimra_b_csam_age_min 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_age_quad          <- lmer(schimra_b_csam_age_min 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_age_quad_2          <- lmer(schimra_b_csam_age_min 
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + time_after_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lrt_csam_age               <- anova(lmm_csam_age_linear, 
                                    lmm_csam_age_quad, 
                                    lmm_csam_age_quad_2, 
                                    test = "LRT")

### Create new data for predictions

pred_df_age <- data.frame(
  group      = c(rep("cbt", 10), rep("waitlist", 10)),
  treatment  = c(0, rep(1, 9) , rep(0, 10)),
  time       = c(0:9, 0:9),
  time_after = c(0, 1:9, rep(0, 10))
) %>% 
  mutate(
    time_sq = time^2
  )

### Predictions from retained model

predict_df_age <- as.data.frame(
  predict(lmm_csam_age_quad, 
          newdata = pred_df_age,
          re.form = NA, 
          se.fit  = TRUE)
)

pred_df_age <- bind_cols(pred_df_age, predict_df_age)

pred_df_age <- pred_df_age %>% 
  mutate(
    ci_lb = fit - se.fit*qnorm(.975),
    ci_ub = fit + se.fit*qnorm(.975)
  )

### Visualization of predicted values

plot_age_predict <- 
  ggplot(pred_df_age,
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
    y     = "Predicted Minimum Age of Youngest Child",
    color = "Group"
  ) +
  theme_classic()

# Effect size calculation

contrast_age   <- predictions(lmm_csam_age_quad,
                                 newdata    = pred_df_age %>% 
                                   filter(time == 9),
                                 hypothesis = "pairwise",
                                 re.form    = NA)

contrast_age_d <- paste(
  round(contrast_age$estimate / sigma(lmm_csam_age_quad), 3),
  " 95% CI [",
  round(contrast_age$conf.low / sigma(lmm_csam_age_quad), 3),
  ", ",
  round(contrast_age$conf.high / sigma(lmm_csam_age_quad), 3),
  "]",
  sep = ""
)

#### Socialization

##### Hours (daily average)

lmm_social_hours_linear    <- lmer(schimra_b_social_hours_avg 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_social_hours_quad      <- lmer(schimra_b_social_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_social_hours_quad_2    <- lmer(schimra_b_social_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_social_hours           <- anova(lmm_social_hours_linear, 
                                    lmm_social_hours_quad, 
                                    lmm_social_hours_quad_2, 
                                    test = "LRT")

##### Youngest age

lmm_social_age_linear    <- lmer(schimra_b_social_age_min 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_social_age_quad      <- lmer(schimra_b_social_age_min 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_social_age_quad_2    <- lmer(schimra_b_social_age_min 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_social_age           <- anova(lmm_social_age_linear, 
                                    lmm_social_age_quad, 
                                    lmm_social_age_quad_2, 
                                    test = "LRT")

#### Sexual interactions

##### Hours (daily average)

lmm_interact_hours_linear    <- lmer(schimra_b_interact_hours_avg 
                                     ~ 1
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + (1|id), 
                                     data = gpp_data_main)

lmm_interact_hours_quad      <- lmer(schimra_b_interact_hours_avg 
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lmm_interact_hours_quad_2    <- lmer(schimra_b_interact_hours_avg 
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + time_after_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lrt_interact_hours           <- anova(lmm_interact_hours_linear, 
                                      lmm_interact_hours_quad, 
                                      lmm_interact_hours_quad_2, 
                                      test = "LRT")

##### Youngest age

lmm_interact_age_linear    <- lmer(schimra_b_interact_age_min 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_interact_age_quad      <- lmer(schimra_b_interact_age_min
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_interact_age_quad_2      <- lmer(schimra_b_interact_age_min
                                     ~ 1 
                                     + treatment 
                                     + time 
                                     + time_after 
                                     + time_sq 
                                     + time_after_sq 
                                     + (1|id), 
                                     data = gpp_data_main)

lrt_interact_age           <- anova(lmm_interact_age_linear, 
                                    lmm_interact_age_quad, 
                                    lmm_interact_age_quad_2, 
                                    test = "LRT")

#### Other behaviors

##### Hours (daily average)

lmm_other_hours_linear    <- lmer(schimra_b_other_hours_avg 
                                  ~ 1
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + (1|id), 
                                  data = gpp_data_main)

lmm_other_hours_quad      <- lmer(schimra_b_other_hours_avg 
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + time_sq 
                                  + (1|id), 
                                  data = gpp_data_main)

lmm_other_hours_quad_2    <- lmer(schimra_b_other_hours_avg 
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + time_sq 
                                  + time_after_sq 
                                  + (1|id), 
                                  data = gpp_data_main)

lrt_other_hours           <- anova(lmm_other_hours_linear, 
                                   lmm_other_hours_quad, 
                                   lmm_other_hours_quad_2, 
                                   test = "LRT")

##### Youngest age

lmm_other_age_linear    <- lmer(schimra_b_other_age_min 
                                ~ 1
                                + treatment 
                                + time 
                                + time_after 
                                + (1|id), 
                                data = gpp_data_main)

lmm_other_age_quad      <- lmer(schimra_b_other_age_min
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                + time_sq 
                                + (1|id), 
                                data = gpp_data_main)

lmm_other_age_quad_2      <- lmer(schimra_b_other_age_min
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + time_sq 
                                  + time_after_sq 
                                  + (1|id), 
                                  data = gpp_data_main)

lrt_other_age           <- anova(lmm_other_age_linear, 
                                 lmm_other_age_quad, 
                                 lmm_other_age_quad_2, 
                                 test = "LRT")

# Visualization export ---------------------------------------------------------

# Model predictions

predict_grid <- plot_grid(plot_ssas_predict,
                          plot_csam_predict,
                          plot_copine_predict,
                          # plot_age_predict,
                          nrow = 2)

save_plot("figures/gpp_primary-outcome-prediction.png",
          predict_grid,
          base_width = 10, base_height = 8)

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

table_ssas <- lmm_ssas_quad %>% 
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
  
table_csam <- lmm_csam_hours_all_quad %>% 
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
  
table_copine <- lmm_csam_copine_linear %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment"
    )
  ) %>% 
  autofit()
  
table_age <- lmm_csam_age_quad %>% 
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

table_missing <- miss_ssas %>% 
  as_flextable() %>% 
  autofit()

table_heckman <- lmm_sass_sens_quad %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment",
      "Time (quadratic)",
      "Inverse Mills Ratio"
    )
  ) %>% 
  autofit()

# Additional outcomes

# Socializing

table_social_hours <- lmm_social_hours_quad %>% 
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

table_social_age <- lmm_social_age_linear %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment"
    )
  ) %>% 
  autofit()

# Interacting

table_interact_hours <- lmm_interact_hours_linear %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment"
    )
  ) %>% 
  autofit()

table_interact_age <- lmm_interact_age_linear %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment"
    )
  ) %>% 
  autofit()

# Other Behaviors

table_other_hours <- lmm_other_hours_linear %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment"
    )
  ) %>% 
  autofit()

table_other_age <- lmm_other_age_linear %>% 
  lmm_table(
    fixed_names = c(
      "Intercept",
      "Treatment start",
      "Time (weeks)",
      "Time in treatment"
    )
  ) %>% 
  autofit()

# Export tables

## Primary and secondary outcomes

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

save_as_docx("SSAS"                  = table_ssas,
             "CSAM Use"              = table_csam,
             "COPINE Severity"       = table_copine,
             "Age of Youngest Child" = table_age,
             path  = "output/gpp_main-tables.docx",
             align = "center")

# Sensitivity analysis

save_as_docx("Missingness Model"       = table_missing,
             "Heckman Selection Model" = table_heckman,
             path  = "output/gpp_sensitivity-table.docx",
             align = "center")

# Additional outcomes

save_as_docx("Socializing - Hours"        = table_social_hours,
             "Socializing - Youngest Age" = table_social_age,
             "Interacting - Hours"        = table_interact_hours,
             "Interacting - Youngest Age" = table_interact_age,
             "Other - Hours"              = table_other_hours,
             "Other - Youngest Age"       = table_other_age,
             path  = "output/gpp_additional-outcome-tables.docx",
             align = "center")

