################################################################################

# Prevent It 2.0 -- Main Analysis

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

lmm_sass_linear      <- lmer(ssas_sum 
                             ~ 1 
                             + treatment 
                             + time 
                             + time_after 
                             + (1|id), 
                             data = gpp_data_main)

lmm_sass_quad        <- lmer(ssas_sum 
                             ~ 1 
                             + treatment 
                             + time 
                             + time_after 
                             + time_sq 
                             + time_after_sq 
                             + (1|id), 
                             data = gpp_data_main)

lrt_sass             <- anova(lmm_ssas_linear, 
                              lmm_ssas_quad, 
                              test = "LRT")

## Visualization of mean SSAS scores

ssas_time_arm <- gpp_data_main %>% 
  group_by(assigned_group, time) %>% 
  summarise(
    mean_ssas = mean(ssas_sum, na.rm = TRUE),
    sd_ssas   = sd(ssas_sum, na.rm = TRUE),
    se_ssas   = sd_ssas/n(),
    ci_lb     = mean_ssas - se_ssas*qnorm(.975),
    ci_ub     = mean_ssas + se_ssas*qnorm(.975),
    n         = n()
  )

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
    breaks = 0:max(ssas_time_arm$time)
  ) +
  scale_y_continuous(
    breaks = 0:4
  ) +
  labs(
    x = "Time",
    y = "Mean SSAS Score",
    color = "Group"
  ) +
  theme_classic()

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

# Time points 10, 11, and 12 will be missing by design, so they will treated as
# missing in the variable indicating (unplanned) missingness.

miss_ssas              <- glmer(ssas_missing 
                                ~ 1 
                                + treatment 
                                + time 
                                + time_after 
                                + ADDITIONAL_PREDICTORS 
                                + (1|id), 
                                data = gpp_data_main, 
                                family = binomial(link = "probit"))

gpp_data_main$inv_mills <- dnorm( predict(miss_ssas) ) / pnorm( predict(miss_ssas) )

### Outcome model

lmm_sass_sens_linear      <- lmer(ssas_sum 
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + inv_mills 
                                  + (1|id), 
                                  data = gpp_data_main)

lmm_sass_sens_quad        <- lmer(ssas_sum 
                                  ~ 1 
                                  + treatment 
                                  + time 
                                  + time_after 
                                  + time_sq 
                                  + time_after_sq 
                                  + inv_mills 
                                  + (1|id), 
                                  data = gpp_data_main)

lrt_sass_sens             <- anova(lmm_sass_sens_linear, 
                                   lmm_sass_sens_quad, 
                                   test = "LRT")

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

##### Hours (daily average)

lmm_csam_hours_linear      <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_csam_hours_quad        <- lmer(schimra_b_csam_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_csam_hours             <- anova(lmm_csam_hours_linear, 
                                    lmm_csam_hours_quad, 
                                    test = "LRT")

###### Visualization of daily average CSAM use

csam_time_arm <- gpp_data_main %>% 
  group_by(assigned_group, time) %>% 
  summarise(
    mean_csam = mean(schimra_b_csam_hours_avg, na.rm = TRUE),
    sd_csam   = sd(schimra_b_csam_hours_avg, na.rm = TRUE),
    se_csam   = sd_csam/n(),
    ci_lb     = mean_csam - se_csam*qnorm(.975),
    ci_ub     = mean_csam + se_csam*qnorm(.975),
    n         = n()
  )

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
    y = "Daily Average CSAM Use",
    color = "Group"
  ) +
  theme_classic()

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
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_csam_copine            <- anova(lmm_csam_copine_linear, 
                                    lmm_csam_copine_quad, 
                                    test = "LRT")

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
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_csam_age               <- anova(lmm_csam_age_linear, 
                                    lmm_csam_age_quad, 
                                    test = "LRT")

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
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_social_hours           <- anova(lmm_social_hours_linear, 
                                    lmm_social_hours_quad, 
                                    test = "LRT")

##### Youngest age

lmm_social_age_linear      <- lmer(schimra_b_social_age_min 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_social_age_quad        <- lmer(schimra_b_social_age_min 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_social_age             <- anova(lmm_social_age_linear, 
                                    lmm_social_age_quad, 
                                    test = "LRT")

#### Sexual interactions

##### Hours (daily average)

lmm_interact_hours_linear  <- lmer(schimra_b_interact_hours_avg 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_interact_hours_quad    <- lmer(schimra_b_interact_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_interact_hours         <- anova(lmm_interact_hours_linear, 
                                    lmm_interact_hours_quad, 
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
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_interact_age           <- anova(lmm_interact_age_linear, 
                                    lmm_interact_age_quad, 
                                    test = "LRT")

#### Other behaviors

##### Hours (daily average)

lmm_other_hours_linear     <- lmer(schimra_b_other_hours_avg 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_other_hours_quad       <- lmer(schimra_b_other_hours_avg 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_other_hours            <- anova(lmm_other_hours_linear, 
                                    lmm_other_hours_quad, 
                                    test = "LRT")

##### Youngest age

lmm_other_age_linear       <- lmer(schimra_b_other_age_min 
                                   ~ 1
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + (1|id), 
                                   data = gpp_data_main)

lmm_other_age_quad         <- lmer(schimra_b_other_age_min 
                                   ~ 1 
                                   + treatment 
                                   + time 
                                   + time_after 
                                   + time_sq 
                                   + time_after_sq 
                                   + (1|id), 
                                   data = gpp_data_main)

lrt_other_age              <- anova(lmm_other_age_linear, 
                                    lmm_other_age_quad, 
                                    test = "LRT")
