################################################################################

# Prevent It 2.0 -- Main Analysis

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("lme4", "lmerTest", "ggplot")

lapply(packages, library, character.only = TRUE)

# Data analysis ----------------------------------------------------------------

# This script is written with the assumption that the data have already been
# wrangled into suitable shapes.

## Preliminary note ------------------------------------------------------------

# For all analyses using mixed-effects models, we will not impute missing data.

## Primary research question ---------------------------------------------------

# The primary research question concerns the effect of the Prevent It 2.0
# treatment, compared to a waitlist control group, on the time spend using CSAM
# to socializing with children, having sexual interactions with children, and
# other behaviors related to sexual interest in children.
# These behaviors will be assessed using Part B of the SChiMRA+ (4 items)

# Each of the four items on Part B of the SChiMRA+ takes a measure of the number 
# of days in which the participant engaged in the behavior of interest, time spent
# in hours engaging in the behavior for each active day of the last seven days, 
# as well as a measure of either the age of the youngest child involved in the 
# activities of each day and/or the severity of the material (using the COPINE 
# scale). For each seven day period, each participant's time measures will be 
# summed (in hours) and divided by 7, to create a daily average measure for that
# week. For the youngest age and COPINE severity measure, we will take the lowest
# reported age for that week and the highest reported COPINE severity for that
# week, and these will serve as dependent measures.

# In total, there will be nine weekly outcome variables from Part B of the
# SChiMRA+. For each of these variables, we will fit a linear mixed effects
# model, with a dummy coded treatment predictor (0 = waitlist, 1 = Prevent It),
# indicating whether treatment has commenced, a time predictor (starting at 0,
# and counting each weekly measurement point), and a time-since-treatment
# predictor (starting at 0, and counting up at each measurement point after
# treatment begins). We will also fit another model adding quadratic terms for
# the time and time-since-treatment predictors. We will compare these two models
# using a likelihood ratio test and retain the better performing model for
# interpretation.

# Some participants will be assigned to receive treatment immediately, and others
# will be assigned to a waitlist during that treatment period (and will receive
# treatment thereafter).
#
# Treatment begins with a baseline measurement, followed by nine weeks of
# treatment, with a measurement taken at each week. The waitlist is measured on
# the same schedule as those who begin treatment immediately, and then a baseline
# measure is taken in four weeks.
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

### CSAM

#### Hours (daily average)

lmm_csam_hours_linear      <- lmer(schimra_b_csam_hours_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_csam_hours_quad        <- lmer(schimra_b_csam_hours_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_csam_hours             <- anova(lmm_csam_hours_linear, lmm_csam_hours_quad, test = "LRT")

#### COPINE severity

lmm_csam_copine_linear     <- lmer(schimra_b_csam_copine_max ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_csam_copine_quad       <- lmer(schimra_b_csam_copine_max ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_csam_copine            <- anova(lmm_csam_copine_linear, lmm_csam_copine_quad, test = "LRT")

#### Youngest age

lmm_csam_age_linear        <- lmer(schimra_b_csam_age_min ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_csam_age_quad          <- lmer(schimra_b_csam_age_min ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_csam_age               <- anova(lmm_csam_age_linear, lmm_csam_age_quad, test = "LRT")

### Socialization

#### Hours (daily average)

lmm_social_hours_linear    <- lmer(schimra_b_social_hours_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_social_hours_quad      <- lmer(schimra_b_social_hours_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_social_hours           <- anova(lmm_social_hours_linear, lmm_social_hours_quad, test = "LRT")

#### Youngest age

lmm_social_age_linear      <- lmer(schimra_b_social_age_min ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_social_age_quad        <- lmer(schimra_b_social_age_min ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_social_age             <- anova(lmm_social_age_linear, lmm_social_age_quad, test = "LRT")

### Sexual interactions

#### Hours (daily average)

lmm_interact_hours_linear  <- lmer(schimra_b_interact_hours_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_interact_hours_quad    <- lmer(schimra_b_interact_hours_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_interact_hours         <- anova(lmm_interact_hours_linear, lmm_interact_hours_quad, test = "LRT")

#### Youngest age

lmm_interact_age_linear    <- lmer(schimra_b_interact_age_min ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_interact_age_quad      <- lmer(schimra_b_interact_age_min ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_interact_age           <- anova(lmm_interact_age_linear, lmm_interact_age_quad, test = "LRT")

### Other behaviors

#### Hours (daily average)

lmm_other_hours_linear     <- lmer(schimra_b_other_hours_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_other_hours_quad       <- lmer(schimra_b_other_hours_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_other_hours            <- anova(lmm_other_hours_linear, lmm_other_hours_quad, test = "LRT")

#### Youngest age

lmm_other_age_linear       <- lmer(schimra_b_other_age_min ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_other_age_quad         <- lmer(schimra_b_other_age_min ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_other_age              <- anova(lmm_other_age_linear, lmm_other_age_quad, test = "LRT")

## Secondary research questions ------------------------------------------------

### Is there a change in motivation to commit future abuse/exploitation? (SChiMRA+ part A)

# The SChiMRA+ part A contains three Likert-type self-report items concerning
# motivation to use CSAM, to socialize with children, and to have sexual
# interactions with children.

#### Watch

lmm_watch_motive_linear    <- lmer(schimra_a_watch ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_watch_motive_quad      <- lmer(schimra_a_watch ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_watch_motive           <- anova(lmm_csam_hours_linear, lmm_csam_hours_quad, test = "LRT")

#### Socialize

lmm_social_motive_linear   <- lmer(schimra_a_social ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_social_motive_quad     <- lmer(schimra_a_social ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_social_motive          <- anova(lmm_social_motive_linear, lmm_social_motive_quad, test = "LRT")

#### Interact

lmm_interact_motive_linear <- lmer(schimra_a_interact ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_interact_motive_quad   <- lmer(schimra_a_interact ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_interact_motive        <- anova(lmm_interact_motive_linear, lmm_interact_motive_quad, test = "LRT")

### Sustainable change (follow-up measure)

# To assess the extent to which the treatment produces a sustainable change in
# the relevant behaviors (as measured by the SChiMRA+ Part B), for each of the
# variables analyzed for the primary analysis, we will estimate the change from
# the post-treatment measure (or in the case of the waitlist, the last weekly) 
# to the follow-up measure, controlling for the first (i.e., baseline) measure.
#
# For each variable, we will fit linear mixed effects models, with treatment
# group (treatment vs. waitlist, dummy coded) as a fixed predictor, measurement 
# point (last weekly measure vs. follow-up, dummy coded) as a fixed predictor,
# baseline measure as a fixed predictor, and random intercepts for each 
# participant. We will fit two models for each variable, one with only main 
# effects and one that adds the interaction term for the treatment and 
# measurement predictors. These models will be compared using a likelihood ratio
# test.

# Wrangling note: It will probably be easiest to wrangle a unique dataset for
# each of these sets of models. Baseline measures will need to be in their own
# columns, and each measurement point will be in its own row.

#### CSAM

##### Hours (daily average)

lmm_csam_hours_followup_main <- lmer(schimra_b_csam_hours_sust ~ treatment + measurement + baseline + (1|id), data = pi_data_csam_hours_sust)
lmm_csam_hours_followup_int  <- lmer(schimra_b_csam_hours_sust ~ treatment * measurement + baseline + (1|id), data = pi_data_csam_hours_sust)
lmm_csam_hours_followup_lrt  <- anova(lmm_csam_hours_followup_main, lmm_csam_hours_followup_int, test = "LRT")

##### COPINE severity

lmm_csam_copine_followup_main <- lmer(schimra_b_csam_copine_sust ~ treatment + measurement + baseline + (1|id), data = pi_data_csam_copine_sust)
lmm_csam_copine_followup_int  <- lmer(schimra_b_csam_copine_sust ~ treatment * measurement + baseline + (1|id), data = pi_data_csam_copine_sust)
lmm_csam_copine_followup_lrt  <- anova(lmm_csam_hours_followup_main, lmm_csam_hours_followup_int, test = "LRT")

##### Youngest age

#### Socialization

##### Hours (daily average)

##### Youngest age

#### Sexual interactions

##### Hours (daily average)

##### Youngest age

#### Other behaviors

##### Hours (daily average)

##### Youngest age

### How does Prevent It 2.0 compare to the first version of Prevent It? (SChiMRA+ part B)                                                          



### Safety/Side-effects

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

### Attrition

# We will create a binary indicator for whether a participant has dropped out at
# each measurement point. The indicator will be coded 0 if the participant
# remains in the trial, and it will be coded 1 if the participant has dropped
# out of the trial. A participant will be said to have dropped out if they
# provide no data at the measurement point in question and no further data.
# A participant will not be coded as a dropout if they have successfully
# completed the treatment early.

# This binary indicator will serve as the primary dependent measure for our
# analyses of attrition and will also be used to calculate the simple rate of
# attrition.

# This indicator will be computed both for the Prevent It 2.0 data and the prior
# Prevent It 1.0 data. Those data sets will be combined for this analysis.
# Additionally, the treatment indicator variable will be expanded such that
# 0 = waitlist (PI2), 1 = treatment (PI2), 2 = placebo (PI1), 3 = treatment (PI1),
# coded with treatment contrasts such that the waitlist is the reference group.

#### Simple analysis of final attrition rate

# We will calculate the dropout rate at the end of the first treatment wave
# (i.e., at the post-treatment measurement, prior to the follow up), for both the
# current trial as well as the PI1 trial. These two rates will be compared using
# a chi-square test.

attrition_rate_chisq <- prop.test(x = c(att_rate_pi2, att_rate_pi1),
                                  n = c(n_pi2, n_pi1),
                                  alternative = "two.sided",
                                  correct = FALSE)

#### Time to dropout

glmer_attrition_linear   <- glmer(dropout ~ treatment + time + (1|id), data = pi1_pi2_data_long)
glmer_attrition_inter    <- glmer(dropout ~ treatment * time + (1|id), data = pi1_pi2_data_long)
glmer_attrition_quad     <- glmer(dropout ~ treatment* time + time_sq + (1|id), data = pi1_pi2_data_long)
glmer_attrition_inter_sq <- glmer(dropout ~ treatment* time * time_sq + (1|id), data = pi1_pi2_data_long)

### Is there an effect on the participants' quality of life?

# Quality of life will be assessed by the EQ-5D, which is administered to
# participants prior to treatment, after treatment, and at the follow-up.
