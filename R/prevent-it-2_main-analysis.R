################################################################################

# Prevent It 2.0 Main Analysis

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("lme4", "lmerTest", "ggplot")

lapply(packages, library, character.only = TRUE)

# Data analysis ----------------------------------------------------------------

## Primary research question ---------------------------------------------------

# The primary research question concerns the effect of the Prevent It 2.0
# treatment, compared to a waitlist control group, on the time spend using CSAM
# to socializing with children, having sexual interactions with children, and
# other behaviors related to sexual interest in children.
# These behaviors will be assessed using Part B of the SChiMRA+ (4 items)

# Each of the four items on Part B of the SChiMRA+ takes a measure of the number 
# of days in which the participant engaged in the behavior of interest, time spent
# in hours enaging in the behavior for each active day of the last seven days, 
# as well as a measure of either the age of the youngest child involved in the 
# activities of each day and/or the severity of the material (using the COPINE 
# scale). For each seven day period, each participant's time and age/severity 
# measures will be averaged, imputing a zero for inactive days and ignoring any 
# days with missing time/age/severity estimates, to create a daily average for 
# that week.

# In total, there will be nine weekly average variables from Part B of the
# SChiMRA+. For each of these variables, we will fit a linear mixed effects
# model, with a dummy coded treatment predictor (0 = waitlist, 1 = Prevent It),
# indicating whether treatment has commenced, a time predictor (starting at 0,
# and counting each weekly measurement point), and a time-since-treatment
# predictor (starting at 0, and counting up at each measurement point after
# treatment begins). We will also fit another model adding quadratic terms for
# the time and time-since-treatment predictors. We will compare these two models
# using a likelihood ratio test and retain the better performing model for
# interpretation.

### CSAM

#### Hours

lmm_csam_hours_linear  <- lmer(schimra_b_csam_hours_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_csam_hours_quad    <- lmer(schimra_b_csam_hours_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_csam_hours         <- anova(lmm_csam_hours_linear, lmm_csam_hours_quad, test = "LRT")

#### COPINE severity

lmm_csam_copine_linear <- lmer(schimra_b_csam_copine_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_csam_copine_quad   <- lmer(schimra_b_csam_copine_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_csam_copine        <- anova(lmm_csam_copine_linear, lmm_csam_copine_quad, test = "LRT")

#### Youngest age

lmm_csam_age_linear    <- lmer(schimra_b_csam_age_avg ~ treatment + time + time_after + (1|id), data = pi_data_long)
lmm_csam_age_quad      <- lmer(schimra_b_csam_age_avg ~ treatment + time + time_after + time_sq + time_after_sq + (1|id), data = pi_data_long)
lrt_csam_age           <- anova(lmm_csam_age_linear, lmm_csam_age_quad, test = "LRT")

### Socialization

schimra_b_social_hours_avg

schimra_b_social_age_avg

### Sexual interactions

schimra_b_interact_hours_avg

schimra_b_interact_age_avg

### Other behaviors

schimra_b_other_hours_avg

schimra_b_other_age_avg

## Secondary research questions ------------------------------------------------

### Is there a change in motivation to commit future abuse/exploitation? (SChiMRA+ part B)



### How does Prevent It 2.0 compare to the first version of Prevent It? (SChiMRA+ part B)                                                          



### Is there a reduction in the negative side effects between Prevent It 1 and 2? (for safety, NEQ-20 total number of side effects; for attrition, % lost to post-treatment))



### Is there an effect on the participants' quality of life? (EQ5D)



### Is there sustainable change (measured by a supplementary study that includes a long term follow up 8 weeks after the normal follow up)?



### Specific effects from the treatment (Prevent It 2.0 compared to Prevent It 1 Placebo); Non-specific effects from treatment (PI1 Placebo compared to Waitlist); Waitlist effects (regression to the mean)

