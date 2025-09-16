################################################################################

# Prevent It 2.0, Global Perpetration Prevention -- Power Analysis

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("tidyverse", "lme4", "lmerTest", "simr")

lapply(packages, library, character.only = TRUE)

# Power analysis ---------------------------------------------------------------

# Data structure

# TREATMENT: 
# time:       00 01 02 03 04 05 06 07 08 09 -- -- -- -- -- -- -- -- -- -- -- -- --
# treatment:  00 01 01 01 01 01 01 01 01 01 -- -- -- -- -- -- -- -- -- -- -- -- --
# time_after: 00 01 02 03 04 05 06 07 08 09 -- -- -- -- -- -- -- -- -- -- -- -- -- 
#
# WAITLIST:  
# time        00 01 02 03 04 05 06 07 08 09 -- -- -- 13 14 15 16 17 18 19 20 21 22
# treatment:  00 00 00 00 00 00 00 00 00 00 -- -- -- 00 01 01 01 01 01 01 01 01 01
# time_after: 00 00 00 00 00 00 00 00 00 00 -- -- -- 00 01 02 03 04 05 06 07 08 09

## Subject level structure

treatment_sub_str <- data.frame(
  id         = 0,
  time       = 0:9,
  treatment  = c(0, rep(1, 9)),
  time_after = 0:9
)

waitlist_sub_str  <- data.frame(
  id         = 1000,
  time       = c(0:9, 13:22),
  treatment  = c(rep(0, 10), 0, rep(1, 9)),
  time_after = c(rep(0, 10), 0:9)
) 

## Generate data structure

n <- 80 # per group sample size

treatment_str <- map_dfr(1:n, ~ treatment_sub_str %>% mutate(id = id + .x))

waitlist_str  <- map_dfr(1:n, ~ waitlist_sub_str %>% mutate(id = id + .x))

pi_data_str <- bind_rows(treatment_str, waitlist_str)

# Parameter specification

fixed <- c( 3.75,
           -0.10,
           -0.25,
           -0.20)

var_corr <- list(
  20 # PI1 observed variance 19.48 here
  ) 

sigma_sim    <- 4.00 # PI1 observed sigma = 3.62

# Create model

lmm_sass_linear <- makeLmer(sass_total 
                            ~ treatment 
                            + time 
                            + time_after 
                            + (1|id), 
                            fixef   = fixed, 
                            VarCorr = var_corr,
                            sigma   = sigma_sim,
                            data    = pi_data_str)    

# Power simulation

power_simulation <- powerSim(
  fit  = lmm_sass_linear,
  test = simr::fixed("time_after", method = "t"),
  nsim = 1000,
  seed = 9982
)

