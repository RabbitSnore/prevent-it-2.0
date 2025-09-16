################################################################################

# Prevent It 2.0, Global Perpetration Prevention -- Baseline Description

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("tidyverse",
              "table1")

lapply(packages, library, character.only = TRUE)

# Wrangling --------------------------------------------------------------------

gpp_pre_1 <- gpp_data_main %>% 
  filter(timepoint == "pre_1") %>% 
  mutate(
    criminal_nonviolent = case_when(
      !is.na(criminal_nonviolent) ~ criminal_nonviolent,
       is.na(criminal_nonviolent) ~ criminal_pre_1b_01,
    ),
    criminal_violent = case_when(
      !is.na(criminal_violent) ~ criminal_violent,
       is.na(criminal_violent) ~ criminal_pre_1b_04,
    ),
    criminal_noncontact = case_when(
      !is.na(criminal_noncontact) ~ criminal_noncontact,
       is.na(criminal_noncontact) ~ criminal_pre_1b_20,
    ),
    criminal_contact = case_when(
      !is.na(criminal_contact) ~ criminal_contact,
       is.na(criminal_contact) ~ criminal_pre_1b_29,
    )
  )

gpp_pre_2 <- gpp_data_main %>% 
  filter(timepoint == "pre_2" & time == 0)

# Flagged for interview --------------------------------------------------------

flagged <- gpp_data_main %>% 
  group_by(id) %>% 
  summarise(
    socialized = sum(schimra_b_social_hours_avg, na.rm = TRUE) > 0,
    interact   = sum(schimra_b_interact_hours_avg, na.rm = TRUE) > 0,
  )

# Table 1 ----------------------------------------------------------------------

table_1_crime <- table1(~ as.factor(criminal_nonviolent)
                        + as.factor(criminal_violent)
                        + as.factor(criminal_noncontact)
                        + as.factor(criminal_contact)
                        + as.factor(lassie_pedophilia_01 > 1)
                        + as.factor(raads_14_sumscore >= 14)
                        | assigned_group,
                        data = gpp_pre_1)

table_1_csa   <- table1(~ as.factor(schimra_b_csam_hours_avg > 0)
                        + as.factor(schimra_b_social_hours_avg > 0)
                        + as.factor(schimra_b_interact_hours_avg > 0)
                        + as.factor(hbi_19_sumscore >= 53)
                        + as.factor(ssas_sumscore >= 13)
                        | assigned_group, 
                        data = gpp_pre_2)

table_1_psych <- table1(~ as.factor(phq_sumscore >= 10)
                        
                        | assigned_group,
                        data = gpp_pre_2)
