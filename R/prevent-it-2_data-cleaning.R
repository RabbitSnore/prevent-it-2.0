################################################################################

# Prevent It 2.0, Global Perpetration Prevention -- Data Cleaning and Wrangling

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("tidyverse",
              "readxl")

lapply(packages, library, character.only = TRUE)

# Data loading -----------------------------------------------------------------

# Pre-Questionnaire

raw_pre_1      <- read_xlsx("data/gpp_data_pre-1.xlsx", 
                            col_types = "text",
                            trim_ws = TRUE)

raw_pre_2      <- read_xlsx("data/gpp_data_pre-2.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

## Pre-revision versions

# The pre-questionnaires were revised early in data collection to become
# consistent with the procedure in the PRIORITY project.

raw_pre_1b    <- read_xlsx("data/gpp_data_pre-1b.xlsx", 
                           col_types = "text",
                           trim_ws = TRUE)

raw_pre_2b    <- read_xlsx("data/gpp_data_pre-2b.xlsx",
                           col_types = "text",
                           trim_ws = TRUE)

# Weekly measures

raw_weekly     <- read_xlsx("data/gpp_data_weekly.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

raw_weekly_258 <- read_xlsx("data/gpp_data_weekly258.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

# Post questionnaire

raw_post       <- read_xlsx("data/gpp_data_post.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

# Follow up

raw_follow     <- read_xlsx("data/gpp_data_followup.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

# Initial cleaning of column names ---------------------------------------------

# Variables names

# The Itherapi platform exports the full item text in the column headers, and as
# such, the column names need to be renamed prior to further wrangling and
# analysis. I have not figured out a neat way to automate this process, so the
# following code does the renaming manually.

## Pre questionnaire 1

vars_pre_1 <- c(
  
  # Study information
  
  "pre_1",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  
  # "acess_emot_abuse",
  # "acess_phys_abuse",
  # "acess_sex_abuse",
  # "acess_emot_neglect",
  # "acess_phys_neglect",
  # "acess_mental_illness",
  # "acess_subs_abuse",
  # "acess_incarcerated",
  # "acess_sum",
  # 
  # "ssas_sum",
  # 
  # "sbims_b_sum",
  # 
  # "audit_sum",
  # 
  # "lassie_s_interests",
  # 
  # "raads_14_mentdef",
  # "raads_14_socanx",
  # "raads_14_sensreact",
  # "raads_14_sum",
  
  # Demographic
  
  "religion_importance",
  "religion_frequency",
  
  # ACEs-s items
  
  "acess_emot_abuse_01",
  "acess_phys_abuse_01",
  "acess_sex_abuse_01",
  "acess_sex_abuse_02",
  "acess_sex_abuse_03",
  "acess_sex_abuse_04",
  "acess_emot_neglect_01",
  "acess_phys_neglect_01",
  "acess_mental_illness_01",
  "acess_mental_illness_02",
  "acess_subs_abuse_01",
  "acess_subs_abuse_02",
  "acess_incarcerated_01",
  
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  # SBIMS-B
  
  "sbimsb_sexualcontact",
  "sbimsb_exposure",
  "sbimsb_sexualinteractions",
  "sbimsb_physicalcontact",
  "sbimsb_csam",
  
  # AUDIT
  
  "audit_01",
  "audit_02",
  "audit_03",
  
  # LASSIE
  
  ## 01 Interest/urges
  ## 02 Acted
  ## 03 Impairment
  ## 04 Duration
  ## 05 Current
  
  "lassie_expositionism_01",
  "lassie_expositionism_02",
  "lassie_expositionism_03",
  "lassie_expositionism_04",
  "lassie_expositionism_05",
  
  "lassie_voyeurism_01",
  "lassie_voyeurism_02",
  "lassie_voyeurism_03",
  "lassie_voyeurism_04",
  "lassie_voyeurism_05",
  
  "lassie_fetishism_01",
  "lassie_fetishism_02",
  "lassie_fetishism_03",
  "lassie_fetishism_04",
  "lassie_fetishism_05",
  
  "lassie_hebephilia_01",
  "lassie_hebephilia_02",
  "lassie_hebephilia_03",
  "lassie_hebephilia_04",
  "lassie_hebephilia_05",
  
  "lassie_pedophilia_01",
  "lassie_pedophilia_02",
  "lassie_pedophilia_03",
  "lassie_pedophilia_04",
  "lassie_pedophilia_05",
  
  "lassie_masochism_01",
  "lassie_masochism_02",
  "lassie_masochism_03",
  "lassie_masochism_04",
  "lassie_masochism_05",
  
  "lassie_sadism_01",
  "lassie_sadism_02",
  "lassie_sadism_03",
  "lassie_sadism_04",
  "lassie_sadism_05",
  
  "lassie_frotteurism_01",
  "lassie_frotteurism_02",
  "lassie_frotteurism_03",
  "lassie_frotteurism_04",
  "lassie_frotteurism_05",
  
  "lassie_scatologia_01",
  "lassie_scatologia_02",
  "lassie_scatologia_03",
  "lassie_scatologia_04",
  "lassie_scatologia_05",
  
  "lassie_necrophilia_01",
  "lassie_necrophilia_02",
  "lassie_necrophilia_03",
  "lassie_necrophilia_04",
  "lassie_necrophilia_05",
  
  "lassie_asphyxia_01",
  "lassie_asphyxia_02",
  "lassie_asphyxia_03",
  "lassie_asphyxia_04",
  "lassie_asphyxia_05",
  
  "lassie_zoophilia_01",
  "lassie_zoophilia_02",
  "lassie_zoophilia_03",
  "lassie_zoophilia_04",
  "lassie_zoophilia_05",
  
  "lassie_urophilia_01",
  "lassie_urophilia_02",
  "lassie_urophilia_03",
  "lassie_urophilia_04",
  "lassie_urophilia_05",
  
  "lassie_coprophilia_01",
  "lassie_coprophilia_02",
  "lassie_coprophilia_03",
  "lassie_coprophilia_04",
  "lassie_coprophilia_05",
  
  "lassie_other_01",
  "lassie_other_01_text",
  "lassie_other_02",
  "lassie_other_02_text",
  "lassie_other_03",
  "lassie_other_04",
  "lassie_other_05",
  
  # RAADS-14
  
  "raads_14_01",
  "raads_14_02",
  "raads_14_03",
  "raads_14_04",
  "raads_14_05",
  "raads_14_06",
  "raads_14_07",
  "raads_14_08",
  "raads_14_09",
  "raads_14_10",
  "raads_14_11",
  "raads_14_12",
  "raads_14_13",
  "raads_14_14",
  
  # Criminal history
  
  "criminal_nonviolent",
  "criminal_violent",
  "criminal_noncontact",
  
  "criminal_noncontact_voyeurism",
  "criminal_noncontact_exhibitionism",
  "criminal_noncontact_csam",
  "criminal_noncontact_producing_csam",
  "criminal_noncontact_other",
  
  "criminal_noncontact_children",
  "criminal_noncontact_unrelated",
  "criminal_noncontact_stranger",
  "criminal_noncontact_male",
  
  "criminal_contact",
  "criminal_contact_number",
  "criminal_contact_children",
  "criminal_contact_youngest_child",
  "criminal_contact_unrelated",
  "criminal_contact_stranger",
  "criminal_contact_male",
  
  "criminal_sentences",
  
  "criminal_age",
  "criminal_romantic_partner"
  
)

vars_pre_1b <- c(
  
  # Study information
  
  "pre_1",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  
  # "acess_emot_abuse",
  # "acess_phys_abuse",
  # "acess_sex_abuse",
  # "acess_emot_neglect",
  # "acess_phys_neglect",
  # "acess_mental_illness",
  # "acess_subs_abuse",
  # "acess_incarcerated",
  # "acess_sum",
  # 
  # "ssas_sum",
  # 
  # "sbims_b_sum",
  # 
  # "audit_sum",
  # 
  # "lassie_s_interests",
  # 
  # "raads_14_mentdef",
  # "raads_14_socanx",
  # "raads_14_sensreact",
  # "raads_14_sum",
  
  # Demographic (relationships)
  
  "relationships_01",
  "relationships_02",
  "relationships_03",
  "relationships_04",
  "relationships_05",
  "relationships_06",
  "relationships_07",
  "relationships_08",
  "relationships_09",
  "relationships_10",
  "relationships_11",
  
  # Demographic
  
  "religion_importance",
  "religion_frequency",
  
  # ACEs-s items
  
  "acess_pre_b_01",
  "acess_pre_b_02",
  "acess_pre_b_03",
  "acess_pre_b_04",
  "acess_pre_b_05",
  "acess_pre_b_06",
  "acess_pre_b_07",
  "acess_pre_b_08",
  "acess_pre_b_09",
  "acess_pre_b_10",
  "acess_pre_b_11",
  "acess_pre_b_12",
  "acess_pre_b_13",
  "acess_pre_b_14",
  "acess_pre_b_15",
  "acess_pre_b_16",
  "acess_pre_b_17",
  "acess_pre_b_18",
  "acess_pre_b_19",
  "acess_pre_b_20",
  "acess_pre_b_21",
  "acess_pre_b_22",
  "acess_pre_b_23",
  "acess_pre_b_24",
  "acess_pre_b_25",
  "acess_pre_b_26",
  "acess_pre_b_27",
  "acess_pre_b_28",
  "acess_pre_b_29",
  "acess_pre_b_30",
  "acess_pre_b_31",
  "acess_pre_b_32",
  
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  # SBIMS-B
  
  "sbimsb_sexualcontact",
  "sbimsb_exposure",
  "sbimsb_sexualinteractions",
  "sbimsb_physicalcontact",
  "sbimsb_csam",
  
  # AUDIT
  
  "audit_01",
  "audit_02",
  "audit_03",
  
  # LASSIE
  
  ## 01 Interest/urges
  ## 02 Acted
  ## 03 Impairment
  ## 04 Duration
  ## 05 Current
  
  "lassie_expositionism_01",
  "lassie_expositionism_02",
  "lassie_expositionism_03",
  "lassie_expositionism_04",
  "lassie_expositionism_05",
  
  "lassie_voyeurism_01",
  "lassie_voyeurism_02",
  "lassie_voyeurism_03",
  "lassie_voyeurism_04",
  "lassie_voyeurism_05",
  
  "lassie_fetishism_01",
  "lassie_fetishism_02",
  "lassie_fetishism_03",
  "lassie_fetishism_04",
  "lassie_fetishism_05",
  
  "lassie_hebephilia_01",
  "lassie_hebephilia_02",
  "lassie_hebephilia_03",
  "lassie_hebephilia_04",
  "lassie_hebephilia_05",
  
  "lassie_pedophilia_01",
  "lassie_pedophilia_02",
  "lassie_pedophilia_03",
  "lassie_pedophilia_04",
  "lassie_pedophilia_05",
  
  "lassie_masochism_01",
  "lassie_masochism_02",
  "lassie_masochism_03",
  "lassie_masochism_04",
  "lassie_masochism_05",
  
  "lassie_sadism_01",
  "lassie_sadism_02",
  "lassie_sadism_03",
  "lassie_sadism_04",
  "lassie_sadism_05",
  
  "lassie_frotteurism_01",
  "lassie_frotteurism_02",
  "lassie_frotteurism_03",
  "lassie_frotteurism_04",
  "lassie_frotteurism_05",
  
  "lassie_scatologia_01",
  "lassie_scatologia_02",
  "lassie_scatologia_03",
  "lassie_scatologia_04",
  "lassie_scatologia_05",
  
  "lassie_necrophilia_01",
  "lassie_necrophilia_02",
  "lassie_necrophilia_03",
  "lassie_necrophilia_04",
  "lassie_necrophilia_05",
  
  "lassie_asphyxia_01",
  "lassie_asphyxia_02",
  "lassie_asphyxia_03",
  "lassie_asphyxia_04",
  "lassie_asphyxia_05",
  
  "lassie_zoophilia_01",
  "lassie_zoophilia_02",
  "lassie_zoophilia_03",
  "lassie_zoophilia_04",
  "lassie_zoophilia_05",
  
  "lassie_urophilia_01",
  "lassie_urophilia_02",
  "lassie_urophilia_03",
  "lassie_urophilia_04",
  "lassie_urophilia_05",
  
  "lassie_coprophilia_01",
  "lassie_coprophilia_02",
  "lassie_coprophilia_03",
  "lassie_coprophilia_04",
  "lassie_coprophilia_05",
  
  "lassie_other_01",
  "lassie_other_01_text",
  "lassie_other_02",
  "lassie_other_02_text",
  "lassie_other_03",
  "lassie_other_04",
  "lassie_other_05",
  
  # Additional LASSIE
  
  "lassie_intensity_01",
  "lassie_intensity_02",
  "lassie_intensity_03",
  "lassie_intensity_04",
  "lassie_intensity_05",
  
  # RAADS-14
  
  "raads_14_01",
  "raads_14_02",
  "raads_14_03",
  "raads_14_04",
  "raads_14_05",
  "raads_14_06",
  "raads_14_07",
  "raads_14_08",
  "raads_14_09",
  "raads_14_10",
  "raads_14_11",
  "raads_14_12",
  "raads_14_13",
  "raads_14_14",
  
  # Criminal history
  
  "criminal_pre_1b_01",
  "criminal_pre_1b_02",
  "criminal_pre_1b_03",
  "criminal_pre_1b_04",
  "criminal_pre_1b_05",
  "criminal_pre_1b_06",
  "criminal_pre_1b_07",
  "criminal_pre_1b_08",
  "criminal_pre_1b_09",
  "criminal_pre_1b_10",
  "criminal_pre_1b_11",
  "criminal_pre_1b_12",
  "criminal_pre_1b_13",
  "criminal_pre_1b_14",
  "criminal_pre_1b_15",
  "criminal_pre_1b_16",
  "criminal_pre_1b_17",
  "criminal_pre_1b_18",
  "criminal_pre_1b_19",
  "criminal_pre_1b_20",
  "criminal_pre_1b_21",
  "criminal_pre_1b_22",
  "criminal_pre_1b_23",
  "criminal_pre_1b_24",
  "criminal_pre_1b_25",
  "criminal_pre_1b_26",
  "criminal_pre_1b_27",
  "criminal_pre_1b_28",
  "criminal_pre_1b_29",
  "criminal_pre_1b_30",
  "criminal_pre_1b_31",
  "criminal_pre_1b_32",
  "criminal_pre_1b_33",
  "criminal_pre_1b_34",
  "criminal_pre_1b_35",
  "criminal_pre_1b_36",
  "criminal_pre_1b_37",
  "criminal_pre_1b_38",
  "criminal_pre_1b_39",
  "criminal_pre_1b_40",
  "criminal_pre_1b_41",
  "criminal_pre_1b_42",
  "criminal_pre_1b_43",
  "criminal_pre_1b_44",
  "criminal_pre_1b_45",
  "criminal_pre_1b_46",
  "criminal_pre_1b_47",
  "criminal_pre_1b_48",
  "criminal_pre_1b_49",
  "criminal_pre_1b_50",
  "criminal_pre_1b_51",
  "criminal_pre_1b_52"
  
)

## Pre questionnaire 2

vars_pre_2 <- c(
  
  # Study information 
  
  "pre_2",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  # 
  # "ssas_sum",
  # 
  # "sbims_05",
  # "sbims_06",
  # "sbims_07",
  # 
  # "sbims_a_sum",
  # "sbims_b_sum",
  # "sbims_ab_sum",
  # 
  # "schimra_a2",
  # "schimra_a3",
  # "schimra_b2",
  # "schimra_b3",
  # 
  # "cecwc_sum",
  # "swchs_sum",
  # 
  # "hbi_19_control",
  # "hbi_19_consequences",
  # "hbi_19_coping",
  # "hbi_19_sum",
  # 
  # "alcohol_multipled",
  # "alcohol_sum",
  # 
  # "fsozuk6_sum",
  # 
  # "rcqs_sum",
  # 
  # "phq_9_sum",
  # "phq_9_suicide",
  # 
  # "ips_8_sum",
  # 
  # "eq5d_sum",
  # "eq5d_health",
  # 
  # "truthfulness_index",
  # 
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  "ssas_next",
  
  # SBIMS
  
  "sbimsa_sexualcontact",
  "sbimsa_exposure",
  "sbimsa_sexualinteractions",
  "sbimsa_physicalcontact",
  
  "sbimsb_sexualcontact",
  "sbimsb_exposure",
  "sbimsb_sexualinteractions",
  "sbimsb_physicalcontact",
  "sbimsb_csam",
  
  # SChiMRA
  
  ## Part A
  
  "schimra_a_watch",
  "schimra_a_socialize",
  "schimra_a_interact",
  
  ## Part B
  
  ### CSAM
  
  "schimra_b_csam",
  
  "schimra_b_csam_day1_hours",
  "schimra_b_csam_day1_minutes",
  
  "schimra_b_csam_day2_hours",
  "schimra_b_csam_day2_minutes",
  
  "schimra_b_csam_day3_hours",
  "schimra_b_csam_day3_minutes",
  
  "schimra_b_csam_day4_hours",
  "schimra_b_csam_day4_minutes",
  
  "schimra_b_csam_day5_hours",
  "schimra_b_csam_day5_minutes",
  
  "schimra_b_csam_day6_hours",
  "schimra_b_csam_day6_minutes",
  
  "schimra_b_csam_day7_hours",
  "schimra_b_csam_day7_minutes",
  
  "schimra_b_csam_copine_day1",
  "schimra_b_csam_copine_day2",
  "schimra_b_csam_copine_day3",
  "schimra_b_csam_copine_day4",
  "schimra_b_csam_copine_day5",
  "schimra_b_csam_copine_day6",
  "schimra_b_csam_copine_day7",
  
  "schimra_b_csam_ayc_day1",
  "schimra_b_csam_ayc_day2",
  "schimra_b_csam_ayc_day3",
  "schimra_b_csam_ayc_day4",
  "schimra_b_csam_ayc_day5",
  "schimra_b_csam_ayc_day6",
  "schimra_b_csam_ayc_day7",
  
  ### Socialization
  
  "schimra_b_socialize",
  
  "schimra_b_socialize_day1_hours",
  "schimra_b_socialize_day1_minutes",
  
  "schimra_b_socialize_day2_hours",
  "schimra_b_socialize_day2_minutes",
  
  "schimra_b_socialize_day3_hours",
  "schimra_b_socialize_day3_minutes",
  
  "schimra_b_socialize_day4_hours",
  "schimra_b_socialize_day4_minutes",
  
  "schimra_b_socialize_day5_hours",
  "schimra_b_socialize_day5_minutes",
  
  "schimra_b_socialize_day6_hours",
  "schimra_b_socialize_day6_minutes",
  
  "schimra_b_socialize_day7_hours",
  "schimra_b_socialize_day7_minutes",
  
  "schimra_b_socialize_ayc_day1",
  "schimra_b_socialize_ayc_day2",
  "schimra_b_socialize_ayc_day3",
  "schimra_b_socialize_ayc_day4",
  "schimra_b_socialize_ayc_day5",
  "schimra_b_socialize_ayc_day6",
  "schimra_b_socialize_ayc_day7",
  
  ### Interacting
  
  "schimra_b_interact",
  
  "schimra_b_interact_day1_hours",
  "schimra_b_interact_day1_minutes",
  
  "schimra_b_interact_day2_hours",
  "schimra_b_interact_day2_minutes",
  
  "schimra_b_interact_day3_hours",
  "schimra_b_interact_day3_minutes",
  
  "schimra_b_interact_day4_hours",
  "schimra_b_interact_day4_minutes",
  
  "schimra_b_interact_day5_hours",
  "schimra_b_interact_day5_minutes",
  
  "schimra_b_interact_day6_hours",
  "schimra_b_interact_day6_minutes",
  
  "schimra_b_interact_day7_hours",
  "schimra_b_interact_day7_minutes",
  
  "schimra_b_interact_ayc_day1",
  "schimra_b_interact_ayc_day2",
  "schimra_b_interact_ayc_day3",
  "schimra_b_interact_ayc_day4",
  "schimra_b_interact_ayc_day5",
  "schimra_b_interact_ayc_day6",
  "schimra_b_interact_ayc_day7",
  
  ### Other behaviors
  
  "schimra_b_other",
  
  "schimra_b_other_day1_behavior",
  "schimra_b_other_day2_behavior",
  "schimra_b_other_day3_behavior",
  "schimra_b_other_day4_behavior",
  "schimra_b_other_day5_behavior",
  "schimra_b_other_day6_behavior",
  "schimra_b_other_day7_behavior",
  
  "schimra_b_other_day1_hours",
  "schimra_b_other_day1_minutes",

  "schimra_b_other_day2_hours",
  "schimra_b_other_day2_minutes",
  
  "schimra_b_other_day3_hours",
  "schimra_b_other_day3_minutes",
  
  "schimra_b_other_day4_hours",
  "schimra_b_other_day4_minutes",
  
  "schimra_b_other_day5_hours",
  "schimra_b_other_day5_minutes",
  
  "schimra_b_other_day6_hours",
  "schimra_b_other_day6_minutes",
  
  "schimra_b_other_day7_hours",
  "schimra_b_other_day7_minutes",
  
  "schimra_b_other_ayc_day1",
  "schimra_b_other_ayc_day2",
  "schimra_b_other_ayc_day3",
  "schimra_b_other_ayc_day4",
  "schimra_b_other_ayc_day5",
  "schimra_b_other_ayc_day6",
  "schimra_b_other_ayc_day7",
  
  # C-ECWC
  
  "cecwc_01",
  "cecwc_02",
  "cecwc_03",
  "cecwc_04",
  "cecwc_05",
  
  # SWCH-s
  
  "swch_01",
  "swch_02",
  "swch_03",
  "swch_04",
  "swch_05",
  "swch_06",
  "swch_07",
  "swch_08",
  
  # HBI-19
  
  "hbi_19_01",
  "hbi_19_02",
  "hbi_19_03",
  "hbi_19_04",
  "hbi_19_05",
  "hbi_19_06",
  "hbi_19_07",
  "hbi_19_08",
  "hbi_19_09",
  "hbi_19_10",
  "hbi_19_11",
  "hbi_19_12",
  "hbi_19_13",
  "hbi_19_14",
  "hbi_19_15",
  "hbi_19_16",
  "hbi_19_17",
  "hbi_19_18",
  "hbi_19_19",
  
  # Alcohol use
  
  "alcoholuse_01",
  "alcoholuse_02",
  
  # F-SozU
  
  "fsozuk6_01",
  "fsozuk6_02",
  "fsozuk6_03",
  "fsozuk6_04",
  "fsozuk6_05",
  "fsozuk6_06",
  
  # RCQ
  
  "rcq_01",
  "rcq_02",
  "rcq_03",
  "rcq_04",
  "rcq_05",
  "rcq_06",
  
  # Demographics
  
  "leisure_working",
  "leisure_sports",
  "leisure_school",
  "leisure_home",
  "leisure_computer",
  "leisure_goingout",
  "leisure_high",
  "leisure_drunk",
  "leisure_lazing",
  "leisure_reading",
  "leisure_cinema",
  "leisure_tv",
  "leisure_music",
  "leisure_what_idk",
  "leisure_what_other",
  
  "leisure_alone",
  "leisure_partner",
  "leisure_friends",
  "leisure_sportcolleagues",
  "leisure_schoolcolleagues",
  "leisure_workcolleagues",
  "leisure_siblings",
  "leisure_children",
  "leisure_family",
  "leisure_who_idk",
  "leisure_who_other",
  
  # PHQ-9
  
  "phq_01",
  "phq_02",
  "phq_03",
  "phq_04",
  "phq_05",
  "phq_06",
  "phq_07",
  "phq_08",
  "phq_09",
  
  # IPS-8
  
  "ips_01",
  "ips_02",
  "ips_03",
  "ips_04",
  "ips_05",
  "ips_06",
  "ips_07",
  "ips_08",
  
  # EQ5D
  
  "eq5d_mobility",
  "eq5d_selfcare",
  "eq5d_usualactivities",
  "eq5d_pain",
  "eq5d_anxiety_depression",
  "eq5d_vas",
  
  # Language
  
  "language_native",
  "language_en_understanding",
  "language_en_read",
  "language_en_write",
  "language_en_oftenhear",
  "language_en_oftenread",
  
  "function_a_01",
  "function_a_02",
  "function_a_03",
  "function_a_04",
  
  "function_b_01",
  "function_b_02",
  "function_b_03",
  "function_b_04",
  
  # Truthfulness
  
  "truthfulness_honest",
  "truthfulness_if_dishonest",
  "truthfulness_other"
  
)

vars_pre_2b <- c(
  
  # Study information 
  
  "pre_2",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  # 
  # "ssas_sum",
  # 
  # "sbims_05",
  # "sbims_06",
  # "sbims_07",
  # 
  # "sbims_a_sum",
  # "sbims_b_sum",
  # "sbims_ab_sum",
  # 
  # "schimra_a2",
  # "schimra_a3",
  # "schimra_b2",
  # "schimra_b3",
  # 
  # "cecwc_sum",
  # "swchs_sum",
  # 
  # "hbi_19_control",
  # "hbi_19_consequences",
  # "hbi_19_coping",
  # "hbi_19_sum",
  # 
  # "alcohol_multipled",
  # "alcohol_sum",
  # 
  # "fsozuk6_sum",
  # 
  # "rcqs_sum",
  # 
  # "phq_9_sum",
  # "phq_9_suicide",
  # 
  # "ips_8_sum",
  # 
  # "eq5d_sum",
  # "eq5d_health",
  # 
  # "truthfulness_index",
  # 
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  "ssas_next",
  
  # SBIMS
  
  "sbimsa_sexualcontact",
  "sbimsa_exposure",
  "sbimsa_sexualinteractions",
  "sbimsa_physicalcontact",
  
  "sbimsb_sexualcontact",
  "sbimsb_exposure",
  "sbimsb_sexualinteractions",
  "sbimsb_physicalcontact",
  "sbimsb_csam",
  
  # SChiMRA
  
  ## Part A
  
  "schimra_a_watch",
  "schimra_a_socialize",
  "schimra_a_interact",
  
  ## Part B
  
  ### CSAM
  
  "schimra_b_csam",
  
  "schimra_b_csam_day1_hours",
  "schimra_b_csam_day1_minutes",
  
  "schimra_b_csam_day2_hours",
  "schimra_b_csam_day2_minutes",
  
  "schimra_b_csam_day3_hours",
  "schimra_b_csam_day3_minutes",
  
  "schimra_b_csam_day4_hours",
  "schimra_b_csam_day4_minutes",
  
  "schimra_b_csam_day5_hours",
  "schimra_b_csam_day5_minutes",
  
  "schimra_b_csam_day6_hours",
  "schimra_b_csam_day6_minutes",
  
  "schimra_b_csam_day7_hours",
  "schimra_b_csam_day7_minutes",
  
  "schimra_b_csam_copine_day1",
  "schimra_b_csam_copine_day2",
  "schimra_b_csam_copine_day3",
  "schimra_b_csam_copine_day4",
  "schimra_b_csam_copine_day5",
  "schimra_b_csam_copine_day6",
  "schimra_b_csam_copine_day7",
  
  "schimra_b_csam_ayc_day1",
  "schimra_b_csam_ayc_day2",
  "schimra_b_csam_ayc_day3",
  "schimra_b_csam_ayc_day4",
  "schimra_b_csam_ayc_day5",
  "schimra_b_csam_ayc_day6",
  "schimra_b_csam_ayc_day7",
  
  ### Socialization
  
  "schimra_b_socialize",
  
  "schimra_b_socialize_day1_hours",
  "schimra_b_socialize_day1_minutes",
  
  "schimra_b_socialize_day2_hours",
  "schimra_b_socialize_day2_minutes",
  
  "schimra_b_socialize_day3_hours",
  "schimra_b_socialize_day3_minutes",
  
  "schimra_b_socialize_day4_hours",
  "schimra_b_socialize_day4_minutes",
  
  "schimra_b_socialize_day5_hours",
  "schimra_b_socialize_day5_minutes",
  
  "schimra_b_socialize_day6_hours",
  "schimra_b_socialize_day6_minutes",
  
  "schimra_b_socialize_day7_hours",
  "schimra_b_socialize_day7_minutes",
  
  "schimra_b_socialize_ayc_day1",
  "schimra_b_socialize_ayc_day2",
  "schimra_b_socialize_ayc_day3",
  "schimra_b_socialize_ayc_day4",
  "schimra_b_socialize_ayc_day5",
  "schimra_b_socialize_ayc_day6",
  "schimra_b_socialize_ayc_day7",
  
  ### Interacting
  
  "schimra_b_interact",
  
  "schimra_b_interact_day1_hours",
  "schimra_b_interact_day1_minutes",
  
  "schimra_b_interact_day2_hours",
  "schimra_b_interact_day2_minutes",
  
  "schimra_b_interact_day3_hours",
  "schimra_b_interact_day3_minutes",
  
  "schimra_b_interact_day4_hours",
  "schimra_b_interact_day4_minutes",
  
  "schimra_b_interact_day5_hours",
  "schimra_b_interact_day5_minutes",
  
  "schimra_b_interact_day6_hours",
  "schimra_b_interact_day6_minutes",
  
  "schimra_b_interact_day7_hours",
  "schimra_b_interact_day7_minutes",
  
  "schimra_b_interact_ayc_day1",
  "schimra_b_interact_ayc_day2",
  "schimra_b_interact_ayc_day3",
  "schimra_b_interact_ayc_day4",
  "schimra_b_interact_ayc_day5",
  "schimra_b_interact_ayc_day6",
  "schimra_b_interact_ayc_day7",
  
  ### Other behaviors
  
  "schimra_b_other",
  
  "schimra_b_other_day1_behavior",
  "schimra_b_other_day2_behavior",
  "schimra_b_other_day3_behavior",
  "schimra_b_other_day4_behavior",
  "schimra_b_other_day5_behavior",
  "schimra_b_other_day6_behavior",
  "schimra_b_other_day7_behavior",
  
  "schimra_b_other_day1_hours",
  "schimra_b_other_day1_minutes",
  
  "schimra_b_other_day2_hours",
  "schimra_b_other_day2_minutes",
  
  "schimra_b_other_day3_hours",
  "schimra_b_other_day3_minutes",
  
  "schimra_b_other_day4_hours",
  "schimra_b_other_day4_minutes",
  
  "schimra_b_other_day5_hours",
  "schimra_b_other_day5_minutes",
  
  "schimra_b_other_day6_hours",
  "schimra_b_other_day6_minutes",
  
  "schimra_b_other_day7_hours",
  "schimra_b_other_day7_minutes",
  
  "schimra_b_other_ayc_day1",
  "schimra_b_other_ayc_day2",
  "schimra_b_other_ayc_day3",
  "schimra_b_other_ayc_day4",
  "schimra_b_other_ayc_day5",
  "schimra_b_other_ayc_day6",
  "schimra_b_other_ayc_day7",
  
  # C-ECWC
  
  "cecwc_01",
  "cecwc_02",
  "cecwc_03",
  "cecwc_04",
  
  "cecwc_pre_2b_01",
  "cecwc_pre_2b_02",
  
  "cecwc_05",
  
  "cecwc_pre_2b_03",
  "cecwc_pre_2b_04",
  "cecwc_pre_2b_05",
  "cecwc_pre_2b_06",
  "cecwc_pre_2b_07",
  
  # SWCH-s
  
  "swch_pre_2b_01",
  
  "swch_01",
  "swch_02",
  "swch_03",
  
  "swch_pre_2b_02",
  "swch_pre_2b_03",
  "swch_pre_2b_04",
  "swch_pre_2b_05",
  "swch_pre_2b_06",
  "swch_pre_2b_07",
  
  "swch_04",
  
  "swch_pre_2b_08",
  "swch_pre_2b_09",
  "swch_pre_2b_10",
  
  "swch_05",
  "swch_06",
  "swch_07",
  "swch_08",
  
  # HBI-19
  
  "hbi_19_01",
  "hbi_19_02",
  "hbi_19_03",
  "hbi_19_04",
  "hbi_19_05",
  "hbi_19_06",
  "hbi_19_07",
  "hbi_19_08",
  "hbi_19_09",
  "hbi_19_10",
  "hbi_19_11",
  "hbi_19_12",
  "hbi_19_13",
  "hbi_19_14",
  "hbi_19_15",
  "hbi_19_16",
  "hbi_19_17",
  "hbi_19_18",
  "hbi_19_19",
  
  # Alcohol use
  
  "alcoholuse_01",
  "alcoholuse_02",
  
  # BSCS
  
  "bscs_01",
  "bscs_02",
  "bscs_03",
  "bscs_04",
  "bscs_05",
  "bscs_06",
  "bscs_07",
  "bscs_08",
  "bscs_09",
  "bscs_10",
  "bscs_11",
  "bscs_12",
  "bscs_13",
  
  # F-SozU
  
  "fsozuk6_01",
  "fsozuk6_02",
  "fsozuk6_03",
  "fsozuk6_04",
  "fsozuk6_05",
  "fsozuk6_06",
  
  # RCQ
  
  "rcq_pre_2b_01",
  "rcq_pre_2b_02",
  "rcq_pre_2b_03",
  
  "rcq_01",
  "rcq_02",
  "rcq_03",
  "rcq_04",
  
  "rcq_pre_2b_04",
  
  "rcq_05",
  
  "rcq_pre_2b_05",
  "rcq_pre_2b_06",
  
  "rcq_06",
  
  # Demographics
  
  "leisure_working",
  "leisure_sports",
  "leisure_school",
  "leisure_home",
  "leisure_computer",
  "leisure_goingout",
  "leisure_high",
  "leisure_drunk",
  "leisure_lazing",
  "leisure_reading",
  "leisure_cinema",
  "leisure_tv",
  "leisure_music",
  "leisure_what_idk",
  "leisure_what_other",
  
  "leisure_alone",
  "leisure_partner",
  "leisure_friends",
  "leisure_sportcolleagues",
  "leisure_schoolcolleagues",
  "leisure_workcolleagues",
  "leisure_siblings",
  "leisure_children",
  "leisure_family",
  "leisure_who_idk",
  "leisure_who_other",
  
  # PHQ-9
  
  "phq_01",
  "phq_02",
  "phq_03",
  "phq_04",
  "phq_05",
  "phq_06",
  "phq_07",
  "phq_08",
  "phq_09",
  
  # IPS-8
  
  "ips_01",
  "ips_02",
  "ips_03",
  "ips_04",
  "ips_05",
  "ips_06",
  "ips_07",
  "ips_08",
  
  # EQ5D
  
  "eq5d_mobility",
  "eq5d_selfcare",
  "eq5d_usualactivities",
  "eq5d_pain",
  "eq5d_anxiety_depression",
  "eq5d_vas",
  
  # BIDR-SF
  
  "bidr_01",
  "bidr_02",
  "bidr_03",
  "bidr_04",
  "bidr_05",
  "bidr_06",
  "bidr_07",
  "bidr_08",
  "bidr_09",
  "bidr_10",
  "bidr_11",
  "bidr_12",
  "bidr_13",
  "bidr_14",
  "bidr_15",
  "bidr_16",
  
  # Language
  
  "language_native",
  "language_en_understanding",
  "language_en_read",
  "language_en_write",
  "language_en_oftenhear",
  "language_en_oftenread",
  
  "function_a_01",
  "function_a_02",
  "function_a_03",
  "function_a_04",
  
  "function_b_01",
  "function_b_02",
  "function_b_03",
  "function_b_04",
  
  # Truthfulness
  
  "truthfulness_honest",
  "truthfulness_if_dishonest",
  "truthfulness_other"
  
)

## Weekly

vars_weekly <- c(
  
  # Study information
  
  "weekly",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  # 
  # "ssas_sum",
  # 
  # "schimra_a2",
  # "schimra_a3",
  # "schimra_b2",
  # "schimra_b3",
  # 
  # "acute_preoccupation",
  # "acute_hostility",
  # "acute_hypersexuality",
  # "acute_rejectioncontrol",
  # "acute_emotional",
  # "acute_losssocial",
  # "acute_substanceuse",
  # "acute_sum",
  # "acute_suicide",
  # 
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  "ssas_next",
  
  # SChiMRA
  
  ## Part A
  
  "schimra_a_watch",
  "schimra_a_socialize",
  "schimra_a_interact",
  
  ## Part B
  
  ### CSAM
  
  "schimra_b_csam",
  
  "schimra_b_csam_day1_hours",
  "schimra_b_csam_day1_minutes",
  
  "schimra_b_csam_day2_hours",
  "schimra_b_csam_day2_minutes",
  
  "schimra_b_csam_day3_hours",
  "schimra_b_csam_day3_minutes",
  
  "schimra_b_csam_day4_hours",
  "schimra_b_csam_day4_minutes",
  
  "schimra_b_csam_day5_hours",
  "schimra_b_csam_day5_minutes",
  
  "schimra_b_csam_day6_hours",
  "schimra_b_csam_day6_minutes",
  
  "schimra_b_csam_day7_hours",
  "schimra_b_csam_day7_minutes",
  
  "schimra_b_csam_copine_day1",
  "schimra_b_csam_copine_day2",
  "schimra_b_csam_copine_day3",
  "schimra_b_csam_copine_day4",
  "schimra_b_csam_copine_day5",
  "schimra_b_csam_copine_day6",
  "schimra_b_csam_copine_day7",
  
  "schimra_b_csam_ayc_day1",
  "schimra_b_csam_ayc_day2",
  "schimra_b_csam_ayc_day3",
  "schimra_b_csam_ayc_day4",
  "schimra_b_csam_ayc_day5",
  "schimra_b_csam_ayc_day6",
  "schimra_b_csam_ayc_day7",
  
  ### Socialization
  
  "schimra_b_socialize",
  
  "schimra_b_socialize_day1_hours",
  "schimra_b_socialize_day1_minutes",
  
  "schimra_b_socialize_day2_hours",
  "schimra_b_socialize_day2_minutes",
  
  "schimra_b_socialize_day3_hours",
  "schimra_b_socialize_day3_minutes",
  
  "schimra_b_socialize_day4_hours",
  "schimra_b_socialize_day4_minutes",
  
  "schimra_b_socialize_day5_hours",
  "schimra_b_socialize_day5_minutes",
  
  "schimra_b_socialize_day6_hours",
  "schimra_b_socialize_day6_minutes",
  
  "schimra_b_socialize_day7_hours",
  "schimra_b_socialize_day7_minutes",
  
  "schimra_b_socialize_ayc_day1",
  "schimra_b_socialize_ayc_day2",
  "schimra_b_socialize_ayc_day3",
  "schimra_b_socialize_ayc_day4",
  "schimra_b_socialize_ayc_day5",
  "schimra_b_socialize_ayc_day6",
  "schimra_b_socialize_ayc_day7",
  
  ### Interacting
  
  "schimra_b_interact",
  
  "schimra_b_interact_day1_hours",
  "schimra_b_interact_day1_minutes",
  
  "schimra_b_interact_day2_hours",
  "schimra_b_interact_day2_minutes",
  
  "schimra_b_interact_day3_hours",
  "schimra_b_interact_day3_minutes",
  
  "schimra_b_interact_day4_hours",
  "schimra_b_interact_day4_minutes",
  
  "schimra_b_interact_day5_hours",
  "schimra_b_interact_day5_minutes",
  
  "schimra_b_interact_day6_hours",
  "schimra_b_interact_day6_minutes",
  
  "schimra_b_interact_day7_hours",
  "schimra_b_interact_day7_minutes",
  
  "schimra_b_interact_ayc_day1",
  "schimra_b_interact_ayc_day2",
  "schimra_b_interact_ayc_day3",
  "schimra_b_interact_ayc_day4",
  "schimra_b_interact_ayc_day5",
  "schimra_b_interact_ayc_day6",
  "schimra_b_interact_ayc_day7",
  
  ### Other behaviors
  
  "schimra_b_other",
  
  "schimra_b_other_day1_behavior",
  "schimra_b_other_day2_behavior",
  "schimra_b_other_day3_behavior",
  "schimra_b_other_day4_behavior",
  "schimra_b_other_day5_behavior",
  "schimra_b_other_day6_behavior",
  "schimra_b_other_day7_behavior",
  
  "schimra_b_other_day1_hours",
  "schimra_b_other_day1_minutes",
  
  "schimra_b_other_day2_hours",
  "schimra_b_other_day2_minutes",
  
  "schimra_b_other_day3_hours",
  "schimra_b_other_day3_minutes",
  
  "schimra_b_other_day4_hours",
  "schimra_b_other_day4_minutes",
  
  "schimra_b_other_day5_hours",
  "schimra_b_other_day5_minutes",
  
  "schimra_b_other_day6_hours",
  "schimra_b_other_day6_minutes",
  
  "schimra_b_other_day7_hours",
  "schimra_b_other_day7_minutes",
  
  "schimra_b_other_ayc_day1",
  "schimra_b_other_ayc_day2",
  "schimra_b_other_ayc_day3",
  "schimra_b_other_ayc_day4",
  "schimra_b_other_ayc_day5",
  "schimra_b_other_ayc_day6",
  "schimra_b_other_ayc_day7",
  
  # ACUTE-2007
  
  "acute_01",
  "acute_02",
  "acute_03",
  "acute_04",
  "acute_05",
  "acute_06",
  "acute_07",
  "acute_08",
  "acute_09",
  "acute_10",
  "acute_11",
  "acute_12",
  "acute_13",
  "acute_14",
  "acute_15",
  "acute_16",
  "acute_17",
  "acute_18",
  "acute_19",
  "acute_20",
  "acute_21"
  
)

## Weekly (2, 5, 8)

vars_weekly_258 <- c(
  
  # Study information
  
  "weekly_258",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  # 
  # "ssas_sum",
  # 
  # "schimra_a2",
  # "schimra_a3",
  # "schimra_b2",
  # "schimra_b3",
  # 
  # "acute_preoccupation",
  # "acute_hostility",
  # "acute_hypersexuality",
  # "acute_rejectioncontrol",
  # "acute_emotional",
  # "acute_losssocial",
  # "acute_substanceuse",
  # "acute_sum",
  # "acute_suicide",
  # 
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  "ssas_next",
  
  # SChiMRA
  
  ## Part A
  
  "schimra_a_watch",
  "schimra_a_socialize",
  "schimra_a_interact",
  
  ## Part B
  
  ### CSAM
  
  "schimra_b_csam",
  
  "schimra_b_csam_day1_hours",
  "schimra_b_csam_day1_minutes",
  
  "schimra_b_csam_day2_hours",
  "schimra_b_csam_day2_minutes",
  
  "schimra_b_csam_day3_hours",
  "schimra_b_csam_day3_minutes",
  
  "schimra_b_csam_day4_hours",
  "schimra_b_csam_day4_minutes",
  
  "schimra_b_csam_day5_hours",
  "schimra_b_csam_day5_minutes",
  
  "schimra_b_csam_day6_hours",
  "schimra_b_csam_day6_minutes",
  
  "schimra_b_csam_day7_hours",
  "schimra_b_csam_day7_minutes",
  
  "schimra_b_csam_copine_day1",
  "schimra_b_csam_copine_day2",
  "schimra_b_csam_copine_day3",
  "schimra_b_csam_copine_day4",
  "schimra_b_csam_copine_day5",
  "schimra_b_csam_copine_day6",
  "schimra_b_csam_copine_day7",
  
  "schimra_b_csam_ayc_day1",
  "schimra_b_csam_ayc_day2",
  "schimra_b_csam_ayc_day3",
  "schimra_b_csam_ayc_day4",
  "schimra_b_csam_ayc_day5",
  "schimra_b_csam_ayc_day6",
  "schimra_b_csam_ayc_day7",
  
  ### Socialization
  
  "schimra_b_socialize",
  
  "schimra_b_socialize_day1_hours",
  "schimra_b_socialize_day1_minutes",
  
  "schimra_b_socialize_day2_hours",
  "schimra_b_socialize_day2_minutes",
  
  "schimra_b_socialize_day3_hours",
  "schimra_b_socialize_day3_minutes",
  
  "schimra_b_socialize_day4_hours",
  "schimra_b_socialize_day4_minutes",
  
  "schimra_b_socialize_day5_hours",
  "schimra_b_socialize_day5_minutes",
  
  "schimra_b_socialize_day6_hours",
  "schimra_b_socialize_day6_minutes",
  
  "schimra_b_socialize_day7_hours",
  "schimra_b_socialize_day7_minutes",
  
  "schimra_b_socialize_ayc_day1",
  "schimra_b_socialize_ayc_day2",
  "schimra_b_socialize_ayc_day3",
  "schimra_b_socialize_ayc_day4",
  "schimra_b_socialize_ayc_day5",
  "schimra_b_socialize_ayc_day6",
  "schimra_b_socialize_ayc_day7",
  
  ### Interacting
  
  "schimra_b_interact",
  
  "schimra_b_interact_day1_hours",
  "schimra_b_interact_day1_minutes",
  
  "schimra_b_interact_day2_hours",
  "schimra_b_interact_day2_minutes",
  
  "schimra_b_interact_day3_hours",
  "schimra_b_interact_day3_minutes",
  
  "schimra_b_interact_day4_hours",
  "schimra_b_interact_day4_minutes",
  
  "schimra_b_interact_day5_hours",
  "schimra_b_interact_day5_minutes",
  
  "schimra_b_interact_day6_hours",
  "schimra_b_interact_day6_minutes",
  
  "schimra_b_interact_day7_hours",
  "schimra_b_interact_day7_minutes",
  
  "schimra_b_interact_ayc_day1",
  "schimra_b_interact_ayc_day2",
  "schimra_b_interact_ayc_day3",
  "schimra_b_interact_ayc_day4",
  "schimra_b_interact_ayc_day5",
  "schimra_b_interact_ayc_day6",
  "schimra_b_interact_ayc_day7",
  
  ### Other behaviors
  
  "schimra_b_other",
  
  "schimra_b_other_day1_behavior",
  "schimra_b_other_day2_behavior",
  "schimra_b_other_day3_behavior",
  "schimra_b_other_day4_behavior",
  "schimra_b_other_day5_behavior",
  "schimra_b_other_day6_behavior",
  "schimra_b_other_day7_behavior",
  
  "schimra_b_other_day1_hours",
  "schimra_b_other_day1_minutes",
  
  "schimra_b_other_day2_hours",
  "schimra_b_other_day2_minutes",
  
  "schimra_b_other_day3_hours",
  "schimra_b_other_day3_minutes",
  
  "schimra_b_other_day4_hours",
  "schimra_b_other_day4_minutes",
  
  "schimra_b_other_day5_hours",
  "schimra_b_other_day5_minutes",
  
  "schimra_b_other_day6_hours",
  "schimra_b_other_day6_minutes",
  
  "schimra_b_other_day7_hours",
  "schimra_b_other_day7_minutes",
  
  "schimra_b_other_ayc_day1",
  "schimra_b_other_ayc_day2",
  "schimra_b_other_ayc_day3",
  "schimra_b_other_ayc_day4",
  "schimra_b_other_ayc_day5",
  "schimra_b_other_ayc_day6",
  "schimra_b_other_ayc_day7",
  
  # ACUTE-2007
  
  "acute_01",
  "acute_02",
  "acute_03",
  "acute_04",
  "acute_05",
  "acute_06",
  "acute_07",
  "acute_08",
  "acute_09",
  "acute_10",
  "acute_11",
  "acute_12",
  "acute_13",
  "acute_14",
  "acute_15",
  "acute_16",
  "acute_17",
  "acute_18",
  "acute_19",
  "acute_20",
  "acute_21",
  
  # WAI-14
  
  "wai_14_01",
  "wai_14_02",
  "wai_14_03",
  "wai_14_04",
  "wai_14_05",
  "wai_14_06",
  "wai_14_07",
  "wai_14_08",
  "wai_14_09",
  "wai_14_10",
  "wai_14_11",
  "wai_14_12",
  "wai_14_13",
  "wai_14_14",
  
  # Child harm
  
  "child_harm"
  
)

## Post

vars_post <- c(
  
  # Study information 
  
  "post",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  # 
  # "ssas_sum",
  # 
  # "sbims_05",
  # "sbims_06",
  # "sbims_07",
  # 
  # "sbims_a_sum",
  # "sbims_b_sum",
  # "sbims_ab_sum",
  # 
  # "schimra_a2",
  # "schimra_a3",
  # "schimra_b2",
  # "schimra_b3",
  # 
  # "acute_preoccupation",
  # "acute_hostility",
  # "acute_hypersexuality",
  # "acute_rejectioncontrol",
  # "acute_emotional",
  # "acute_losssocial",
  # "acute_substanceuse",
  # "acute_sum",
  # "acute_suicide",
  # 
  # "cecwc_sum",
  # "swchs_sum",
  # 
  # "hbi_19_control",
  # "hbi_19_consequences",
  # "hbi_19_coping",
  # "hbi_19_sum",
  # 
  # "alcohol_multipled",
  # "alcohol_sum",
  # 
  # "fsozuk6_sum",
  # 
  # "rcqs_sum",
  # 
  # "phq_9_sum",
  # "phq_9_suicide",
  # 
  # "ips_8_sum",
  # 
  # "eq5d_sum",
  # "eq5d_health",
  # 
  # "neq_20_frequency_ne",
  # "neq_20_frequency_t",
  # "neq_20_frequency_o",
  # "neq_20_sum_nit",
  # "neq_20_sum_nio",
  # "neq_20_sum_01",
  # "neq_20_sum_02",
  # "neq_20_sum_03",
  # "neq_20_sum_04",
  # "neq_20_sum_05",
  # 
  # "truthfulness_index",
  # 
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  "ssas_next",
  
  # SBIMS
  
  "sbimsa_sexualcontact",
  "sbimsa_exposure",
  "sbimsa_sexualinteractions",
  "sbimsa_physicalcontact",
  
  "sbimsb_sexualcontact",
  "sbimsb_exposure",
  "sbimsb_sexualinteractions",
  "sbimsb_physicalcontact",
  "sbimsb_csam",
  
  # SChiMRA
  
  ## Part A
  
  "schimra_a_watch",
  "schimra_a_socialize",
  "schimra_a_interact",
  
  ## Part B
  
  ### CSAM
  
  "schimra_b_csam",
  
  "schimra_b_csam_day1_hours",
  "schimra_b_csam_day1_minutes",
  
  "schimra_b_csam_day2_hours",
  "schimra_b_csam_day2_minutes",
  
  "schimra_b_csam_day3_hours",
  "schimra_b_csam_day3_minutes",
  
  "schimra_b_csam_day4_hours",
  "schimra_b_csam_day4_minutes",
  
  "schimra_b_csam_day5_hours",
  "schimra_b_csam_day5_minutes",
  
  "schimra_b_csam_day6_hours",
  "schimra_b_csam_day6_minutes",
  
  "schimra_b_csam_day7_hours",
  "schimra_b_csam_day7_minutes",
  
  "schimra_b_csam_copine_day1",
  "schimra_b_csam_copine_day2",
  "schimra_b_csam_copine_day3",
  "schimra_b_csam_copine_day4",
  "schimra_b_csam_copine_day5",
  "schimra_b_csam_copine_day6",
  "schimra_b_csam_copine_day7",
  
  "schimra_b_csam_ayc_day1",
  "schimra_b_csam_ayc_day2",
  "schimra_b_csam_ayc_day3",
  "schimra_b_csam_ayc_day4",
  "schimra_b_csam_ayc_day5",
  "schimra_b_csam_ayc_day6",
  "schimra_b_csam_ayc_day7",
  
  ### Socialization
  
  "schimra_b_socialize",
  
  "schimra_b_socialize_day1_hours",
  "schimra_b_socialize_day1_minutes",
  
  "schimra_b_socialize_day2_hours",
  "schimra_b_socialize_day2_minutes",
  
  "schimra_b_socialize_day3_hours",
  "schimra_b_socialize_day3_minutes",
  
  "schimra_b_socialize_day4_hours",
  "schimra_b_socialize_day4_minutes",
  
  "schimra_b_socialize_day5_hours",
  "schimra_b_socialize_day5_minutes",
  
  "schimra_b_socialize_day6_hours",
  "schimra_b_socialize_day6_minutes",
  
  "schimra_b_socialize_day7_hours",
  "schimra_b_socialize_day7_minutes",
  
  "schimra_b_socialize_ayc_day1",
  "schimra_b_socialize_ayc_day2",
  "schimra_b_socialize_ayc_day3",
  "schimra_b_socialize_ayc_day4",
  "schimra_b_socialize_ayc_day5",
  "schimra_b_socialize_ayc_day6",
  "schimra_b_socialize_ayc_day7",
  
  ### Interacting
  
  "schimra_b_interact",
  
  "schimra_b_interact_day1_hours",
  "schimra_b_interact_day1_minutes",
  
  "schimra_b_interact_day2_hours",
  "schimra_b_interact_day2_minutes",
  
  "schimra_b_interact_day3_hours",
  "schimra_b_interact_day3_minutes",
  
  "schimra_b_interact_day4_hours",
  "schimra_b_interact_day4_minutes",
  
  "schimra_b_interact_day5_hours",
  "schimra_b_interact_day5_minutes",
  
  "schimra_b_interact_day6_hours",
  "schimra_b_interact_day6_minutes",
  
  "schimra_b_interact_day7_hours",
  "schimra_b_interact_day7_minutes",
  
  "schimra_b_interact_ayc_day1",
  "schimra_b_interact_ayc_day2",
  "schimra_b_interact_ayc_day3",
  "schimra_b_interact_ayc_day4",
  "schimra_b_interact_ayc_day5",
  "schimra_b_interact_ayc_day6",
  "schimra_b_interact_ayc_day7",
  
  ### Other behaviors
  
  "schimra_b_other",
  
  "schimra_b_other_day1_behavior",
  "schimra_b_other_day2_behavior",
  "schimra_b_other_day3_behavior",
  "schimra_b_other_day4_behavior",
  "schimra_b_other_day5_behavior",
  "schimra_b_other_day6_behavior",
  "schimra_b_other_day7_behavior",
  
  "schimra_b_other_day1_hours",
  "schimra_b_other_day1_minutes",
  
  "schimra_b_other_day2_hours",
  "schimra_b_other_day2_minutes",
  
  "schimra_b_other_day3_hours",
  "schimra_b_other_day3_minutes",
  
  "schimra_b_other_day4_hours",
  "schimra_b_other_day4_minutes",
  
  "schimra_b_other_day5_hours",
  "schimra_b_other_day5_minutes",
  
  "schimra_b_other_day6_hours",
  "schimra_b_other_day6_minutes",
  
  "schimra_b_other_day7_hours",
  "schimra_b_other_day7_minutes",
  
  "schimra_b_other_ayc_day1",
  "schimra_b_other_ayc_day2",
  "schimra_b_other_ayc_day3",
  "schimra_b_other_ayc_day4",
  "schimra_b_other_ayc_day5",
  "schimra_b_other_ayc_day6",
  "schimra_b_other_ayc_day7",
  
  # ACUTE-2007
  
  "acute_01",
  "acute_02",
  "acute_03",
  "acute_04",
  "acute_05",
  "acute_06",
  "acute_07",
  "acute_08",
  "acute_09",
  "acute_10",
  "acute_11",
  "acute_12",
  "acute_13",
  "acute_14",
  "acute_15",
  "acute_16",
  "acute_17",
  "acute_18",
  "acute_19",
  "acute_20",
  "acute_21",
  
  # C-ECWC
  
  "cecwc_01",
  "cecwc_02",
  "cecwc_03",
  "cecwc_04",
  "cecwc_05",
  
  # SWCH-s
  
  "swch_01",
  "swch_02",
  "swch_03",
  "swch_04",
  "swch_05",
  "swch_06",
  "swch_07",
  "swch_08",
  
  # HBI-19
  
  "hbi_19_01",
  "hbi_19_02",
  "hbi_19_03",
  "hbi_19_04",
  "hbi_19_05",
  "hbi_19_06",
  "hbi_19_07",
  "hbi_19_08",
  "hbi_19_09",
  "hbi_19_10",
  "hbi_19_11",
  "hbi_19_12",
  "hbi_19_13",
  "hbi_19_14",
  "hbi_19_15",
  "hbi_19_16",
  "hbi_19_17",
  "hbi_19_18",
  "hbi_19_19",
  
  # Alcohol use
  
  "alcoholuse_01",
  "alcoholuse_02",
  
  # F-SozU
  
  "fsozuk6_01",
  "fsozuk6_02",
  "fsozuk6_03",
  "fsozuk6_04",
  "fsozuk6_05",
  "fsozuk6_06",
  
  # RCQ
  
  "rcq_01",
  "rcq_02",
  "rcq_03",
  "rcq_04",
  "rcq_05",
  "rcq_06",
  
  # Demographics
  
  "leisure_working",
  "leisure_sports",
  "leisure_school",
  "leisure_home",
  "leisure_computer",
  "leisure_goingout",
  "leisure_high",
  "leisure_drunk",
  "leisure_lazing",
  "leisure_reading",
  "leisure_cinema",
  "leisure_tv",
  "leisure_music",
  "leisure_what_idk",
  "leisure_what_other",
  
  "leisure_alone",
  "leisure_partner",
  "leisure_friends",
  "leisure_sportcolleagues",
  "leisure_schoolcolleagues",
  "leisure_workcolleagues",
  "leisure_siblings",
  "leisure_children",
  "leisure_family",
  "leisure_who_idk",
  "leisure_who_other",
  
  # PHQ-9
  
  "phq_01",
  "phq_02",
  "phq_03",
  "phq_04",
  "phq_05",
  "phq_06",
  "phq_07",
  "phq_08",
  "phq_09",
  
  # IPS-8
  
  "ips_01",
  "ips_02",
  "ips_03",
  "ips_04",
  "ips_05",
  "ips_06",
  "ips_07",
  "ips_08",
  
  # EQ5D
  
  "eq5d_mobility",
  "eq5d_selfcare",
  "eq5d_usualactivities",
  "eq5d_pain",
  "eq5d_anxiety_depression",
  "eq5d_vas",
  
  # NEQ-20
  
  "neq_20_01",
  "neq_20_01a",
  "neq_20_01b",
  "neq_20_02",
  "neq_20_02a",
  "neq_20_02b",
  "neq_20_03",
  "neq_20_03a",
  "neq_20_03b",
  "neq_20_04",
  "neq_20_04a",
  "neq_20_04b",
  "neq_20_05",
  "neq_20_05a",
  "neq_20_05b",
  "neq_20_06",
  "neq_20_06a",
  "neq_20_06b",
  "neq_20_07",
  "neq_20_07a",
  "neq_20_07b",
  "neq_20_08",
  "neq_20_08a",
  "neq_20_08b",
  "neq_20_09",
  "neq_20_09a",
  "neq_20_09b",
  "neq_20_10",
  "neq_20_10a",
  "neq_20_10b",
  "neq_20_11",
  "neq_20_11a",
  "neq_20_11b",
  "neq_20_12",
  "neq_20_12a",
  "neq_20_12b",
  "neq_20_13",
  "neq_20_13a",
  "neq_20_13b",
  "neq_20_14",
  "neq_20_14a",
  "neq_20_14b",
  "neq_20_15",
  "neq_20_15a",
  "neq_20_15b",
  "neq_20_16",
  "neq_20_16a",
  "neq_20_16b",
  "neq_20_17",
  "neq_20_17a",
  "neq_20_17b",
  "neq_20_18",
  "neq_20_18a",
  "neq_20_18b",
  "neq_20_19",
  "neq_20_19a",
  "neq_20_19b",
  "neq_20_20",
  "neq_20_20a",
  "neq_20_20b",
  "neq_20_other",
  
  # Feedback
  
  "feedback_p1_01",
  "feedback_p1_02",
  "feedback_p1_03",
  "feedback_p1_04",
  "feedback_p1_05",
  "feedback_p1_06",
  "feedback_p1_07",
  "feedback_p1_08",
  "feedback_p1_09",
  
  "feedback_p2_01",
  "feedback_p2_02",
  "feedback_p2_03",
  "feedback_p2_04",
  "feedback_p2_05",
  "feedback_p2_06",
  "feedback_p2_07",
  "feedback_p2_08",
  "feedback_p2_09",
  "feedback_p2_10",
  "feedback_p2_11",
  "feedback_p2_12",
  "feedback_p2_13",
  "feedback_p2_14",
  "feedback_p2_15",
  "feedback_p2_16",
  "feedback_p2_17",
  "feedback_p2_18",
  "feedback_p2_19",
  "feedback_p2_20",
  "feedback_p2_21",
  "feedback_p2_22",
  
  # Truthfulness
  
  "truthfulness_honest",
  "truthfulness_if_dishonest",
  "truthfulness_other",
  
  # Additional consent
  
  "additional_consent"
  
)

## Follow Up

vars_followup <- c(
  
  # Study information 
  
  "follow_up",
  
  "study_code",
  "groups",
  "valid_from",
  "valid_until",
  "completed",
  
  # Composites created by platform
  # 
  # "ssas_sum",
  # 
  # "sbims_05",
  # "sbims_06",
  # "sbims_07",
  # 
  # "sbims_a_sum",
  # "sbims_b_sum",
  # "sbims_ab_sum",
  # 
  # "schimra_a2",
  # "schimra_a3",
  # "schimra_b2",
  # "schimra_b3",
  # 
  # "cecwc_sum",
  # "swchs_sum",
  # 
  # "hbi_19_control",
  # "hbi_19_consequences",
  # "hbi_19_coping",
  # "hbi_19_sum",
  # 
  # "alcohol_multipled",
  # "alcohol_sum",
  # 
  # "fsozuk6_sum",
  # 
  # "rcqs_sum",
  # 
  # "phq_9_sum",
  # "phq_9_suicide",
  # 
  # "ips_8_sum",
  # 
  # "eq5d_sum",
  # "eq5d_health",
  # 
  # "truthfulness_index",
  # 
  # SSAS
  
  "ssas_01",
  "ssas_02",
  "ssas_03",
  "ssas_04",
  "ssas_05",
  "ssas_06",
  "ssas_07",
  "ssas_08",
  "ssas_09",
  "ssas_10",
  "ssas_11",
  "ssas_12",
  
  "ssas_next",
  
  # SBIMS
  
  "sbimsa_sexualcontact",
  "sbimsa_exposure",
  "sbimsa_sexualinteractions",
  "sbimsa_physicalcontact",
  
  "sbimsb_sexualcontact",
  "sbimsb_exposure",
  "sbimsb_sexualinteractions",
  "sbimsb_physicalcontact",
  "sbimsb_csam",
  
  # SChiMRA
  
  ## Part A
  
  "schimra_a_watch",
  "schimra_a_socialize",
  "schimra_a_interact",
  
  ## Part B
  
  ### CSAM
  
  "schimra_b_csam",
  
  "schimra_b_csam_day1_hours",
  "schimra_b_csam_day1_minutes",
  
  "schimra_b_csam_day2_hours",
  "schimra_b_csam_day2_minutes",
  
  "schimra_b_csam_day3_hours",
  "schimra_b_csam_day3_minutes",
  
  "schimra_b_csam_day4_hours",
  "schimra_b_csam_day4_minutes",
  
  "schimra_b_csam_day5_hours",
  "schimra_b_csam_day5_minutes",
  
  "schimra_b_csam_day6_hours",
  "schimra_b_csam_day6_minutes",
  
  "schimra_b_csam_day7_hours",
  "schimra_b_csam_day7_minutes",
  
  "schimra_b_csam_copine_day1",
  "schimra_b_csam_copine_day2",
  "schimra_b_csam_copine_day3",
  "schimra_b_csam_copine_day4",
  "schimra_b_csam_copine_day5",
  "schimra_b_csam_copine_day6",
  "schimra_b_csam_copine_day7",
  
  "schimra_b_csam_ayc_day1",
  "schimra_b_csam_ayc_day2",
  "schimra_b_csam_ayc_day3",
  "schimra_b_csam_ayc_day4",
  "schimra_b_csam_ayc_day5",
  "schimra_b_csam_ayc_day6",
  "schimra_b_csam_ayc_day7",
  
  ### Socialization
  
  "schimra_b_socialize",
  
  "schimra_b_socialize_day1_hours",
  "schimra_b_socialize_day1_minutes",
  
  "schimra_b_socialize_day2_hours",
  "schimra_b_socialize_day2_minutes",
  
  "schimra_b_socialize_day3_hours",
  "schimra_b_socialize_day3_minutes",
  
  "schimra_b_socialize_day4_hours",
  "schimra_b_socialize_day4_minutes",
  
  "schimra_b_socialize_day5_hours",
  "schimra_b_socialize_day5_minutes",
  
  "schimra_b_socialize_day6_hours",
  "schimra_b_socialize_day6_minutes",
  
  "schimra_b_socialize_day7_hours",
  "schimra_b_socialize_day7_minutes",
  
  "schimra_b_socialize_ayc_day1",
  "schimra_b_socialize_ayc_day2",
  "schimra_b_socialize_ayc_day3",
  "schimra_b_socialize_ayc_day4",
  "schimra_b_socialize_ayc_day5",
  "schimra_b_socialize_ayc_day6",
  "schimra_b_socialize_ayc_day7",
  
  ### Interacting
  
  "schimra_b_interact",
  
  "schimra_b_interact_day1_hours",
  "schimra_b_interact_day1_minutes",
  
  "schimra_b_interact_day2_hours",
  "schimra_b_interact_day2_minutes",
  
  "schimra_b_interact_day3_hours",
  "schimra_b_interact_day3_minutes",
  
  "schimra_b_interact_day4_hours",
  "schimra_b_interact_day4_minutes",
  
  "schimra_b_interact_day5_hours",
  "schimra_b_interact_day5_minutes",
  
  "schimra_b_interact_day6_hours",
  "schimra_b_interact_day6_minutes",
  
  "schimra_b_interact_day7_hours",
  "schimra_b_interact_day7_minutes",
  
  "schimra_b_interact_ayc_day1",
  "schimra_b_interact_ayc_day2",
  "schimra_b_interact_ayc_day3",
  "schimra_b_interact_ayc_day4",
  "schimra_b_interact_ayc_day5",
  "schimra_b_interact_ayc_day6",
  "schimra_b_interact_ayc_day7",
  
  ### Other behaviors
  
  "schimra_b_other",
  
  "schimra_b_other_day1_behavior",
  "schimra_b_other_day2_behavior",
  "schimra_b_other_day3_behavior",
  "schimra_b_other_day4_behavior",
  "schimra_b_other_day5_behavior",
  "schimra_b_other_day6_behavior",
  "schimra_b_other_day7_behavior",
  
  "schimra_b_other_day1_hours",
  "schimra_b_other_day1_minutes",
  
  "schimra_b_other_day2_hours",
  "schimra_b_other_day2_minutes",
  
  "schimra_b_other_day3_hours",
  "schimra_b_other_day3_minutes",
  
  "schimra_b_other_day4_hours",
  "schimra_b_other_day4_minutes",
  
  "schimra_b_other_day5_hours",
  "schimra_b_other_day5_minutes",
  
  "schimra_b_other_day6_hours",
  "schimra_b_other_day6_minutes",
  
  "schimra_b_other_day7_hours",
  "schimra_b_other_day7_minutes",
  
  "schimra_b_other_ayc_day1",
  "schimra_b_other_ayc_day2",
  "schimra_b_other_ayc_day3",
  "schimra_b_other_ayc_day4",
  "schimra_b_other_ayc_day5",
  "schimra_b_other_ayc_day6",
  "schimra_b_other_ayc_day7",
  
  # C-ECWC
  
  "cecwc_01",
  "cecwc_02",
  "cecwc_03",
  "cecwc_04",
  "cecwc_05",
  
  # SWCH-s
  
  "swch_01",
  "swch_02",
  "swch_03",
  "swch_04",
  "swch_05",
  "swch_06",
  "swch_07",
  "swch_08",
  
  # HBI-19
  
  "hbi_19_01",
  "hbi_19_02",
  "hbi_19_03",
  "hbi_19_04",
  "hbi_19_05",
  "hbi_19_06",
  "hbi_19_07",
  "hbi_19_08",
  "hbi_19_09",
  "hbi_19_10",
  "hbi_19_11",
  "hbi_19_12",
  "hbi_19_13",
  "hbi_19_14",
  "hbi_19_15",
  "hbi_19_16",
  "hbi_19_17",
  "hbi_19_18",
  "hbi_19_19",
  
  # Alcohol use
  
  "alcoholuse_01",
  "alcoholuse_02",
  
  # F-SozU
  
  "fsozuk6_01",
  "fsozuk6_02",
  "fsozuk6_03",
  "fsozuk6_04",
  "fsozuk6_05",
  "fsozuk6_06",
  
  # RCQ
  
  "rcq_01",
  "rcq_02",
  "rcq_03",
  "rcq_04",
  "rcq_05",
  "rcq_06",
  
  # Demographics
  
  "leisure_working",
  "leisure_sports",
  "leisure_school",
  "leisure_home",
  "leisure_computer",
  "leisure_goingout",
  "leisure_high",
  "leisure_drunk",
  "leisure_lazing",
  "leisure_reading",
  "leisure_cinema",
  "leisure_tv",
  "leisure_music",
  "leisure_what_idk",
  "leisure_what_other",
  
  "leisure_alone",
  "leisure_partner",
  "leisure_friends",
  "leisure_sportcolleagues",
  "leisure_schoolcolleagues",
  "leisure_workcolleagues",
  "leisure_siblings",
  "leisure_children",
  "leisure_family",
  "leisure_who_idk",
  "leisure_who_other",
  
  # PHQ-9
  
  "phq_01",
  "phq_02",
  "phq_03",
  "phq_04",
  "phq_05",
  "phq_06",
  "phq_07",
  "phq_08",
  "phq_09",
  
  # IPS-8
  
  "ips_01",
  "ips_02",
  "ips_03",
  "ips_04",
  "ips_05",
  "ips_06",
  "ips_07",
  "ips_08",
  
  # EQ5D
  
  "eq5d_mobility",
  "eq5d_selfcare",
  "eq5d_usualactivities",
  "eq5d_pain",
  "eq5d_anxiety_depression",
  "eq5d_vas",
  
  # Truthfulness
  
  "truthfulness_honest",
  "truthfulness_if_dishonest",
  "truthfulness_other"
  
)

# Rename columns

colnames(raw_pre_1)      <- vars_pre_1
colnames(raw_pre_2)      <- vars_pre_2
colnames(raw_pre_1b)     <- vars_pre_1b
colnames(raw_pre_2b)     <- vars_pre_2b
colnames(raw_weekly)     <- vars_weekly
colnames(raw_weekly_258) <- vars_weekly_258
colnames(raw_post)       <- vars_post
colnames(raw_follow)     <- vars_followup

# Add timepoint information ----------------------------------------------------

raw_pre_1 <- raw_pre_1 %>% 
  mutate(
    timepoint = "pre_1"
  )

raw_pre_2 <- raw_pre_2 %>% 
  mutate(
    timepoint = "pre_2"
  )

raw_pre_1b <- raw_pre_1b %>% 
  mutate(
    timepoint = "pre_1"
  )

raw_pre_2b <- raw_pre_2b %>% 
  mutate(
    timepoint = "pre_2"
  )

raw_weekly <- raw_weekly %>% 
  mutate(
    timepoint = "weekly"
  )

raw_weekly_258 <- raw_weekly_258 %>%
  mutate(
    timepoint = "weekly"
  )

raw_post <- raw_post %>% 
  mutate(
    timepoint = "post"
  )

raw_follow <- raw_follow %>% 
  mutate(
    timepoint = "follow_up"
  )

# Bind questionnaires ----------------------------------------------------------

# Data from all questionnaires must be assembled into a single rectangular data
# table, arranged by participant and by timepoint. Here, we use the "valid from"
# date of the questionnaire to organize the data, since it will be a unique
# identifier of when the measurement occurred.

# Bind all questionnaires

raw_bound <- bind_rows(
  raw_pre_1, raw_pre_2,
  raw_pre_1b, raw_pre_2b,
  raw_weekly, raw_weekly_258,
  raw_post,
  raw_follow
  ) %>% 
  
  # Arrange by participant and date
  
  arrange(study_code, valid_from) %>% 
  relocate(timepoint, .after = study_code) %>% 
  
  # Remove extraneous columns that identify the questionnaire
  
  select(
    -pre_1, -pre_2,
    -weekly, -weekly_258,
    -post, -follow_up
  )

#  Coding of time variables ----------------------------------------------------

# For the planned analysis, it is necessary to correctly identify (1) how many
# weeks the participants  have been in the trial (`time`), (2) when they have
# begun treatment (`treatment`), and (3) how many weeks the participants have
# been in treatment (`time_after`).

# Transform date variables

raw_bound$valid_from  <- as_datetime(raw_bound$valid_from)
raw_bound$valid_until <- as_datetime(raw_bound$valid_until)
raw_bound$completed   <- ymd_hm(raw_bound$completed)

# Identify the group to which each participant is assigned

raw_bound <- raw_bound %>% 
  mutate(
    assigned_group = case_when(
      str_detect(groups, "WAITLIST")  ~ "waitlist", 
      str_detect(groups, "TREATMENT") ~ "cbt"
    )
  ) %>% 
  relocate(assigned_group, .before = timepoint)

# Identify treatment start date 

# Note that this is not necessarily the actual start date of treatment. Rather,
# this is the date at which it was possible to start for each participant.

raw_bound <- raw_bound %>% 
  group_by(study_code) %>% 
  mutate(
    treatment_date = case_when(
      assigned_group == "cbt"      ~ completed[which(timepoint == "pre_2")[1]],
      assigned_group == "waitlist" ~ completed[which(timepoint == "pre_2")[2]]
    ),
    treatment      = as.numeric(treatment_date < valid_from)
  ) %>% 
  ungroup() %>% 
  relocate(treatment_date, 
           treatment, 
           .after = timepoint)

## Set NA to 0

# The procedure above gives a missing value to waitlist participants who did not
# start the treatment. These cases should be set to 0.

raw_bound$treatment[is.na(raw_bound$treatment)] <- 0

# Identify first weekly measure on waitlist and in treatment

raw_bound <- raw_bound %>% 
  group_by(study_code) %>% 
  mutate(
    
    first_waitlist_weekly = case_when(
      
      assigned_group == "waitlist" 
      ~ valid_from[which(timepoint == "weekly")[1]],
      
      assigned_group == "cbt"      
      ~ NA
      
    ),
    
    first_treatment_weekly = case_when(
      
      assigned_group == "waitlist" 
      ~ valid_from[which(timepoint == "weekly" & treatment == 1)[1]],
      
      assigned_group == "cbt"      
      ~ valid_from[which(timepoint == "weekly")[1]]
      
    ),
    
    waitlist_reference_weekly = case_when(
      
      assigned_group == "waitlist" & treatment == 0 ~ first_waitlist_weekly,
      assigned_group == "waitlist" & treatment == 1 ~ first_treatment_weekly,
      assigned_group == "cbt"                       ~ NA
      
    )
    
  ) %>% 
  ungroup() %>% 
  relocate(first_waitlist_weekly, 
           first_treatment_weekly,
           waitlist_reference_weekly,
           .after = treatment_date)

# Calculate time variable (in weeks), for modeling

raw_bound <- raw_bound %>% 
  group_by(study_code) %>% 
  mutate(
    time = case_when(
      
      assigned_group == "waitlist" & valid_from < first_waitlist_weekly  ~ 0,
      assigned_group == "waitlist" & valid_from == first_waitlist_weekly ~ 1,
      
      assigned_group == "waitlist" & valid_from > first_waitlist_weekly 
      ~ as.numeric(difftime(valid_from, first_waitlist_weekly, units = "weeks")) + 1,
      
      assigned_group == "cbt" & valid_from < first_treatment_weekly      ~ 0,
      assigned_group == "cbt" & valid_from >= first_treatment_weekly  
      ~ as.numeric(difftime(valid_from, first_treatment_weekly, units = "weeks")) + 1
      
    )
  ) %>% 
  ungroup() %>% 
  relocate(time, 
           .before = treatment)

# Calculate time since treatment (in weeks), for modeling

raw_bound <- raw_bound %>% 
  group_by(study_code) %>% 
  mutate(
    time_after = case_when(
      
      assigned_group == "cbt" ~ time,
      
      assigned_group == "waitlist" & treatment == 0 ~ 0,
      assigned_group == "waitlist" & treatment == 1 
      ~ as.numeric(difftime(valid_from, first_treatment_weekly, units = "weeks")) + 1,
      
    )
  ) %>% 
  ungroup() %>% 
  relocate(time_after,
           .after = treatment)

# Round time variables to nearest week

raw_bound$time       <- round(raw_bound$time) 
raw_bound$time_after <- round(raw_bound$time_after)

# Calculate quadratic terms

raw_bound <- raw_bound %>% 
  mutate(
    time_sq       = time^2,
    time_after_sq = time_after^2,
  ) %>% 
  relocate(time_sq,
           time_after_sq,
           .after = time_after)

# Handling of specific cases ---------------------------------------------------

# Load masked information

# This script is used to specify information that is masked from public view.

source("R/prevent-it-2_masked.R")

# Remove case from PRIORITY that was mistakenly marked as part of GPP

raw_bound <- raw_bound %>% 
  filter(study_code != case_priority)

# Remove test cases

raw_bound <- raw_bound %>% 
  filter(!str_detect(groups, "Test patients"))

# Remove all cases not randomized

raw_bound <- raw_bound %>% 
  filter(str_detect(groups, "randomized") | study_code == case_nottagged)

## There was a case that was in fact randomized but was not tagged as such. This
## case is handled with the object case_nottagged

# Cases with missing waitlist questionnaires

# This waitlist participant was erroneously never sent any questionnaires during
# the waitlist period. As such, their time variables need to be adjusted to
# account for this.

wl_missed_time <- 
  as.numeric(
    round(
      difftime(
        raw_bound[raw_bound$study_code == case_wl_missed, ]$valid_from, 
        min(raw_bound[raw_bound$study_code == case_wl_missed, ]$valid_from), 
        units = "weeks")
    )
  )

raw_bound[raw_bound$study_code == case_wl_missed, ]$time    <- wl_missed_time
raw_bound[raw_bound$study_code == case_wl_missed, ]$time_sq <- wl_missed_time^2

# This waitlist participant either did not complete or never received the weekly
# questionnaires during the waitlist period

wl_missed_time_2 <- 
  as.numeric(
    round(
      difftime(
        raw_bound[raw_bound$study_code == case_wl_missed_2, ]$valid_from, 
        min(raw_bound[raw_bound$study_code == case_wl_missed_2, ]$valid_from), 
        units = "weeks")
    )
  )

raw_bound[raw_bound$study_code == case_wl_missed_2, ]$time    <- wl_missed_time_2
raw_bound[raw_bound$study_code == case_wl_missed_2, ]$time_sq <- wl_missed_time_2^2

# Add the group indicator for the case that was erroneously not tagged as
# randomized

raw_bound$assigned_group[raw_bound$study_code == case_nottagged] <- "waitlist"

# Remove masked objects, just in case

rm(case_wl_missed) 
rm(case_wl_missed_2) 
rm(case_nottagged)

# Calculate SSAS total score ---------------------------------------------------

# SSAS total score

# Create temporary subset of only SSAS data

ssas_df <- raw_bound %>% 
  select(starts_with("ssas_")) %>% 
  select(
    # -ssas_sum, 
    -ssas_next)

ssas_df <- map_df(ssas_df, as.numeric)

# Special handling for modifications to items 3, 6, and 8

# The original SSAS scale requested time estimates of certain behaviors using
# prespecified intervals. We modified the scale to request an amount of time in
# hours for these items. As planned, these items will be transformed into a 0 to
# 4 scale by creating quantiles for the non-zero values.

quantiles_ssas_03 <- quantile(ssas_df$ssas_03[ssas_df$ssas_03 != 0], 
                              na.rm = TRUE)

quantiles_ssas_06 <- quantile(ssas_df$ssas_06[ssas_df$ssas_06 != 0], 
                              na.rm = TRUE)

quantiles_ssas_08 <- quantile(ssas_df$ssas_08[ssas_df$ssas_08 != 0], 
                              na.rm = TRUE)

ssas_df <- ssas_df %>% 
  mutate(
    ssas_03 = case_when(
      ssas_03 >= quantiles_ssas_03[[4]]                                    ~ 4,
      ssas_03 >= quantiles_ssas_03[[3]] & ssas_03 < quantiles_ssas_03[[4]] ~ 3,
      ssas_03 >= quantiles_ssas_03[[2]] & ssas_03 < quantiles_ssas_03[[3]] ~ 2,
      ssas_03 >= quantiles_ssas_03[[1]] & ssas_03 < quantiles_ssas_03[[2]] ~ 1,
      ssas_03 == 0                                                         ~ 0
    ),
    ssas_06 = case_when(
      ssas_06 >= quantiles_ssas_06[[4]]                                    ~ 4,
      ssas_06 >= quantiles_ssas_06[[3]] & ssas_06 < quantiles_ssas_06[[4]] ~ 3,
      ssas_06 >= quantiles_ssas_06[[2]] & ssas_06 < quantiles_ssas_06[[3]] ~ 2,
      ssas_06 >= quantiles_ssas_06[[1]] & ssas_06 < quantiles_ssas_06[[2]] ~ 1,
      ssas_06 == 0                                                         ~ 0
    ),
    ssas_08 = case_when(
      ssas_08 >= quantiles_ssas_08[[4]]                                    ~ 4,
      ssas_08 >= quantiles_ssas_08[[3]] & ssas_08 < quantiles_ssas_08[[4]] ~ 3,
      ssas_08 >= quantiles_ssas_08[[2]] & ssas_08 < quantiles_ssas_08[[3]] ~ 2,
      ssas_08 >= quantiles_ssas_08[[1]] & ssas_08 < quantiles_ssas_08[[2]] ~ 1,
      ssas_08 == 0                                                         ~ 0
    )
  )

# Calculate total scores

raw_bound$ssas_sumscore  <- rowSums(ssas_df)

# Calculate SChiMRA Part B Behaviors -------------------------------------------

# CSAM use

# CSAM use is measured in hours and minutes, and to analyze this variable as
# hours, these measures will need to be transformed and combined.

## Time watched

csam_watch_df <- raw_bound %>% 
  select(starts_with("schimra_b_csam_day"))

csam_watch_df[csam_watch_df == "_1"] <- "0" # Change "less than 1" to 0

csam_watch_hours   <- csam_watch_df %>% 
  select(ends_with("hours")) %>% 
  map_df(as.numeric)

csam_watch_minutes <- csam_watch_df %>% 
  select(ends_with("minutes")) %>% 
  map_df(as.numeric)

csam_watch_minutes <- csam_watch_minutes/60

csam_watch_hours <- csam_watch_hours + csam_watch_minutes

### Add sum of hours to main data object and calculate daily average

raw_bound$schimra_b_csam_hours_sum <- rowSums(csam_watch_hours, na.rm = TRUE)

raw_bound$schimra_b_csam_hours_sum[is.na(raw_bound$schimra_b_csam)] <- NA

raw_bound <- raw_bound %>% 
  mutate(
    schimra_b_csam_hours_sum = case_when(
      timepoint == "pre_1" ~ NA,
      timepoint != "pre_1" ~ schimra_b_csam_hours_sum
    )
  )

# raw_bound$schimra_b_csam_hours_avg <- raw_bound$schimra_b_csam_hours_sum/7
raw_bound$schimra_b_csam_hours_avg <- raw_bound$schimra_b_csam_hours_sum

## COPINE severity

csam_copine_df <- raw_bound %>% 
  select(starts_with("schimra_b_csam_copine")) %>% 
  type_convert()

### Weekly maximum

copine_max <- apply(csam_copine_df, 1, max, na.rm = TRUE)
copine_max[copine_max == -Inf] <- NA

copine_max[!is.na(raw_bound$schimra_b_csam) & is.na(copine_max)] <- 0

### Weekly average

copine_mean <- rowMeans(csam_copine_df, na.rm = TRUE)

copine_mean[!is.na(raw_bound$schimra_b_csam) & is.na(copine_mean)] <- 0

### Add to data

raw_bound$schimra_b_csam_copine_max  <- copine_max
raw_bound$schimra_b_csam_copine_mean <- copine_mean

## Age of Youngest Child

csam_ayc_df <- raw_bound %>% 
  select(starts_with("schimra_b_csam_ayc"))

csam_ayc_df[csam_ayc_df == "_1"] <- "0" 

csam_ayc_df <- csam_ayc_df %>% 
  type_convert()

### Weekly maximum

ayc_min <- apply(csam_ayc_df, 1, min, na.rm = TRUE)
ayc_min[ayc_min == Inf] <- NA

### Weekly average

ayc_mean <- rowMeans(csam_ayc_df, na.rm = TRUE)

### Add to data

raw_bound$schimra_b_csam_age_min  <- ayc_min
raw_bound$schimra_b_csam_age_mean <- ayc_mean

# Socializing 

# Socializing is measured in hours and minutes, and to analyze this variable as
# hours, these measures will need to be transformed and combined.

## Time spent

socializing_df <- raw_bound %>% 
  select(starts_with("schimra_b_socialize_day"))

socializing_df[socializing_df == "_1"] <- "0" # Change "less than 1" to 0

socializing_hours   <- socializing_df %>% 
  select(ends_with("hours")) %>% 
  map_df(as.numeric)

sociailizing_minutes <- socializing_df %>% 
  select(ends_with("minutes")) %>% 
  map_df(as.numeric)

sociailizing_minutes <- sociailizing_minutes/60

socializing_hours <- socializing_hours + sociailizing_minutes

### Add sum of hours to main data object and calculate daily average

raw_bound$schimra_b_social_hours_sum <- 
  rowSums(socializing_hours, na.rm = TRUE)

raw_bound$schimra_b_social_hours_sum[is.na(raw_bound$schimra_b_socialize)] <- NA

raw_bound <- raw_bound %>% 
  mutate(
    schimra_b_social_hours_sum = case_when(
      timepoint == "pre_1" ~ NA,
      timepoint != "pre_1" ~ schimra_b_social_hours_sum
    )
  )

raw_bound$schimra_b_social_hours_avg <- 
  raw_bound$schimra_b_social_hours_sum/7

## Age of Youngest Child

social_ayc_df <- raw_bound %>% 
  select(starts_with("schimra_b_socialize_ayc_day"))

social_ayc_df[social_ayc_df == "_1"] <- "0" 

social_ayc_df <- social_ayc_df %>% 
  type_convert()

### Weekly maximum

ayc_soc_min <- apply(social_ayc_df, 1, min, na.rm = TRUE)
ayc_soc_min[ayc_soc_min == Inf] <- NA

### Weekly average

ayc_soc_mean <- rowMeans(social_ayc_df, na.rm = TRUE)

### Add to data

raw_bound$schimra_b_social_age_min  <- ayc_soc_min
raw_bound$schimra_b_social_age_mean <- ayc_soc_mean

# Interacting 

# Interacting is measured in hours and minutes, and to analyze this variable as
# hours, these measures will need to be transformed and combined.

## Time spent

interacting_df <- raw_bound %>% 
  select(starts_with("schimra_b_interact_day"))

interacting_df[interacting_df == "_1"] <- "0" # Change "less than 1" to 0

interacting_hours   <- interacting_df %>% 
  select(ends_with("hours")) %>% 
  map_df(as.numeric)

sociailizing_minutes <- interacting_df %>% 
  select(ends_with("minutes")) %>% 
  map_df(as.numeric)

sociailizing_minutes <- sociailizing_minutes/60

interacting_hours <- interacting_hours + sociailizing_minutes

### Add sum of hours to main data object and calculate daily average

raw_bound$schimra_b_interact_hours_sum <- 
  rowSums(interacting_hours, na.rm = TRUE)

raw_bound$schimra_b_interact_hours_sum[is.na(raw_bound$schimra_b_interact)] <- NA

raw_bound <- raw_bound %>% 
  mutate(
    schimra_b_interact_hours_sum = case_when(
      timepoint == "pre_1" ~ NA,
      timepoint != "pre_1" ~ schimra_b_interact_hours_sum
    )
  )

raw_bound$schimra_b_interact_hours_avg <- 
  raw_bound$schimra_b_interact_hours_sum/7

## Age of Youngest Child

interact_ayc_df <- raw_bound %>% 
  select(starts_with("schimra_b_interact_ayc_day"))

interact_ayc_df[interact_ayc_df == "_1"] <- "0" 

interact_ayc_df <- interact_ayc_df %>% 
  type_convert()

### Weekly maximum

ayc_soc_min <- apply(interact_ayc_df, 1, min, na.rm = TRUE)
ayc_soc_min[ayc_soc_min == Inf] <- NA

### Weekly average

ayc_soc_mean <- rowMeans(interact_ayc_df, na.rm = TRUE)

### Add to data

raw_bound$schimra_b_interact_age_min  <- ayc_soc_min
raw_bound$schimra_b_interact_age_mean <- ayc_soc_mean

# Other behaviors 

# Other problematic behaviors are measured in hours and minutes, and to analyze
# this variable as hours, these measures will need to be transformed and
# combined.

## Time spent

other_df <- raw_bound %>% 
  select(starts_with("schimra_b_other_day"))

other_df[other_df == "_1"] <- "0" # Change "less than 1" to 0

other_hours   <- other_df %>% 
  select(ends_with("hours")) %>% 
  map_df(as.numeric)

sociailizing_minutes <- other_df %>% 
  select(ends_with("minutes")) %>% 
  map_df(as.numeric)

sociailizing_minutes <- sociailizing_minutes/60

other_hours <- other_hours + sociailizing_minutes

### Add sum of hours to main data object and calculate daily average

raw_bound$schimra_b_other_hours_sum <- 
  rowSums(other_hours, na.rm = TRUE)

raw_bound$schimra_b_other_hours_sum[is.na(raw_bound$schimra_b_other)] <- NA

raw_bound <- raw_bound %>% 
  mutate(
    schimra_b_other_hours_sum = case_when(
      timepoint == "pre_1" ~ NA,
      timepoint != "pre_1" ~ schimra_b_other_hours_sum
    )
  )

raw_bound$schimra_b_other_hours_avg <- 
  raw_bound$schimra_b_other_hours_sum/7

## Age of Youngest Child

other_ayc_df <- raw_bound %>% 
  select(starts_with("schimra_b_other_ayc_day"))

other_ayc_df[other_ayc_df == "_1"] <- "0" 

other_ayc_df <- other_ayc_df %>% 
  type_convert()

### Weekly maximum

ayc_soc_min <- apply(other_ayc_df, 1, min, na.rm = TRUE)
ayc_soc_min[ayc_soc_min == Inf] <- NA

### Weekly average

ayc_soc_mean <- rowMeans(other_ayc_df, na.rm = TRUE)

### Add to data

raw_bound$schimra_b_other_age_min  <- ayc_soc_min
raw_bound$schimra_b_other_age_mean <- ayc_soc_mean

# Calculate HBI-19 scores ------------------------------------------------------

# HBI-19 total score

# Create temporary subset of only HBI-19 data

hbi_19_df <- raw_bound %>% 
  select(starts_with("hbi_19_"))

hbi_19_df <- map_df(hbi_19_df, as.numeric)

# Calculate total scores

raw_bound$hbi_19_sumscore  <- rowSums(hbi_19_df)

# Calculate RAADS-14 scores ----------------------------------------------------

# RAADS-14 total score

# Create temporary subset of only RAADS-14 data

raads_14_df <- raw_bound %>% 
  select(starts_with("raads_14_"))

raads_14_df <- map_df(raads_14_df, as.numeric)

# Calculate total scores

raw_bound$raads_14_sumscore  <- rowSums(raads_14_df)

# Calculate PHQ-9 scores -------------------------------------------------------

# PHQ-9 total score

# Create temporary subset of only PHQ-9 data

phq_df <- raw_bound %>% 
  select(starts_with("phq_"))

phq_df <- map_df(phq_df, as.numeric)

# Calculate total scores

raw_bound$phq_sumscore  <- rowSums(phq_df)

# Calculate AUDIT scores -------------------------------------------------------

# AUDIT total score

# Create temporary subset of only AUDIT data

audit_df <- raw_bound %>% 
  select(starts_with("audit_"))

audit_df <- map_df(audit_df, as.numeric)

# Calculate total scores

raw_bound$audit_sumscore  <- rowSums(audit_df)

# Calculate SWCH scores --------------------------------------------------------

# SWCH total score

# Create temporary subset of only SWCH data

swch_df <- raw_bound %>% 
  select(starts_with("swch_0"))

swch_df <- map_df(swch_df, as.numeric)

# Calculate total scores

raw_bound$swch_sumscore  <- rowSums(swch_df)

# Calculate C-ECWC scores ------------------------------------------------------

# CECWC total score

# Create temporary subset of only CECWC data

cecwc_df <- raw_bound %>% 
  select(starts_with("cecwc_0"))

cecwc_df <- map_df(cecwc_df, as.numeric)

# Calculate total scores

raw_bound$cecwc_sumscore  <- rowSums(cecwc_df)

# Calculate ACUTE-2007 scores --------------------------------------------------

# ACUTE-2007 total score

# Create temporary subset of only ACUTE data

acute_df <- raw_bound %>% 
  select(starts_with("acute_"))

acute_df <- map_df(acute_df, as.numeric)

# Calculate total scores

raw_bound$acute_sum  <- rowSums(acute_df)

# Potential predictors on missingness and baselines ----------------------------

predictor_df <- raw_bound %>% 
  select(
    study_code,
    time,
    lassie_pedophilia_01,
    cecwc_sumscore,
    swch_sumscore,
    raads_14_sumscore,
    phq_sumscore,
    ssas_sumscore,
    hbi_19_sumscore
  ) %>% 
  filter(time == 0) %>% 
  group_by(study_code) %>% 
  summarise(
    lassie_pedophilia_01_baseline = mean(as.numeric(lassie_pedophilia_01), na.rm = TRUE),
    cecwc_sumscore_baseline       = mean(cecwc_sumscore, na.rm = TRUE),
    swch_sumscore_baseline        = mean(swch_sumscore, na.rm = TRUE),
    raads_14_sumscore_baseline    = mean(raads_14_sumscore, na.rm = TRUE), 
    phq_sumscore_baseline         = mean(phq_sumscore, na.rm = TRUE),
    ssas_sumscore_baseline        = mean(ssas_sumscore, na.rm = TRUE),
    hbi_sumscore_baseline         = mean(hbi_19_sumscore, na.rm = TRUE)
  ) %>% 
  ungroup()

raw_bound <- raw_bound %>% 
  left_join(predictor_df, by = "study_code")

# Missingness indicator --------------------------------------------------------

raw_bound$ssas_missing <- as.numeric(is.na(raw_bound$ssas_sumscore))

# Pre-Post indicator -----------------------------------------------------------

# For analyses of variables only measured at the pre and post timepoints, it is
# convenient to have an indicator for whether the measure has been taken pre or
# post.

raw_bound <- raw_bound %>% 
  mutate(
    pre_post = case_when(
      timepoint == "pre_1"             ~ 0,
      timepoint == "pre_2" & time == 0 ~ 0,
      timepoint == "pre_2" & time != 0 ~ 0,
      timepoint == "post"              ~ 1,
      timepoint == "weekly"            ~ NA
    ),
    treat_prepost = case_when(
      assigned_group == "cbt"      & timepoint == "pre_1"                  ~ 1,
      assigned_group == "cbt"      & timepoint == "pre_2"                  ~ 1,
      assigned_group == "cbt"      & timepoint == "post"                   ~ 1,
      assigned_group == "waitlist" & timepoint == "pre_1"                  ~ 0,
      assigned_group == "waitlist" & timepoint == "pre_2" & time == 0      ~ 0,
      assigned_group == "waitlist" & timepoint == "pre_2" & time != 0      ~ 1,
      assigned_group == "waitlist" & timepoint == "post"  & treatment == 0 ~ 0,
      assigned_group == "waitlist" & timepoint == "post"  & treatment == 1 ~ 1
    )
  ) %>% 
  relocate(
    pre_post, treat_prepost,
    .after = timepoint
  )

# Pre vs. Follow-Up indicator --------------------------------------------------

raw_bound <- raw_bound %>% 
  mutate(
    post_follow = case_when(
      timepoint == "post"              ~ 0,
      timepoint == "pre_1"             ~ NA,
      timepoint == "pre_2" & time == 0 ~ NA,
      timepoint == "pre_2" & time != 0 ~ 1,
      timepoint == "follow_up"         ~ 1,
      timepoint == "weekly"            ~ NA
    ),
    treat_postfollow = case_when(
      assigned_group == "cbt"      & timepoint == "post"                  ~ 1,
      assigned_group == "cbt"      & timepoint == "follow_up"             ~ 1,
      assigned_group == "waitlist" & timepoint == "post" & treatment == 0 ~ 0,
      assigned_group == "waitlist" & timepoint == "post" & treatment == 1 ~ 1,
      assigned_group == "waitlist" & timepoint == "pre_2" & time == 0     ~ NA,
      assigned_group == "waitlist" & timepoint == "pre_2" & time != 0     ~ 0,
      assigned_group == "waitlist" & timepoint == "follow_up"             ~ 1
    )
  ) %>% 
  relocate(
    post_follow, treat_postfollow,
    .after = timepoint
  )

# Data for analysis and export -------------------------------------------------

# For analysis of primary and secondary outcomes

gpp_data_main <- raw_bound %>% 
  filter(timepoint != "follow_up") %>% 
  select(
    -first_waitlist_weekly, 
    -first_treatment_weekly, 
    -waitlist_reference_weekly,
    -treatment_date,
    -post_follow,
    -treat_postfollow
  ) %>% 
  rename(
    id = study_code
  ) %>% 
  type_convert()

## Subgroup: People who entered the trial as active CSAM users

gpp_data_active_csam <- gpp_data_main %>% 
  group_by(id) %>% 
  mutate(
    active_csam = case_when(
      schimra_b_csam_hours_avg[which(timepoint == "pre_2")[1]] >  0 ~ 1,
      schimra_b_csam_hours_avg[which(timepoint == "pre_2")[1]] == 0 ~ 0
    )
  ) %>% 
  ungroup() %>% 
  filter(active_csam == 1)

# Follow up data

gpp_data_followup <- raw_bound %>%
  select(
    -first_waitlist_weekly, 
    -first_treatment_weekly, 
    -waitlist_reference_weekly,
    -treatment_date
  ) %>% 
  rename(
    id = study_code
  ) %>% 
  type_convert()

# Export cleaned data file

write_csv(gpp_data_main, "data/gpp_data_main_cleaned.csv")
