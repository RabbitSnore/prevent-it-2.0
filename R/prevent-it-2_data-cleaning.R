################################################################################

# Prevent It 2.0, Global Perpetration Prevention -- Data Cleaning and Wrangling

# Author: Timothy J Luke

################################################################################

# Set up -----------------------------------------------------------------------

packages <- c("tidyverse",
              "readxl")

lapply(packages, library, character.only = TRUE)

# Data loading -----------------------------------------------------------------

#  Note that this code is currently written to load test data, rather than the
#  real data.

# Pre-Questionnaire

raw_pre_1      <- read_xlsx("data/gpp_test-data_pre-1.xlsx", 
                            col_types = "text",
                            trim_ws = TRUE)

raw_pre_2      <- read_xlsx("data/gpp_test-data_pre-2.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

# Weekly measures

raw_weekly     <- read_xlsx("data/gpp_test-data_weekly.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

raw_weekly_258 <- read_xlsx("data/gpp_test-data_weekly258.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

# Post questionnaire

raw_post       <- read_xlsx("data/gpp_test-data_post.xlsx",
                            col_types = "text",
                            trim_ws = TRUE)

# Follow up

raw_follow     <- read_xlsx("data/gpp_test-data_followup.xlsx",
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
  
  "acess_emot_abuse",
  "acess_phys_abuse",
  "acess_sex_abuse",
  "acess_emot_neglect",
  "acess_phys_neglect",
  "acess_mental_illness",
  "acess_subs_abuse",
  "acess_incarcerated",
  "acess_sum",
  
  "ssas_sum",
  
  "sbims_b_sum",
  
  "audit_sum",
  
  "lassie_s_interests",
  
  "raads_14_mentdef",
  "raads_14_socanx",
  "raads_14_sensreact",
  "raads_14_sum",
  
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
  
  "ssas_sum",
  
  "sbims_05",
  "sbims_06",
  "sbims_07",
  
  "sbims_a_sum",
  "sbims_b_sum",
  "sbims_ab_sum",
  
  "schimra_a2",
  "schimra_a3",
  "schimra_b2",
  "schimra_b3",
  
  "cecwc_sum",
  "swchs_sum",
  
  "hbi_19_control",
  "hbi_19_consequences",
  "hbi_19_coping",
  "hbi_19_sum",
  
  "alcohol_multipled",
  "alcohol_sum",
  
  "fsozuk6_sum",
  
  "rcqs_sum",
  
  "phq_9_sum",
  "phq_9_suicide",
  
  "ips_8_sum",
  
  "eq5d_sum",
  "eq5d_health",
  
  "truthfulness_index",
  
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
  
  "ssas_sum",
  
  "schimra_a2",
  "schimra_a3",
  "schimra_b2",
  "schimra_b3",
  
  "acute_preoccupation",
  "acute_hostility",
  "acute_hypersexuality",
  "acute_rejectioncontrol",
  "acute_emotional",
  "acute_losssocial",
  "acute_substanceuse",
  "acute_sum",
  "acute_suicide",
  
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
  
  "ssas_sum",
  
  "schimra_a2",
  "schimra_a3",
  "schimra_b2",
  "schimra_b3",
  
  "acute_preoccupation",
  "acute_hostility",
  "acute_hypersexuality",
  "acute_rejectioncontrol",
  "acute_emotional",
  "acute_losssocial",
  "acute_substanceuse",
  "acute_sum",
  "acute_suicide",
  
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
  
  "ssas_sum",
  
  "sbims_05",
  "sbims_06",
  "sbims_07",
  
  "sbims_a_sum",
  "sbims_b_sum",
  "sbims_ab_sum",
  
  "schimra_a2",
  "schimra_a3",
  "schimra_b2",
  "schimra_b3",
  
  "acute_preoccupation",
  "acute_hostility",
  "acute_hypersexuality",
  "acute_rejectioncontrol",
  "acute_emotional",
  "acute_losssocial",
  "acute_substanceuse",
  "acute_sum",
  "acute_suicide",
  
  "cecwc_sum",
  "swchs_sum",
  
  "hbi_19_control",
  "hbi_19_consequences",
  "hbi_19_coping",
  "hbi_19_sum",
  
  "alcohol_multipled",
  "alcohol_sum",
  
  "fsozuk6_sum",
  
  "rcqs_sum",
  
  "phq_9_sum",
  "phq_9_suicide",
  
  "ips_8_sum",
  
  "eq5d_sum",
  "eq5d_health",
  
  "neq_20_frequency_ne",
  "neq_20_frequency_t",
  "neq_20_frequency_o",
  "neq_20_sum_nit",
  "neq_20_sum_nio",
  "neq_20_sum_01",
  "neq_20_sum_02",
  "neq_20_sum_03",
  "neq_20_sum_04",
  "neq_20_sum_05",
  
  "truthfulness_index",
  
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
  
  "ssas_sum",
  
  "sbims_05",
  "sbims_06",
  "sbims_07",
  
  "sbims_a_sum",
  "sbims_b_sum",
  "sbims_ab_sum",
  
  "schimra_a2",
  "schimra_a3",
  "schimra_b2",
  "schimra_b3",
  
  "cecwc_sum",
  "swchs_sum",
  
  "hbi_19_control",
  "hbi_19_consequences",
  "hbi_19_coping",
  "hbi_19_sum",
  
  "alcohol_multipled",
  "alcohol_sum",
  
  "fsozuk6_sum",
  
  "rcqs_sum",
  
  "phq_9_sum",
  "phq_9_suicide",
  
  "ips_8_sum",
  
  "eq5d_sum",
  "eq5d_health",

  "truthfulness_index",
  
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
      str_detect(groups, "TREATMENT") ~ "cbt", 
      str_detect(groups, "WAITLIST")  ~ "waitlist", 
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
      ~ as.numeric(difftime(valid_from, waitlist_reference_weekly, units = "weeks")) + 1,
      
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

# Calculate quadradic terms

raw_bound <- raw_bound %>% 
  mutate(
    time_sq       = time^2,
    time_after_sq = time_after^2,
  ) %>% 
  relocate(time_sq,
           time_after_sq,
           .after = time_after)

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

raw_bound <- raw_bound %>% 
  mutate(
    schimra_b_csam_hours_sum = case_when(
      timepoint == "pre_1" ~ NA,
      timepoint != "pre_1" ~ schimra_b_csam_hours_sum
    )
  )

raw_bound$schimra_b_csam_hours_avg <- raw_bound$schimra_b_csam_hours_sum/7

# Data for analysis and export -------------------------------------------------

# For analysis of primary and secondary outcomes

gpp_data_main <- raw_bound %>% 
  filter(timepoint != "follow_up") %>% 
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
