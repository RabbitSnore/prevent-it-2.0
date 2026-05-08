################################################################################

# GPP and PRIORITY -- Dropout Analysis, Data Wrangling

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c(
  "tidyverse", 
  "haven",
  "readxl"
)

lapply(packages, library, character.only = TRUE)

# Load data files --------------------------------------------------------------

# GPP

gpp_data_main      <- read_csv("data/gpp_data_main_cleaned.csv")

# PRIORITY

priority_data_main <- read_spss("data/priority/priority_data_main.sav")

## Remove SChiMRA Part B

priority_data_main <- priority_data_main %>% 
  select(
    -starts_with("schimra_b"),
    -starts_with("leisure")
  )

## SSAS data

priority_ssas_data <-  priority_data_main %>% 
  filter(timepoint %in% c("pre_1", "pre_2")) %>% 
  select(
    id,
    assigned_group,
    time,
    timepoint,
    completed,
    starts_with("ssas"),
    -ssas_next
  )

priority_ssas_data <- priority_ssas_data %>% 
  mutate(
    timepoint = case_when(
      timepoint == "pre_1" ~ "pre_1",
      timepoint == "pre_2" & time == 0 ~ "pre_2",
      timepoint == "pre_2" & time != 0 ~ "pre_3",
    )
  )

gpp_ssas_data <- gpp_data_main %>% 
  filter(timepoint %in% c("pre_1", "pre_2")) %>% 
  select(
    id,
    assigned_group,
    time,
    timepoint,
    completed,
    starts_with("ssas"),
    -ssas_next
  )

gpp_ssas_data <- gpp_ssas_data %>% 
  mutate(
    timepoint = case_when(
      timepoint == "pre_1" ~ "pre_1",
      timepoint == "pre_2" & time == 0 ~ "pre_2",
      timepoint == "pre_2" & time != 0 ~ "pre_3",
    )
  )

ssas_data <- bind_rows(gpp_ssas_data, priority_ssas_data)

# Worksheet completion data

worksheets_01 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 1)
worksheets_02 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 2)
worksheets_03 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 3)
worksheets_04 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 4)
worksheets_05 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 5)
worksheets_06 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 6)
worksheets_07 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 7)
worksheets_08 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 8)
worksheets_09 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 9)
worksheets_10 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 10)
worksheets_11 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 11)
worksheets_12 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 12)
worksheets_13 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 13)
worksheets_14 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 14)
worksheets_15 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 15)
worksheets_16 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 16)
worksheets_17 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 17)
worksheets_18 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 18)
worksheets_19 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 19)
worksheets_20 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 20)
worksheets_21 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 21)
worksheets_22 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 22)
worksheets_23 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 23)
worksheets_24 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 24)
worksheets_25 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 25)
worksheets_26 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 26)
worksheets_27 <- read_xlsx("data/TheStudy_worksheets_260402 (1).xlsx", sheet = 27)

# Module reading data

reading <- read_xlsx("data/priority/GPP_PRIORITY_USERLOG_MODULESREAD.xlsx")

# Demographics

demographics <- read_xlsx("data/priority/GPP PRIORITY_demographics.xlsx")

# Data wrangling ---------------------------------------------------------------

# Sum scores for PRIORITY

# HBI-19 total score

## Create temporary subset of only HBI-19 data

hbi_19_df <- priority_data_main %>% 
  select(starts_with("hbi_19_"))

hbi_19_df <- map_df(hbi_19_df, as.numeric)

## Calculate total scores

priority_data_main$hbi_19_sumscore  <- rowSums(hbi_19_df)

# PHQ-9 total score

## Create temporary subset of only PHQ-9 data

phq_df <- priority_data_main %>% 
  select(starts_with("phq_"))

phq_df <- map_df(phq_df, as.numeric)

## Calculate total scores

priority_data_main$phq_sumscore  <- rowSums(phq_df)

# Data combination

pi2 <- bind_rows(gpp_data_main, priority_data_main)

pi2 <- pi2 %>% 
  mutate(
    completed_pre_2 = case_when(
      time < 9 & timepoint == "pre_2" & !is.na(completed) ~ 1,
      time < 9 & timepoint == "pre_2" &  is.na(completed) ~ 0
    )
  )

included_ids <- pi2 %>% 
  filter(completed_pre_2 == 1) %>% 
  pull(id)

pi2 <- pi2 %>% 
  filter(id %in% included_ids)

# Variable selection

raads_data <- pi2 %>% 
  filter(timepoint == "pre_1") %>% 
  select(id, starts_with("raads"))

criminal_data <- pi2 %>% 
  filter(timepoint == "pre_1") %>% 
  select(
    id,
    criminal_nonviolent,
    criminal_violent,
    criminal_noncontact,
    criminal_contact,
    criminal_pre_1b_01,
    criminal_pre_1b_04,
    criminal_pre_1b_20,
    criminal_pre_1b_29
  ) %>% 
  mutate(
    criminal_nonviolent = case_when(
      !is.na(criminal_nonviolent) ~ criminal_nonviolent,
       is.na(criminal_nonviolent) ~ criminal_pre_1b_01
    ),
    criminal_violent = case_when(
      !is.na(criminal_violent) ~ criminal_violent,
       is.na(criminal_violent) ~ criminal_pre_1b_04
    ),
    criminal_noncontact = case_when(
      !is.na(criminal_noncontact) ~ criminal_noncontact,
       is.na(criminal_noncontact) ~ criminal_pre_1b_20
    ),
    criminal_contact = case_when(
      !is.na(criminal_contact) ~ criminal_contact,
       is.na(criminal_contact) ~ criminal_pre_1b_29
    )
  ) %>% 
  select(
    -criminal_pre_1b_01,
    -criminal_pre_1b_04,
    -criminal_pre_1b_20,
    -criminal_pre_1b_29
  )

pre2_data <- pi2 %>% 
  filter(timepoint == "pre_2") %>%
  filter(!(assigned_group == "waitlist" & time == 0)) %>% 
  select(
    id,
    assigned_group,
    time,
    starts_with("ssas"),
    -ssas_sumscore_baseline,
    -ssas_missing,
    -ssas_next,
    starts_with("schimra_a"),
    hbi_19_sumscore,
    phq_sumscore,
    starts_with("sbimsa"), 
    truthfulness_honest,
    completed
  )

post_data <- pi2 %>% 
  filter(timepoint == "post") %>% 
  select(id, time, valid_from, completed) %>% 
  mutate(
    post_complete = case_when(
      !is.na(completed) ~ 1,
       is.na(completed) ~ 0
    )
  ) %>% 
  select(-completed,
         end_of_treatment = valid_from) %>% 
  group_by(id) %>% 
  filter(time == max(time)) %>% 
  ungroup()

demo_data <- demographics %>% 
  select(
    id = `Study ID`,
    participant_age,
    participant_gender,
    participant_medication,
    education_level, education_level_ISCED,
    current_relationship_status,
    have_children, number_of_children,
    participant_therapy,
    suicidality,
    version_language
  )

baseline_data <- raads_data %>% 
  left_join(criminal_data, by = "id") %>% 
  left_join(pre2_data, by = "id") %>% 
  left_join(post_data, by = "id") %>% 
  left_join(demo_data, by = "id")

# Module engagement

## worksheet completion

worksheets_01$worksheet <- 1
worksheets_02$worksheet <- 2
worksheets_03$worksheet <- 3
worksheets_04$worksheet <- 4
worksheets_05$worksheet <- 5
worksheets_06$worksheet <- 6
worksheets_07$worksheet <- 7
worksheets_08$worksheet <- 8
worksheets_09$worksheet <- 9
worksheets_10$worksheet <- 10
worksheets_11$worksheet <- 11
worksheets_12$worksheet <- 12
worksheets_13$worksheet <- 13
worksheets_14$worksheet <- 14
worksheets_15$worksheet <- 15
worksheets_16$worksheet <- 16
worksheets_17$worksheet <- 17
worksheets_18$worksheet <- 18
worksheets_19$worksheet <- 19
worksheets_20$worksheet <- 20
worksheets_21$worksheet <- 21
worksheets_22$worksheet <- 22
worksheets_23$worksheet <- 23
worksheets_24$worksheet <- 24
worksheets_25$worksheet <- 25
worksheets_26$worksheet <- 26
worksheets_27$worksheet <- 27

worksheets <- bind_rows(
  worksheets_01,
  worksheets_02,
  worksheets_03,
  worksheets_04,
  worksheets_05,
  worksheets_06,
  worksheets_07,
  worksheets_08,
  worksheets_09,
  worksheets_10,
  worksheets_11,
  worksheets_12,
  worksheets_13,
  worksheets_14,
  worksheets_15,
  worksheets_16,
  worksheets_17,
  worksheets_18,
  worksheets_19,
  worksheets_20,
  worksheets_21,
  worksheets_22,
  worksheets_23,
  worksheets_24,
  worksheets_25,
  worksheets_26,
  worksheets_27
) %>% 
  select(
    id                  = `Study code`,
    worksheet,
    groups              = Groups,
    first_upload_module = Created,
    last_updated_module = `Last updated`  
  )

### Identify modules associated with each worksheet

worksheets <- worksheets %>% 
  mutate(
    module = case_when(
      worksheet %in% c(1, 2)               ~ 1,
      worksheet %in% c(3, 4, 5, 6)         ~ 2,
      worksheet %in% c(7, 8, 9, 10, 11)    ~ 3,
      worksheet %in% c(12)                 ~ 4,
      worksheet %in% c(13, 14)             ~ 5,
      worksheet %in% c(15, 16)             ~ 6,
      worksheet %in% c(17, 18, 19, 20, 21) ~ 7,
      worksheet %in% c(22, 23, 24, 25)     ~ 8,
      worksheet %in% c(26, 27)             ~ 9
    )
  )

### Get first upload for each module and updates

worksheets <- worksheets %>% 
  left_join(
    select(post_data, id, end_of_treatment),
    by = "id"
  )

worksheets_latest <- worksheets %>% 
  group_by(id, module) %>% 
  filter(first_upload_module <= end_of_treatment) %>%
  summarise(
    upload_module        = max(first_upload_module, na.rm = TRUE),
    first_updated_module = min(last_updated_module, na.rm = TRUE),
    last_updated_module  = max(last_updated_module, na.rm = TRUE)
  )

## Module reading

reading <- reading %>% 
  left_join(select(post_data,
                   id,
                   end_of_treatment),
            by = c("Study_Code" = "id")) %>% 
  filter(Time <= end_of_treatment)

assigned <- reading %>% 
  filter(Type == "MODULE_ASSIGN" | Type == "MODULE_ASSIGN_AUTO")

reading <- reading %>% 
  filter(Type == "MODULE_READ")

reading <- reading %>% 
  select(
    id          = Study_Code,
    module      = Module_No,
    read_module = Time
  )

reading_latest <- reading %>% 
  group_by(id, module) %>% 
  summarise(
    first_read_module = min(read_module),
    last_read_module  = max(read_module)
  )

## Module assignment

assigned <- assigned %>% 
  select(
    id = Study_Code,
    module = Module_No,
    assigned_date = Time
  )

assigned_latest <- assigned %>% 
  group_by(id) %>% 
  filter(assigned_date == max(assigned_date)) %>% 
  select(
    id,
    module_last_assigned = module,
    date_module_assigned = assigned_date
  ) %>% 
  filter(module_last_assigned == max(module_last_assigned)) %>% 
  ungroup()

assigned_latest <- unique(assigned_latest)

## Joined data

latest <-  reading_latest %>% 
  left_join(worksheets_latest, by = c("id", "module"))

last_recorded <- latest %>% 
  group_by(id) %>% 
  filter(!is.na(upload_module)) %>% 
  filter(module == max(module)) %>% 
  ungroup() %>% 
  select(
    id,
    module_last_uploaded = module,
    date_first_uploaded  = upload_module,
    -first_read_module, -last_read_module, -first_updated_module, -last_updated_module
  )

last_read <- latest %>% 
  group_by(id) %>% 
  filter(!is.na(last_read_module)) %>% 
  filter(module == max(module)) %>% 
  ungroup() %>% 
  select(
    id,
    module_last_read = module,
    date_first_read  = first_read_module,
    -last_read_module, -first_updated_module, -last_updated_module
  )

dropout_data <- baseline_data %>% 
  left_join(last_recorded, by = "id") %>% 
  left_join(last_read, by = "id") %>% 
  left_join(assigned_latest, by = "id")

# Last Touches 

dropout_data <- dropout_data %>% 
  rename(
    week_start = time.x,
    week_end   = time.y
  )

# Export data ------------------------------------------------------------------

write_csv(dropout_data, "data/prevent-it-2_dropout-data.csv")

write_csv(ssas_data, "data/prevent-it-2_ssas-data.csv")

