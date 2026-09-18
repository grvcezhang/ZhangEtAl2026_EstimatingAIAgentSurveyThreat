# Mass AI detection in surveys across platforms - Full (private OSF)
# Studies: Dec 2025
# last edited: Aug 22, 2025
# Author: Grace Zhang

library(tidyverse)
library(data.table)
library(rlang)
library(scales)
library(patchwork)
library(forcats)
library(lubridate)
library(rstudioapi)
library(MASS)
library(mediation)

# set working directory here to access files
setwd()

#### load datasets (*check note) ####
# *Note: the data has been cleaned in the following ways:
# - removed IP address, location, and participant IDs
# - removed the code calculating specific criteria for typing metrics
### - typing metrics already within initial csv files
# - cint theorem csv file has two renamed columns as not to confuse with survey collected data
### - we collected data through the survey in columns "age" and "gender"
### - cint recorded age and gender in columns "cint_age" and "cint_gender," renamed from a second version of "age" and "gender"

# load Mindworks (in-person lab data, both versions)
mindworks_ver1 <- read_csv("Zhangetal_AIDetection_HumanInPerson_Mindworks_Ver1_Public.csv") %>% 
  arrange(StartDate)
mindworks_ver2 <- read_csv("Zhangetal_AIDetection_HumanInPerson_Mindworks_Ver2_Public.csv") %>% 
  arrange(StartDate)
# pool data together
mindworks <- bind_rows(mindworks_ver1, mindworks_ver2) %>% 
  mutate(source = "Human (In-Person)",
         platform = "Human (In-Person)")

# load raw data from online platforms
prolific <- read_csv("Zhangetal_AIDetection_Online_Prolific_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")
qualtrics <- read_csv("Zhangetal_AIDetection_Online_Qualtrics_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")
crconnect <- read_csv("Zhangetal_AIDetection_Online_CloudResearch_Connect_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")
crmturk <- read_csv("Zhangetal_AIDetection_Online_CloudResearch_MTurk_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")
clickworker <- read_csv("Zhangetal_AIDetection_Online_Clickworker_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")
cint <- read_csv("Zhangetal_AIDetection_Online_CintTheorem_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")
verasight <- read_csv("Zhangetal_AIDetection_Online_Verasight_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "Online")

# load data collected through AI agents
AIatlas <- read_csv("Zhangetal_AIDetection_AIAgent_Atlas_GeneralPrompt_Public.csv") %>% 
  arrange(StartDate) %>% 
  # remove rows due to experimenter error in procedure
  filter(!str_detect(openend, "HUMAN: I just guessed. I dont know anything about orange juice nutrition")) %>% 
  mutate(source = "AI Agent (General Prompt)",
         prompt = ifelse(prompt == 1, "Autonomous", "Ask Help"))
AIcomet <- read_csv("Zhangetal_AIDetection_AIAgent_Comet_GeneralPrompt_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Agent (General Prompt)",
         prompt = ifelse(prompt == 1, "Autonomous", "Ask Help"))
AImanus <- read_csv("Zhangetal_AIDetection_AIAgent_Manus_GeneralPrompt_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Agent (General Prompt)",
         platform = "Manus",
         prompt = ifelse(prompt == 1, "Autonomous", "Ask Help"))

AIsuper_atlasauto <- read_csv("Zhangetal_AIDetection_AIAgent_Atlas_Enhanced_Autonomous_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Super Prompt",
         platform = "Westwood Super Atlas Autonomous",
         prompt = "Autonomous")
AIsuper_atlashelp <- read_csv("Zhangetal_AIDetection_AIAgent_Atlas_Enhanced_HumanAssisted_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Super Prompt",
         platform = "Westwood Super Atlas Human Assisted",
         prompt = "Ask Help")
AIsuper_cometauto <- read_csv("Zhangetal_AIDetection_AIAgent_Comet_Enhanced_Autonomous_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Super Prompt",
         platform = "Westwood Super Comet Autonomous",
         prompt = "Autonomous")
AIsuper_comethelp <- read_csv("Zhangetal_AIDetection_AIAgent_Comet_Enhanced_HumanAssisted_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Super Prompt",
         platform = "Westwood Super Comet Human Assisted",
         prompt = "Ask Help")
AIsuper_manusauto <- read_csv("Zhangetal_AIDetection_AIAgent_Manus_Enhanced_Autonomous_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Super Prompt",
         platform = "Westwood Super Manus Autonomous",
         prompt = "Autonomous")
AIsuper_manushelp <- read_csv("Zhangetal_AIDetection_AIAgent_Manus_Enhanced_HumanAssisted_Public.csv") %>% 
  arrange(StartDate) %>% 
  mutate(source = "AI Super Prompt",
         platform = "Westwood Super Manus Human Assisted",
         prompt = "Ask Help")
#### end ####

#### filter completes and incompletes ####
# save incomplete responses
inc_mindworks <- mindworks %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_prolific <- prolific %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_crconnect <- crconnect %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_crmturk <- crmturk %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_qualtrics <- qualtrics %>% 
  filter(DistributionChannel == "onlinePanel") %>% 
  filter(Finished != 1)

inc_clickworker <- clickworker %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_cint <- cint %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_verasight <- verasight %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIatlas <- AIatlas %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIcomet <- AIcomet %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AImanus <- AImanus %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIsuper_atlasauto <- AIsuper_atlasauto %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIsuper_atlashelp <- AIsuper_atlashelp %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIsuper_cometauto <- AIsuper_cometauto %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIsuper_comethelp <- AIsuper_comethelp %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIsuper_manusauto <- AIsuper_manusauto %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

inc_AIsuper_manushelp <- AIsuper_manushelp %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished != 1)

# clear out RA tests, previews, and incompletes
mindworks <- mindworks %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(is.na(ra_test)) %>% 
  filter(!str_detect(openend, "test"))

prolific <- prolific %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

crconnect <- crconnect %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

crmturk <- crmturk %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

qualtrics <- qualtrics %>% 
  filter(DistributionChannel == "onlinePanel") %>% 
  filter(Finished == 1)

clickworker <- clickworker %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

cint <- cint %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

verasight <- verasight %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

AIatlas <- AIatlas %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

AIcomet <- AIcomet %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

AImanus <- AImanus %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1) %>% 
  filter(!str_detect(openend, "test"))

AIsuper_atlasauto <- AIsuper_atlasauto %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1)

AIsuper_atlashelp <- AIsuper_atlashelp %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1)

AIsuper_cometauto <- AIsuper_cometauto %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1)

AIsuper_comethelp <- AIsuper_comethelp %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1)

AIsuper_manusauto <- AIsuper_manusauto %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1)

AIsuper_manushelp <- AIsuper_manushelp %>% 
  filter(DistributionChannel == "anonymous") %>% 
  filter(Finished == 1)
#### end ####

#### rename variables ####
# some variables have extra spaces, or have an extra tag at the end
mindworks <- mindworks %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1,
  )

prolific <- prolific %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1,
  )

crconnect <- crconnect %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

crmturk <- crmturk %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

qualtrics <- qualtrics %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

clickworker <- clickworker %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

cint <- cint %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

verasight <- verasight %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIatlas <- AIatlas %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIcomet <- AIcomet %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AImanus <- AImanus %>% 
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    `metainfo_Browser` = `metainfo _Browser`,
    `metainfo_Version` = `metainfo _Version`,
    `metainfo_Operating Syst` = `metainfo _Operating System`,
    `metainfo_Resolution` = `metainfo _Resolution`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIsuper_atlasauto <- AIsuper_atlasauto %>%
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIsuper_atlashelp <- AIsuper_atlashelp %>%
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIsuper_cometauto <- AIsuper_cometauto %>%
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIsuper_comethelp <- AIsuper_comethelp %>%
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIsuper_manusauto <- AIsuper_manusauto %>%
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )

AIsuper_manushelp <- AIsuper_manushelp %>%
  rename(
    `time_scale2_Click Count` = `time_scale2 _Click Count`,
    `time_scale2_First Click` = `time_scale2 _First Click`,
    `time_scale2_Last Click` = `time_scale2 _Last Click`,
    `time_scale2_Page Submit` = `time_scale2 _Page Submit`,
    duration = `Duration (in seconds)`,
    ratesurvey = ratesurvey_1,
    ethicalAI = ethicalAI_1
  )
#### end ####

#### convert variables into numerics ####
mindworks <- mindworks %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

prolific <- prolific %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

qualtrics <- qualtrics %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        starts_with("pureSpectrum"),
        Q_TerminateFlag,
        Q_BallotBoxStuffing,
        starts_with("_os_api_vendor"),
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

crconnect <- crconnect %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

crmturk <- crmturk %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

clickworker <- clickworker %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

cint <- cint %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

verasight <- verasight %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        surveyid,
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIatlas <- AIatlas %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIcomet <- AIcomet %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AImanus <- AImanus %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIsuper_atlasauto <- AIsuper_atlasauto %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>%
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIsuper_atlashelp <- AIsuper_atlashelp %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>%
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIsuper_cometauto <- AIsuper_cometauto %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>%
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIsuper_comethelp <- AIsuper_comethelp %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>%
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIsuper_manusauto <- AIsuper_manusauto %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>%
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))

AIsuper_manushelp <- AIsuper_manushelp %>%
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>%
  mutate(
    across(
      -c(
        ends_with("date"),
        ends_with("Id"),
        ends_with("ID"),
        ends_with("id"),
        UserLanguage,
        starts_with("metainfo"),
        brandpref,
        openend,
        vidanswer,
        animal,
        gender,
        browser_0_TEXT,
        usedAI_4_TEXT,
        describeAI,
        comments,
        platform,
        source,
        prompt,
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))
#### end ####

#### bind all rows from all platforms, alldata dataset ####
alldata <- bind_rows(
  mindworks,
  prolific,
  crconnect,
  crmturk,
  qualtrics,
  clickworker,
  cint,
  verasight,
  AIatlas,
  AIcomet,
  AImanus,
  AIsuper_atlasauto,
  AIsuper_cometauto,
  AIsuper_manusauto,
  AIsuper_atlashelp,
  AIsuper_comethelp,
  AIsuper_manushelp
) %>% 
  mutate(
    # create response index
    index = row_number()
  ) %>% 
  relocate(platform, .before = 1) %>% 
  relocate(source, .before = 1) %>% 
  relocate(index, .before = 1)

platform_order <- c(
  "Human (In-Person)",
  "Prolific",
  "CR Connect",
  "Verasight",
  "Clickworker",
  "Cint Theorem",
  "Qualtrics",
  "CR MTurk",
  "Atlas",
  "Comet",
  "Manus",
  "Westwood Super Atlas Autonomous",
  "Westwood Super Comet Autonomous",
  "Westwood Super Manus Autonomous",
  "Westwood Super Atlas Human Assisted",
  "Westwood Super Comet Human Assisted",
  "Westwood Super Manus Human Assisted"
)
source_order <- c(
  "Human (In-Person)",
  "Online",
  "AI Agent (General Prompt)",
  "AI Super Prompt"
)

alldata <- alldata %>%
  mutate(platform = factor(platform, levels = platform_order),
         source = factor(source, levels = source_order))
#### end ####

#### create AI tests ####
# create dummy codes for failing certain checks
alldata <- alldata %>% 
  mutate(
    # reCAPTCHA
    fail_captcha = as.numeric(Q_RecaptchaScore <= 0.5),
    
    # visual processing
    fail_math = as.numeric(!is.na(mathproblem) & mathproblem != 5),
    
    # scale reversal attention check
    fail_scaleattn = as.numeric(scale2 < 0),
    
    # trolling/impossible question
    fail_trolling = as.numeric(impossible == 1),
    # fill in 0 for those who did not see the question
    fail_trolling = ifelse(is.na(impossible), 0, fail_trolling),
    
    # slider use detection
    fail_sliderclicks = as.numeric(`time_ratebrand_Click Count` == 0),
    
    # prompt injection (choice): brand 15 is fake brand, Golden Grove
    fail_fakebrand = as.numeric(str_detect(brandpref, "15")),
    
    # typing metrics: a portion of the typing metrics flag
    fail_keystrokecountOJ = as.numeric(StrokeCount_OJ < 10),
    fail_nostrokeintvlsOJ = as.numeric(is.na(StrokeIntervals_OJ)),
    
    # prompt injection (text): health rocks, accounting for uppercase
    fail_openendhealth = as.numeric(str_detect(openend, "ealth rocks")),
    
    # Concealing an non-human identity: fail if anything but "no" was selected (No = 0)
    fail_usedAI = as.numeric(usedAI != 0),
    
    # Multi-part Reverse Shibboleth: fail if all four are in the correct range
    fail_OJnutrition = as.numeric(
      between(estimation_1, 105, 120) &
        between(estimation_2, 23, 29) &
        between(estimation_3, 0, 1) &
        between(estimation_4, 1, 3)
    ),
    
    # converting elephant and orange guesses to lowercase in order to create checks
    # visual recall
    animal_lowercase = str_to_lower(animal),
    # audio recall
    orange_lowercase = str_to_lower(vidanswer),
    
    # describeAI is a question that only shows up if 1 or 2 was chosen in usedAI. not used in analysis
    fail_keystrokecountAI = as.numeric(!is.na(describeAI) & StrokeCount_AI < 10),
    fail_openendpineapple = as.numeric(!is.na(describeAI) & str_detect(describeAI, "pineapple")),
    # not used in analyses
    fail_seenstudy = as.numeric(seenstudy == 1)
  )

table(alldata$animal_lowercase)
# accounting for misspellings in animal (elephant) recall question
elephant_words <- c(
  "elephant", "elepheant", "ellephant", "elaphant", "elevant", "elepant", "elephand", "ellaphant", "elefantes",
  "elephantt", "elephent")
# other animals that were guessed
animal_words <- c(
  "bear", "beat", "teddy", "mouse", "cat", "cow", "ox", "dog", "puppy", "giraffe", "gariffe", "koala", "kolha", "kuwala", "kaola", 
  "rabbit", "bunny", "monkey", "panda", "pig", "zebra", "lion", "owl", "sheep", "lemur", "sloth", "deer", "goat", "sheep",
  "raccoon", "racoon", "platapus", "wasp", "hornet", "bee", "horse", "bird", "squirrel", "kangaroo", "beaver", 
  "donkey", "puppy", "whinny the pooh", "lion", "bird", "duck")

table(alldata$orange_lowercase)
# accounting for misspellings and relevant answers in orange (hamlin) recall question
hamlin_words <- c(
  "hamlin", "hadler", "hellman", "hanlon", "hanlin", "handlin", "hamrin", "hamlon",
  "hamline", "hamlet", "hamlen", "hamilton", "hammer", "nilan", "hammlet", "hsmlet",
  "hqmlin", "havlin", "havland", "haverin", "happly", "hannling", "hanlen", "hanlan", 
  "handlen", "handlelin", "handland", "hamylan", "hammelen", "hamlett", "hamlan", 
  "hamla", "hamilin", "hadlin", "amla", "himalayan", "havliand", "harmlin", "harmen", 
  "harlingen", "handling", "hammond", "hamlam", "hamilon", "hameln", "hamarine", 
  "hannline", "handler", "hamlot", "amylin", "havilin", "hemlin", "havelan", "hamley", 
  "hambline", "hamin", "havvelon", "handalegen", "hamond", "hamblin", "hanlem", "hamlem", 
  "hadlen", "hamntlen", "hammelt", "hamland", "hamelit", "halmet", "hamel",
  "it starts with an h", "lhyma", "hammrock", "hamelin", "hamiln", "helmish",
  "harmon", "halyin", "comlon", "hamalan", "hambling", "hamelet", "hamlend", "hemaron") 
# words that were in the video, but not directly the correct answer
related_words <- c("juicing", "juice", "juicy", "jui y", "jucy", "resilient", "resilding", "resilian", "reslient")

# create failed vars
alldata <- alldata %>% 
  mutate(
    # fails to recall elephants
    fail_elephant = as.numeric(!coalesce(str_detect(
      animal_lowercase,
      paste0("\\b(", paste(elephant_words, collapse = "|"), ")\\b")
    ), TRUE)),
    
    # fail if not ELEPHANT and more than 10 words (likely AI)
    fail_elephant_under10words = as.numeric(
      fail_elephant == 1 & str_count(animal_lowercase, "\\S+") > 10
    ),
    
    # pass if people guess any animal, including elephants, and is under 10 words
    fail_elephant_guessanimal = case_when(
      fail_elephant == 0 ~ 0,
      
      # pass if elephant wrong, but guessed animal, and answer < 10 characters
      fail_elephant == 1 &
        str_detect(
          dplyr::coalesce(animal_lowercase, ""),
          paste0("\\b(", paste(animal_words, collapse = "|"), ")\\b")
        ) & fail_elephant_under10words == 0 ~ 0,
      
      # fail if elephant wrong, guessed animal, but answer >= 10 characters
      fail_elephant == 1 &
        str_detect(
          dplyr::coalesce(animal_lowercase, ""),
          paste0("\\b(", paste(animal_words, collapse = "|"), ")\\b")
        ) & fail_elephant_under10words == 1 ~ 1,
      
      # fail if elephant wrong and did not guessed animal
      fail_elephant == 1 &
        !str_detect(
          dplyr::coalesce(animal_lowercase, ""),
          paste0("\\b(", paste(animal_words, collapse = "|"), ")\\b")
        ) ~ 1,
      
      TRUE ~ NA_real_
    ),
    
    fail_hamlin = as.numeric(!coalesce(str_detect(
      orange_lowercase,
      paste0("\\b(", paste(hamlin_words, collapse = "|"), ")\\b")
    ), TRUE)),
    
    # fail if people don't guess any related words from the video, including the correct answer
    fail_hamlin_guessrelevant = case_when(
      # pass if guessed right answer
      fail_hamlin == 0 ~ 0,
      # pass if guessed wrong answer, but guessed relevant answer
      fail_hamlin == 1 &
        str_detect(
          dplyr::coalesce(orange_lowercase, ""),
          paste0("\\b(", paste(related_words, collapse = "|"), ")\\b")
        ) ~ 0,
      # fail if failed to guess relevant answer
      fail_hamlin == 1 &
        !str_detect(
          dplyr::coalesce(orange_lowercase, ""),
          paste0("\\b(", paste(related_words, collapse = "|"), ")\\b")
        ) ~ 1,
      
      TRUE ~ NA_real_
    ),
    
    # fail if not relevant and more than 10 words (likely AI)
    fail_hamlin_under10words = as.numeric(
      fail_hamlin_guessrelevant == 1 & str_count(orange_lowercase, "\\S+") > 10
    )
  )
#### end ####

#### fail flags: Mindworks (Humans) <2% and effective tests ####
# failed at least one (clear) AI check
failedAI_df <- alldata %>% 
  dplyr::select(-c(fail_scaleattn,
                   fail_seenstudy)) %>% 
  mutate(failedanyAI = as.numeric(rowSums(across(starts_with("fail_")), na.rm = TRUE) > 0))
alldata <- alldata %>% 
  left_join(failedAI_df %>% dplyr::select(index, failedanyAI), by = "index")

# examine fail rates for each of the checks at Mindworks, arrange in order of lowest fail rate
failedAI_df %>% 
  filter(platform == "Human (In-Person)") %>% 
  summarise(
    across(starts_with("fail_"), \(x) mean(x, na.rm = TRUE))
  ) %>% 
  t() %>% 
  view()
# disregard optional questions, such as openendpineapple, under10words, keystrokesAI

# create a new failed typing metric. fail if any of the typing metrics failed
alldata <- alldata %>% 
  mutate(
    fail_typing = as.numeric(
      if_any(
        c(
          failtype_keystrokecountOJ,
          failtype_nostrokeintvlsOJ,
          failtype_keyspastesOJ
        ),
        ~ coalesce(.x, 0) == 1)))

# examine fail rates for each of the checks at Mindworks, arrange in order of lowest fail rate
failedAI_df %>% 
  filter(platform == "Human (In-Person)") %>% 
  summarise(
    across(starts_with("fail_"), \(x) mean(x, na.rm = TRUE))
  ) %>% 
  t() %>% 
  view()
# disregard optional questions, such as openendpineapple, under10words, keystrokesAI

# failed any Mindworks 0% and <2% check
alldata <- alldata %>% 
  mutate(
    failedanyMW0 = as.numeric(
      if_any(
        c(
          fail_captcha,
          fail_trolling,
          fail_sliderclicks,
          fail_typing,
          failtype_keystrokecountOJ,
          failtype_nostrokeintvlsOJ,
          failtype_keyspastesOJ
        ),
        ~ coalesce(.x, 0) == 1)),
    failedanyMW2 = as.numeric(
      if_any(
        c(
          fail_captcha,
          fail_trolling,
          fail_sliderclicks,
          fail_typing,
          failtype_keystrokecountOJ,
          failtype_nostrokeintvlsOJ,
          failtype_keyspastesOJ,
          fail_math,
          fail_openendhealth,
          fail_fakebrand,
          fail_usedAI,
          fail_OJnutrition
        ),
        ~ coalesce(.x, 0) == 1)),
    failedeffective = as.numeric(
      if_any(
        c(
          fail_typing,
          failtype_keystrokecountOJ,
          failtype_nostrokeintvlsOJ,
          failtype_keyspastesOJ,
          fail_OJnutrition,
          fail_fakebrand,
          fail_openendhealth,
          fail_captcha
        ),
        ~ coalesce(.x, 0) == 1)))

alldata <- alldata %>% 
  mutate(
    # no. failed all 12 checks
    n_failedall = rowSums(across(c(
      fail_typing, # includes 7 subchecks/flags
      fail_OJnutrition,
      fail_fakebrand, 
      fail_openendhealth, 
      fail_captcha,
      fail_sliderclicks,
      fail_math,
      fail_trolling, 
      fail_usedAI, 
      fail_elephant,
      fail_hamlin,
      fail_scaleattn))),
    # no. failed 5 effective checks
    n_failedeffective = rowSums(across(c(
      fail_typing, # includes 7 subchecks/flags
      fail_OJnutrition,
      fail_fakebrand, 
      fail_openendhealth, 
      fail_captcha))),
    # no. failed checks with <2% fail rate at mindworks (top 9)
    n_failedanyMW2 = rowSums(across(c(
      fail_typing, # includes 7 subchecks/flags
      fail_OJnutrition,
      fail_fakebrand, 
      fail_openendhealth, 
      fail_captcha,
      fail_sliderclicks,
      fail_math,
      fail_trolling, 
      fail_usedAI))))
#### end ####

#### clean alldata ####
# get names of columns that are just NA
alldata %>%
  summarise(
    across(everything(), ~ all(is.na(.x)))
  ) %>%
  pivot_longer(
    everything(),
    names_to  = "column",
    values_to = "all_na"
  ) %>%
  filter(all_na) %>%
  pull(column)

# remove unnecessary columns
alldata <- alldata %>% 
  dplyr::select(-c(Status,
                   Progress,
                   consent,
                   ra_test,
                   Q_RelevantIDDuplicate,
                   Q_RelevantIDLastStartDate,
                   Q_BallotBoxStuffing,
                   state
  ))

# rearrange order of fail criteria to the start of the DF, in order of effectiveness
alldata <- alldata %>% 
  dplyr::select(index, source, platform, prompt, ResponseId, StartDate, EndDate, RecordedDate, duration, mobiledevice,
                starts_with("n_failed"), failedanyAI, failedanyMW0, failedanyMW2, failedeffective,
                fail_typing, starts_with("failtype"), flag_More_char_than_strokes_OJ,
                fail_OJnutrition,
                fail_fakebrand,
                fail_openendhealth,
                fail_captcha,
                fail_sliderclicks,
                fail_math,
                fail_trolling,
                fail_usedAI,
                fail_elephant, fail_elephant_guessanimal, fail_elephant_under10words,
                fail_hamlin, fail_hamlin_guessrelevant, fail_hamlin_under10words,
                fail_scaleattn,
                fail_openendpineapple,
                fail_seenstudy,
                everything())

alldata <- alldata %>%
  mutate(platform = factor(platform, levels = platform_order),
         source = factor(source, levels = source_order))
#### end ####

#### separate the platforms ####
humandata <- alldata %>% 
  filter(platform == "Human (In-Person)")

prolific <- alldata %>% 
  filter(platform == "Prolific")
crconnect <- alldata %>% 
  filter(platform == "CR Connect")
crmturk <- alldata %>% 
  filter(platform == "CR MTurk")
qualtrics <- alldata %>% 
  filter(platform == "Qualtrics")
clickworker <- alldata %>% 
  filter(platform == "Clickworker")
cint <- alldata %>% 
  filter(platform == "Cint Theorem")
verasight <- alldata %>% 
  filter(platform == "Verasight")

AIatlas <- alldata %>% 
  filter(platform == "Atlas")
AIcomet <- alldata %>% 
  filter(platform == "Comet")
AImanus <- alldata %>% 
  filter(platform == "Manus")

# super = enhanced prompt
AIsuper_atlasauto <- alldata %>%
  filter(platform == "Westwood Super Atlas Autonomous")
AIsuper_atlashelp <- alldata %>%
  filter(platform == "Westwood Super Atlas Human Assisted")
AIsuper_cometauto <- alldata %>%
  filter(platform == "Westwood Super Comet Autonomous")
AIsuper_comethelp <- alldata %>%
  filter(platform == "Westwood Super Comet Human Assisted")
AIsuper_manusauto <- alldata %>%
  filter(platform == "Westwood Super Manus Autonomous")
AIsuper_manushelp <- alldata %>%
  filter(platform == "Westwood Super Manus Human Assisted")

# online and AI subsets
onlinedata <- alldata %>% 
  filter(source == "Online")

AIdata <- alldata %>% 
  filter(source == "AI Agent (General Prompt)")

AIsupers <- alldata %>% 
  filter(source == "AI Super Prompt")

AIall <- alldata %>% 
  filter(source %in% c("AI Agent (General Prompt)", "AI Super Prompt"))
#### end ####

write_csv(alldata, "AI_detection_all.csv")

# ------ ANALYSES ------

#### function to get mode ####
get_mode <- function(v) {
  if (length(v) == 0) return(NA)  # Handle empty input
  
  # Remove NA values
  v <- na.omit(v)
  
  if (length(v) == 0) return(NA)  # All values were NA
  
  # Count occurrences of each unique value
  tbl <- table(v)
  
  # Return the value(s) with the highest frequency
  modes <- names(tbl)[tbl == max(tbl)]
  
  # Convert to numeric if possible
  if (all(!is.na(as.numeric(modes)))) {
    return(as.numeric(modes))
  }
  return(modes)
}
#### end ####

# ------ Overall Statistics ------

#### fail rates for overall and each check ####
# across each platform — overall checks
failoverall_platform <- alldata %>% 
  group_by(platform, source) %>% 
  summarize(
    failedeffective = mean(failedeffective),
    failedanyMW2 = mean(failedanyMW2),
    failedanyMW0 = mean(failedanyMW0),
    failedanyAI = mean(failedanyAI),
    failedcaptcha = mean(fail_captcha)
  )

# across each platform — individual checks
failchecks_platform <- alldata %>% 
  group_by(platform, source) %>% 
  summarize(across(starts_with("fail_"), \(x) mean(x, na.rm = TRUE)))

# across each source — overall checks
failoverall_source <- alldata %>% 
  group_by(source) %>% 
  summarize(
    failedeffective = mean(failedeffective),
    failedanyMW2 = mean(failedanyMW2),
    failedanyMW0 = mean(failedanyMW0),
    failedanyAI = mean(failedanyAI),
    failedcaptcha = mean(fail_captcha)
  )

# across each source — lower and upper bounds for overall checks
failoverall_minmax_source <- failoverall_platform %>% 
  group_by(source) %>% 
  summarize(
    failedeffective_min = min(failedeffective),
    failedeffective_max = max(failedeffective),
    failedanyMW2_min = min(failedanyMW2),
    failedanyMW2_max = max(failedanyMW2),
    failedanyMW0_min = min(failedanyMW0),
    failedanyMW0_max = max(failedanyMW0)
  )

# across each source — lower and upper bounds for individual checks
failchecks_minmax_source <- failchecks_platform %>%
  filter(source != "Human (In-Person)") %>% 
  group_by(source) %>%
  summarize(
    across(
      starts_with("fail_"),
      list(
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"))

# across each source — individual checks
failchecks_source <- alldata %>% 
  group_by(source) %>% 
  summarize(across(starts_with("fail_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    source = factor(source, levels = c("Human (In-Person)", "Online", "AI Agent (General Prompt)", "AI Super Prompt"))
  ) %>% 
  pivot_longer(
    cols = -source,
    names_to = "fail_check",
    values_to = "rate"
  ) %>%
  pivot_wider(
    names_from = source,
    values_from = rate
  )
#### end ####

#### dot plots/graphs for fail rates ####
# across sources, in order of effectiveness
# fail checks to include (controls order)
fail_cols <- c(
  "fail_typing", "fail_OJnutrition", "fail_fakebrand", "fail_openendhealth", "fail_captcha",
  "fail_sliderclicks", "fail_math", "fail_trolling", "fail_usedAI",
  "fail_elephant", "fail_hamlin", "fail_scaleattn"
)

# rename mapping (names must match after removing "fail_")
fail_labels <- c(
  typing        = "Typing metrics",
  OJnutrition   = "Multi-part reverse shibboleth",
  fakebrand     = "Prompt injection (choice)",
  openendhealth = "Prompt injection (text)",
  captcha       = "reCAPTCHA",
  sliderclicks  = "Slider usage detection",
  math          = "Visual processing",
  trolling      = "Trolling",
  usedAI        = "Concealing nonhuman identity",
  elephant      = "Visual recall",
  hamlin        = "Audio recall",
  scaleattn     = "Scale reversal attention check"
)

# build plotting dataset (no check_type plumbing)
failchecks_source_plot <- alldata %>%
  group_by(source) %>%
  summarize(across(starts_with("fail_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = -source, names_to = "fail_check", values_to = "rate") %>%
  filter(fail_check %in% fail_cols, source != "AI Super Prompt") %>%
  mutate(
    fail_key   = str_remove(fail_check, "^fail_"),
    fail_key   = factor(fail_key, levels = str_remove(fail_cols, "^fail_")),
    fail_label = factor(
      unname(fail_labels[as.character(fail_key)]),
      levels = unname(fail_labels[str_remove(fail_cols, "^fail_")])
    )
  ) %>%
  dplyr::select(source, fail_label, rate) %>%
  pivot_wider(names_from = source, values_from = rate) %>%
  pivot_longer(
    cols = c(`Human (In-Person)`, Online, `AI Agent (General Prompt)`),
    names_to = "source",
    values_to = "fail_rate"
  )

# Desired TOP -> BOTTOM check order
checks <- c(
  "Typing metrics",
  "Multi-part reverse shibboleth",
  "Prompt injection (choice)",
  "Prompt injection (text)",
  "reCAPTCHA",
  "Slider usage detection",
  "Visual processing",
  "Trolling",
  "Concealing nonhuman identity",
  "Visual recall",
  "Audio recall",
  "Scale reversal attention check"
)

# y-axis layout with gaps + headers
y_axis <- c(
  "__gap0__",
  "__hdr_effective__",
  checks[1:5],
  "__gap1__",
  "__hdr_pass__",
  checks[6:9],
  "__gap2__",
  "__hdr_fail__",
  checks[10:12]
)

axis_map <- tibble(
  ycat = y_axis,
  ypos = rev(seq_along(y_axis))
)

axis_labels <- setNames(y_axis, y_axis)
axis_labels[c(
  "__gap0__", "__gap1__", "__gap2__", "__gap3__",
  "__hdr_effective__", "__hdr_pass__", "__hdr_fail__"
)] <- ""

# Main plotting data
plot_df <- failchecks_source_plot %>%
  mutate(
    fail_label = trimws(as.character(fail_label)),
    source     = trimws(as.character(source)),
    fail_pct   = fail_rate * 100
  ) %>%
  filter(fail_label %in% checks) %>%
  mutate(ycat = fail_label) %>%
  left_join(axis_map, by = "ycat") %>% 
  mutate(source = ifelse(source == "Human", "Human (In-Person)", source),
         source = factor(
           source,
           levels = c("Human (In-Person)", "Online", "AI Agent (General Prompt)")
         ))

# Section headers
hdr_df <- tibble(
  ycat = c("__hdr_effective__", "__hdr_pass__", "__hdr_fail__"),
  label = c(
    "Effective tests (humans pass and AI fails)",
    "Ineffective tests (humans and AI pass)",
    "Ineffective tests (humans fail)"
  )
) %>%
  left_join(axis_map, by = "ycat")

# Break lines at gaps
break_df <- tibble(
  ycat = c("__gap1__", "__gap2__")
) %>%
  left_join(axis_map, by = "ycat")

x_max <- max(plot_df$fail_pct, na.rm = TRUE)

# Colors (used for both outline + fill base)
src_cols <- c(
  `Human (In-Person)` = "black",
  Online = "blue3",
  `AI Agent (General Prompt)` = "brown2"
)

src_labs <- c(
  `Human (In-Person)`           = "Human\n(In-Person)",
  Online                       = "Online",
  `AI Agent (General Prompt)`  = "AI Agent\n(General Prompt)"
)

ggplot(plot_df, aes(x = fail_pct, y = ypos)) +
  geom_point(
    aes(
      color = source,
      fill  = after_scale(alpha(color, 0.45))
    ),
    shape = 21,
    size = 4.5,
    stroke = 1.2,
    position = position_dodge2(width = 0.75, preserve = "single")
  ) +
  geom_text(
    data = hdr_df,
    aes(x = -Inf, y = ypos, label = label),
    inherit.aes = FALSE,
    hjust = -0.02,
    fontface = "bold",
    size = 4.8
  ) +
  geom_hline(
    data = break_df,
    aes(yintercept = ypos),
    color = "grey75",
    linewidth = 0.6
  ) +
  scale_color_manual(values = src_cols, labels = src_labs) +
  scale_x_continuous(
    breaks = pretty_breaks(6),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(xlim = c(0, x_max * 1.05), clip = "off") +
  scale_y_continuous(
    breaks = axis_map$ypos,
    labels = axis_labels[axis_map$ycat]
  ) +
  labs(
    title = "Average fail rate by check and source",
    x = "Fail rate (%)",
    y = NULL,
    color = "Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    
    # BIGGER AXIS LABELS (titles)
    axis.title.x = element_text(size = 15, margin = margin(t = 12)),
    
    # BIGGER AXIS TICK LABELS
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13, angle = 0),
    
    plot.margin = margin(15, 30, 15, 150),
    panel.grid.minor.y = element_blank()
  )

# ———— across online platforms vs. humans ————
failchecks_online_plot <- failchecks_platform %>%
  filter(source %in% c("Human (In-Person)", "Online")) %>% 
  group_by(platform) %>%
  summarise(across(starts_with("fail_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = starts_with("fail_"),
               names_to = "fail_check",
               values_to = "fail_rate") %>%
  filter(fail_check %in% fail_cols) %>%
  mutate(
    fail_key   = str_remove(fail_check, "^fail_"),
    fail_key   = factor(fail_key, levels = str_remove(fail_cols, "^fail_")),
    fail_label = factor(
      unname(fail_labels[as.character(fail_key)]),
      levels = unname(fail_labels[str_remove(fail_cols, "^fail_")])
    )
  ) %>%
  transmute(platform, fail_label = as.character(fail_label), fail_rate)

# reuse your y-axis layout (checks, axis_map, hdr_df, break_df)
plot_df_online <- failchecks_online_plot %>%
  mutate(
    fail_label = trimws(as.character(fail_label)),
    platform   = trimws(as.character(platform)),
    fail_pct   = fail_rate * 100,
    ycat       = fail_label
  ) %>%
  filter(fail_label %in% checks) %>%
  left_join(axis_map, by = "ycat") %>%
  mutate(
    platform = factor(platform, levels = sort(unique(platform)))
  )

desired_platform_order <- c(
  "Human (In-Person)",
  "Prolific",
  "Verasight",
  "CR Connect",
  "Clickworker",
  "Cint Theorem",
  "Qualtrics",
  "CR MTurk"
)

plot_df_online <- plot_df_online %>%
  mutate(
    platform = factor(platform, levels = desired_platform_order)
  )

x_max_plat <- max(plot_df_online$fail_pct, na.rm = TRUE)

special_platform <- "Human (In-Person)"   # change to your exact platform name
online_levels <- levels(plot_df_online$platform)
online_cols <- c(
  setNames("black", special_platform),   # manual override
  setNames(
    hue_pal()(length(setdiff(online_levels, special_platform))),
    setdiff(online_levels, special_platform)
  )
)
online_labs <- setNames(online_levels, online_levels)  

# plot (same structure, legend = platform)
ggplot(plot_df_online, aes(x = fail_pct, y = ypos)) +
  geom_point(
    aes(
      color = platform,
      fill  = after_scale(alpha(color, 0.45))
    ),
    shape = 21,
    size = 4.5,
    stroke = 1.2,
    position = position_dodge2(width = 0.75, preserve = "single")
  ) +
  geom_text(
    data = hdr_df,
    aes(x = -Inf, y = ypos, label = label),
    inherit.aes = FALSE,
    hjust = -0.02,
    fontface = "bold",
    size = 4.8
  ) +
  geom_hline(
    data = break_df,
    aes(yintercept = ypos),
    color = "grey75",
    linewidth = 0.6
  ) +
  scale_color_manual(values = online_cols, labels = online_labs) +
  scale_x_continuous(
    breaks = pretty_breaks(6),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(xlim = c(0, x_max_plat * 1.05), clip = "off") +
  scale_y_continuous(
    breaks = axis_map$ypos,
    labels = axis_labels[axis_map$ycat]
  ) +
  labs(
    title = "Average fail rate by check and platform (Human vs. Online) ",
    x = "Fail rate (%)",
    y = NULL,
    color = "Platform"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    axis.title.x = element_text(size = 15, margin = margin(t = 12)),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13, angle = 0),
    plot.margin  = margin(15, 30, 15, 150),
    panel.grid.minor.y = element_blank()
  )

# ———— across super agents ————
failchecks_agents_plot <- failchecks_platform %>%
  filter(source %in% c("Human (In-Person)", "AI Agent (General Prompt)", "AI Super Prompt")) %>% 
  group_by(platform) %>%
  summarise(across(starts_with("fail_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = starts_with("fail_"),
               names_to = "fail_check",
               values_to = "fail_rate") %>%
  filter(fail_check %in% fail_cols) %>%
  mutate(
    fail_key   = str_remove(fail_check, "^fail_"),
    fail_key   = factor(fail_key, levels = str_remove(fail_cols, "^fail_")),
    fail_label = factor(
      unname(fail_labels[as.character(fail_key)]),
      levels = unname(fail_labels[str_remove(fail_cols, "^fail_")])
    )
  ) %>%
  transmute(platform, fail_label = as.character(fail_label), fail_rate)

# reuse your y-axis layout (checks, axis_map, hdr_df, break_df)
plot_df_agent <- failchecks_agents_plot %>%
  mutate(
    fail_label = trimws(as.character(fail_label)),
    platform   = trimws(as.character(platform)),
    fail_pct   = fail_rate * 100,
    ycat       = fail_label
  ) %>%
  filter(fail_label %in% checks) %>%
  left_join(axis_map, by = "ycat") %>%
  mutate(
    platform = factor(platform, levels = sort(unique(platform)))  # change ordering if you want
  )

desired_agent_order <- c(
  "Human (In-Person)",
  "Atlas",
  "Comet",
  "Manus",
  "Westwood Super Atlas Autonomous",
  "Westwood Super Comet Autonomous",
  "Westwood Super Manus Autonomous",
  "Westwood Super Atlas Human Assisted",
  "Westwood Super Comet Human Assisted",
  "Westwood Super Manus Human Assisted"
)

plot_df_agent <- plot_df_agent %>%
  mutate(
    platform = factor(platform, levels = desired_agent_order)
  )

x_max_plat <- max(plot_df_agent$fail_pct, na.rm = TRUE)

special_platform <- "Human (In-Person)"   # change to your exact platform name
agent_levels <- levels(plot_df_agent$platform)
agent_cols <- c(
  setNames("black", special_platform),   # manual override
  setNames(
    hue_pal()(length(setdiff(agent_levels, special_platform))),
    setdiff(agent_levels, special_platform)
  )
)
agent_labs <- setNames(agent_levels, agent_levels)  

# plot (same structure, legend = platform)
ggplot(plot_df_agent, aes(x = fail_pct, y = ypos)) +
  geom_point(
    aes(
      color = platform,
      fill  = after_scale(alpha(color, 0.45))
    ),
    shape = 21,
    size = 4.5,
    stroke = 1.2,
    position = position_dodge2(width = 0.75, preserve = "single")
  ) +
  geom_text(
    data = hdr_df,
    aes(x = -Inf, y = ypos, label = label),
    inherit.aes = FALSE,
    hjust = -0.02,
    fontface = "bold",
    size = 4.8
  ) +
  geom_hline(
    data = break_df,
    aes(yintercept = ypos),
    color = "grey75",
    linewidth = 0.6
  ) +
  scale_color_manual(values = agent_cols, labels = agent_labs) +
  scale_x_continuous(
    breaks = pretty_breaks(6),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(xlim = c(0, x_max_plat * 1.05), clip = "off") +
  scale_y_continuous(
    breaks = axis_map$ypos,
    labels = axis_labels[axis_map$ycat]
  ) +
  labs(
    title = "Average fail rate by check and platform (Human vs. AI Agents) ",
    x = "Fail rate (%)",
    y = NULL,
    color = "Platform"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", size = 20, margin = margin(b = 10)),
    axis.title.x = element_text(size = 15, margin = margin(t = 12)),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13, angle = 0),
    plot.margin  = margin(15, 30, 15, 150),
    panel.grid.minor.y = element_blank()
  )
#### end ####

#### proportion tests for MW vs. Online and Prolific vs. Online ####
# functions
prop_test_vec <- function(x, y, correct = FALSE, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  
  # counts of "success"
  sx <- sum(x == 1)
  sy <- sum(y == 1)
  
  # sample sizes
  nx <- length(x)
  ny <- length(y)
  
  prop.test(x = c(sx, sy), n = c(nx, ny), correct = correct)
}

prop_pval <- function(x, y) {
  prop_test_vec(x, y)$p.value
}

format_p <- function(p, digits = 4) {
  ifelse(
    is.na(p),
    NA_character_,
    ifelse(
      p < 1e-4,
      formatC(p, format = "e", digits = 2),
      formatC(p, format = "f", digits = digits)
    )
  )
}

# prop test with mindworks against all other online platforms
mw_results <- tibble(
  ref = "Human (In-Person)",
  comp = c("Prolific", "CRConnect", "CRMTurk", "Qualtrics",
           "Clickworker", "Cint", "Verasight"),
  failedanyMW2 = c(
    prop_pval(humandata$failedanyMW2, prolific$failedanyMW2),
    prop_pval(humandata$failedanyMW2, crconnect$failedanyMW2),
    prop_pval(humandata$failedanyMW2, crmturk$failedanyMW2),
    prop_pval(humandata$failedanyMW2, qualtrics$failedanyMW2),
    prop_pval(humandata$failedanyMW2, clickworker$failedanyMW2),
    prop_pval(humandata$failedanyMW2, cint$failedanyMW2),
    prop_pval(humandata$failedanyMW2, verasight$failedanyMW2)
  ),
  failedeffective = c(
    prop_pval(humandata$failedeffective, prolific$failedeffective),
    prop_pval(humandata$failedeffective, crconnect$failedeffective),
    prop_pval(humandata$failedeffective, crmturk$failedeffective),
    prop_pval(humandata$failedeffective, qualtrics$failedeffective),
    prop_pval(humandata$failedeffective, clickworker$failedeffective),
    prop_pval(humandata$failedeffective, cint$failedeffective),
    prop_pval(humandata$failedeffective, verasight$failedeffective)
  ),
  failedanyMW2_p = format_p(failedanyMW2),
  failedeffective_p = format_p(failedeffective)
) %>% 
  dplyr::select(-c(failedanyMW2, failedeffective))

# prop test with prolific against all other online platforms
prolific_results <- tibble(
  ref = "Prolific",
  comp = c("CRConnect", "CRMTurk", "Qualtrics",
           "Clickworker", "Cint", "Verasight"),
  failedanyMW2 = c(
    prop_pval(prolific$failedanyMW2, crconnect$failedanyMW2),
    prop_pval(prolific$failedanyMW2, crmturk$failedanyMW2),
    prop_pval(prolific$failedanyMW2, qualtrics$failedanyMW2),
    prop_pval(prolific$failedanyMW2, clickworker$failedanyMW2),
    prop_pval(prolific$failedanyMW2, cint$failedanyMW2),
    prop_pval(prolific$failedanyMW2, verasight$failedanyMW2)
  ),
  failedeffective = c(
    prop_pval(prolific$failedeffective, crconnect$failedeffective),
    prop_pval(prolific$failedeffective, crmturk$failedeffective),
    prop_pval(prolific$failedeffective, qualtrics$failedeffective),
    prop_pval(prolific$failedeffective, clickworker$failedeffective),
    prop_pval(prolific$failedeffective, cint$failedeffective),
    prop_pval(prolific$failedeffective, verasight$failedeffective)
  ),
  failedanyMW2_p = format_p(failedanyMW2),
  failedeffective_p = format_p(failedeffective)
) %>% 
  dplyr::select(-c(failedanyMW2, failedeffective))

# results
prop_test_table <- bind_rows(mw_results, prolific_results)

# vs. Mindworks
prop_test_vec(humandata$failedanyMW2, prolific$failedanyMW2)
prop_test_vec(humandata$failedanyMW2, crconnect$failedanyMW2)
prop_test_vec(humandata$failedanyMW2, crmturk$failedanyMW2)
prop_test_vec(humandata$failedanyMW2, qualtrics$failedanyMW2)
prop_test_vec(humandata$failedanyMW2, clickworker$failedanyMW2)
prop_test_vec(humandata$failedanyMW2, cint$failedanyMW2)
prop_test_vec(humandata$failedanyMW2, verasight$failedanyMW2)

prop_test_vec(humandata$failedeffective, prolific$failedeffective)
prop_test_vec(humandata$failedeffective, crconnect$failedeffective)
prop_test_vec(humandata$failedeffective, crmturk$failedeffective)
prop_test_vec(humandata$failedeffective, qualtrics$failedeffective)
prop_test_vec(humandata$failedeffective, clickworker$failedeffective)
prop_test_vec(humandata$failedeffective, cint$failedeffective)
prop_test_vec(humandata$failedeffective, verasight$failedeffective)

# vs. Prolific
prop_test_vec(prolific$failedanyMW2, crconnect$failedanyMW2)
prop_test_vec(prolific$failedanyMW2, crmturk$failedanyMW2)
prop_test_vec(prolific$failedanyMW2, qualtrics$failedanyMW2)
prop_test_vec(prolific$failedanyMW2, clickworker$failedanyMW2)
prop_test_vec(prolific$failedanyMW2, cint$failedanyMW2)
prop_test_vec(prolific$failedanyMW2, verasight$failedanyMW2)

prop_test_vec(prolific$failedeffective, crconnect$failedeffective)
prop_test_vec(prolific$failedeffective, crmturk$failedeffective)
prop_test_vec(prolific$failedeffective, qualtrics$failedeffective)
prop_test_vec(prolific$failedeffective, clickworker$failedeffective)
prop_test_vec(prolific$failedeffective, cint$failedeffective)
prop_test_vec(prolific$failedeffective, verasight$failedeffective)
#### end ####

#### AI super prompt failed checks ####
failchecks_AIsupers <- alldata %>% 
  filter(source == "AI Super Prompt") %>% 
  group_by(platform) %>% 
  summarize(across(starts_with("fail_"), \(x) mean(x, na.rm = TRUE))) %>% 
  t()

failchecks_AIsupers_effective <- alldata %>% 
  filter(source == "AI Super Prompt") %>% 
  group_by(platform) %>% 
  summarize(across(starts_with("fail_"), \(x) mean(x, na.rm = TRUE))) %>% 
  dplyr::select(platform, fail_typing, fail_OJnutrition, fail_fakebrand, fail_openendhealth, fail_captcha)

failchecks_AIsupers_MW2 <- alldata %>% 
  filter(source == "AI Super Prompt") %>% 
  group_by(platform) %>% 
  summarize(across(starts_with("fail_"), \(x) mean(x, na.rm = TRUE))) %>% 
  dplyr::select(platform, fail_typing, fail_OJnutrition, fail_fakebrand, fail_openendhealth, fail_captcha,
                fail_sliderclicks, fail_usedAI, fail_math, fail_trolling)
#### end ####

# ------ AI Ethicality Analyses ------

#### AI Ethicality Rates and Graphs ####
# AI ethicality means across platforms
AIethical <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    ethicalAI = mean(ethicalAI, na.rm = TRUE),
    .groups = "drop"
  )
# AI ethicality means across platforms, after filtering out those who failed effective checks
AIethical_filtered_eff <- alldata %>% 
  filter(failedeffective == 0) %>% 
  group_by(platform) %>% 
  summarize(
    ethicalAI_filtered_eff = mean(ethicalAI, na.rm = TRUE),
    .groups = "drop"
  )

ethicalAI_platforms <- AIethical %>% 
  left_join(AIethical_filtered_eff, by = "platform")

# create comparison, t-tests between mindworks and each platforms, on different criteria
# how does ethical AI compare to MW?
mw_vals <- alldata %>%
  filter(platform == "Human (In-Person)") %>%
  pull(ethicalAI)
pvals_full <- alldata %>%
  filter(source == "Online") %>%
  group_by(platform) %>%
  summarize(
    p_value_vs_MW = t.test(ethicalAI, mw_vals)$p.value,
    .groups = "drop"
  )
# removed failed effective, how does ethical AI compare to MW now?
mw_vals_filt_eff <- alldata %>%
  filter(platform == "Human (In-Person)", failedeffective == 0) %>%
  pull(ethicalAI)
pvals_filtered_eff <- alldata %>%
  filter(source == "Online", failedeffective == 0) %>%
  group_by(platform) %>%
  summarize(
    p_value_vs_MW_filtered_eff = t.test(ethicalAI, mw_vals_filt_eff)$p.value,
    .groups = "drop"
  )

# look at online vs. human only
ethicalAI_platforms <- ethicalAI_platforms %>%
  left_join(pvals_full, by = "platform") %>%
  left_join(pvals_filtered_eff, by = "platform") %>% 
  filter(platform %in% c(
    "Human (In-Person)",
    "Prolific",
    "CR Connect",
    "CR MTurk",
    "Qualtrics",
    "Clickworker",
    "Cint Theorem",
    "Verasight"
  ))

# add all online vs. Mindworks/human
AIethical_source <- alldata %>% 
  group_by(source) %>% 
  summarize(
    ethicalAI = mean(ethicalAI, na.rm = TRUE),
    .groups = "drop"
  )

AIethical_source_eff <- alldata %>% 
  filter(failedeffective == 0) %>% 
  group_by(source) %>% 
  summarize(
    ethicalAI_filtered_eff = mean(ethicalAI, na.rm = TRUE),
    .groups = "drop"
  )

ethicalAI_source <- AIethical_source %>% 
  left_join(AIethical_source_eff, by = "source") %>% 
  filter(source == "Online")

# how does ethical AI compare to MW (humans) for all online pooled?
pvals_full_online <- alldata %>%
  filter(source == "Online") %>%
  summarize(
    p_value_vs_MW = t.test(ethicalAI, mw_vals)$p.value
  ) %>% 
  pull(p_value_vs_MW)

pvals_filtered_eff_online <- alldata %>%
  filter(source == "Online", failedeffective == 0) %>%
  summarize(
    p_value_vs_MW_filtered_eff = t.test(ethicalAI, mw_vals_filt_eff)$p.value
  ) %>% 
  pull(p_value_vs_MW_filtered_eff)

ethicalAI_source <- ethicalAI_source %>% 
  mutate(
    p_value_vs_MW = pvals_full_online,
    p_value_vs_MW_filtered_eff = pvals_filtered_eff_online
  ) %>% 
  mutate(platform = "Online (Pooled)")

ethicalAI_compare <- bind_rows(ethicalAI_platforms, ethicalAI_source)

# add significance stars
ethicalAI_compare <- ethicalAI_compare %>% 
  mutate(
    p_value_vs_MW          = ifelse(platform == "Human (In-Person)", NA, p_value_vs_MW),
    p_value_vs_MW_filtered_eff = ifelse(platform == "Human (In-Person)", NA, p_value_vs_MW_filtered_eff)) %>% 
  mutate(
    p_star_MW = case_when(
      is.na(p_value_vs_MW)        ~ "",
      p_value_vs_MW < 0.001       ~ "***",
      p_value_vs_MW < 0.01        ~ "**",
      p_value_vs_MW < 0.05        ~ "*",
      p_value_vs_MW < 0.1         ~ "†",
      TRUE                        ~ ""
    ),
    p_star_MW_filtered_eff = case_when(
      is.na(p_value_vs_MW)        ~ "",
      p_value_vs_MW_filtered_eff < 0.001       ~ "***",
      p_value_vs_MW_filtered_eff < 0.01        ~ "**",
      p_value_vs_MW_filtered_eff < 0.05        ~ "*",
      p_value_vs_MW_filtered_eff < 0.1         ~ "†",
      TRUE                        ~ ""
    )
  )

# rearrange data to plot
ethicalAI_compare_plot <- ethicalAI_compare %>%
  pivot_longer(
    cols = c(ethicalAI, ethicalAI_filtered_eff),
    names_to = "series",
    values_to = "ethicalAI_value"
  ) %>% 
  mutate(
    subgroup = ifelse(series == "ethicalAI", "All Responses", "After AI Exclusion"),
    subgroup = factor(subgroup, levels = c("All Responses", "After AI Exclusion")),
    p_star_eff = ifelse(subgroup == "All Responses", p_star_MW, p_star_MW_filtered_eff)
  )

ethicalAI_compare_plot <- ethicalAI_compare_plot %>% 
  mutate(platform = factor(platform, levels = c(
    "Human (In-Person)",
    "Prolific",
    "Verasight",
    "CR Connect",
    "Cint Theorem",
    "Clickworker",
    "Qualtrics",
    "CR MTurk",
    "Online (Pooled)"
  )))

# add pooled online fail rates
fail_plot_online <- alldata %>% 
  filter(source == "Online") %>% 
  summarize(failedeffective = round(mean(failedeffective), 3)*100) %>% 
  mutate(platform = "Online (Pooled)")
fail_plot_eff <- failoverall_platform %>% 
  filter(source %in% c("Human (In-Person)", "Online")) %>% 
  dplyr::select(platform, failedeffective) %>% 
  mutate(failedeffective = round(failedeffective * 100, 1))
fail_plot_eff <- bind_rows(fail_plot_eff, fail_plot_online)

ethicalAI_compare_plot <- ethicalAI_compare_plot %>% 
  merge(fail_plot_eff, by = "platform")

platform_levels_eff <- c(
  "Human (In-Person)",
  "Prolific",
  "Verasight",
  "CR Connect",
  "Cint Theorem",
  "Clickworker",
  "Qualtrics",
  "CR MTurk",
  "Online (Pooled)"
)

# add raw fail rates:
df_plot_eff <- ethicalAI_compare_plot %>%
  mutate(platform = factor(platform, levels = platform_levels_eff))

# ————— Create Plots ——————
# graph AI agent means pooled as a dotted line
ethicalAI_mean_AIall <- AIall %>%
  summarize(m = mean(ethicalAI, na.rm = TRUE)) %>%
  pull(m)

aiagent_mean_line <- list(
  geom_hline(
    yintercept = ethicalAI_mean_AIall,
    linetype = "dashed",
    linewidth = 0.9,
    color = "#4D4D4D"
  ),
  annotate(
    "text",
    x = Inf,
    y = ethicalAI_mean_AIall + 0.08,
    label = sprintf("All AI Agents Mean = %.2f", ethicalAI_mean_AIall),
    hjust = 0,
    vjust = 0,
    color = "black",
    size = 4
  )
)

# final graph:
bottom_df <- df_plot_eff %>%
  distinct(platform, failedeffective) %>%     # value same across subgroup rows
  mutate(
    pct_label = sprintf("%.1f%%", failedeffective),
    
    # numeric x positions for discrete axis to draw underline segments
    xi      = as.numeric(platform),
    x_start = xi - 0.35,
    x_end   = xi + 0.35
  )
bottom_df <- bottom_df %>%
  mutate(platform_label = case_when(
    platform == "Human (In-Person)" ~ "Human\n(In-Person)",
    platform == "Online (Pooled)" ~ "Online\n(Pooled)",
    TRUE ~ as.character(platform)
  ))

# control vertical placement of underline + labels (data units)
pad <- 0.06
y_line <- -0.22
y_pct <- -0.35
y_platform <- -0.70
ymax_eff <- max(df_plot_eff$ethicalAI_value, na.rm = TRUE)

new_ethicalAI_plot <- ggplot(
  df_plot_eff,
  aes(x = platform, y = ethicalAI_value, fill = subgroup, group = subgroup)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = p_star_eff, y = ethicalAI_value + pad),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = 0
  ) +
  
  # ---- underline segment under each platform group
  geom_segment(
    data = bottom_df,
    aes(x = x_start, xend = x_end, y = y_line, yend = y_line),
    inherit.aes = FALSE,
    linewidth = 0.6
  ) +
  # ---- "X%" label
  geom_text(
    data = bottom_df,
    aes(x = platform, y = y_pct, label = pct_label),
    inherit.aes = FALSE,
    size = 4
  ) +
  # ---- platform label (not rotated)
  geom_text(
    data = bottom_df,
    aes(x = platform, y = y_platform, label = as.character(platform_label)),
    inherit.aes = FALSE,
    size = 4
  ) +
  
  scale_fill_manual(
    values = c("All Responses" = "#1f77b4", "After AI Exclusion" = "#ff7f0e"),
    breaks = c("All Responses", "After AI Exclusion")
  ) +
  
  scale_y_continuous(
    breaks = c(0, 0.5, 1, 1.5, 2, 2.5),
    expand = expansion(mult = c(0, .05))
  ) +
  
  coord_cartesian(
    ylim = c(y_platform - 0.18, ymax_eff + 3 * pad),
    clip = "off"
  ) +
  
  labs(
    title = "AI Ethicality Ratings Before and After Exclusions for AI-Test Failure (Effective Tests)",
    subtitle = "Significance tested against human (in-person) sample\n \n † p < .1, * p < .05, ** p < .01, *** p < .001",
    x = "Platform",
    # break before parentheses onto a new line
    y = "AI Ethicality Mean Rating\n(0 = not ethical, 6 = ethical)",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 18, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 18)),
    
    # remove default x labels/ticks (we draw them ourselves)
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(
      size = 15,
      margin = margin(t = 6)
    ),
    
    axis.title.y = element_text(size = 14, margin = margin(r = 6)),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # extra space for underline + % + platform labels
    plot.margin = margin(15, 40, 28, 20),
    
    legend.text = element_text(size = 12),
    legend.position = "right"
  ) +
  
  # ---- left-side horizontal label under y-axis area
  geom_text(
    data = tibble(
      x = length(levels(df_plot_eff$platform)) + 0.55,
      y = y_pct,
      label = "% failed effective AI tests"
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4
  ) +
  
  # ---- keep your existing mean line layer if you have it
  aiagent_mean_line

new_ethicalAI_plot

# are the reductions significant - effective
t.test(prolific %>% filter(failedeffective == 1) %>% pull(ethicalAI), prolific %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(verasight %>% filter(failedeffective == 1) %>% pull(ethicalAI), verasight %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(crconnect %>% filter(failedeffective == 1) %>% pull(ethicalAI), crconnect %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(clickworker %>% filter(failedeffective == 1) %>% pull(ethicalAI), clickworker %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(cint %>% filter(failedeffective == 1) %>% pull(ethicalAI), cint %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(qualtrics %>% filter(failedeffective == 1) %>% pull(ethicalAI), qualtrics %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(crmturk %>% filter(failedeffective == 1) %>% pull(ethicalAI), crmturk %>% filter(failedeffective == 0) %>% pull(ethicalAI))
t.test(onlinedata %>% filter(failedeffective == 1) %>% pull(ethicalAI), onlinedata %>% filter(failedeffective == 0) %>% pull(ethicalAI))

t.test(humandata %>% pull(ethicalAI), crmturk %>% pull(ethicalAI))
t.test(humandata %>% filter(failedeffective == 0) %>% pull(ethicalAI), crmturk %>% filter(failedeffective == 0) %>% pull(ethicalAI))
#### end ####

# ------ Failing conditional on passing X ------

#### pass typing, still flagged as AI ####
passtyping_failAI <- alldata %>% 
  group_by(platform, source) %>% 
  summarize(
    faileff_all = mean(failedeffective, na.rm = TRUE),
    faileff_passtyping = mean(
      failedeffective[fail_typing == 0],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# graph
passtyping_failAI_plot <- passtyping_failAI %>%
  filter(source %in% c("Human (In-Person)", "Online")) %>% 
  dplyr::select(-c(source)) %>% 
  mutate(platform = factor(platform, levels = c(
    "Human (In-Person)",
    "Prolific",
    "CR Connect",
    "Verasight",
    "Clickworker",
    "Cint Theorem",
    "Qualtrics",
    "CR MTurk"
  )))
passtyping_failAI_plot <- passtyping_failAI_plot %>% 
  pivot_longer(
    cols = -platform,
    names_to = "subgroup",
    values_to = "fail_rate"
  ) %>% 
  mutate(fail_rate_pct = round(fail_rate*100, 1)) %>% 
  mutate(subgroup = case_when(subgroup == "faileff_all" ~ "All Responses",
                              subgroup == "faileff_passtyping" ~ "After Passing Typing Metrics")) %>% 
  mutate(subgroup = factor(subgroup, levels = c("All Responses", "After Passing Typing Metrics")))

ggplot(passtyping_failAI_plot, aes(x = platform, y = fail_rate_pct, fill = subgroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(fail_rate_pct, "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.25,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10)
  ) +
  labs(
    title = "Effective Test Fail Rates Before and After Exclusions for Typing Metrics Failure",
    x = "Platform",
    y = "Fail rate (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )
#### end ####

#### audio recall vs. flag as AI ####
# passing audio recall (hamlin), still flagged as AI
passhamlin_failAI <- alldata %>% 
  group_by(platform, source) %>% 
  summarize(
    faileff_all = mean(failedeffective, na.rm = TRUE),
    faileff_passhamlin = mean(
      failedeffective[fail_hamlin == 0],
      na.rm = TRUE
    ),
    failMW2_passhamlin = mean(
      failedanyMW2[fail_hamlin == 0],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# passing AI, get audio recall (hamlin) wrong
passAI_failhamlin <- alldata %>% 
  group_by(platform, source) %>% 
  summarize(
    failhamlin_all = mean(fail_hamlin, na.rm = TRUE),
    failhamlin_pass_eff = mean(
      fail_hamlin[failedeffective == 0],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# graph
passhamlin_failAI_plot <- passhamlin_failAI %>%
  filter(source %in% c("Human (In-Person)", "Online")) %>% 
  dplyr::select(-c(source)) %>% 
  mutate(platform = factor(platform, levels = c(
    "Human (In-Person)",
    "Prolific",
    "CR Connect",
    "Verasight",
    "Clickworker",
    "Cint Theorem",
    "Qualtrics",
    "CR MTurk"
  )))
passhamlin_failAI_plot <- passhamlin_failAI_plot %>% 
  pivot_longer(
    cols = -platform,
    names_to = "subgroup",
    values_to = "fail_rate"
  ) %>% 
  mutate(fail_rate_pct = round(fail_rate*100, 1)) %>% 
  mutate(subgroup = case_when(subgroup == "faileff_all" ~ "All Responses",
                              subgroup == "faileff_passhamlin" ~ "After Passing Audio Recall")) %>% 
  mutate(subgroup = factor(subgroup, levels = c("All Responses", "After Passing Audio Recall")))

ggplot(passhamlin_failAI_plot, aes(x = platform, y = fail_rate_pct, fill = subgroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(fail_rate_pct, "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.25,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10)
  ) +
  labs(
    title = "Effective Test Fail Rates Before and After Exclusions for Audio Recall Failure",
    x = "Platform",
    y = "Fail rate (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )
#### end ####

# ------ Sieve Stacks ------

# use <2% at mindworks (human) checks (Top 9), in order of effectiveness

#### all data - % retained and failure through checks, top ranked checks ####
# retained
alldata_top1 <- alldata %>% 
  filter(fail_typing == 0)
alldata_top1 <- alldata_top1 %>% 
  group_by(platform) %>% 
  summarize(failedeffective_top1 = mean(failedeffective))

alldata_top2 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0)
alldata_top2 <- alldata_top2 %>% 
  group_by(platform) %>% 
  summarize(failedeffective_top2 = mean(failedeffective))

alldata_top3 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0)
alldata_top3 <- alldata_top3 %>% 
  group_by(platform) %>% 
  summarize(failedeffective_top3 = mean(failedeffective))

alldata_top4 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0) %>% 
  filter(fail_openendhealth == 0)
alldata_top4 <- alldata_top4 %>% 
  group_by(platform) %>% 
  summarize(failedeffective_top4 = mean(failedeffective))

alldata_top5 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0) %>% 
  filter(fail_openendhealth == 0) %>%
  filter(fail_captcha == 0)
alldata_top5 <- alldata_top5 %>%  
  group_by(platform) %>% 
  summarize(failedeffective_top5 = mean(failedeffective))

alldata_top6 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0) %>% 
  filter(fail_openendhealth == 0) %>%
  filter(fail_captcha == 0) %>% 
  filter(fail_sliderclicks == 0)
alldata_top6 <- alldata_top6 %>%  
  group_by(platform) %>% 
  summarize(failedeffective_top6 = mean(failedeffective))

alldata_top7 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0) %>% 
  filter(fail_openendhealth == 0) %>%
  filter(fail_captcha == 0) %>% 
  filter(fail_sliderclicks == 0) %>% 
  filter(fail_math == 0)
alldata_top7 <- alldata_top7 %>%  
  group_by(platform) %>% 
  summarize(failedeffective_top7 = mean(failedeffective))

alldata_top8 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0) %>% 
  filter(fail_openendhealth == 0) %>%
  filter(fail_captcha == 0) %>% 
  filter(fail_sliderclicks == 0) %>% 
  filter(fail_math == 0) %>% 
  filter(fail_trolling == 0)
alldata_top8 <- alldata_top8 %>%  
  group_by(platform) %>% 
  summarize(failedeffective_top8 = mean(failedeffective))

alldata_top9 <- alldata %>% 
  filter(fail_typing == 0) %>% 
  filter(fail_OJnutrition == 0) %>% 
  filter(fail_fakebrand == 0) %>% 
  filter(fail_openendhealth == 0) %>%
  filter(fail_captcha == 0) %>% 
  filter(fail_sliderclicks == 0) %>% 
  filter(fail_math == 0) %>% 
  filter(fail_trolling == 0) %>% 
  filter(fail_usedAI == 0)
alldata_top9 <- alldata_top9 %>%  
  group_by(platform) %>% 
  summarize(failedeffective_top9 = mean(failedeffective))

alldata_retained <- alldata %>%
  group_by(platform) %>% 
  summarize(failedeffective = mean(failedeffective)) %>% 
  ungroup() %>% 
  left_join(alldata_top1, by = "platform") %>% 
  left_join(alldata_top2, by = "platform") %>% 
  left_join(alldata_top3, by = "platform") %>% 
  left_join(alldata_top4, by = "platform") %>% 
  left_join(alldata_top5, by = "platform") %>% 
  left_join(alldata_top6, by = "platform") %>% 
  left_join(alldata_top7, by = "platform") %>% 
  left_join(alldata_top8, by = "platform") %>% 
  left_join(alldata_top9, by = "platform")

# failed
alldata <- alldata %>% 
  mutate(
    failtop1 = as.numeric(fail_typing == 1),
    failtop2 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1),
    failtop3 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1),
    failtop4 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1 |
                            fail_openendhealth == 1),
    failtop5 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1 |
                            fail_openendhealth == 1 |
                            fail_captcha == 1),
    failtop6 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1 |
                            fail_openendhealth == 1 |
                            fail_captcha == 1 |
                            fail_sliderclicks == 1),
    failtop7 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1 |
                            fail_openendhealth == 1 |
                            fail_captcha == 1 |
                            fail_sliderclicks == 1 |
                            fail_math == 1),
    failtop8 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1 |
                            fail_openendhealth == 1 |
                            fail_captcha == 1 |
                            fail_sliderclicks == 1 |
                            fail_math == 1 |
                            fail_trolling == 1),
    failtop9 = as.numeric(fail_typing == 1 |
                            fail_OJnutrition == 1 |
                            fail_fakebrand == 1 |
                            fail_openendhealth == 1 |
                            fail_captcha == 1 |
                            fail_sliderclicks == 1 |
                            fail_math == 1 |
                            fail_trolling == 1 |
                            fail_usedAI == 1))

alldata_failed <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    across(starts_with("failtop"), ~ mean(.x, na.rm = TRUE)),
    failedanyMW2 = mean(failedanyMW2),
    .groups = "drop"
  ) %>% 
  mutate(across(c(starts_with("failtop"), failedanyMW2), ~ round(.x, 3)*100))
#### end ####

#### online data pooled - % retained and failure through checks ####
onlinedata_r1 <- alldata %>% 
  filter(source == "Online") %>% 
  filter(fail_typing == 0)
onlinedata_r2 <- onlinedata_r1 %>% 
  filter(fail_OJnutrition == 0)
onlinedata_r3 <- onlinedata_r2 %>% 
  filter(fail_fakebrand == 0)
onlinedata_r4 <- onlinedata_r3 %>% 
  filter(fail_openendhealth == 0)
onlinedata_r5 <- onlinedata_r4 %>% 
  filter(fail_captcha == 0)
onlinedata_r6 <- onlinedata_r5 %>% 
  filter(fail_sliderclicks == 0)
onlinedata_r7 <- onlinedata_r6 %>% 
  filter(fail_math == 0)
onlinedata_r8 <- onlinedata_r7 %>% 
  filter(fail_trolling == 0)
onlinedata_r9 <- onlinedata_r8 %>% 
  filter(fail_usedAI == 0)

onlinedata_retained <- data.frame(
  round = c(1:9),
  passedcheck = c(
    "Typing metrics",
    "Multi-part reverse shibboleth",
    "Prompt injection (choice)",
    "Prompt injection (text)",
    "reCAPTCHA",
    "Slider usage detection",
    "Visual processing",
    "Trolling",
    "Concealing nonhuman identity"
  ),
  data_left_pc = c(
    nrow(onlinedata_r1)/nrow(onlinedata),
    nrow(onlinedata_r2)/nrow(onlinedata),
    nrow(onlinedata_r3)/nrow(onlinedata),
    nrow(onlinedata_r4)/nrow(onlinedata),
    nrow(onlinedata_r5)/nrow(onlinedata),
    nrow(onlinedata_r6)/nrow(onlinedata),
    nrow(onlinedata_r7)/nrow(onlinedata),
    nrow(onlinedata_r8)/nrow(onlinedata),
    nrow(onlinedata_r9)/nrow(onlinedata)
  ),
  failedeffective = c(
    mean(onlinedata_r1$failedeffective),
    mean(onlinedata_r2$failedeffective),
    mean(onlinedata_r3$failedeffective),
    mean(onlinedata_r4$failedeffective),
    mean(onlinedata_r5$failedeffective),
    mean(onlinedata_r6$failedeffective),
    mean(onlinedata_r7$failedeffective),
    mean(onlinedata_r8$failedeffective),
    mean(onlinedata_r9$failedeffective)
  ),
  failedanyMW2 = c(
    mean(onlinedata_r1$failedanyMW2),
    mean(onlinedata_r2$failedanyMW2),
    mean(onlinedata_r3$failedanyMW2),
    mean(onlinedata_r4$failedanyMW2),
    mean(onlinedata_r5$failedanyMW2),
    mean(onlinedata_r6$failedanyMW2),
    mean(onlinedata_r7$failedanyMW2),
    mean(onlinedata_r8$failedanyMW2),
    mean(onlinedata_r9$failedanyMW2)
  )
)

plot_onlinedata_retained <- onlinedata_retained %>%
  arrange(round) %>%
  mutate(
    passedcheck = factor(passedcheck, levels = passedcheck), # preserve order
    pct = data_left_pc * 100,
    label = paste0(round(pct, 1), "%")
  )

ggplot(plot_onlinedata_retained, aes(x = passedcheck, y = pct, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = label),
    vjust = -0.6,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(75, 100),
    expand = expansion(mult = c(0, .05))
  ) +
  labs(
    title = "Cumulative Percent of Online Participants Retained",
    x = "Additional test passed",
    y = "Oneline Data Retained (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
#### end ####

# ------ Actual Choices: Proportions ------

#### Proportions (primary shopper, english, AI familiarity, AI browser, past AI use, seen study) ####
# 1: is primary shopper, english primary language, is familiar with AI, has AI browser, used AI in past surveys, and seen this survey before
choicesprops <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    primaryshopper = mean(primaryshopper, na.rm = TRUE),
    english = mean(English, na.rm = TRUE),
    familiarAI = mean(familiarAI, na.rm = TRUE),
    browserAI = mean(browserAI, na.rm = TRUE),
    pastAI = mean(pastAI, na.rm = TRUE), 
    seenstudy = mean(seenstudy, na.rm = TRUE)
  )
#### end ####

# ------ Actual Choices: Histograms ------

#### function for histograms graphs ####
plot_platform_discrete_hist <- function(data = alldata,
                                        dv,
                                        subtitle = NULL,
                                        platform_col = platform,
                                        ncol = 5,
                                        mean_digits = 2,
                                        show_mean = TRUE,
                                        drop_na = TRUE) {
  
  dv_sym   <- rlang::ensym(dv)
  plat_sym <- rlang::ensym(platform_col)
  dv_name  <- rlang::as_label(dv_sym)
  
  df <- data %>%
    dplyr::transmute(
      platform = !!plat_sym,
      dv_num   = as.numeric(!!dv_sym)
    )
  
  if (drop_na) {
    df <- df %>% dplyr::filter(!is.na(dv_num), !is.na(platform))
  }
  
  x_vals <- sort(unique(df$dv_num))
  
  sumdf <- df %>%
    dplyr::count(platform, dv_num, name = "n") %>%
    tidyr::complete(platform, dv_num = x_vals, fill = list(n = 0L)) %>%
    dplyr::group_by(platform) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(sumdf, ggplot2::aes(dv_num, pct)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::facet_wrap(~platform, ncol = ncol) +
    ggplot2::scale_x_continuous(breaks = x_vals) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, .03))
    ) +
    ggplot2::labs(
      title = paste0(dv_name, " by platform"),
      subtitle = subtitle,
      x = dv_name,
      y = "% of responses"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (show_mean) {
    
    means_df <- df %>%
      dplyr::group_by(platform) %>%
      dplyr::summarise(mean_dv = mean(dv_num), .groups = "drop") %>%
      dplyr::mutate(mean_lab = formatC(mean_dv, format = "f", digits = mean_digits))
    
    p <- p +
      ggplot2::geom_vline(
        data = means_df,
        ggplot2::aes(xintercept = mean_dv),
        inherit.aes = FALSE,
        linetype = "dashed",
        linewidth = 0.7
      ) +
      ggplot2::geom_text(
        data = means_df,
        ggplot2::aes(x = mean_dv, y = 0.98, label = mean_lab),
        inherit.aes = FALSE,
        vjust = 1,
        size = 3.6
      )
  }
  
  return(p)
}
#### end ####

#### pulp (distribution) ####
# 0: no pulp, 1: some pulp, 2: full pulp
pulp_subtitle <- c("0: no pulp, 1: some pulp, 2: full pulp")

plot_platform_discrete_hist(
  dv = pulp_prefer,
  subtitle = pulp_subtitle,
  show_mean = FALSE
)
#### end ####

#### shop freq (distribution) ####
# 0: <1/week, 1: 1-2/week, 2: 3+/week
shopfreq_subtitle <- c("Go grocery shopping: 0: <1/week, 1: 1-2/week, 2: 3+/week")

plot_platform_discrete_hist(
  dv = shop_freq,
  subtitle = shopfreq_subtitle,
  show_mean = FALSE
)
#### end ####

#### buy OJ freq (distribution) ####
# 0-6/month
buyfreq_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_buyfreq = mean(buy_freq),
    sd_buyfreq = sd(buy_freq),
    min_buyfreq = min(buy_freq),
    median_buyfreq = median(buy_freq),
    max_buyfreq = max(buy_freq)
  )

buyfreq_subtitle <- c("Buy OJ 0-6+ times per month")

plot_platform_discrete_hist(
  dv = buy_freq,
  subtitle = buyfreq_subtitle,
  show_mean = TRUE
)
#### end ####

#### buy OJ size (distribution) ####
# 1: Single size serving (6-8 oz.)
# 2: Plastic bottle (32-46 oz.)
# 3: Carton (52-59 oz.)
# 4: Plastic jug (64-89 oz.)
# 5: Gallon jug (128 oz.)
buysize_subtitle <- c("Buy OJ size 0-6+ times per month: 1: Single size serving (6-8 oz.), 2: Plastic bottle (32-46 oz.),\n3: Carton (52-59 oz.), 4: Plastic jug (64-89 oz.), 5: Gallon jug (128 oz.)")

plot_platform_discrete_hist(
  dv = buy_size,
  subtitle = buysize_subtitle,
  show_mean = TRUE
)
#### end ####

#### age (distribution) ####
age_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_age = mean(age),
    sd_age = sd(age),
    min_age = min(age),
    median_age = median(age),
    max_age = max(age)
  )

min(alldata$age)
max(alldata$age)
age_subtitle <- c("Min age: 9, Max age: 82")

plot_platform_discrete_hist(
  dv = age,
  subtitle = age_subtitle,
  show_mean = TRUE
)
#### end ####

#### income (distribution) ####
income_stats <- alldata %>% 
  filter(income != 0) %>% 
  group_by(platform) %>% 
  summarize(
    mean_income = mean(income),
    sd_income = sd(income),
    min_income = min(income),
    median_income = median(income),
    max_income = max(income)
  )

alldata <- alldata %>% 
  mutate(income = ifelse(income != 0, income - 10, income))

# 1: Less than $10,000
# 12: More than $150,000
# 0: Prefer not to say
income_subtitle <- c("Yearly Income: 0: Prefer not to say, 1: Less than $10,000, 12: More than $150,000")

plot_platform_discrete_hist(
  dv = income,
  subtitle = income_subtitle,
  show_mean = TRUE
)
#### end ####

#### rate survey (distribution) ####
ratesurvey_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_ratesurvey = mean(ratesurvey),
    sd_ratesurvey = sd(ratesurvey),
    min_ratesurvey = min(ratesurvey),
    median_ratesurvey = median(ratesurvey),
    max_ratesurvey = max(ratesurvey)
  )

# 1-5 stars
ratesurvey_subtitle <- c("Rate survey: 1-5 stars")

plot_platform_discrete_hist(
  dv = ratesurvey,
  subtitle = ratesurvey_subtitle,
  show_mean = TRUE
)
#### end ####

#### Browser used (distribution) ####
# 1: Google Chrome, 2: Safari, 3: Firefox, 4: Microsoft Edge, 5: Opera, 6: DuckDuckGo, 7: Brave, 0: Other
# Mindworks - Chrome
browser_subtitle <- c("1: Google Chrome, 2: Safari, 3: Firefox, 4: Microsoft Edge, 5: Opera, 6: DuckDuckGo, 7: Brave, 0: Other")

plot_platform_discrete_hist(
  dv = browser,
  subtitle = browser_subtitle,
  show_mean = FALSE
)
#### end ####

#### AI used (distribution) ####
# 1: Yes, I used Al/an LLM to complete the entire survey
# 2: Yes, I used Al/an LLM to help complete parts of the survey
# 3: No, 1 did not use Al/an LLM at all
# 4: Other (explain)
usedAI_subtitle <- c("1: Yes, I used Al/an LLM to complete the entire survey, 2: Yes, I used Al/an LLM to help complete parts of the survey,\n3: No, 1 did not use Al/an LLM at all, 4: Other (explain)")

plot_platform_discrete_hist(
  dv = usedAI,
  subtitle = usedAI_subtitle,
  show_mean = FALSE
)
#### end ####

# ------ Actual Choices: Multipart Distributions ------

#### function for histograms graphs (choose brands) ####
plot_platform_multipart_hist <- function(data = alldata,
                                         dv,
                                         subtitle = NULL,
                                         platform_col = platform,
                                         ncol = 5,
                                         mean_digits = 2,
                                         show_mean = TRUE,
                                         drop_na = TRUE) {
  
  dv_sym   <- rlang::ensym(dv)
  plat_sym <- rlang::ensym(platform_col)
  dv_name  <- rlang::as_label(dv_sym)
  
  df <- data %>%
    dplyr::transmute(
      platform = !!plat_sym,
      dv_raw   = as.character(!!dv_sym)
    ) %>%
    tidyr::separate_rows(dv_raw, sep = ",") %>%
    dplyr::mutate(
      dv_raw = trimws(dv_raw),
      dv_num = suppressWarnings(as.numeric(dv_raw))
    )
  
  if (drop_na) {
    df <- df %>% dplyr::filter(!is.na(dv_num), !is.na(platform))
  }
  
  x_vals <- sort(unique(df$dv_num))
  
  sumdf <- df %>%
    dplyr::count(platform, dv_num, name = "n") %>%
    tidyr::complete(platform, dv_num = x_vals, fill = list(n = 0L)) %>%
    dplyr::group_by(platform) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(sumdf, ggplot2::aes(dv_num, pct)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::facet_wrap(~platform, ncol = ncol) +
    ggplot2::scale_x_continuous(breaks = x_vals) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, .03))
    ) +
    ggplot2::labs(
      title = paste0(dv_name, " by platform"),
      subtitle = subtitle,
      x = dv_name,
      y = "% selecting option"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (show_mean) {
    means_df <- df %>%
      dplyr::group_by(platform) %>%
      dplyr::summarise(mean_dv = mean(dv_num, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(mean_lab = formatC(mean_dv, format = "f", digits = mean_digits))
    
    p <- p +
      ggplot2::geom_vline(
        data = means_df,
        ggplot2::aes(xintercept = mean_dv),
        inherit.aes = FALSE,
        linetype = "dashed",
        linewidth = 0.7
      ) +
      ggplot2::geom_text(
        data = means_df,
        ggplot2::aes(x = mean_dv, y = 0.98, label = mean_lab),
        inherit.aes = FALSE,
        vjust = 1,
        size = 3.6
      )
  }
  
  return(p)
}
#### end ####

#### choose brands (distributions) ####
brandpref_subtitle <- c("1: Tropicana, 2: Simply, 3: Florida's Natural, 4: Minute Maid, 5: Sunny D, 6: Bolthouse Farms, 7: Natalie's,\n8: Naked Juice, 9: Great Value, 10: Good & Gather, 11: Dole, 12: Uncle Matt's, 13: Nature's Nectar, 14: Langers,\n15: Fake Brand (Golden Grove) - Prompt Injection, 0: None of the above")

plot_platform_multipart_hist(
  dv = brandpref,
  subtitle = brandpref_subtitle,
  show_mean = FALSE
)
#### end ####

#### functions for mean and distribution output (brand ratings) ####
# mean bars (optionally SE bars)mean bars (optionally SE bars)
plot_platform_multipart_mean <- function(data = alldata,
                                         dv_cols,
                                         subtitle = NULL,
                                         platform_col = platform,
                                         ncol = 5,
                                         show_se = TRUE,
                                         drop_na = TRUE,
                                         mean_digits = 2) {
  plat_sym <- rlang::ensym(platform_col)
  
  df_long <- data %>%
    dplyr::select(!!plat_sym, {{ dv_cols }}) %>%
    tidyr::pivot_longer(
      cols = {{ dv_cols }},
      names_to = "part",
      values_to = "value"
    ) %>%
    dplyr::rename(platform = !!plat_sym) %>%
    dplyr::mutate(value = as.numeric(value))
  
  if (drop_na) {
    df_long <- df_long %>% dplyr::filter(!is.na(platform), !is.na(value))
  }
  
  # Root name for title (e.g., "estimation" from "estimation_1")
  first_name <- df_long$part[which(!is.na(df_long$part))[1]]
  root_name  <- sub("_[^_]+$", "", first_name)
  
  # Extract numeric suffix and use it as the x-axis label (1,2,3,...)
  df_long <- df_long %>%
    dplyr::mutate(
      part_num = suppressWarnings(as.integer(stringr::str_extract(part, "(?<=_)\\d+$"))),
      x = factor(part_num, levels = sort(unique(part_num)))
    )
  
  # Summaries per platform x part
  sumdf <- df_long %>%
    dplyr::group_by(platform, x) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      se = stats::sd(value, na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    ) %>%
    dplyr::mutate(mean_lab = formatC(mean, format = "f", digits = mean_digits))
  
  p <- ggplot2::ggplot(sumdf, ggplot2::aes(x = x, y = mean)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::facet_wrap(~platform, ncol = ncol) +
    ggplot2::geom_text(
      ggplot2::aes(label = mean_lab),
      vjust = -0.4,
      size = 3.5
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.10))) +
    ggplot2::labs(
      title = paste0(root_name, " (mean) by platform"),
      subtitle = subtitle,
      x = NULL,
      y = "Mean"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (show_se) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        linewidth = 0.6
      )
  }
  
  p
}

# violin + jitter distribution
plot_platform_multipart_distribution <- function(data = alldata,
                                                 dv_cols,
                                                 subtitle = NULL,
                                                 platform_col = platform,
                                                 ncol = 5,
                                                 drop_na = TRUE,
                                                 jitter_width = 0.10,
                                                 point_alpha = 0.25) {
  plat_sym <- rlang::ensym(platform_col)
  
  df_long <- data %>%
    dplyr::select(!!plat_sym, {{ dv_cols }}) %>%
    tidyr::pivot_longer(
      cols = {{ dv_cols }},
      names_to = "part",
      values_to = "value"
    ) %>%
    dplyr::rename(platform = !!plat_sym) %>%
    dplyr::mutate(value = as.numeric(value))
  
  if (drop_na) {
    df_long <- df_long %>% dplyr::filter(!is.na(platform), !is.na(value))
  }
  
  # Root name for title
  first_name <- df_long$part[which(!is.na(df_long$part))[1]]
  root_name  <- sub("_[^_]+$", "", first_name)
  
  # X-axis label is ONLY numeric suffix (1,2,3,...)
  df_long <- df_long %>%
    dplyr::mutate(
      part_num = suppressWarnings(as.integer(stringr::str_extract(part, "(?<=_)\\d+$"))),
      x = factor(part_num, levels = sort(unique(part_num)))
    )
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = x, y = value)) +
    ggplot2::geom_violin(trim = FALSE) +
    ggplot2::geom_jitter(
      width = jitter_width,
      height = 0,
      alpha = point_alpha,
      size = 1.2
    ) +
    ggplot2::facet_wrap(~platform, ncol = ncol) +
    ggplot2::labs(
      title = paste0(root_name, " (distribution) by platform"),
      subtitle = subtitle,
      x = NULL,
      y = "Response"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
#### end ####

#### brand ratings (separated by brand) ####
# 1: Tropicana, 2: Simply, 3: Minute Maid, 4: Florida's Natural, 5: Natalie's, 6: Langers, 7: Naked Juice
ratebrand_subtitle <- c("Rating: 1-7 (higher = better)\n1: Tropicana, 2: Simply, 3: Minute Maid, 4: Florida's Natural, 5: Natalie's, 6: Langers, 7: Naked Juice")

plot_platform_multipart_mean(
  dv_cols = starts_with("ratebrand_"),
  subtitle = ratebrand_subtitle,
  show_se = FALSE
)

plot_platform_multipart_distribution(
  dv_cols = starts_with("ratebrand_"),
  subtitle = ratebrand_subtitle
)
#### end ####

#### OJ Nutrition: Calories ####
OJcal_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_OJcal = mean(estimation_1),
    sd_OJcal = sd(estimation_1),
    min_OJcal = min(estimation_1),
    median_OJcal = median(estimation_1),
    max_OJcal = max(estimation_1)
  )

OJcal_subtitle <- c("Calorie estimates for 8oz OJ")
plot_platform_discrete_hist(
  dv = estimation_1,
  subtitle = OJcal_subtitle,
  show_mean = TRUE
)
#### end ####

#### OJ Nutrition: Carbs ####
OJcarb_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_OJcarb = mean(estimation_2),
    sd_OJcarb = sd(estimation_2),
    min_OJcarb = min(estimation_2),
    median_OJcarb = median(estimation_2),
    max_OJcarb = max(estimation_2)
  )

OJcarb_subtitle <- c("Carbohydrates (g) estimates for 8oz OJ")
plot_platform_discrete_hist(
  dv = estimation_2,
  subtitle = OJcarb_subtitle,
  show_mean = TRUE
)
#### end ####

#### OJ Nutrition: Fat ####
OJfat_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_OJfat = mean(estimation_3),
    sd_OJfat = sd(estimation_3),
    min_OJfat = min(estimation_3),
    median_OJfat = median(estimation_3),
    max_OJfat = max(estimation_3)
  )

OJfat_subtitle <- c("Fat (g) estimates for 8oz OJ")
plot_platform_discrete_hist(
  dv = estimation_3,
  subtitle = OJfat_subtitle,
  show_mean = TRUE
)
#### end ####

#### OJ Nutrition: Protein ####
OJprotein_stats <- alldata %>% 
  group_by(platform) %>% 
  summarize(
    mean_OJprotein = mean(estimation_4),
    sd_OJprotein = sd(estimation_4),
    min_OJprotein = min(estimation_4),
    median_OJprotein = median(estimation_4),
    max_OJprotein = max(estimation_4)
  )

OJprotein_subtitle <- c("Protein (g) estimates for 8oz OJ")
plot_platform_discrete_hist(
  dv = estimation_4,
  subtitle = OJprotein_subtitle,
  show_mean = TRUE
)
#### end ####