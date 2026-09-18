# Mass AI Detection Adversarial Challenge/Hackathon - No Knowledge of Specific Tests - Public OSF
# Analyzing the 20 trials per bot reproduction by Kianté
# Data collected: June 2026
# last edited: Aug 22, 2026
# Authors: Grace Zhang, Robert Walatka

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
library(pROC)

# set working directory here to access files
setwd()

#### load datasets ####
# *Note: the data has been cleaned in the following ways:
# - removed IP address, location, and participant IDs
# - removed the code calculating specific criteria for typing metrics
### - typing metrics already within initial csv files
# - cint theorem csv file has two renamed columns as not to confuse with survey collected data
### - we collected data through the survey in columns "age" and "gender"
### - cint recorded age and gender in columns "cint_age" and "cint_gender," renamed from a second version of "age" and "gender"

df_runs_raw <- read_csv("ZhangEtAl_AIDetection_AdversarialAgents_Challenge_NoKnowledge_Public.csv")[-c(1:2),]
    
df_runs <- df_runs_raw %>% 
  filter(run != "00") %>% # remove test run
  arrange(StartDate) %>% 
  filter(Status == 0)

# remove duplicates 
df_runs <- df_runs %>%
  mutate(start_date = as.POSIXct(StartDate)) %>% 
  arrange(bot, run, desc(start_date)) %>%
  group_by(bot, run) %>%
  slice(1) %>%
  ungroup() %>% 
  mutate(index = row_number(),
         run = as.integer(run))

# rename variables
df_runs <- df_runs %>% 
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

# convert variables to numerics
df_runs <- df_runs %>% 
  dplyr::select(-c(starts_with("Recipient"), ExternalReference, DistributionChannel)) %>% 
  mutate(
    across(
      -c(
        ends_with("date"),
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
        ends_with("_item"),
        starts_with("StrokeIntervals"),
        starts_with("PasteTimestamps"),
        bot,
        Q_DuplicateRespondent), ~ suppressWarnings(as.numeric(as.character(.x)))))
#### end ####

#### create AI checks ####
df_runs <- df_runs %>% 
  mutate(
    mobileuser = as.numeric(str_detect(`metainfo_Operating Syst`, "iPhone") | str_detect(`metainfo_Operating Syst`, "Android"))
  )

# create dummy codes for failing certain checks
df_runs <- df_runs %>% 
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

table(df_runs$animal_lowercase)
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

table(df_runs$orange_lowercase)
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
df_runs <- df_runs %>% 
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
# create a new failed typing metric. fail if any of the typing metrics failed
df_runs <- df_runs %>%
  mutate(
    fail_typing = as.numeric(
      if_any(
        c(
          failtype_keystrokecountOJ,
          failtype_nostrokeintvlsOJ,
          failtype_keyspastesOJ
        ),
        ~ coalesce(.x, 0) == 1)))

# failed any Mindworks 0% and <2% check
df_runs <- df_runs %>%
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

df_runs <- df_runs %>%
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

#### clean df_runs ####
# get names of columns that are just NA
df_runs %>%
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
df_runs <- df_runs %>% 
  dplyr::select(-c(Status,
                   Q_RelevantIDDuplicate,
                   Q_RelevantIDLastStartDate
  ))

# rearrange order of fail criteria to the start of the DF, in order of effectiveness
df_runs <- df_runs %>% 
  dplyr::select(index, bot, run, Progress, consent, Finished, ResponseId, StartDate, EndDate, RecordedDate, duration, 
                IPAddress, starts_with("Location"), mobiledevice, mobileuser,
                starts_with("n_failed"), failedeffective,
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
                everything()) %>% 
  arrange(index)
#### end ####

#### Analyze data for table ####
# ——— Conditional on completing survey ———
df_runs_complete <- df_runs %>% 
  filter(Finished == 1) # n = 53

# How many bots remain?
n_distinct(df_runs_complete$bot) # 3

# How many failed ANY effective check? 
mean(df_runs_complete$failedeffective) # 100%

# Disaggregated effective check failure rate
# Typing
mean(df_runs_complete$fail_typing) # 100%
# Multi-part reverse shibbo
mean(df_runs_complete$fail_OJnutrition) # 100%
# Prompt inject (choice)
mean(df_runs_complete$fail_fakebrand) # 69.81%
# Prompt inject (text)
mean(df_runs_complete$fail_openendhealth) # 69.81%
# reCAPTCHA
mean(df_runs_complete$fail_captcha) # 71.69%

# Cumulative failure rate
df_runs_cumulative <- df_runs_complete %>% 
  # In order of table in the paper
  mutate(failed_top1_effective = if_else(fail_typing == 1, 1, 0),
         failed_top2_effective = if_else(failed_top1_effective == 1 | fail_OJnutrition == 1, 1, 0),
         failed_top3_effective = if_else(failed_top2_effective == 1 | fail_fakebrand == 1, 1, 0),
         failed_top4_effective = if_else(failed_top3_effective == 1 | fail_openendhealth == 1, 1, 0),
         failed_top5_effective = if_else(failed_top4_effective == 1 | fail_captcha == 1, 1, 0))

# Typing
mean(df_runs_cumulative$failed_top2_effective) # 100%

# Typing + Multi-part reverse shibboleth
mean(df_runs_cumulative$failed_top2_effective) # 100%

# Typing + Multi-part reverse shibboleth + Prompt inject (choice)
mean(df_runs_cumulative$failed_top3_effective) # 100%

# Typing + Multi-part reverse shibboleth + Prompt inject (choice) + Prompt inject (text)
mean(df_runs_cumulative$failed_top4_effective) # 100%

# Typing + Multi-part reverse shibboleth + Prompt inject (choice) + Prompt inject (text) + reCAPTCHA
mean(df_runs_cumulative$failed_top5_effective) # 100%

# ——— Unconditional on completion results ———
# Disaggregated effective check failure rate

# Typing - 100%
mean(df_runs$fail_typing)
# Multi-part reverse shibboleth - 100%
mean(df_runs$fail_OJnutrition, na.rm = T) 
# Prompt inject (choice) - 66.1%
mean(df_runs$fail_fakebrand, na.rm = T)
# Prompt inject (text) - 67.3% 
mean(df_runs$fail_openendhealth, na.rm = T)
# reCAPTCHA - 70.8%
mean(df_runs$fail_captcha, na.rm = T)
#### end ####
