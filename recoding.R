# rm(list=ls())
# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(plotKML)
library(stringr)

population <-c("adolescent","caregiver")[1]
write <-c("yes","no")[1]

# data --------------------------------------------------------------------
if (population == "adolescent"){
  hh <- read.csv("04 Data/01_adolescent/01_data_collection/daily_data/HH.csv",stringsAsFactors = FALSE, 
                 na.strings = c(""," ", "n/a",NA))
  indv <- read.csv("04 Data/01_adolescent/01_data_collection/daily_data/indv.csv",stringsAsFactors = FALSE, 
                 na.strings = c(""," ", "n/a",NA))
}

if (population == "caregiver"){
  hh <- read.csv("04 Data/02_caregiver/01_data_collection/daily_data/HH.csv",stringsAsFactors = FALSE, 
                 na.strings = c(""," ", "n/a",NA))
  indv <- read.csv("04 Data/02_caregiver/01_data_collection/daily_data/indv.csv",stringsAsFactors = FALSE, 
                   na.strings = c(""," ", "n/a",NA))
}

# colnames  ---------------------------------------------------------------
some_primary <- c("elementary_school_standard_1", "elementary_school_standard_2",
                  "elementary_school_standard_3","elementary_school_standard_4","madrasha")

primary_higher <- c("middle_school_standard_5", "middle_school_standard_6", "middle_school_standard_7", 
                    "middle_school_standard_8", "high_school_standard_9","high_school_standard_10",
                    "tertiary_education")

threats_safety_colnames<- c("threats_1","threats_2","threats_3","threats_6","threats_8","threats_9",
                            "threats_14")

adol_hh_chores_daily_life <- c("resp_activities_1","resp_activities_2","resp_activities_3","resp_activities_4",
                               "resp_activities_6")

adol_attend_centre_learning_daily_life <- c("resp_activities_7","resp_activities_8","resp_activities_9","resp_activities_10",
                                            "resp_activities_11")
seek_treat_r = c("seek_treat.public.ngo_clinic", "seek_treat.private_clinic", "seek_treat.traditional_healer")


# hh_to_hh ----------------------------------------------------------------

final_data_hh <- hh %>% mutate(
  i.hh_size_small = if_else(hh_size < 5, "yes","no",NULL),
  i.hh_size_large = if_else(hh_size > 4, "yes","no",NULL),
  i.threats_1_some_always =if_else(threats_1 == "sometimes_concerned" | threats_1 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_2_some_always =if_else(threats_2 == "sometimes_concerned" | threats_2 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_3_some_always =if_else(threats_3 == "sometimes_concerned" | threats_3 == 
                                     "always_concerned","yes","no",NULL), 
  
  i.threats_4_some_always =if_else(threats_4 == "sometimes_concerned" | threats_4 == 
                                     "always_concerned","yes","no",NULL),
  i.threats_5_some_always =if_else(threats_5 == "sometimes_concerned" | threats_5 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_6_some_always =if_else(threats_6 == "sometimes_concerned" | threats_6 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_7_some_always =if_else(threats_7 == "sometimes_concerned" | threats_7 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_8_some_always =if_else(threats_8 == "sometimes_concerned" | threats_8 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_9_some_always =if_else(threats_9 == "sometimes_concerned" | threats_9 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_10_some_always =if_else(threats_10 == "sometimes_concerned" | threats_10 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_11_some_always =if_else(threats_11 == "sometimes_concerned" | threats_11 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_12_some_always =if_else(threats_12 == "sometimes_concerned" | threats_12 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_13_some_always =if_else(threats_13 == "sometimes_concerned" | threats_13 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_14_some_always =if_else(threats_14 == "sometimes_concerned" | threats_14 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_harmful_practice = if_else(threats_4 == "yes" | threats_5 == "yes","yes","no",NULL),
  i.threats_accidental = if_else(threats_10 == "yes" | threats_11 == "yes"| threats_12 == "yes" ,"yes","no",NULL),
  
  threat_safety_na = rowSums(is.na(hh[threats_safety_colnames])),
  threat_safety_sum = rowSums(hh[threats_safety_colnames] == "yes",na.rm = T),
  threats_safety_t = if_else( threat_safety_na > 0,"",
                              if_else(threat_safety_sum > 0,"yes","no",NULL)),
  i.threats_safety = if_else(threats_safety_t != "", threats_safety_t,NULL,NULL)
  
)

if(population == "adolescent") {
  final_data_v2 <- final_data_hh %>% mutate(
    i.hh_no_formal_edu = if_else(edu_hoh == "none","yes","no",NULL),
    i.hh_some_primary = if_else(edu_hoh %in% some_primary,"yes","no",NULL),
    i.hh_primary_higher = if_else(edu_hoh %in% primary_higher,"yes","no",NULL),
    hh_chores_daily_life_rowsum = rowSums(hh[adol_hh_chores_daily_life] == "yes"), ##naaaa
    i.adol_hh_chores_daily_life = if_else(hh_chores_daily_life_rowsum >0 ,"yes","no",NULL),
    adol_attend_centre_learning_daily_life_rowsum = rowSums(hh[adol_attend_centre_learning_daily_life] == "yes"),
    i.adol_attend_centre_learning_daily_life = if_else(adol_attend_centre_learning_daily_life_rowsum >0 ,"yes","no",NULL),
    i.daily_act_chores_school = if_else(i.adol_hh_chores_daily_life=="yes" &
                                          i.adol_attend_centre_learning_daily_life == "yes","yes","no",NULL),
    i.daily_act_chores_work = if_else(i.adol_hh_chores_daily_life == "yes" & 
                                        resp_activities_14 == "yes","yes","no",NULL),
    i.daily_act_school_work = if_else(i.adol_attend_centre_learning_daily_life == "yes" &
                                        resp_activities_14 == "yes","yes","no",NULL),
    seek_treat_rowsum = rowSums(hh[seek_treat_r]),
    i.married.hc = if_else(resp_marr == "yes" & seek_treat_rowsum >0 ,"yes","no",NULL),
    i.married.edu = if_else(resp_marr == "yes" & access_lc == "yes","yes","no",NULL),
    i.married.cp = if_else(resp_marr == "yes" & access_cp == "yes","yes","no",NULL),
    i.working_hc = if_else(resp_activities_14 == "yes" & seek_treat_rowsum >0 ,"yes","no",NULL),
    i.working_edu =if_else(resp_activities_14 == "yes" & access_lc == "yes","yes","no",NULL),
    i.working_cp = if_else(resp_activities_14 == "yes" & access_cp == "yes","yes","no",NULL),
    i.adol_not_seek_health_treatment_safety = if_else(not_seek_treat.not_feel_comfortable_at_the_health_center== "yes" |
                                                        not_seek_treat.not_feel_comfortable == "yes","yes","no",NULL),
    i.adol_not_seek_services_safety = if_else( i.adol_not_seek_health_treatment_safety == "yes"| 
                                               child_protection_services.comfortable_walking_to_the_CFS.AFS == "yes"|
                                                 attend_a_learning_center.comfortable_walking_to_the_learning_center == "yes",
                                               "yes","no",NULL),
                                               
    i.adol_felt_unsafe_any_service = if_else(if_challenges_hc.i_didn.t_feel_safe == "yes" |
                                              challenges_lc.i_didnt_feel_safe == "yes"|
                                              challenges_cp.didnt_feel_safe == "yes","yes","no",NULL)
    
  )
}

if(population == "caregiver") {
  final_data_v2 <- final_data_hh %>% mutate(
    i.hh_no_formal_edu = if_else(resp_high_level_edu == "none","yes","no",NULL),
    i.hh_some_primary = if_else(resp_high_level_edu %in% some_primary,"yes","no",NULL),
    i.hh_primary_higher = if_else(resp_high_level_edu %in% primary_higher,"yes","no",NULL),
  )
}

# HH_TO_INDV --------------------------------------------------------------

join_data <- hh %>% left_join(indv,by = c("X_uuid"="X_submission__uuid"))

hh_to_indv <- join_data %>% group_by(X_uuid) %>% summarise(
  i.hh_baby = if_else(any(ind_age >1 ),"yes","no",NULL),
  i.hh_child = if_else(any(ind_age %in% 3:5),"yes","no",NULL),
  i.hh_adol = if_else(any(ind_age %in% 6:12),"yes","no",NULL),
  i.hh_older_adol = if_else(any(ind_age %in% 13:17),"yes","no",NULL)
)

# combind_data -----------------------------------------------------------------

final_combind <- final_data_v2 %>% left_join( hh_to_indv,by = c("X_uuid"))

# write_csv ---------------------------------------------------------------


if (write == "yes"){
    write.csv(final_data_v2,paste0("Output/",population,"_recoding_HH.csv"))
    write.csv(hh_to_indv,paste0("Output/",population,"_recoding_HH_INDV.csv"))
    write.csv(final_combind,paste0("Output/",population,"_recoding_final_combind.csv"))
  }
  
