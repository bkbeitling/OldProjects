library(tidyverse)

filtered2021TSwGRF <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/COVID Impact/Data for Noelle/filtered2021TSwGRF.rds")
surveyanalysis2018 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/COVID Impact/Data for Noelle/2018_Teacher_Survey_withGRF.rds")
surveyanalysis2019 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/COVID Impact/Data for Noelle/2019_Teacher_Survey_withGRF.rds")


#This function does a lot of the nonsense
#I know I could have gotten around my constant data naming switching by throwing it as a variable into
#the function but the function was originally created for a single year teacher survey and I didn't bother
#to fix for this instance, oops.
analyze1 = function(keys, co = FALSE, tab = FALSE){
  table <- surveyanalysis %>%
    select(Studentid, keys) %>%
    gather(key = "Statement", value = "endorse", -Studentid)
  table <- table %>%
    group_by(Statement, endorse) %>%
    summarize(n = length(Studentid)) %>%
    ungroup() %>%
    group_by(Statement) %>%
    mutate(pct = round(n/sum(n)*100,1))
  table <- table %>%
    gather(variable, value, -(Statement:endorse)) %>%
    unite(temp, endorse, variable) %>%
    spread(temp, value)
  if(co == TRUE & tab == FALSE){
    table$AorSA <- table$'3_n' + table$'4_n'
    table$AorSApct <- table$'3_pct' + table$'4_pct'
  }
  if(co == TRUE & tab == TRUE){
    table$AorSA <- table$'2_n' + table$'3_n'
    table$AorSApct <- table$'2_pct' + table$'3_pct'
  }
  view(table)
}

analyze2 = function(keys, co = FALSE, tab = FALSE){
  table <- surveyanalysis %>%
    select(Studentid, keys) %>%
    gather(key = "Statement", value = "endorse", -Studentid) %>%
    filter(!is.na(endorse))
  table <- table %>%
    group_by(Statement, endorse) %>%
    summarize(n = length(Studentid)) %>%
    ungroup() %>%
    group_by(Statement) %>%
    mutate(pct = round(n/sum(n)*100,1))
  table <- table %>%
    gather(variable, value, -(Statement:endorse)) %>%
    unite(temp, endorse, variable) %>%
    spread(temp, value)
  if(co == TRUE & tab == FALSE){
    table$AorSA <- table$'3_n' + table$'4_n'
    table$AorSApct <- table$'3_pct' + table$'4_pct'
  }
  if(co == TRUE & tab == TRUE){
    table$AorSA <- table$'2_n' + table$'3_n'
    table$AorSApct <- table$'2_pct' + table$'3_pct'
  }
  view(table)
}


surveyanalysis <- filtered2021TSwGRF

#Table 8
keys <- c("N_1_1", "N_1_2", "N_1_3", "N_1_4", "N_1_5", "N_1_6", "N_1_7")
analyze2(keys)

#Table 9
surveyanalysis <- filtered2021TSwGRF %>%
  mutate(N_1_1 = recode(N_1_1, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(N_1_2 = recode(N_1_2, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(N_1_3 = recode(N_1_3, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(N_1_4 = recode(N_1_4, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(N_1_5 = recode(N_1_5, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(N_1_6 = recode(N_1_6, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(N_1_7 = recode(N_1_7, `0` = 0L, `1` = 0L, `2` = 0L, `3` = 1L, `4` = 1L)) %>%
  mutate(ELrecode = recode(esolparticipationcode, `0` = 0L, `1` = 1L, `2` = 1L, `3` = 1L, `4` = 1L, `5` = 1L, `6` = 1L))


setbydemo = function(demo){
  if(demo != "band"){
    table <- surveyanalysis %>%
    select(Studentid, demo = demo, N_1_1, N_1_2, N_1_3, N_1_4, N_1_5, N_1_6, N_1_7) %>%
    pivot_longer(cols = N_1_1:N_1_7, names_to = "setting", values_to = "endorse") %>%
    filter(!is.na(endorse))
  table <- table %>%
    group_by(demo, setting, endorse) %>%
    summarize(n = length(Studentid)) %>%
    ungroup() %>%
    group_by(demo, setting) %>%
    mutate(pct = round(n/sum(n)*100,1))
  view(table)
  }
  if(demo == "band"){
    table <- surveyanalysis %>%
      select(Studentid, demo = demo, N_1_1, N_1_2, N_1_3, N_1_4, N_1_5, N_1_6, N_1_7) %>%
      pivot_longer(cols = N_1_1:N_1_7, names_to = "setting", values_to = "endorse") %>%
      filter(!is.na(endorse) & !is.na(demo))
    table <- table %>%
      group_by(demo, setting, endorse) %>%
      summarize(n = length(Studentid)) %>%
      ungroup() %>%
      group_by(demo, setting) %>%
      mutate(pct = round(n/sum(n)*100,1))
    view(table)
  }
}
setbydemo("gender")
setbydemo("comprehensiverace")
setbydemo("hispanicethnicity")
setbydemo("ELrecode")

surveyanalysis <- surveyanalysis %>%
  pivot_longer(cols = final_ela:final_sci, names_to = "subject", values_to = "band")
setbydemo("band")


#Table 10
surveyanalysis <- filtered2021TSwGRF
keys <- c("N_2_1", "N_2_2", "N_2_3", "N_2_4")
analyze2(keys)

#Table 11
keys <- "N_3"
analyze2(keys)

#Table 12
#2021
keys <- c("B3_2_1", "B3_2_2", "B3_2_3", "B3_2_4", "B3_2_5", "B3_2_6", "B3_2_7",
          "B3_2_8", "B3_2_9")
analyze2(keys)
#2019
surveyanalysis <- surveyanalysis2019
keys <- c("B3_1_1", "B3_1_2", "B3_1_3", "B3_1_4", "B3_1_5", "B3_1_6", "B3_1_7",
          "B3_1_8", "B3_1_9")
analyze2(keys)
#2018
surveyanalysis <- surveyanalysis2018
keys <- c("B3_1_1", "B3_1_2", "B3_1_3", "B3_1_4", "B3_1_5", "B3_1_6", "B3_1_7",
          "B3_1_8", "B3_1_9")
analyze2(keys)

#Table 13
#2021
surveyanalysis <- filtered2021TSwGRF
keys <- c("B6_2_1", "B6_2_2", "B6_2_3", "B6_2_4", "B6_2_5", "B6_2_6", "B6_2_7",
          "B6_2_8", "B6_2_9")
analyze2(keys)
#2019
surveyanalysis <- surveyanalysis2019
keys <- c("B4_1_1", "B4_1_2", "B4_1_3", "B4_1_4", "B4_1_5", "B4_1_6", "B4_1_7",
          "B4_1_8", "B4_1_9")
analyze2(keys)
#2018
surveyanalysis <- surveyanalysis2018
keys <- c("B4_1_1", "B4_1_2", "B4_1_3", "B4_1_4", "B4_1_5", "B4_1_6", "B4_1_7",
          "B4_1_8", "B4_1_9")
analyze2(keys)

#Table 14
#2021
surveyanalysis <- filtered2021TSwGRF %>%
  mutate(B7_2_1 = recode(B7_2_1, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_2 = recode(B7_2_2, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_3 = recode(B7_2_3, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_4 = recode(B7_2_4, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_5 = recode(B7_2_5, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_6 = recode(B7_2_6, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_7 = recode(B7_2_7, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_8 = recode(B7_2_8, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_9 = recode(B7_2_9, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_2_10 = recode(B7_2_10, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L))
keys <- c("B7_2_1", "B7_2_2", "B7_2_3", "B7_2_4", "B7_2_5", "B7_2_6", "B7_2_7",
          "B7_2_8", "B7_2_9", "B7_2_10")
analyze2(keys)
#2019
surveyanalysis <- surveyanalysis2019 %>%
  mutate(B5_1_1 = recode(B5_1_1, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_2 = recode(B5_1_2, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_3 = recode(B5_1_3, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_4 = recode(B5_1_4, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_5 = recode(B5_1_5, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_6 = recode(B5_1_6, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_7 = recode(B5_1_7, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_8 = recode(B5_1_8, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_9 = recode(B5_1_9, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_1_10 = recode(B5_1_10, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L))
keys <- c("B5_1_1", "B5_1_2", "B5_1_3", "B5_1_4", "B5_1_5", "B5_1_6", "B5_1_7",
          "B5_1_8", "B5_1_9", "B5_1_10")
analyze2(keys)
#2018; note bin change
surveyanalysis <- surveyanalysis2018 %>%
  mutate(B6_1_1 = recode(B6_1_1, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_2 = recode(B6_1_2, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_3 = recode(B6_1_3, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_4 = recode(B6_1_4, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_5 = recode(B6_1_5, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_6 = recode(B6_1_6, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_7 = recode(B6_1_7, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_8 = recode(B6_1_8, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_9 = recode(B6_1_9, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_1_10 = recode(B6_1_10, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L))
keys <- c("B6_1_1", "B6_1_2", "B6_1_3", "B6_1_4", "B6_1_5", "B6_1_6", "B6_1_7",
          "B6_1_8", "B6_1_9", "B6_1_10")
analyze2(keys)

#Table 15
#2021
surveyanalysis <- filtered2021TSwGRF %>%
  mutate(B7_3_1 = recode(B7_3_1, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_3_2 = recode(B7_3_2, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_3_3 = recode(B7_3_3, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_3_4 = recode(B7_3_4, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_3_5 = recode(B7_3_5, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_3_6 = recode(B7_3_6, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B7_3_7 = recode(B7_3_7, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L))
keys <- c("B7_3_1", "B7_3_2", "B7_3_3", "B7_3_4", "B7_3_5", "B7_3_6", "B7_3_7")
analyze2(keys)
#2019
surveyanalysis <- surveyanalysis2019 %>%
  mutate(B5_2_1 = recode(B5_2_1, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_2_2 = recode(B5_2_2, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_2_3 = recode(B5_2_3, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_2_4 = recode(B5_2_4, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_2_5 = recode(B5_2_5, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_2_6 = recode(B5_2_6, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L)) %>%
  mutate(B5_2_7 = recode(B5_2_7, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 1L, `4` = 2L))
keys <- c("B5_2_1", "B5_2_2", "B5_2_3", "B5_2_4", "B5_2_5", "B5_2_6", "B5_2_7")
analyze2(keys)
#2018; note bin change
surveyanalysis <- surveyanalysis2018 %>%
  mutate(B6_2_1 = recode(B6_2_1, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_2_2 = recode(B6_2_2, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_2_3 = recode(B6_2_3, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_2_4 = recode(B6_2_4, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_2_5 = recode(B6_2_5, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_2_6 = recode(B6_2_6, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L)) %>%
  mutate(B6_2_7 = recode(B6_2_7, `0` = 0L, `1` = 0L, `2` = 1L, `3` = 2L, `4` = 2L))
keys <- c("B6_2_1", "B6_2_2", "B6_2_3", "B6_2_4", "B6_2_5", "B6_2_6", "B6_2_7")
analyze2(keys)

#Table 16 was all Jennifer K via SPSS I think

#Table 17
#2021-- latter two categories collapsed in table
surveyanalysis <- filtered2021TSwGRF
keys <- "A_6"
analyze2(keys)
#2019
surveyanalysis <- surveyanalysis2019
keys <- "A_3"
analyze2(keys)
#2018
surveyanalysis <- surveyanalysis2018
keys <- "A_4"
analyze2(keys)

#Table 18
match2021 <- filtered2021TSwGRF %>%
  filter(Studentid %in% surveyanalysis2019$Studentid) %>%
  select(Studentid, response2021 = A_6) %>%
  unique()
match2019 <- surveyanalysis2019 %>%
  filter(Studentid %in% filtered2021TSwGRF$Studentid) %>%
  select(Studentid, response2019 = A_3) %>%
  unique()
matchall <- left_join(match2019, match2021, by = "Studentid")
matchall <- matchall %>%
  filter(!is.na(response2019) & !is.na(response2021)) %>%
  mutate(response2019 = recode(response2019, `1` = 1L, `2` = 1L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L)) %>%
  mutate(response2021 = recode(response2021, `1` = 1L, `2` = 1L, `3` = 3L, `4` = 4L, `5` = 5L, `6` = 6L, `7` = 6L)) %>%
  group_by(response2019, response2021) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(pct = round(count/sum(count)*100,1))

#Table 20
surveyanalysis <- filtered2021TSwGRF
keys <- c("N_5_1", "N_5_2", "N_5_3", "N_5_4", "N_5_5")
analyze2(keys)

#Table 21
#These numbers are slightly off in the current table-- just realized an issue
#Combine last category + NA category
surveyanalysis <- filtered2021TSwGRF %>%
  filter(N_5_2 == 1 | N_5_3 == 1 | N_5_4 == 1)
keys <- c("N_6_1", "N_6_2", "N_6_3", "N_6_4")
analyze1(keys)

#Table 22
#2021
surveyanalysis <- filtered2021TSwGRF
keys <- c("A_3")
analyze2(keys, co = TRUE) #last two columns
keys <- c("A_4_1", "A_4_2")
analyze2(keys, co = TRUE, tab = TRUE) #last two columns
#2019
surveyanalysis <- surveyanalysis2019
keys <- c("A_1_3", "A_1_4", "A_1_5")
analyze2(keys, co = TRUE, tab = TRUE) #last two columns
#2018
surveyanalysis <- surveyanalysis2018
keys <- c("A_2_3", "A_2_5", "A_2_6")
analyze2(keys, co = TRUE, tab = TRUE) #last two columns

#Table 23
#2021
surveyanalysis <- filtered2021TSwGRF
analyze3 = function(keys, co = FALSE, tab = FALSE){
  table <- surveyanalysis %>%
    select(Studentid, keys) %>%
    gather(key = "Statement", value = "endorse", -Studentid) %>%
    filter(!is.na(endorse) & !endorse == 4)
  table <- table %>%
    group_by(Statement, endorse) %>%
    summarize(n = length(Studentid)) %>%
    ungroup() %>%
    group_by(Statement) %>%
    mutate(pct = round(n/sum(n)*100,1))
  table <- table %>%
    gather(variable, value, -(Statement:endorse)) %>%
    unite(temp, endorse, variable) %>%
    spread(temp, value)
  if(co == TRUE & tab == FALSE){
    table$AorSA <- table$'3_n' + table$'4_n'
    table$AorSApct <- table$'3_pct' + table$'4_pct'
  }
  if(co == TRUE & tab == TRUE){
    table$AorSA <- table$'2_n' + table$'3_n'
    table$AorSApct <- table$'2_pct' + table$'3_pct'
  }
  view(table)
}
keys <- c("A_5_1", "A_5_3", "A_5_4")
analyze3(keys, co = TRUE, tab = TRUE) #last two columns
#2019
surveyanalysis <- surveyanalysis2019
keys <- c("A_2_1", "A_2_3", "A_2_4")
analyze3(keys, co = TRUE, tab = TRUE) #last two columns
#2018
surveyanalysis <- surveyanalysis2018
keys <- c("A_3_1", "A_3_3", "A_3_4")
analyze3(keys, co = TRUE, tab = TRUE) #last two columns
