library(tidyverse)
library(openxlsx)
Batch <- "Batch1"
rundate <- "20230525"
year <- "2023"
update <- "yes" #use "yes" if running post 2 week review; ensure Deliverable Files location contains copies of all GRFs first (originals
#for states without updates, updated files for states with updates)



## read data for QC count or if all GRFs original
GRF <- readRDS(paste0("S:/Projects/DLM Secure/0-All Content/Scoring/", year, "/GRF/", Batch, "/", rundate, "/Internal DLM GRF/Full_", Batch, "_GRF_", rundate, ".rds"))

## create count of ISRs to be QCed manually
QCcount <- GRF %>%
  select(State, Subject, Current_Grade_Level) %>%
  group_by(State, Subject, Current_Grade_Level) %>%
  summarize(QCcount = ceiling(.1*n())) %>%
  pivot_wider(names_from = "State", values_from = "QCcount") %>%
  arrange(Subject, Current_Grade_Level)
ifelse("Wisconsin" %in% GRF$State | "Delaware" %in% GRF$State, 
       QCsummarycount <- GRF %>%
         select(State, Current_Grade_Level) %>%
         filter(State %in% c("Wisconsin", "Delaware")) %>%
         group_by(State, Current_Grade_Level) %>%
         summarize(QCcount = ceiling(.1*n())) %>%
         pivot_wider(names_from = "State", values_from = "QCcount") %>%
         arrange(Current_Grade_Level),
       "")

saveRDS(QCcount, paste0("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Score Report QC/", year, " QC Counts/", Batch, ".rds"))


## if GRF updates use the below ensuring all state GRFs (original and updated) are saved to the file path
if(update == "yes"){
setwd(paste0("S:/Projects/DLM Secure/0-All Content/Deliverable Files/", year, "/Updated (not final) GRFs/", Batch))
files <- list.files(pattern=".xlsx")
## read data using loop
GRF <- NULL
for (f in files) {
  dat <- read.xlsx(f, colNames = TRUE)
  dat <- dat %>%
    select(Unique_Row_Identifier, Kite_Student_Identifier, State, Attendance_District_Identifier, Attendance_School_Identifier, 
           Kite_Educator_Identifier, Subject, Invalidation_Code, Performance_Level)
  GRF <- rbind(GRF, dat)
}
}

## counts to check against uploads in EP
### this should match GRF rows uploaded
statecount <- GRF %>%
  select(State) %>%
  group_by(State) %>%
  summarize(statecount = n())
### should match state + district aggregate reports generated on GRF approval
stadistcount <- GRF %>%
  filter(!Performance_Level == 9 & !Invalidation_Code == 1) %>%
  select(State, Attendance_District_Identifier) %>%
  unique() %>%
  group_by(State) %>%
  summarize(stadistcount = n() + 1)
### should match school aggregate reports generated on GRF approval
schoolcount <- GRF %>%
  filter(!Invalidation_Code == 1) %>%
  select(State, Attendance_School_Identifier) %>%
  unique() %>%
  group_by(State) %>%
  summarize(schoolcount = n())
### should match class aggregate reports generated on GRF approval
classcount <- GRF %>%
  filter(!Invalidation_Code == 1) %>%
  select(State, Attendance_School_Identifier, Kite_Educator_Identifier) %>%
  unique() %>%
  select(-Attendance_School_Identifier) %>%
  group_by(State) %>%
  summarize(classcount = n())

## load DCPS file to remove from DE ISR counts and calculate DCPS report numbers
ifelse("Delaware" %in% GRF$State, 
  DCPS <- readRDS(paste0("S:/Projects/DLM Secure/0-All Content/Scoring/", year, "/GRF/", Batch, "/", rundate, "/Internal DLM GRF/de_score_reporting-delaware_dcps.rds")),
  DCPS <- as.data.frame("test")
)


## counts of ISRs by state; should match number uploaded for normal ISRs
ISRcount <- GRF %>%
  filter(!Subject == "SS" & Invalidation_Code == 0 & !Kite_Student_Identifier %in% DCPS$Kite_Student_Identifier) %>%
  select(State) %>%
  group_by(State) %>%
  summarize(isrcount = n())
## load OK SS file to calculate OK SS report numbers
ifelse("Oklahoma" %in% GRF$State,
       OKSS <- readRDS(paste0("S:/Projects/DLM Secure/0-All Content/Scoring/", year, "/GRF/", Batch, "/", rundate, "/Internal DLM GRF/Score Report Files/oklahoma-ss-", rundate, ".rds")),
       OKSS <- as.data.frame("test")
)
## counts of special reports follow starting with summary reports and followed by DCPS and OK SS
## these should match special reports uploaded in EP
summaryreportscount <- GRF %>%
  filter(State %in% c("Delaware", "Wisconsin") & Invalidation_Code == 0 & !Kite_Student_Identifier %in% DCPS$Kite_Student_Identifier) %>%
  select(State, Kite_Student_Identifier) %>%
  unique() %>%
  group_by(State) %>%
  summarize(sumrepcount = n())
specialreportscount <- summaryreportscount
specialreportscount$DCPScount <- NA
OK <- c("Oklahoma", NA, NA)
specialreportscount$OKSScount <- NA
specialreportscount <- rbind(specialreportscount, OK)
for(i in 1:nrow(specialreportscount)){
  if(specialreportscount$State[i] == "Delaware"){
    specialreportscount$DCPScount[i] <- (nrow(DCPS)/3)*2
  }
  if(specialreportscount$State[i] == "Oklahoma"){
    specialreportscount$OKSScount[i] <- nrow(OKSS)
  }
}
  

## extra qc step-- check ISR numbers above against report numbers from index files
setwd(paste0("S:/Projects/KITE Reports ", year, "/DLM/Score Reports/Student"))
files <- list.files(pattern=".csv")
## read data using loop
index <- NULL
for (f in files) {
  dat <- read.csv(f, header=T)
  index <- rbind(index, dat)
}

ISRindexcount <- index %>%
  select(abb = State) %>%
  group_by(abb) %>%
  summarize(indexcount = n())
states <- cbind(state.name, state.abb)
colnames(states) <- c("State", "abb")
ISRindexcount <- left_join(ISRindexcount, as.data.frame(states), by = "abb")
for(i in 1:nrow(ISRindexcount)){
if(ISRindexcount$abb[i] == "DC"){
  ISRindexcount$State[i] <- "District of Columbia"
}
}
ISRindexcount <- ISRindexcount %>%
  select(-abb)

## combine all of the counts into one file per batch
fullcount <- left_join(statecount, stadistcount, by = "State")
fullcount <- left_join(fullcount, schoolcount, by = "State")
fullcount <- left_join(fullcount, classcount, by = "State")
fullcount <- left_join(fullcount, ISRcount, by = "State")
fullcount <- left_join(fullcount, ISRindexcount, by = "State")
fullcount <- left_join(fullcount, specialreportscount)
rm(statecount, stadistcount, schoolcount, classcount, ISRcount, ISRindexcount, dat, states, summaryreportscount, specialreportscount)
fullcount$ISRdiff <- fullcount$isrcount - fullcount$indexcount
view(fullcount)

saveRDS(fullcount, paste0("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Score Report QC/", year, " Expected Counts/", Batch, ".rds"))
