library(tidyverse)
library(openxlsx)
library(RSQLite)

FC2013 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2013/First_Contact_Census_6_4_2013.csv")
FC2014 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2014/Assessment ID 3 fc Data_062014.csv")
FC2015 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2015/fcs_tiny_data_2015.csv")
FC2016 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2016/fcs_tiny_data_2016.csv")
FC2017 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2017/fcs_tiny_data_2017.csv")
FC2018 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2018/fcs_tiny_data_2018.csv")
FC2019 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2019/fcs_tiny_data_2019.csv")
#FC2020 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2020/fcs_tiny_data_2020.csv")
#FC2021 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2021/fcs_tiny_data_2021.csv")
#FC2022 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2022/fcs_tiny_data_2022.csv")

#Pull in State data

##2014
state2014 <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Data/2014/Assessment ID 3 Student Records from 2014-02-17 to 2014-03-01_FULL.csv")
state2014 <- state2014 %>%
  filter(!grepl("Teacher Survey", state2014$Form.name, fixed = TRUE)) %>%
  filter(!State == "CETE Organization") %>%
  select(Studentid, State) %>%
  unique()
FC2014 <- left_join(FC2014, state2014, by = "Studentid")
rm(state2014)

##2015
sqlite <- dbDriver("SQLite")
dlmdb_connection <- dbConnect(sqlite, "S:/CETE/Psychometric Data/DLM/PII/archive_2015/dlm2015.sqlite")
state2015 <- dbGetQuery(dlmdb_connection, "select student.id, student.state
                      from student;")
colnames(state2015) <- c("studentid", "State")
state2015 <- state2015 %>%
  filter(!State == "AMP QC State") %>%
  unique()
FC2015 <- left_join(FC2015, state2015, by = "studentid")
rm(state2015)

##2016
dlmdb_connection <- dbConnect(sqlite, "S:/CETE/Psychometric Data/DLM/PII/archive_2016/dlm2016.sqlite")
state2016 <- dbGetQuery(dlmdb_connection, "select student.id, student.state
                      from student;")
colnames(state2016) <- c("studentid", "State")
state2016 <- state2016 %>%
  filter(!State == "AMP QC State") %>%
  unique()
FC2016 <- left_join(FC2016, state2016, by = "studentid")
rm(state2016)

##2017
dlmdb_connection <- dbConnect(sqlite, "S:/CETE/Psychometric Data/DLM/PII/archive_2017/dlm2017.sqlite")
state2017 <- dbGetQuery(dlmdb_connection, "select student.id, student.state
                      from student;")
colnames(state2017) <- c("studentid", "State")
state2017 <- state2017 %>%
  filter(!State == "AMP QC State") %>%
  unique()
FC2017 <- left_join(FC2017, state2017, by = "studentid")
rm(state2017)

##2018
dlmdb_connection <- dbConnect(sqlite, "S:/CETE/Psychometric Data/DLM/PII/archive_2018/dlm2018.sqlite")
state2018 <- dbGetQuery(dlmdb_connection, "select student.id, student.state
                      from student;")
colnames(state2018) <- c("studentid", "State")
state2018 <- state2018 %>%
  filter(!State == "AMP QC State") %>%
  unique()
FC2018 <- left_join(FC2018, state2018, by = "studentid")
rm(state2018)

##2019
dlmdb_connection <- dbConnect(sqlite, "S:/CETE/Psychometric Data/DLM/PII/archive_2019/dlm2019.sqlite")
state2019 <- dbGetQuery(dlmdb_connection, "select student.id, student.state
                      from student;")
colnames(state2019) <- c("studentid", "State")
state2019 <- state2019 %>%
  filter(!State == "AMP QC State") %>%
  unique()
FC2019 <- left_join(FC2019, state2019, by = "studentid")
rm(state2019)

##Save data
saveRDS(FC2013, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2013.rds")
saveRDS(FC2014, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2014.rds")
saveRDS(FC2015, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2015.rds")
saveRDS(FC2016, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2016.rds")
saveRDS(FC2017, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2017.rds")
saveRDS(FC2018, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2018.rds")
saveRDS(FC2019, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2019.rds")


#Analysis
FC2013 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2013.rds")
FC2014 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2014.rds")
FC2015 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2015.rds")
FC2016 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2016.rds")
FC2017 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2017.rds")
FC2018 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2018.rds")
FC2019 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/FC2019.rds")

##Number of states per year
analysis <- FC2013 %>%
  select(V1, Q13_1) %>%
  unique() %>%
  group_by(Q13_1) %>%
  summarize(count = n())

statefun = function(dat, id){
  analysis <<- dat %>%
    select(id, State) %>%
    filter(!grepl("DLM QC", State, fixed = TRUE) & !grepl("Playground", State, fixed = TRUE) & 
             !grepl("Flatland", State, fixed = TRUE) & !grepl("KAP", State, fixed = TRUE)) %>%
    unique() %>%
    group_by(State) %>%
    summarize(count = n())
  view(analysis)
}

statefun(FC2014, "Studentid")
statefun(FC2015, "studentid")
statefun(FC2016, "studentid")
statefun(FC2017, "studentid")
statefun(FC2018, "studentid")
statefun(FC2019, "studentid")


## Clean bad states from data
FC2013 <- FC2013 %>%
  filter(!is.na(Q13_1) & !Q13_1 == 15)

cleanfun = function(dat){
  dat <<- dat %>%
    filter(!is.na(State) & !grepl("DLM QC", State, fixed = TRUE) & !grepl("Playground", State, fixed = TRUE) & 
             !grepl("Flatland", State, fixed = TRUE) & !grepl("KAP", State, fixed = TRUE)) %>%
    unique()
}

cleanfun(FC2014)
FC2014 <- dat
cleanfun(FC2015)
FC2015 <- dat
cleanfun(FC2016)
FC2016 <- dat
cleanfun(FC2017)
FC2017 <- dat
cleanfun(FC2018)
FC2018 <- dat
cleanfun(FC2019)
FC2019 <- dat
rm(dat)


FC2013$Q13_1 <- recode(FC2013$Q13_1, `1` = "Iowa", `2` = "Kansas", `3` = "Michigan", `4` = "Mississippi", 
         `5` = "Missouri", `6` = "New Jersey", `7` = "North Carolina", `8` = "Oklahoma", 
         `9` = "Utah", `10` = "Vermont", `11` = "Virginia", `12` = "Washington", 
         `13` = "West Virginia", `14` = "Wisconsin")


##Primary disability per year
fulldisfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, Q16 = qid) %>%
    filter(!is.na(Q16) & !Q16 == 91) %>%
    unique() %>%
    group_by(Q16) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  view(analysis)
}

fulldisfun(FC2013, "Q16_1", "V1")
fulldisfun(FC2014, "Q16_1", "Studentid")
fulldisfun(FC2015, "Q16_1", "studentid")
fulldisfun(FC2016, "Q16_1", "studentid")
fulldisfun(FC2017, "q16", "studentid")
fulldisfun(FC2018, "q16", "studentid")
fulldisfun(FC2019, "Q16", "studentid")

##Primary disability by state and year
statedisfun = function(dat, qid, state, stuid){
  states <<- dat %>%
    select(State = state) %>%
    unique() %>%
    arrange(State)
  tableall <<- as.data.frame(1:16)
  colnames(tableall) <<- "Q16"
  for(i in 1:nrow(states)){
     tablemore <<- dat %>%
       select(studentid = stuid, Q16 = qid, State = state) %>%
       filter(!is.na(Q16) & !Q16 == 91 & State == states[i,1]) %>%
       unique() %>%
       group_by(Q16) %>%
       summarize(count = n()) %>%
       ungroup() %>%
       mutate(pct = round(count/sum(count)*100,1))
     tablemore$state <<- states[i,1]
     tableall <<- full_join(tableall, tablemore, by = "Q16")
  if(i == nrow(states)){
    tableall <<- tableall %>%
      bind_rows(summarize(., across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
                          across(where(is.character), ~"Total")))
  }
    else{}
  }
}

statedisfun(FC2013, "Q16_1", "Q13_1", "V1")
statedisfun(FC2014, "Q16_1", "State", "Studentid")
statedisfun(FC2015, "Q16_1", "State", "studentid")
statedisfun(FC2016, "Q16_1", "State", "studentid")
statedisfun(FC2017, "q16", "State", "studentid")
statedisfun(FC2018, "q16", "State", "studentid")
statedisfun(FC2019, "Q16", "State", "studentid")

rm(tablemore)

##These previous tables were ridiculous and unwieldy, exploring other options
FC2013$year <- "01/01/2013"
FC2014$year <- "01/01/2014"
FC2015$year <- "01/01/2015"
FC2016$year <- "01/01/2016"
FC2017$year <- "01/01/2017"
FC2018$year <- "01/01/2018"
FC2019$year <- "01/01/2019"

disdata <- FC2013 %>%
  select(studentid = V1, year, Q16 = Q16_1, State = Q13_1) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata$studentid <- as.character(disdata$studentid)

temp <- FC2014 %>%
  select(studentid = Studentid, year, Q16 = Q16_1, State) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata <- rbind(disdata, temp)

temp <- FC2015 %>%
  select(studentid, year, Q16 = Q16_1, State) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata <- rbind(disdata, temp)

temp <- FC2016 %>%
  select(studentid, year, Q16 = Q16_1, State) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata <- rbind(disdata, temp)

temp <- FC2017 %>%
  select(studentid, year, Q16 = q16, State) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata <- rbind(disdata, temp)

temp <- FC2018 %>%
  select(studentid, year, Q16 = q16, State) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata <- rbind(disdata, temp)

temp <- FC2019 %>%
  select(studentid, year, Q16, State) %>%
  filter(!is.na(Q16) & !Q16 == 91) %>%
  unique()

disdata <- rbind(disdata, temp)

disdata$State <- recode(disdata$State, "BIE-Choctaw" = "BIE", "BIE-Miccosukee" = "BIE", "Miccosukee Indian School" = "BIE")

disdata1 <- disdata %>%
  filter(as.Date(year, "%m/%d/%Y") < as.Date("01/01/2017", "%m/%d/%Y"))
disdata2 <- disdata %>%
  filter(as.Date(year, "%m/%d/%Y") > as.Date("01/01/2016", "%m/%d/%Y"))

disdata1$Q16 <- recode(disdata1$Q16, `1` = "Autism", `2` = "Deaf-blindness", `3` = "Developmental delay", 
                             `4` = "Emotional disturbance", `5` = "Hearing impairment", `6` = "Intellectual disability", 
                             `7` = "Multiple disabilities", `8` = "Orthopedic impairment", `9` = "Other health impairment", 
                             `10` = "Specific learning disability", `11` = "Speech or language impairment", 
                             `12` = "Traumatic brain injury", `13` = "Visual impairment", `14` = "Non-categorical", 
                             `15` = "Deafness")
disdata2$Q16 <- recode(disdata2$Q16, `1` = "Autism", `2` = "Deaf-blindness", `3` = "Deafness", `4` = "Developmental delay", 
                             `5` = "Emotional disturbance", `6` = "Hearing impairment", `7` = "Intellectual disability", 
                             `8` = "Multiple disabilities", `9` = "Orthopedic impairment", `10` = "Other health impairment", 
                             `11` = "Specific learning disability", `12` = "Speech or language impairment", 
                             `13` = "Traumatic brain injury", `14` = "Visual impairment", `15` = "Non-categorical", 
                             `16` = "Eligible individual")
disdata <- rbind(disdata1, disdata2)

rm(disdata1, disdata2, temp)

saveRDS(disdata, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/disdata.rds")
write.xlsx(disdata, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Final Data/disdata.xlsx")


###GIF for primary disability by state?
library(ggplot2)
library(gganimate)
library(RColorBrewer)

####Pretty up the data for plotting
disdatasum <- disdata %>%
  select(year, State, Q16) %>%
  group_by(year, State, Q16) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year, State) %>%
  mutate(pct = round(count/sum(count)*100,1)) 

####Split dataset so colors are somewhat distinguishable; remove states with only one year of data
disdatasumbig <- disdatasum %>%
  filter(Q16 %in% c("Autism", "Developmental delay", "Intellectual disability", "Multiple disabilities", 
                    "Other health impairment", "Specific learning disability", "Speech or language impairment", "Eligible individual")) %>%
  filter(!State %in% c("Arkansas", "District of Columbia", "Pennsylvania", "Washington"))
disdatasumsm <- disdatasum %>%
  filter(Q16 %in% c("Deaf-blindness", "Emotional disturbance", "Hearing impairment", "Non-categorical", 
                    "Orthopedic impairment", "Traumatic brain injury", "Visual impairment", "Deafness")) %>%
  filter(!State %in% c("Arkansas", "District of Columbia", "Pennsylvania", "Washington"))

####Line graphs for each
ggplot(disdatasumbig, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  guides(colour = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/fulllinebig.png", width = 10, height = 8, device = "png")
ggplot(disdatasumsm, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  guides(colour = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/fulllinesm.png", width = 10, height = 8, device = "png")


####Limit to only states with data in 2018 and 2019 (at minimum)
disdatlatter <- disdatasum %>%
  filter(State %in% c("Alaska", "Colorado", "Delaware", "Illinois", "Iowa", "Kansas", "Maryland",
                      "Missouri", "New Hampshire", "New Jersey", "New York", "North Dakota", 
                      "Oklahoma", "Rhode Island", "Utah", "West Virginia", "Wisconsin"))

####Split states by waiver/exceed cap status
disdatlatter1 <- disdatlatter %>%
  filter(State %in% c("Alaska", "Iowa", "New Hampshire", "Utah"))
disdatlatter2 <- disdatlatter %>%
  filter(State %in% c("Colorado", "Kansas", "Maryland", "Missouri", "North Dakota", "Wisconsin"))
disdatlatter3 <- disdatlatter %>%
  filter(State %in% c("Delaware", "Illinois", "New Jersey", "New York", "Oklahoma", "Rhode Island", "West Virginia"))

####Add variable for break between waiver/exceed cap status years
disdatlatter1$yearbreak <- as.Date("07/01/2017", "%m/%d/%Y")
disdatlatter2$yearbreak <- as.Date("07/01/2018", "%m/%d/%Y")
disdatlatter3$yearbreak <- as.Date("07/01/2019", "%m/%d/%Y")

disdatlatter <- rbind(disdatlatter1, disdatlatter2, disdatlatter3)
rm(disdatlatter1, disdatlatter2, disdatlatter3)

####Split dataset so colors are somewhat distinguishable
disdatlatterbig <- disdatlatter %>%
  filter(Q16 %in% c("Autism", "Developmental delay", "Intellectual disability", "Multiple disabilities", 
                    "Other health impairment", "Specific learning disability", "Speech or language impairment", "Eligible individual"))
disdatlattersm <- disdatlatter %>%
  filter(Q16 %in% c("Deaf-blindness", "Emotional disturbance", "Hearing impairment", "Non-categorical", 
                    "Orthopedic impairment", "Traumatic brain injury", "Visual impairment", "Deafness"))

####Make plots
ggplot(disdatlatterbig, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("07/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = yearbreak), linetype="longdash", color = "blue") + 
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/latterlinebig.png", width = 10, height = 8, device = "png")
ggplot(disdatlattersm, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("07/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = yearbreak), linetype="longdash", color = "blue") + 
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/latterlinesm.png", width = 10, height = 8, device = "png")


####Limit data to states in ALL data years 2013-2019
disdatallyear <- disdatasum %>%
  filter(State %in% c("Kansas", "Iowa", "Missouri", "New Jersey", "Oklahoma", "Utah", "West Virginia", "Wisconsin"))

####Create gif showing primary disability categorizations by state through the years
allani <- ggplot(disdatallyear, aes(Q16, pct, fill = Q16)) +
  geom_bar(stat = "identity") +
  facet_wrap(~State) +
  labs(title = 'Year: {frame_time}', x = '', y = 'Percent') +
  transition_time(as.integer(year)) +
  theme(axis.text.x=element_blank()) + 
  guides(fill = guide_legend(title = "Primary Disability")) +
  view_follow(fixed_x = TRUE, fixed_y = TRUE)

animate(allani, height = 800, width = 800)
anim_save("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/pdallyearsanimate.gif")

####Split data by waiver/exceed status
disdatallyear1 <- disdatallyear %>%
  filter(State %in% c("Iowa", "Utah"))
disdatallyear2 <- disdatallyear %>%
  filter(State %in% c("Kansas", "Missouri", "Wisconsin"))
disdatallyear3 <- disdatallyear %>%
  filter(State %in% c("New Jersey", "Oklahoma", "West Virginia"))

####Assign year break by waiver/exceed status
disdatallyear1$yearbreak <- as.Date("07/01/2017", "%m/%d/%Y")
disdatallyear2$yearbreak <- as.Date("07/01/2018", "%m/%d/%Y")
disdatallyear3$yearbreak <- as.Date("07/01/2019", "%m/%d/%Y")

disdatallyear <- rbind(disdatallyear1, disdatallyear2, disdatallyear3)
rm(disdatallyear1, disdatallyear2, disdatallyear3)

####Split data by most/least used codes for color aesthetics
disdatallyearbig <- disdatallyear %>%
  filter(Q16 %in% c("Autism", "Developmental delay", "Intellectual disability", "Multiple disabilities", 
                    "Other health impairment", "Specific learning disability", "Speech or language impairment", "Eligible individual"))
disdatallyearsm <- disdatallyear %>%
  filter(Q16 %in% c("Deaf-blindness", "Emotional disturbance", "Hearing impairment", "Non-categorical", 
                    "Orthopedic impairment", "Traumatic brain injury", "Visual impairment", "Deafness"))

####Create plots
ggplot(disdatallyearbig, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") +
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("07/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = yearbreak), linetype="longdash", color = "blue") + 
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/alllinebig.png", width = 10, height = 8, device = "png")
ggplot(disdatallyearsm, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("07/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = yearbreak), linetype="longdash", color = "blue") + 
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/alllinesm.png", width = 10, height = 8, device = "png")


####Limit data to states in ALL DLM operational years and remove 2013 and 2014
disdatopyears <- disdatasum %>%
  filter(State %in% c("Alaska", "Colorado", "Illinois", "Iowa", "Kansas", "Missouri", 
                      "New Hampshire", "New Jersey", "North Dakota", "Oklahoma", 
                      "Utah", "West Virginia", "Wisconsin")) %>%
  filter(!year == "2013" & !year == "2014")

####Create gif for primary disability code in all operational years by states over time
opani <- ggplot(disdatopyears, aes(Q16, pct, fill = Q16)) +
  geom_bar(stat = "identity") +
  facet_wrap(~State) +
  labs(title = 'Year: {frame_time}', x = '', y = 'Percent') +
  transition_time(as.integer(year)) +
  theme(axis.text.x=element_blank()) + 
  guides(fill = guide_legend(title = "Primary Disability")) +
  view_follow(fixed_x = TRUE, fixed_y = TRUE)

animate(opani, height = 800, width = 800)
anim_save("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/pdopyearsanimate.gif")

####Separate states by exceed/waier status
disdatopyears1 <- disdatopyears %>%
  filter(State %in% c("Alaska", "Iowa", "New Hampshire", "Utah"))
disdatopyears2 <- disdatopyears %>%
  filter(State %in% c("Colorado", "Kansas", "Missouri", "North Dakota", "Wisconsin"))
disdatopyears3 <- disdatopyears %>%
  filter(State %in% c("Illinois", "New Jersey", "Oklahoma", "West Virginia"))

####Add year breaks for exceed/waiver status to data
disdatopyears1$yearbreak <- as.Date("07/01/2017", "%m/%d/%Y")
disdatopyears2$yearbreak <- as.Date("07/01/2018", "%m/%d/%Y")
disdatopyears3$yearbreak <- as.Date("07/01/2019", "%m/%d/%Y")

disdatopyears <- rbind(disdatopyears1, disdatopyears2, disdatopyears3)
rm(disdatopyears1, disdatopyears2, disdatopyears3)

####Separate data by primary disability code use to make colors distinguishable 
disdatopyearsbig <- disdatopyears %>%
  filter(Q16 %in% c("Autism", "Developmental delay", "Intellectual disability", "Multiple disabilities", 
                    "Other health impairment", "Specific learning disability", "Speech or language impairment", "Eligible individual"))
disdatopyearssm <- disdatopyears %>%
  filter(Q16 %in% c("Deaf-blindness", "Emotional disturbance", "Hearing impairment", "Non-categorical", 
                    "Orthopedic impairment", "Traumatic brain injury", "Visual impairment", "Deafness"))

####Make plots
ggplot(disdatopyearsbig, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2015", "%m/%d/%Y"), as.Date("07/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = yearbreak), linetype="longdash", color = "blue") + 
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/oplinebig.png", width = 10, height = 8, device = "png")
ggplot(disdatopyearssm, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  facet_wrap(~State) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2015", "%m/%d/%Y"), as.Date("07/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = yearbreak), linetype="longdash", color = "blue") + 
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/oplinesm.png", width = 10, height = 8, device = "png")


####Clean-up
rm(allani, opani, disdatallyear, disdatallyearbig, disdatallyearsm, disdatasum, disdatasumbig, disdatasumsm, disdatlatter, 
   disdatlatterbig, disdatlattersm, disdatopyears, disdatopyearsbig, disdatopyearssm)


####Overall disability plot
disdataoverallsum <- disdata %>%
  select(year, Q16) %>%
  group_by(year, Q16) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = round(count/sum(count)*100,1))

####Separate data by primary disability code use to make colors distinguishable 
disdataoverallsumbig <- disdataoverallsum %>%
  filter(Q16 %in% c("Autism", "Developmental delay", "Intellectual disability", "Multiple disabilities", 
                    "Other health impairment", "Specific learning disability", "Speech or language impairment", "Eligible individual"))
disdataoverallsumsm <- disdataoverallsum %>%
  filter(Q16 %in% c("Deaf-blindness", "Emotional disturbance", "Hearing impairment", "Non-categorical", 
                    "Orthopedic impairment", "Traumatic brain injury", "Visual impairment", "Deafness"))

####Make plots
ggplot(disdataoverallsumbig, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("12/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = as.Date("07/01/2017", "%m/%d/%Y")), linetype="longdash", color = "firebrick1", size = 1) + 
  geom_vline(aes(xintercept = as.Date("07/01/2018", "%m/%d/%Y")), linetype="longdash", color = "firebrick3", size = 1) +
  geom_vline(aes(xintercept = as.Date("07/01/2019", "%m/%d/%Y")), linetype="longdash", color = "firebrick4", size = 1) +
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2") +
  geom_label(label="ESSA legislation passed", x=as.Date("12/10/2015", "%m/%d/%Y"), y=40, label.padding = unit(0.25, "lines"),
    label.size = 0.35, color = "black", fill="white") + 
  geom_label(label="Waivers/enforcement \n of 1% cap from \n this point forward", x=as.Date("07/01/2017", "%m/%d/%Y"), y=40, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  geom_label(label="15/19 states \n request waiver \n or exceed cap \n in 2017-2018 \n (excludes BIE)", x=as.Date("07/01/2018", "%m/%d/%Y"), y=40, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  geom_label(label="9/19 states \n request waiver \n or exceed cap \n in 2018-2019 \n (excludes BIE)", x=as.Date("07/01/2019", "%m/%d/%Y"), y=40, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  theme(text = element_text(size = 16)) 
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/overallpdbig.png", device = "png")
ggplot(disdataoverallsumsm, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q16)) + 
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("12/31/2019", "%m/%d/%Y")) +
  ylim(0,10) +
  guides(colour = guide_legend(title = "Primary Disability"), linetype = guide_legend(title = "Primary Disability")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = as.Date("07/01/2017", "%m/%d/%Y")), linetype="longdash", color = "firebrick1", size = 1) + 
  geom_vline(aes(xintercept = as.Date("07/01/2018", "%m/%d/%Y")), linetype="longdash", color = "firebrick3", size = 1) +
  geom_vline(aes(xintercept = as.Date("07/01/2019", "%m/%d/%Y")), linetype="longdash", color = "firebrick4", size = 1) +
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2") +
  geom_label(label="ESSA legislation passed", x=as.Date("12/10/2015", "%m/%d/%Y"), y=8, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") + 
  geom_label(label="Waivers/enforcement \n of 1% cap from \n this point forward", x=as.Date("07/01/2017", "%m/%d/%Y"), y=8, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  geom_label(label="15/19 states \n request waiver \n or exceed cap \n in 2017-2018 \n (excludes BIE)", x=as.Date("07/01/2018", "%m/%d/%Y"), y=8, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  geom_label(label="9/19 states \n request waiver \n or exceed cap \n in 2018-2019 \n (excludes BIE)", x=as.Date("07/01/2019", "%m/%d/%Y"), y=8, label.padding = unit(0.25, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  theme(text = element_text(size = 16)) 
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/overallpdsm.png", width = 24, height = 10, device = "png")

rm(disdataoverallsum, disdataoverallsumbig, disdataoverallsumsm)


###Analysis
library(rcompanion)
library(pwr)
analysis <- disdata %>%
  unique() %>%
  select(-State) %>%
  group_by(year, Q16) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count)
analysis <- analysis %>%
  select(-Q16) %>%
  filter(!is.na(`01/01/2013`))
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`, analysis$`01/01/2014`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`, analysis$`01/01/2014`)))
chisq.test(cbind(analysis$`01/01/2014`, analysis$`01/01/2015`))
cramerV(as.matrix(cbind(analysis$`01/01/2014`, analysis$`01/01/2015`)))
chisq.test(cbind(analysis$`01/01/2015`, analysis$`01/01/2016`))
cramerV(as.matrix(cbind(analysis$`01/01/2015`, analysis$`01/01/2016`)))
chisq.test(cbind(analysis$`01/01/2016`, analysis$`01/01/2017`))
cramerV(as.matrix(cbind(analysis$`01/01/2016`, analysis$`01/01/2017`)))
chisq.test(cbind(analysis$`01/01/2017`, analysis$`01/01/2018`))
cramerV(as.matrix(cbind(analysis$`01/01/2017`, analysis$`01/01/2018`)))
chisq.test(cbind(analysis$`01/01/2018`, analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2018`, analysis$`01/01/2019`)))
chisq.test(cbind(analysis$`01/01/2013`, analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`, analysis$`01/01/2019`)))
chisq.test(cbind(analysis$`01/01/2015`, analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2015`, analysis$`01/01/2019`)))
chisq.test(cbind(analysis$`01/01/2016`, analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2016`, analysis$`01/01/2019`)))
chisq.test(cbind(analysis$`01/01/2017`, analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2017`, analysis$`01/01/2019`)))
analysisprop <- disdata %>%
  unique() %>%
  select(-State) %>%
  group_by(year, Q16) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct) %>%
  filter(!is.na(`01/01/2013`))
hval <- data.frame(matrix(ncol = 1, nrow = 14))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
    H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
    hval[i,1] <- H
}
hval <- data.frame(matrix(ncol = 1, nrow = 14))
year1 <- 2015
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}
hval <- data.frame(matrix(ncol = 1, nrow = 14))
year1 <- 2016
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}
hval <- data.frame(matrix(ncol = 1, nrow = 14))
year1 <- 2017
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}
hval <- data.frame(matrix(ncol = 1, nrow = 14))
year1 <- 2015
year2 <- 2016
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}
hval <- data.frame(matrix(ncol = 1, nrow = 14))
year1 <- 2017
year2 <- 2018
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}




##Educational placement by year
edplacefun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, Q17 = qid) %>%
    filter(!is.na(Q17)) %>%
    unique() %>%
    group_by(Q17) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  view(analysis)
}

edplacefun(FC2013, "Q17", "V1")
edplacefun(FC2014, "Q17", "Studentid")
edplacefun(FC2015, "Q17", "studentid")
edplacefun(FC2016, "Q17", "studentid")
edplacefun(FC2017, "q17", "studentid")
edplacefun(FC2018, "q17", "studentid")
edplacefun(FC2019, "Q17", "studentid")

###Combine years
edplacedata <- FC2013 %>%
  select(year, Q17, studentid = "V1") %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata$studentid <- as.character(edplacedata$studentid)
temp <- FC2014 %>%
  select(year, Q17, studentid = "Studentid") %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata <- rbind (edplacedata, temp)
temp <- FC2015 %>%
  select(year, Q17, studentid) %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata <- rbind (edplacedata, temp)
temp <- FC2016 %>%
  select(year, Q17, studentid) %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata <- rbind (edplacedata, temp)
temp <- FC2017 %>%
  select(year, Q17 = "q17", studentid) %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata <- rbind (edplacedata, temp)
temp <- FC2018 %>%
  select(year, Q17 = "q17", studentid) %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata <- rbind (edplacedata, temp)
temp <- FC2019 %>%
  select(year, Q17, studentid) %>% 
  filter(!is.na(Q17)) %>%
  unique()
edplacedata <- rbind (edplacedata, temp)

rm(temp)

edplacedatasum <- edplacedata %>%
  select(year, Q17) %>%
  group_by(year, Q17) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = round(count/sum(count)*100,1))

edplacedatasum$Q17 <- factor(recode(edplacedatasum$Q17, `1` = "Regular class (> 79%)", `2` = "Regular class (40% - 79%)", `3` = "Regular class (< 40%)", 
                                              `4` = "Separate school", `5` = "Residential facility", `6` = "Homebound/hospital"), 
                                levels = c("Regular class (> 79%)","Regular class (40% - 79%)","Regular class (< 40%)", "Separate school", "Residential facility", "Homebound/hospital"))

###Make plots
ggplot(edplacedatasum, aes(as.Date(year, "%m/%d/%Y"), pct, colour = Q17)) + 
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Percent") + 
  xlim(as.Date("01/01/2013", "%m/%d/%Y"), as.Date("12/31/2019", "%m/%d/%Y")) +
  guides(colour = guide_legend(title = "Educational Placement"), linetype = guide_legend(title = "Educational Placement")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(aes(xintercept = as.Date("07/01/2017", "%m/%d/%Y")), linetype="longdash", color = "firebrick1", size = 1) + 
  geom_vline(aes(xintercept = as.Date("07/01/2018", "%m/%d/%Y")), linetype="longdash", color = "firebrick3", size = 1) +
  geom_vline(aes(xintercept = as.Date("07/01/2019", "%m/%d/%Y")), linetype="longdash", color = "firebrick4", size = 1) +
  geom_vline(aes(xintercept = as.Date("12/10/2015", "%m/%d/%Y")), color = "blue") +
  scale_color_brewer(palette = "Dark2") +
  geom_label(label="ESSA legislation passed", x=as.Date("12/10/2015", "%m/%d/%Y"), y=45, label.padding = unit(0.55, "lines"),
             label.size = 0.35, color = "black", fill="white") + 
  geom_label(label="Waivers/enforcement of 1% cap \n from this point forward", x=as.Date("07/01/2017", "%m/%d/%Y"), y=45, label.padding = unit(0.55, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  geom_label(label="15/19 states request waiver or \n exceed cap in 2017-2018 \n (excludes BIE)", x=as.Date("07/01/2018", "%m/%d/%Y"), y=45, label.padding = unit(0.55, "lines"),
             label.size = 0.35, color = "black", fill="white") +
  geom_label(label="9/19 states request waiver or \n exceed cap in 2018-2019 \n (excludes BIE)", x=as.Date("07/01/2019", "%m/%d/%Y"), y=45, label.padding = unit(0.55, "lines"),
             label.size = 0.35, color = "black", fill="white")
ggsave(file = "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/FC pop 2022/Figures/edplace.png", width = 24, height = 10, device = "png")

rm(edplacedatasum)


###Analysis
analysis <- edplacedata %>%
  unique() %>%
  group_by(year, Q17) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q17)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2015`,analysis$`01/01/2016`))
cramerV(as.matrix(cbind(analysis$`01/01/2015`,analysis$`01/01/2016`)))

analysisprop <- edplacedata %>%
  unique() %>%
  group_by(year, Q17) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 6))
year1 <- 2015
year2 <- 2016
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


##Hearing questions
hearfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, Q19 = qid) %>%
    filter(!is.na(Q19)) %>%
    unique() %>%
    group_by(Q19) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  view(analysis)
}

hearfun(FC2013, "Q19", "V1")
hearfun(FC2014, "Q19", "Studentid")
hearfun(FC2015, "Q19", "studentid")
hearfun(FC2016, "Q19", "studentid")
hearfun(FC2017, "q19", "studentid")
hearfun(FC2018, "q19", "studentid")
hearfun(FC2019, "Q19", "studentid")


hearclassfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, Q330 = qid) %>%
    filter(!is.na(Q330)) %>%
    unique() %>%
    group_by(Q330) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  view(analysis)
  print(sum(analysis$count))
}

hearclassfun(FC2016, "Q330", "studentid")
hearclassfun(FC2017, "q330", "studentid")
hearclassfun(FC2018, "q330", "studentid")
hearclassfun(FC2019, "Q330", "studentid")


###Analysis
hearclassdata <- FC2016 %>%
  select(studentid, year, Q330) %>%
  filter(!is.na(Q330))
temp <- FC2017 %>%
  select(studentid, year, Q330 = q330) %>%
  filter(!is.na(Q330))
hearclassdata <- rbind(hearclassdata, temp)
temp <- FC2018 %>%
  select(studentid, year, Q330 = q330) %>%
  filter(!is.na(Q330))
hearclassdata <- rbind(hearclassdata, temp)
temp <- FC2019 %>%
  select(studentid, year, Q330) %>%
  filter(!is.na(Q330))
hearclassdata <- rbind(hearclassdata, temp)

analysis <- hearclassdata %>%
  unique() %>%
  group_by(year, Q330) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q330)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2016`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2016`,analysis$`01/01/2019`)))

analysisprop <- hearclassdata %>%
  unique() %>%
  group_by(year, Q330) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 6))
year1 <- 2016
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2014)], analysisprop[i,(year1-2014)])
  hval[i,1] <- H
}


##Vision questions
visfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, Q22 = qid) %>%
    filter(!is.na(Q22)) %>%
    unique() %>%
    group_by(Q22) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  view(analysis)
}

visfun(FC2013, "Q22", "V1")
visfun(FC2014, "Q22", "Studentid")
visfun(FC2015, "Q22", "studentid")
visfun(FC2016, "Q22", "studentid")
visfun(FC2017, "q22", "studentid")
visfun(FC2018, "q22", "studentid")
visfun(FC2019, "Q22", "studentid")


visclassfun = function(dat, mskeys, stuid){
  msdenom <<- dat %>%
    select(studentid = stuid, mskeys) %>%
    pivot_longer(cols = mskeys, names_to = "msq", values_to = "val") %>%
    filter(!is.na(val)) %>%
    select(studentid) %>%
    unique()
  analysis <<- dat %>%
    select(studentid = stuid, mskeys) %>%
    pivot_longer(cols = mskeys, names_to = "msq", values_to = "val") %>%
    filter(!is.na(val)) %>%
    group_by(msq) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/nrow(msdenom)*100,1))
  print(nrow(msdenom))
  view(analysis)
}

mskeys <- c("Q429_1", "Q429_2", "Q429_3", "Q429_4", "Q429_5")
mskeyslower <- c("q429_1", "q429_2", "q429_3", "q429_4", "q429_5")
visclassfun(FC2016, mskeys, "studentid")
visclassfun(FC2017, mskeyslower, "studentid")
visclassfun(FC2018, mskeyslower, "studentid")
visclassfun(FC2019, mskeys, "studentid")


##Motor questions
motfun = function(dat, mskeys, stuid){
  msdenom <<- dat %>%
    select(studentid = stuid, mskeys) %>%
    pivot_longer(cols = mskeys, names_to = "msq", values_to = "val") %>%
    filter(!is.na(val)) %>%
    select(studentid) %>%
    unique()
  analysis <<- dat %>%
    select(studentid = stuid, mskeys) %>%
    pivot_longer(cols = mskeys, names_to = "msq", values_to = "val") %>%
    filter(!is.na(val)) %>%
    group_by(msq) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/nrow(msdenom)*100,1))
  print(nrow(msdenom))
  view(analysis)
}

mskeys <- c("Q29_1", "Q29_2", "Q29_3", "Q29_4")
mskeyslower <- c("q29_1", "q29_2", "q29_3", "q29_4")
motfun(FC2014, mskeys, "Studentid")
motfun(FC2015, mskeys, "studentid")
motfun(FC2016, mskeys, "studentid")
motfun(FC2017, mskeyslower, "studentid")
motfun(FC2018, mskeyslower, "studentid")
motfun(FC2019, mskeys, "studentid")


##Expressive communication
commfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

commfun(FC2013, "Q36", "V1")
commfun(FC2013, "Q39", "V1")
commfun(FC2013, "Q43", "V1")
commfun(FC2014, "Q36", "Studentid")
commfun(FC2014, "Q39", "Studentid")
commfun(FC2014, "Q43", "Studentid")
commfun(FC2015, "Q36", "studentid")
commfun(FC2015, "Q39", "studentid")
commfun(FC2015, "Q43", "studentid")
commfun(FC2016, "Q36", "studentid")
commfun(FC2016, "Q39", "studentid")
commfun(FC2016, "Q43", "studentid")
commfun(FC2017, "q36", "studentid")
commfun(FC2017, "q39", "studentid")
commfun(FC2017, "q43", "studentid")
commfun(FC2018, "q36", "studentid")
commfun(FC2018, "q39", "studentid")
commfun(FC2018, "q43", "studentid")
commfun(FC2019, "Q36", "studentid")
commfun(FC2019, "Q39", "studentid")
commfun(FC2019, "Q43", "studentid")


nocommfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

nocommfun(FC2013, "Q47", "V1")
nocommfun(FC2014, "Q47", "Studentid")
nocommfun(FC2015, "Q47", "studentid")
nocommfun(FC2016, "Q47", "studentid")
nocommfun(FC2017, "q47", "studentid")
nocommfun(FC2018, "q47", "studentid")
nocommfun(FC2019, "Q47", "studentid")

highcommfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

highcommfun(FC2013, "Q37", "V1")
highcommfun(FC2013, "Q40", "V1")
highcommfun(FC2013, "Q44", "V1")
highcommfun(FC2014, "Q37", "Studentid")
highcommfun(FC2014, "Q40", "Studentid")
highcommfun(FC2014, "Q44", "Studentid")
highcommfun(FC2015, "Q37", "studentid")
highcommfun(FC2015, "Q40", "studentid")
highcommfun(FC2015, "Q44", "studentid")
highcommfun(FC2016, "Q37", "studentid")
highcommfun(FC2016, "Q40", "studentid")
highcommfun(FC2016, "Q44", "studentid")
highcommfun(FC2017, "q37", "studentid")
highcommfun(FC2017, "q40", "studentid")
highcommfun(FC2017, "q44", "studentid")
highcommfun(FC2018, "q37", "studentid")
highcommfun(FC2018, "q40", "studentid")
highcommfun(FC2018, "q44", "studentid")
highcommfun(FC2019, "Q37", "studentid")
highcommfun(FC2019, "Q40", "studentid")
highcommfun(FC2019, "Q44", "studentid")


###Analysis
FC2013$V1 <- as.character(FC2013$V1)
signdat <- FC2013 %>%
  select(studentid = V1, year, Q39) %>%
  filter(!is.na(Q39))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q39) %>%
  filter(!is.na(Q39))
signdat <- rbind(signdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q39) %>%
  filter(!is.na(Q39))
signdat <- rbind(signdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q39) %>%
  filter(!is.na(Q39))
signdat <- rbind(signdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q39 = q39) %>%
  filter(!is.na(Q39))
signdat <- rbind(signdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q39 = q39) %>%
  filter(!is.na(Q39))
signdat <- rbind(signdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q39) %>%
  filter(!is.na(Q39))
signdat <- rbind(signdat, temp)

analysis <- signdat %>%
  unique() %>%
  group_by(year, Q39) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q39)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- signdat %>%
  unique() %>%
  group_by(year, Q39) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 2))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


aacdat <- FC2013 %>%
  select(studentid = V1, year, Q43) %>%
  filter(!is.na(Q43))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q43) %>%
  filter(!is.na(Q43))
aacdat <- rbind(aacdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q43) %>%
  filter(!is.na(Q43))
aacdat <- rbind(aacdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q43) %>%
  filter(!is.na(Q43))
aacdat <- rbind(aacdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q43 = q43) %>%
  filter(!is.na(Q43))
aacdat <- rbind(aacdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q43 = q43) %>%
  filter(!is.na(Q43))
aacdat <- rbind(aacdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q43) %>%
  filter(!is.na(Q43))
aacdat <- rbind(aacdat, temp)

analysis <- aacdat %>%
  unique() %>%
  group_by(year, Q43) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q43)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- aacdat %>%
  unique() %>%
  group_by(year, Q43) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 2))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


otherdat <- FC2013 %>%
  select(studentid = V1, year, Q47) %>%
  filter(!is.na(Q47))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q47) %>%
  filter(!is.na(Q47))
otherdat <- rbind(otherdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q47) %>%
  filter(!is.na(Q47))
otherdat <- rbind(otherdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q47) %>%
  filter(!is.na(Q47))
otherdat <- rbind(otherdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q47 = q47) %>%
  filter(!is.na(Q47))
otherdat <- rbind(otherdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q47 = q47) %>%
  filter(!is.na(Q47))
otherdat <- rbind(otherdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q47) %>%
  filter(!is.na(Q47))
otherdat <- rbind(otherdat, temp)

analysis <- otherdat %>%
  unique() %>%
  group_by(year, Q47) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q47)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- otherdat %>%
  unique() %>%
  group_by(year, Q47) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


spsophdat <- FC2013 %>%
  select(studentid = V1, year, Q37) %>%
  filter(!is.na(Q37))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q37) %>%
  filter(!is.na(Q37))
spsophdat <- rbind(spsophdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q37) %>%
  filter(!is.na(Q37))
spsophdat <- rbind(spsophdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q37) %>%
  filter(!is.na(Q37))
spsophdat <- rbind(spsophdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q37 = q37) %>%
  filter(!is.na(Q37))
spsophdat <- rbind(spsophdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q37 = q37) %>%
  filter(!is.na(Q37))
spsophdat <- rbind(spsophdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q37) %>%
  filter(!is.na(Q37))
spsophdat <- rbind(spsophdat, temp)

analysis <- spsophdat %>%
  unique() %>%
  group_by(year, Q37) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q37)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- spsophdat %>%
  unique() %>%
  group_by(year, Q37) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


slsophdat <- FC2013 %>%
  select(studentid = V1, year, Q40) %>%
  filter(!is.na(Q40))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q40) %>%
  filter(!is.na(Q40))
slsophdat <- rbind(slsophdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q40) %>%
  filter(!is.na(Q40))
slsophdat <- rbind(slsophdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q40) %>%
  filter(!is.na(Q40))
slsophdat <- rbind(slsophdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q40 = q40) %>%
  filter(!is.na(Q40))
slsophdat <- rbind(slsophdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q40 = q40) %>%
  filter(!is.na(Q40))
slsophdat <- rbind(slsophdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q40) %>%
  filter(!is.na(Q40))
slsophdat <- rbind(slsophdat, temp)

analysis <- slsophdat %>%
  unique() %>%
  group_by(year, Q40) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q40)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- slsophdat %>%
  unique() %>%
  group_by(year, Q40) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


aacsophdat <- FC2013 %>%
  select(studentid = V1, year, Q44) %>%
  filter(!is.na(Q44))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q44) %>%
  filter(!is.na(Q44))
aacsophdat <- rbind(aacsophdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q44) %>%
  filter(!is.na(Q44))
aacsophdat <- rbind(aacsophdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q44) %>%
  filter(!is.na(Q44))
aacsophdat <- rbind(aacsophdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q44 = q44) %>%
  filter(!is.na(Q44))
aacsophdat <- rbind(aacsophdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q44 = q44) %>%
  filter(!is.na(Q44))
aacsophdat <- rbind(aacsophdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q44) %>%
  filter(!is.na(Q44))
aacsophdat <- rbind(aacsophdat, temp)

analysis <- aacsophdat %>%
  unique() %>%
  group_by(year, Q44) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q44)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- aacsophdat %>%
  unique() %>%
  group_by(year, Q44) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}






##Receptive communication
reccommfun = function(dat, mskeys, stuid){
  for(i in 1:length(mskeys)){
    if(i == 1){
      qid <- mskeys[i]
      dat[,qid] <- recode(dat[,qid], `1` = 2, `2` = 2, `3` = 3, `4` = 4, `5` = 5)
      analysis <<- dat %>%
        select(studentid = stuid, quest = qid) %>%
        filter(!is.na(quest)) %>%
        unique() %>%
        group_by(quest) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(pct = round(count/sum(count)*100,1))
    }
    else{
      qid <- mskeys[i]
      dat[,qid] <- recode(dat[,qid], `1` = 2, `2` = 2, `3` = 3, `4` = 4, `5` = 5)
      temp <<- dat %>%
        select(studentid = stuid, quest = qid) %>%
        filter(!is.na(quest)) %>%
        unique() %>%
        group_by(quest) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(pct = round(count/sum(count)*100,1))
      analysis <<- cbind(analysis, temp)
    }
  }
  View(analysis)
}

mskeys1 <- c("Q49_1", "Q49_2", "Q49_3", "Q49_4", "Q49_5", "Q49_6")
mskeys2 <- c("q491", "q492", "q493", "q494", "q495", "q496")
mskeys3 <- c("Q491", "Q492", "Q493", "Q494", "Q495", "Q496")

reccommfun(FC2013, mskeys1, "V1")
reccommfun(FC2014, mskeys1, "Studentid")
reccommfun(FC2015, mskeys1, "studentid")
reccommfun(FC2016, mskeys1, "studentid")
reccommfun(FC2017, mskeys2, "studentid")
reccommfun(FC2018, mskeys2, "studentid")
reccommfun(FC2019, mskeys3, "studentid")

recdata <- FC2013 %>%
  select(Q49_1, Q49_2, Q49_3, Q49_4, Q49_5, Q49_6, studentid = V1, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
temp <- FC2014 %>%
  select(Q49_1, Q49_2, Q49_3, Q49_4, Q49_5, Q49_6, studentid = Studentid, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
recdata <- rbind(recdata, temp)
temp <- FC2015 %>%
  select(Q49_1, Q49_2, Q49_3, Q49_4, Q49_5, Q49_6, studentid, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
recdata <- rbind(recdata, temp)
temp <- FC2016 %>%
  select(Q49_1, Q49_2, Q49_3, Q49_4, Q49_5, Q49_6, studentid, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
recdata <- rbind(recdata, temp)
temp <- FC2017 %>%
  select(Q49_1 = q491, Q49_2 = q492, Q49_3 = q493, Q49_4 = q494, Q49_5 = q495, Q49_6 = q496, studentid, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
recdata <- rbind(recdata, temp)
temp <- FC2018 %>%
  select(Q49_1 = q491, Q49_2 = q492, Q49_3 = q493, Q49_4 = q494, Q49_5 = q495, Q49_6 = q496, studentid, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
recdata <- rbind(recdata, temp)
temp <- FC2019 %>%
  select(Q49_1 = Q491, Q49_2 = Q492, Q49_3 = Q493, Q49_4 = Q494, Q49_5 = Q495, Q49_6 = Q496, studentid, year) %>%
  pivot_longer(Q49_1:Q49_6, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
recdata <- rbind(recdata, temp)
recdata$val <- recode(recdata$val, `1` = 2, `2` = 2, `3` = 3, `4` = 4, `5` = 5)

rectestfun = function(qid){
  analysis <<- recdata %>%
    filter(quest == qid) %>%
    select(-quest) %>%
    group_by(year, val) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = "year", values_from = "count") %>%
    select(-val)
  print(chisq.test(analysis))
  print(cramerV(as.matrix(analysis)))
  print(chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))
  print(cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))))
  
  analysisprop <<- recdata %>%
    filter(quest == qid) %>%
    select(-quest) %>%
    group_by(year, val) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(pct = count/sum(count)) %>%
    select(-count) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = pct)
  hval <<- data.frame(matrix(ncol = 1, nrow = 4))
  year1 <<- 2013
  year2 <<- 2019
  for(i in 1:nrow(analysisprop)){
    H <<- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
    hval[i,1] <<- H
  }
  View(hval)
}

rectestfun("Q49_1")
rectestfun("Q49_2")
rectestfun("Q49_3")
rectestfun("Q49_4")
rectestfun("Q49_5")
rectestfun("Q49_6")




##Reading
readfun = function(dat, mskeys, stuid){
  for(i in 1:length(mskeys)){
    if(i == 1){
      qid <- mskeys[i]
      dat[,qid] <- recode(dat[,qid], `1` = 4, `2` = 4, `3` = 4, `4` = 4, `5` = 5)
      analysis <<- dat %>%
        select(studentid = stuid, quest = qid) %>%
        filter(!is.na(quest)) %>%
        unique() %>%
        group_by(quest) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(pct = round(count/sum(count)*100,1))
    }
    else{
      qid <- mskeys[i]
      dat[,qid] <- recode(dat[,qid], `1` = 4, `2` = 4, `3` = 4, `4` = 4, `5` = 5)
      temp <<- dat %>%
        select(studentid = stuid, quest = qid) %>%
        filter(!is.na(quest)) %>%
        unique() %>%
        group_by(quest) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(pct = round(count/sum(count)*100,1))
      analysis <<- cbind(analysis, temp)
    }
  }
  View(analysis)
}

mskeys1 <- c("Q51_1", "Q51_2", "Q51_3", "Q51_4", "Q51_5", "Q51_6", "Q51_7", "Q51_8")
mskeys2 <- c("q511", "q512", "q513", "q514", "q515", "q516", "q517", "q518")
mskeys3 <- c("Q511", "Q512", "Q513", "Q514", "Q515", "Q516", "Q517", "Q518")

readfun(FC2013, mskeys1, "V1")
readfun(FC2014, mskeys1, "Studentid")
readfun(FC2015, mskeys1, "studentid")
readfun(FC2016, mskeys1, "studentid")
readfun(FC2017, mskeys2, "studentid")
readfun(FC2018, mskeys2, "studentid")
readfun(FC2019, mskeys3, "studentid")



readdata <- FC2013 %>%
  select(Q51_1, Q51_2, Q51_3, Q51_4, Q51_5, Q51_6, Q51_7, Q51_8, studentid = V1, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
temp <- FC2014 %>%
  select(Q51_1, Q51_2, Q51_3, Q51_4, Q51_5, Q51_6, Q51_7, Q51_8, studentid = Studentid, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
readdata <- rbind(readdata, temp)
temp <- FC2015 %>%
  select(Q51_1, Q51_2, Q51_3, Q51_4, Q51_5, Q51_6, Q51_7, Q51_8, studentid, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
readdata <- rbind(readdata, temp)
temp <- FC2016 %>%
  select(Q51_1, Q51_2, Q51_3, Q51_4, Q51_5, Q51_6, Q51_7, Q51_8, studentid, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
readdata <- rbind(readdata, temp)
temp <- FC2017 %>%
  select(Q51_1 = q511, Q51_2 = q512, Q51_3 = q513, Q51_4 = q514, Q51_5 = q515, Q51_6 = q516, Q51_7 = q517, Q51_8 = q518, studentid, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
readdata <- rbind(readdata, temp)
temp <- FC2018 %>%
  select(Q51_1 = q511, Q51_2 = q512, Q51_3 = q513, Q51_4 = q514, Q51_5 = q515, Q51_6 = q516, Q51_7 = q517, Q51_8 = q518, studentid, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
readdata <- rbind(readdata, temp)
temp <- FC2019 %>%
  select(Q51_1 = Q511, Q51_2 = Q512, Q51_3 = Q513, Q51_4 = Q514, Q51_5 = Q515, Q51_6 = Q516, Q51_7 = Q517, Q51_8 = Q518, studentid, year) %>%
  pivot_longer(Q51_1:Q51_8, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
readdata <- rbind(readdata, temp)
readdata$val <- recode(readdata$val, `1` = 1, `2` = 1, `3` = 1, `4` = 1, `5` = 5)

readtestfun = function(qid){
  analysis <<- readdata %>%
    filter(quest == qid) %>%
    select(-quest) %>%
    group_by(year, val) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = "year", values_from = "count") %>%
    select(-val)
  print(chisq.test(analysis))
  print(cramerV(as.matrix(analysis)))
  print(chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))
  print(cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))))
  
  analysisprop <<- readdata %>%
    filter(quest == qid) %>%
    select(-quest) %>%
    group_by(year, val) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(pct = count/sum(count)) %>%
    select(-count) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = pct)
  hval <<- data.frame(matrix(ncol = 1, nrow = 2))
  year1 <<- 2013
  year2 <<- 2019
  for(i in 1:nrow(analysisprop)){
    H <<- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
    hval[i,1] <<- H
  }
  View(hval)
}

readtestfun("Q51_1")
readtestfun("Q51_2")
readtestfun("Q51_3")
readtestfun("Q51_4")
readtestfun("Q51_5")
readtestfun("Q51_6")
readtestfun("Q51_7")
readtestfun("Q51_8")



###Reading Comprehension Level
readlevelfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

readlevelfun(FC2013, "Q52", "V1")
readlevelfun(FC2014, "Q52", "Studentid")
readlevelfun(FC2015, "Q52", "studentid")
readlevelfun(FC2016, "Q52", "studentid")
readlevelfun(FC2017, "q52", "studentid")
readlevelfun(FC2018, "q52", "studentid")
readlevelfun(FC2019, "Q52", "studentid")

readlevdat <- FC2013 %>%
  select(studentid = V1, year, Q52) %>%
  filter(!is.na(Q52))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q52) %>%
  filter(!is.na(Q52))
readlevdat <- rbind(readlevdat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q52) %>%
  filter(!is.na(Q52))
readlevdat <- rbind(readlevdat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q52) %>%
  filter(!is.na(Q52))
readlevdat <- rbind(readlevdat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q52 = q52) %>%
  filter(!is.na(Q52))
readlevdat <- rbind(readlevdat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q52 = q52) %>%
  filter(!is.na(Q52))
readlevdat <- rbind(readlevdat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q52) %>%
  filter(!is.na(Q52))
readlevdat <- rbind(readlevdat, temp)

analysis <- readlevdat %>%
  unique() %>%
  group_by(year, Q52) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q52)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- readlevdat %>%
  unique() %>%
  group_by(year, Q52) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 6))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}


###Writing
FC2013$Q500 <- NA
for(i in 1:nrow(FC2013)){
  if(FC2013$Q56_8[i] > 1 & !is.na(FC2013$Q56_8[i])){
    FC2013$Q500[i] <- 1
  }
  else if(FC2013$Q56_7[i] > 1 & !is.na(FC2013$Q56_7[i])){
    FC2013$Q500[i] <- 3
  }
  else if(FC2013$Q56_6[i] > 1 & !is.na(FC2013$Q56_6[i])){
    FC2013$Q500[i] <- 4
  }
  else if((FC2013$Q56_4[i] > 1 & !is.na(FC2013$Q56_4[i])) | (FC2013$Q56_5[i] > 1 & !is.na(FC2013$Q56_5[i]))){
    FC2013$Q500[i] <- 5
  }
  else if(FC2013$Q56_3[i] > 1 & !is.na(FC2013$Q56_3[i])){
    FC2013$Q500[i] <- 6
  }
  else if((FC2013$Q56_1[i] > 1 & !is.na(FC2013$Q56_1[i])) | (FC2013$Q56_2[i] > 1 & !is.na(FC2013$Q56_2[i]))){
    FC2013$Q500[i] <- 7
  }
}

FC2014$Q500 <- NA
for(i in 1:nrow(FC2014)){
  if(FC2014$Q56_8[i] > 1 & !is.na(FC2014$Q56_8[i])){
    FC2014$Q500[i] <- 1
  }
  else if(FC2014$Q56_7[i] > 1 & !is.na(FC2014$Q56_7[i])){
    FC2014$Q500[i] <- 3
  }
  else if(FC2014$Q56_6[i] > 1 & !is.na(FC2014$Q56_6[i])){
    FC2014$Q500[i] <- 4
  }
  else if((FC2014$Q56_4[i] > 1 & !is.na(FC2014$Q56_4[i])) | (FC2014$Q56_5[i] > 1 & !is.na(FC2014$Q56_5[i]))){
    FC2014$Q500[i] <- 5
  }
  else if(FC2014$Q56_3[i] > 1 & !is.na(FC2014$Q56_3[i])){
    FC2014$Q500[i] <- 6
  }
  else if((FC2014$Q56_1[i] > 1 & !is.na(FC2014$Q56_1[i])) | (FC2014$Q56_2[i] > 1 & !is.na(FC2014$Q56_2[i]))){
    FC2014$Q500[i] <- 7
  }
}

FC2015$Q500 <- NA
for(i in 1:nrow(FC2015)){
  if(FC2015$Q56_8[i] > 1 & !is.na(FC2015$Q56_8[i])){
    FC2015$Q500[i] <- 1
  }
  else if(FC2015$Q56_7[i] > 1 & !is.na(FC2015$Q56_7[i])){
    FC2015$Q500[i] <- 3
  }
  else if(FC2015$Q56_6[i] > 1 & !is.na(FC2015$Q56_6[i])){
    FC2015$Q500[i] <- 4
  }
  else if((FC2015$Q56_4[i] > 1 & !is.na(FC2015$Q56_4[i])) | (FC2015$Q56_5[i] > 1 & !is.na(FC2015$Q56_5[i]))){
    FC2015$Q500[i] <- 5
  }
  else if(FC2015$Q56_3[i] > 1 & !is.na(FC2015$Q56_3[i])){
    FC2015$Q500[i] <- 6
  }
  else if((FC2015$Q56_1[i] > 1 & !is.na(FC2015$Q56_1[i])) | (FC2015$Q56_2[i] > 1 & !is.na(FC2015$Q56_2[i]))){
    FC2015$Q500[i] <- 7
  }
}

writefun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

writefun(FC2013, "Q500", "V1")
writefun(FC2014, "Q500", "Studentid")
writefun(FC2015, "Q500", "studentid")
writefun(FC2016, "Q500", "studentid")
writefun(FC2017, "q500", "studentid")
writefun(FC2018, "q500", "studentid")
writefun(FC2019, "Q500", "studentid")


##Math
mathfun = function(dat, mskeys, stuid){
  for(i in 1:length(mskeys)){
    if(i == 1){
      qid <- mskeys[i]
      dat[,qid] <- recode(dat[,qid], `1` = 4, `2` = 4, `3` = 4, `4` = 4, `5` = 5)
      analysis <<- dat %>%
        select(studentid = stuid, quest = qid) %>%
        filter(!is.na(quest)) %>%
        unique() %>%
        group_by(quest) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(pct = round(count/sum(count)*100,1))
    }
    else{
      qid <- mskeys[i]
      dat[,qid] <- recode(dat[,qid], `1` = 4, `2` = 4, `3` = 4, `4` = 4, `5` = 5)
      temp <<- dat %>%
        select(studentid = stuid, quest = qid) %>%
        filter(!is.na(quest)) %>%
        unique() %>%
        group_by(quest) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(pct = round(count/sum(count)*100,1))
      analysis <<- cbind(analysis, temp)
    }
  }
  View(analysis)
}

mskeys1 <- c("Q54_1", "Q54_10", "Q54_11", "Q54_12", "Q54_13", "Q54_2", "Q54_3", "Q54_4", "Q54_5", "Q54_6", "Q54_7", "Q54_8", "Q54_9")
mskeys2 <- c("q541", "q5410", "q5411", "q5412", "q5413", "q542", "q543", "q544", "q545", "q546", "q547", "q548", "q549")
mskeys3 <- c("Q541", "Q5410", "Q5411", "Q5412", "Q5413", "Q542", "Q543", "Q544", "Q545", "Q546", "Q547", "Q548", "Q549")

mathfun(FC2013, mskeys1, "V1")
mathfun(FC2014, mskeys1, "Studentid")
mathfun(FC2015, mskeys1, "studentid")
mathfun(FC2016, mskeys1, "studentid")
mathfun(FC2017, mskeys2, "studentid")
mathfun(FC2018, mskeys2, "studentid")
mathfun(FC2019, mskeys3, "studentid")



mathdata <- FC2013 %>%
  select(Q54_1, Q54_2, Q54_3, Q54_4, Q54_5, Q54_6, Q54_7, Q54_8, Q54_9, Q54_10, Q54_11, Q54_12, Q54_13, studentid = V1, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
temp <- FC2014 %>%
  select(Q54_1, Q54_2, Q54_3, Q54_4, Q54_5, Q54_6, Q54_7, Q54_8, Q54_9, Q54_10, Q54_11, Q54_12, Q54_13, studentid = Studentid, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
mathdata <- rbind(mathdata, temp)
temp <- FC2015 %>%
  select(Q54_1, Q54_2, Q54_3, Q54_4, Q54_5, Q54_6, Q54_7, Q54_8, Q54_9, Q54_10, Q54_11, Q54_12, Q54_13, studentid, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
mathdata <- rbind(mathdata, temp)
temp <- FC2016 %>%
  select(Q54_1, Q54_2, Q54_3, Q54_4, Q54_5, Q54_6, Q54_7, Q54_8, Q54_9, Q54_10, Q54_11, Q54_12, Q54_13, studentid, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
mathdata <- rbind(mathdata, temp)
temp <- FC2017 %>%
  select(Q54_1 = q541, Q54_2 = q542, Q54_3 = q543, Q54_4 = q544, Q54_5 = q545, Q54_6 = q546, Q54_7 = q547, Q54_8 = q548, Q54_9 = q549, Q54_10 = q5410, Q54_11 = q5411, Q54_12 = q5412, Q54_13 = q5413, studentid, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
mathdata <- rbind(mathdata, temp)
temp <- FC2018 %>%
  select(Q54_1 = q541, Q54_2 = q542, Q54_3 = q543, Q54_4 = q544, Q54_5 = q545, Q54_6 = q546, Q54_7 = q547, Q54_8 = q548, Q54_9 = q549, Q54_10 = q5410, Q54_11 = q5411, Q54_12 = q5412, Q54_13 = q5413, studentid, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
mathdata <- rbind(mathdata, temp)
temp <- FC2019 %>%
  select(Q54_1 = Q541, Q54_2 = Q542, Q54_3 = Q543, Q54_4 = Q544, Q54_5 = Q545, Q54_6 = Q546, Q54_7 = Q547, Q54_8 = Q548, Q54_9 = Q549, Q54_10 = Q5410, Q54_11 = Q5411, Q54_12 = Q5412, Q54_13 = Q5413, studentid, year) %>%
  pivot_longer(Q54_1:Q54_13, names_to = "quest", values_to = "val") %>%
  filter(!is.na(val))
mathdata <- rbind(mathdata, temp)
mathdata$val <- recode(mathdata$val, `1` = 1, `2` = 1, `3` = 1, `4` = 1, `5` = 5)

mathtestfun = function(qid){
  analysis <<- mathdata %>%
    filter(quest == qid) %>%
    select(-quest) %>%
    group_by(year, val) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = "year", values_from = "count") %>%
    select(-val)
  print(chisq.test(analysis))
  print(cramerV(as.matrix(analysis)))
  print(chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))
  print(cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))))
  
  analysisprop <<- mathdata %>%
    filter(quest == qid) %>%
    select(-quest) %>%
    group_by(year, val) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(pct = count/sum(count)) %>%
    select(-count) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = pct)
  hval <<- data.frame(matrix(ncol = 1, nrow = 2))
  year1 <<- 2013
  year2 <<- 2019
  for(i in 1:nrow(analysisprop)){
    H <<- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
    hval[i,1] <<- H
  }
  View(hval)
}

mathtestfun("Q54_1")
mathtestfun("Q54_2")
mathtestfun("Q54_3")
mathtestfun("Q54_4")
mathtestfun("Q54_5")
mathtestfun("Q54_6")
mathtestfun("Q54_7")
mathtestfun("Q54_8")
mathtestfun("Q54_9")
mathtestfun("Q54_10")
mathtestfun("Q54_11")
mathtestfun("Q54_12")
mathtestfun("Q54_13")


###Computer use
compusefun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

#Note, for 2013, 2014, and 2015; 1 is 1, 2 is 3, 3 is 4

compusefun(FC2013, "Q143", "V1")
compusefun(FC2014, "Q143", "Studentid")
compusefun(FC2015, "Q143", "studentid")
compusefun(FC2016, "Q143", "studentid")
compusefun(FC2017, "q143", "studentid")
compusefun(FC2018, "q143", "studentid")
compusefun(FC2019, "Q143", "studentid")

compusedat <- FC2013 %>%
  select(studentid = V1, year, Q143) %>%
  filter(!is.na(Q143))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q143) %>%
  filter(!is.na(Q143))
compusedat <- rbind(compusedat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q143) %>%
  filter(!is.na(Q143))
temp$Q143 <- recode(temp$Q143, `1` = 1, `3` = 2, `4` = 3)
compusedat <- rbind(compusedat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q143) %>%
  filter(!is.na(Q143))
temp$Q143 <- recode(temp$Q143, `1` = 1, `2` = 2, `3` = 2, `4` = 3, `5` = 3)
compusedat <- rbind(compusedat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q143 = q143) %>%
  filter(!is.na(Q143))
temp$Q143 <- recode(temp$Q143, `1` = 1, `2` = 2, `3` = 2, `4` = 3, `5` = 3)
compusedat <- rbind(compusedat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q143 = q143) %>%
  filter(!is.na(Q143))
temp$Q143 <- recode(temp$Q143, `1` = 1, `2` = 2, `3` = 2, `4` = 3, `5` = 3)
compusedat <- rbind(compusedat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q143) %>%
  filter(!is.na(Q143))
temp$Q143 <- recode(temp$Q143, `1` = 1, `2` = 2, `3` = 2, `4` = 3, `5` = 3)
compusedat <- rbind(compusedat, temp)

analysis <- compusedat %>%
  unique() %>%
  group_by(year, Q143) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q143)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- compusedat %>%
  unique() %>%
  group_by(year, Q143) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}

compusefun(FC2013, "Q147", "V1")
compusefun(FC2014, "Q147", "Studentid")
compusefun(FC2015, "Q147", "studentid")
compusefun(FC2016, "Q147", "studentid")
compusefun(FC2017, "q147", "studentid")
compusefun(FC2018, "q147", "studentid")
compusefun(FC2019, "Q147", "studentid")


loafun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

loafun(FC2013, "Q146", "V1")
loafun(FC2014, "Q146", "Studentid")
loafun(FC2015, "Q146", "studentid")
loafun(FC2016, "Q201", "studentid")
loafun(FC2017, "q201", "studentid")
loafun(FC2018, "q201", "studentid")
loafun(FC2019, "Q201", "studentid")

comploadat <- FC2013 %>%
  select(studentid = V1, year, Q146) %>%
  filter(!is.na(Q146))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q146) %>%
  filter(!is.na(Q146))
comploadat <- rbind(comploadat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q146) %>%
  filter(!is.na(Q146))
comploadat <- rbind(comploadat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q146 = Q201) %>%
  filter(!is.na(Q146))
comploadat <- rbind(comploadat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q146 = q201) %>%
  filter(!is.na(Q146))
comploadat <- rbind(comploadat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q146 = q201) %>%
  filter(!is.na(Q146))
comploadat <- rbind(comploadat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q146 = Q201) %>%
  filter(!is.na(Q146))
comploadat <- rbind(comploadat, temp)

analysis <- comploadat %>%
  unique() %>%
  group_by(year, Q146) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q146)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- comploadat %>%
  unique() %>%
  group_by(year, Q146) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}

loafun(FC2013, "Q142", "V1")
loafun(FC2014, "Q142", "Studentid")
loafun(FC2015, "Q142", "studentid")
loafun(FC2016, "Q202", "studentid")
loafun(FC2017, "q202", "studentid")
loafun(FC2018, "q202", "studentid")
loafun(FC2019, "Q202", "studentid")

teachloadat <- FC2013 %>%
  select(studentid = V1, year, Q142) %>%
  filter(!is.na(Q142))
temp <- FC2014 %>%
  select(studentid = Studentid, year, Q142) %>%
  filter(!is.na(Q142))
teachloadat <- rbind(teachloadat, temp)
temp <- FC2015 %>%
  select(studentid, year, Q142) %>%
  filter(!is.na(Q142))
teachloadat <- rbind(teachloadat, temp)
temp <- FC2016 %>%
  select(studentid, year, Q142 = Q202) %>%
  filter(!is.na(Q142))
teachloadat <- rbind(teachloadat, temp)
temp <- FC2017 %>%
  select(studentid, year, Q142 = q202) %>%
  filter(!is.na(Q142))
teachloadat <- rbind(teachloadat, temp)
temp <- FC2018 %>%
  select(studentid, year, Q142 = q202) %>%
  filter(!is.na(Q142))
teachloadat <- rbind(teachloadat, temp)
temp <- FC2019 %>%
  select(studentid, year, Q142 = Q202) %>%
  filter(!is.na(Q142))
teachloadat <- rbind(teachloadat, temp)

analysis <- teachloadat %>%
  unique() %>%
  group_by(year, Q142) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = count) %>%
  select(-Q142)
chisq.test(analysis)
cramerV(as.matrix(analysis))
chisq.test(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`))
cramerV(as.matrix(cbind(analysis$`01/01/2013`,analysis$`01/01/2019`)))

analysisprop <- teachloadat %>%
  unique() %>%
  group_by(year, Q142) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = pct)
hval <- data.frame(matrix(ncol = 1, nrow = 3))
year1 <- 2013
year2 <- 2019
for(i in 1:nrow(analysisprop)){
  H <- ES.h(analysisprop[i,(year2-2011)], analysisprop[i,(year1-2011)])
  hval[i,1] <- H
}

##English as primary language
langfun = function(dat, qid, stuid){
  analysis <<- dat %>%
    select(studentid = stuid, quest = qid) %>%
    filter(!is.na(quest)) %>%
    unique() %>%
    group_by(quest) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(pct = round(count/sum(count)*100,1))
  View(analysis)
  print(sum(analysis$count))
}

langfun(FC2017, "q501", "studentid")
langfun(FC2018, "q501", "studentid")
langfun(FC2019, "Q501", "studentid")
langfun(FC2017, "q502", "studentid")
langfun(FC2018, "q502", "studentid")
langfun(FC2019, "Q502", "studentid")
langfun(FC2017, "q503", "studentid")
langfun(FC2018, "q503", "studentid")
langfun(FC2019, "Q503", "studentid")







##Extra chi square tests per Karen
dat <- inner_join(edplacedata, readdata, by = c("studentid", "year"))
yrs <- unique(dat$year)
multvarstest = function(qid){
  dattest <<- data.frame(matrix(ncol = 7, nrow = 3))
for(i in 1:7){  
  analysis <<- dat %>%
    filter(quest == qid, year == yrs[i]) %>%
    select(-quest, -year) %>%
    group_by(val, Q17) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = "val", values_from = "count") %>%
    select(-Q17)
  analysis$`1` <<- ifelse(is.na(analysis$`1`), .001, analysis$`1`)
  analysis$`5` <<- ifelse(is.na(analysis$`5`), .001, analysis$`5`)
  x <<- chisq.test(analysis)
  dattest[1,i] <<- x$statistic
  dattest[2,i] <<- x$p.value
  dattest[3,i] <<- cramerV(as.matrix(analysis))
}
  View(dattest)
}

multvarstest("Q51_1")
multvarstest("Q51_2")
multvarstest("Q51_3")
multvarstest("Q51_4")
multvarstest("Q51_5")
multvarstest("Q51_6")
multvarstest("Q51_7")
multvarstest("Q51_8")

dat <- inner_join(edplacedata, mathdata, by = c("studentid", "year"))
multvarstest("Q54_1")
multvarstest("Q54_10")
multvarstest("Q54_11")
multvarstest("Q54_12")
multvarstest("Q54_13")
multvarstest("Q54_2")
multvarstest("Q54_3")
multvarstest("Q54_4")
multvarstest("Q54_5")
multvarstest("Q54_6")
multvarstest("Q54_7")
multvarstest("Q54_8")
multvarstest("Q54_9")

dat <- inner_join(edplacedata, compusedat, by = c("studentid", "year"))

dattest <- data.frame(matrix(ncol = 7, nrow = 3))
for(i in 1:7){  
  analysis <- dat %>%
    filter(year == yrs[i]) %>%
    select(-year) %>%
    group_by(Q143, Q17) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = "Q143", values_from = "count") %>%
    select(-Q17)
  analysis$`1` <- ifelse(is.na(analysis$`1`), .001, analysis$`1`)
  analysis$`2` <- ifelse(is.na(analysis$`2`), .001, analysis$`2`)
  analysis$`3` <- ifelse(is.na(analysis$`3`), .001, analysis$`3`)
  x <- chisq.test(analysis)
  dattest[1,i] <- x$statistic
  dattest[2,i] <- x$p.value
  dattest[3,i] <- cramerV(as.matrix(analysis))
}






multvarstest = function(qid){
  dattest <<- data.frame(matrix(ncol = 7, nrow = 3))
  for(i in 1:7){  
    analysis <<- dat %>%
      filter(quest == qid, year == yrs[i]) %>%
      select(-quest, -year) %>%
      group_by(val, Q16) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = "val", values_from = "count") %>%
      select(-Q16)
    analysis$`1` <<- ifelse(is.na(analysis$`1`), .001, analysis$`1`)
    analysis$`5` <<- ifelse(is.na(analysis$`5`), .001, analysis$`5`)
    x <<- chisq.test(analysis)
    dattest[1,i] <<- x$statistic
    dattest[2,i] <<- x$p.value
    dattest[3,i] <<- cramerV(as.matrix(analysis))
  }
  View(dattest)
}

dat <- inner_join(disdata, readdata, by = c("studentid", "year"))
multvarstest("Q51_1")
multvarstest("Q51_2")
multvarstest("Q51_3")
multvarstest("Q51_4")
multvarstest("Q51_5")
multvarstest("Q51_6")
multvarstest("Q51_7")
multvarstest("Q51_8")

dat <- inner_join(disdata, mathdata, by = c("studentid", "year"))
multvarstest("Q54_1")
multvarstest("Q54_10")
multvarstest("Q54_11")
multvarstest("Q54_12")
multvarstest("Q54_13")
multvarstest("Q54_2")
multvarstest("Q54_3")
multvarstest("Q54_4")
multvarstest("Q54_5")
multvarstest("Q54_6")
multvarstest("Q54_7")
multvarstest("Q54_8")
multvarstest("Q54_9")

dat <- inner_join(disdata, compusedat, by = c("studentid", "year"))

dattest <- data.frame(matrix(ncol = 7, nrow = 3))
for(i in 1:7){  
  analysis <- dat %>%
    filter(year == yrs[i]) %>%
    select(-year) %>%
    group_by(Q143, Q16) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = "Q143", values_from = "count") %>%
    select(-Q16)
  analysis$`1` <- ifelse(is.na(analysis$`1`), .001, analysis$`1`)
  analysis$`2` <- ifelse(is.na(analysis$`2`), .001, analysis$`2`)
  analysis$`3` <- ifelse(is.na(analysis$`3`), .001, analysis$`3`)
  x <- chisq.test(analysis)
  dattest[1,i] <- x$statistic
  dattest[2,i] <- x$p.value
  dattest[3,i] <- cramerV(as.matrix(analysis))
}
