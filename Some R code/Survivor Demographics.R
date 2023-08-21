#Semester Project Part 2
survivor <- read.csv("C:/Users/b864b226/Desktop/Data 824/Survivor Dataset.csv")

library(summarytools)
library(tidyverse)
library(knitr)
library(kableExtra)
library(corrplot)

descrip <- descr(survivor)
descrip <- as.data.frame(t(descrip))
descrip$outlierminTukey <- descrip$Median - 1.5*descrip$IQR
descrip$outliermaxTukey <- descrip$Median + 1.5*descrip$IQR
descrip$outlierminMAD <- descrip$Median - 3*descrip$MAD
descrip$outliermaxMAD <- descrip$Median + 3*descrip$MAD
descrip$vars <- rownames(descrip)
descrip <- descrip %>%
  select(Mean, Std.Dev, Median, Min, Max, IQR, outlierminTukey, outliermaxTukey, MAD, outlierminMAD, outliermaxMAD)
descrip[,] <-round(descrip[,],2)

print(dfSummary(survivor), method = "viewer")

descrip %>%
  kable() %>%
  kable_styling() %>%
  save_kable(file = "table1.html", self_contained = T)


MAD_outlier = function(varname){
  survivor %>%
    select(Name, varname) %>%
    filter_(paste0(varname, "< descrip[varname, 'outlierminMAD'] | ", varname, "> descrip[varname, 'outliermaxMAD']"))
  }

varnames <- rownames(descrip)
outliers = list()
j = 1
for(i in 1:length(varnames)){
  outliers[[j]]=MAD_outlier(varnames[i])
  j = j + 1
}

survivornum <- survivor %>%
  select(Appearance, Age, Season, Placement, DaysIn, VotesAgainst, IndImmunities,
         TC_Pct, TC_Score, Idols_Found, Idols_Played, Votes_Voided, Boot_Avoided,
         Tie_Avoided, Merge, Jury, Final_Tribal, RunnerUp, Winner,
         AppStatus, Gender, Average_Confessionals)
corrs <- cor(survivornum, use = "pairwise.complete.obs")
corrplot(corrs, type = "upper", order = "alphabet", tl.col = "black")

#Semester Project Part 3
library(ggplot2)
library(gganimate)
library(gifski)
library(maps)
library(mapdata)

States = map_data("state")

US <- States %>%
  ggplot(aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3) +
  geom_polygon(color="black", fill="grey87")

survivormap <- survivor %>%
  mutate(region = tolower(LocationFrom)) %>%
  select(Name, Season, region, AppStatus)  %>%
  filter(AppStatus == 1) %>%
  select(-AppStatus) %>%
  group_by(Season, region) %>%
  summarize(count = n()) %>%
  ungroup %>%
  add_row(Season = 8, region = "district of columbia", count = .01) %>%
  add_row(Season = 20, region = "district of columbia", count = .01) %>%
  add_row(Season = 27, region = "district of columbia", count = .01) %>%
  add_row(Season = 31, region = "district of columbia", count = .01) %>%
  add_row(Season = 34, region = "district of columbia", count = .01) %>%
  arrange(Season) %>%
  inner_join(States, by=c("region" = "region"))

applicants <- US +
  geom_polygon(data=survivormap, aes(x=long, y=lat, group=group, fill=count), color="black") +
  labs(title="Season {frame_time} Applicants") +
  transition_time(Season) +
  scale_fill_gradient(low = "wheat", high = "orangered4", limits = c(1, 10), breaks = c(2, 4, 6, 8, 10),
                      guide = guide_legend(
                        title = "Number of Contestants")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

survivormap <- survivor %>%
  mutate(region = tolower(LocationFrom)) %>%
  select(Name, Season, region, AppStatus)  %>%
  filter(AppStatus == 0) %>%
  select(-AppStatus) %>%
  group_by(Season, region) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  add_row(Season = 2, region = "district of columbia", count = .01) %>%
  add_row(Season = 4, region = "district of columbia", count = .01) %>%
  arrange(Season) %>%
  inner_join(States, by=c("region" = "region"))

recruits <- US +
  geom_polygon(data=survivormap, aes(x=long, y=lat, group=group, fill=count), color="black") +
  labs(title="Season {frame_time} Recruits") +
  transition_time(Season) +
  scale_fill_gradient(low = "wheat", high = "orangered4", limits = c(1, 10), breaks = c(2, 4, 6, 8, 10),
                       guide = guide_legend(
                         title = "Number of Contestants")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

animate(recruits, duration = 37, nframes = 37)
anim_save("C:/Users/b864b226/Desktop/Data 824/recruit.html")
animate(applicants, duration = 37, nframes = 37)
anim_save("C:/Users/b864b226/Desktop/Data 824/applicant.gif")

survivormappost <- survivor %>%
  select(Name, Season, LocationFrom, AppStatus) %>%
  group_by(Season, AppStatus, LocationFrom) %>%
  summarize(`Number of Participants` = n())

statedata <- read.xlsx("C:/Users/b864b226/Downloads/nst-est2018-01.xlsx", startRow = 2)
statedata <- statedata %>%
  gather("Year", "Population", -X1)
colnames(statedata) <- c("State", "Year", "Population")

USpops <- statedata %>%
  filter(State == "United States") %>%
  select(-State)
colnames(USpops) <- c("Year", "USPop")

statedata <- statedata %>%
  left_join(USpops, by=c("Year" = "Year")) %>%
  filter(!State == "United States") %>%
  mutate(Percent = round((Population/USPop)*100, 2))

survivormappost <- read.xlsx("C:/Users/b864b226/Desktop/Data 824/survivormappost.xlsx")
survivormappost$YearFilmed <- as.character(survivormappost$YearFilmed)
colnames(survivormappost) <- c("Season", "AppStatus", "LocationFrom", "Number of Participants", "YearFilmed")
survivormappost <- survivormappost %>%
  left_join(statedata, by = c("LocationFrom" = "State", "YearFilmed" = "Year")) %>%
  group_by(Season, AppStatus) %>%
  mutate(NAppbySeason = sum(`Number of Participants`)) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(NbySeason = sum(`Number of Participants`)) %>%
  ungroup() %>%
  mutate(PctAppbySeason = round((`Number of Participants`/NAppbySeason)*100,2)) %>%
  mutate(PctbySeason = round((`Number of Participants`/NbySeason)*100,2)) %>%
  mutate(PctObsMinExpwApp = (PctAppbySeason - Percent)) %>%
  mutate(PctObsMinExp = (PctbySeason - Percent))

statedata2 <- statedata
statedata2$PctAppbySeason <- 0.00
statedata2$PctbySeason <- 0.00
statedata2$PctObsMinExpwApp <- -1*statedata2$Percent
statedata2$PctObsMinExp <- -1*statedata2$Percent

survivormappost2 <- survivormappost %>%
  rename("State" = "LocationFrom", "Year" = "YearFilmed")

year <- 2000

for(i in 1:37) {
  season <- i
  statedata3 <- statedata2 %>%
    filter(Year == year)
  statedata3$Season <- NA
  statedata4 <- statedata3
  statedata3$AppStatus <- 0
  statedata4$AppStatus <- 1
  statedata3 <- rbind(statedata3, statedata4)
  statedata3$Season <- season
  stateinseasonapp <- survivormappost2 %>%
    filter(Season == season, AppStatus == 1) %>%
    select(State)
  stateinseasonrec <- survivormappost2 %>%
    filter(Season == season, AppStatus == 0) %>%
    select(State)
  for(j in 1:nrow(statedata3)){
    if((!statedata3$State[j] %in% stateinseasonapp$State) & statedata3$AppStatus[j] == 1){
      
      survivormappost2 <- full_join(survivormappost2, statedata3[j,])
    }
    else if((!statedata3$State[j] %in% stateinseasonrec$State) & statedata3$AppStatus[j]==0){
      survivormappost2 <- full_join(survivormappost2, statedata3[j,])
    }
    else{}
  }
  if(season%%2 == 0){
    year <- year + 1
  }
  season <- season + 1
}

write.xlsx(survivormappost2, "C:/Users/b864b226/Desktop/Data 824/survivormappost2.xlsx")

rm(survivormappost, statedata3, statedata4, stateinseasonapp, stateinseasonrec, i, j, season, year)

survivormappost3 <- survivormappost2 %>%
  select(State, `Number of Participants`) %>%
  group_by(State) %>%
  summarize(count = sum(`Number of Participants`, na.rm=T)) %>%
  mutate(pct = round((count/sum(count))*100,2))

write.xlsx(survivormappost3, "C:/Users/b864b226/Desktop/Data 824/survivormappost3.xlsx")
