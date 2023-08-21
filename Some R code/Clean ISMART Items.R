########## I-SMART Teacher Survey Data Cleaning ##########

#Load packages
needed_packages <- c("tidyverse", "RSQLite", "openxlsx", "lubridate", "here",
                     "glue")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
vapply(needed_packages, load_packages, logical(1))

#Load data
ismart <- read.csv("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/ISMART 2019/Teacher_Survey_Forms.csv")
questions <- read.xlsx("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/ISMART 2019/MatrixIDs.xlsx")

#Filter out incomplete or unneeded data
ismart <- ismart %>%
  filter(teststatus == "complete", grepl("I-SMART Teacher Survey", specificationname), is.na(exitwithdrawaldate)) %>%
  arrange(id, externaltaskid, enddatetime)

ismart$id <- paste0(ismart$id, "_", ismart$testsectionid)

#Data to 'longer' format (one answer column per student/item)
ismartlong <- ismart %>%
  select(id, externaltaskid, foilsorder, responsetext) %>%
  gather(type, answer, foilsorder:responsetext)

#Remove data-less rows
ismartlong <- ismartlong %>%
  filter(!(answer == "NA") & !(answer == "null") & !(answer == ""))

#Add readable response
ismartread <- ismart %>%
  select(id, externaltaskid, readableresponse)
ismartlong <- left_join(ismartlong, ismartread, by = c("id", "externaltaskid"))

#Divide data up (just need matrix for now but just in case for the future...)
MC <- ismartlong %>%
  filter(type == "foilsorder")

matrix <- ismartlong %>%
  filter(grepl("foilId", answer))

OE <- ismartlong %>%
  filter(grepl("~", answer))

MCMS <- ismartlong %>%
  filter(type == "responsetext", !grepl("foilId", answer), !grepl("~", answer))

#Clean up matrix items
questionstable <- questions %>%
  filter(type == "matrix") %>%
  select(combo, column_key) %>%
  distinct()
rowpattern <- "foil-.-[:digit:]+"
colpattern <- "column-.-[:digit:]+"
matrix$tablerow <- str_extract_all(matrix$answer, rowpattern)
matrix$tablecol <- str_extract_all(matrix$answer, colpattern) 
matrix <- matrix %>%
  select(-readableresponse) %>%
  separate_rows(tablerow, tablecol) %>%
  filter(!tablerow == "c", !tablerow == "foil", !tablerow == "", !tablecol == "c", 
         !tablecol == "column", !tablecol == "")

matrix$zipper <- rep(1:(nrow(matrix)/2), each = 2)
matrix <- matrix %>%
  group_by(zipper) %>%
  summarise_all(max, na.rm = TRUE)
matrix$combo <- paste0(matrix$externaltaskid, "_", matrix$tablerow)
matrix <- matrix %>%
  left_join(questionstable, "combo") %>%
  select(-tablerow, -answer, -zipper, -combo, -externaltaskid, -type) %>%
  filter(!is.na(column_key)) %>%
  spread(column_key, tablecol) %>%
  group_by(id) %>%
  summarise_all(max, na.rm = TRUE)

ismartdemos <- ismart %>%
  select(id, statestudentidentifier, grade, legalfirstname, legalmiddlename, legallastname, generationcode, username,
         firstlanguage, dateofbirth, gender, comprehensiverace, hispanicethnicity, primarydisabilitycode, esolparticipationcode,
         attendanceschoolidentifier, aypschoolidentifier, state, districtcode, district, schoolcode, school, educatorfirstname,
         educatorlastname, educatorusername, educatorstateid, educatorkiteid, final_ela, final_math, final_sci, comm_band,
         model, year)

matrixdemo <- left_join(ismartdemos, matrix, by = "id") %>%
  distinct()

write.xlsx(matrixdemo, "S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/ISMART 2019/MatrixItems.xlsx")
