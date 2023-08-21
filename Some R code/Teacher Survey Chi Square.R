library(tidyverse)
library(rcompanion)

ct <- function(var1, var2){
  table <- surveyanalysis %>%
    filter(State %in% c("Arkansas", "Delaware", "Iowa", "Kansas", "Missouri", "North Dakota")) %>%
    select(Studentid, var1 = var1, var2 = var2) %>%
    unique() %>%
    filter(!is.na(var1) & !is.na(var2))
  print(nrow(table))
  print(kruskal.test(var1 ~ var2, data = table))
  print(epsilonSquared(table$var1, table$var2))
  table <- table %>%
    group_by(var1, var2) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from = "var2", values_from = "count") %>%
    ungroup()
  view(table)
  table <- table %>%
    select(-var1)
  print(chisq.test(table))
  print(cramerV(as.matrix(table)))
  x <- chisq.test(table)
  print(x$expected)
}

ct("B1_2", "B1_1_1")
ct("B1_3", "B1_1_1")
ct("B1_2", "B1_1_4")
ct("B1_3", "B1_1_4")

ct("C_2", "B1_1_1")
ct("C_5", "B1_1_1")
ct("C_4", "B1_1_1")
ct("C_6_1", "B1_1_1")
ct("C_6_2", "B1_1_1")
ct("C_6_3", "B1_1_1")
ct("C_6_4", "B1_1_1")

ct("C_2", "B1_1_4")
ct("C_5", "B1_1_4")
ct("C_4", "B1_1_4")
ct("C_6_1", "B1_1_4")
ct("C_6_2", "B1_1_4")
ct("C_6_3", "B1_1_4")
ct("C_6_4", "B1_1_4")
