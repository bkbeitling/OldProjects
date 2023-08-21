OEoutputN4 <- readRDS("S:/Projects/DLM Secure/Staff Projects/Brianna Projects/Teacher Survey/Teacher Survey 2021/OEoutputN4.rds")
library(tidyverse)
library(tidytext)

textdf <- OEoutputN4 %>%
  unnest_tokens(word, Response)
data(stop_words)
textdf <- textdf %>%
  anti_join(stop_words, by = "word")
textdf <- textdf %>%
  filter(!word == "nbsp")
textdf %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  labs(x = "", y = "", title = "Most Common Words") +
  coord_flip()
