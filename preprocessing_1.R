### libraries

library(readxl)
library(lubridate)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(topicmodels)
library(ggplot2)
library(stm)
library(parallel)
library(stringr)
library(ggmap)
library(sf)
library(jsonlite)
library(magrittr)
set.seed(580124)


### Data wrangling

# Import
data <- read_excel("data.xlsx")    # 'immigr'
data2 <- read_excel("data2.xlsx")  # 'asylum'
data3 <- read_excel("data3.xlsx")  # 'migrant'

# Variable reduction
v_list <- c("id", "created_at", "language", "content", "account.location", "account.username", 
            "account.followers_count", "uri", "url", "account.display_name", "account.created_at", "account.note", 
            "account.url", "account.last_status_at", "account.following_count", "account.verified", "account.website")
df1 <- data %>% 
  select(all_of(v_list))
df2 <- data2 %>% 
  select(all_of(v_list))
df3 <- data3 %>% 
  select(all_of(v_list))

# Merging
df <- bind_rows(df1, df2, df3)

# Variable renaming
df <- df %>%
  rename(
    time = created_at,
    lang = language,
    truth = content,
    loca = account.location,
    name = account.username,
    follower = account.followers_count)

# Date Converting
df$time <- ymd_hms(df$time)

## Text pre-processing  
# Tokenization
text <- df %>% unnest_tokens(word, truth) 

# Initial stop words
data(stop_words)
text <- text %>% anti_join(stop_words, by = "word")

# Additional stop words
dumps <- c("span", "class", "https", "href", "rel", "br", "truthsocial.com",
           "mention", "apos", "target", "noreferrer", "noopener", "nofollow",
           "_blank", "link", "ellipsis", "links.truthsocial.com", "â", 
           "tag", "tags", "hashtag", "ðŸ", "amp", "quot", "www", "invisible",
           "card", "url", "2024", "ï", "users", "statuses", "itâ", "donâ", 
           "time", "2", "foxnews.com", "ºðÿ", "02", "utm_source", "1", "don",
           "03", "maga", "3", "rt", "thegatewaypundit.com", "inline", "apps",
           "utm_medium", "realdonaldtrump", "ð", "video", "fox", "4", "utm_campaign",
           "theyâ", "thatâ", "ve", "iâ", "10", "bidenâ", "5", "trump2024", "20", 
           "http", "01", "youâ", "breitbart.com", "8", "href", "clessmention", 
           "classinvisible", "hashtag ", "ppa", "relnofollow", "intervene.ppa",
           "a href", "havenapost")
dump_words <- data.frame(word = dumps, lexicon = "SMART")
remove <- tibble(text_dumps=dumps) %>% 
  unnest_tokens(word, text_dumps) 
text <- text %>% 
  anti_join(remove, by = "word") 


# Stemming 
text <- text %>%
  mutate(across(.cols = "word", .fns = ~wordStem(., language = "en")))

# Gsub
df <- df %>% 
  mutate(truth = gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]", "", truth)) %>%  
  mutate(truth = gsub("(http[^ ]*)|(www.[^ ]*)", "", truth)) %>% 
  mutate(truth = gsub("’", "'", truth)) %>% 
  mutate(truth = gsub("[^A-Za-z0-9 .,!?]", "", truth)) %>% 
  mutate(truth = gsub("\\b\\S{15,}\\b", "", truth)) %>% 
  mutate(truth = gsub("^p|p$", "", truth)) %>%
  mutate(truth = gsub("\\.pp", "", truth)) %>%
  mutate(truth = gsub("\\bpp(a)?|pp(a)?\\b", "", truth)) %>%
  mutate(truth = gsub("span$", "", truth)) %>%
  mutate(truth = gsub("apos$", "", truth)) %>%
  mutate(truth = gsub("url$", "", truth)) %>%
  mutate(truth = gsub("^span", "", truth)) %>%
  mutate(truth = gsub("^html", "", truth)) %>%
  mutate(truth = gsub("^url", "", truth)) %>%
  mutate(truth = gsub("^apos", "", truth)) %>%
  mutate(truth = gsub("br$", "", truth)) %>%
  distinct(truth, .keep_all = TRUE)