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


### Data wrangling

# Import
data <- read_excel("data.xlsx")

# Variable reduction
df <- data %>% 
  select(id, created_at, language, content, account.location, account.username, account.followers_count,
         uri, url, account.display_name, account.created_at, account.note, account.url, account.last_status_at, 
         account.following_count, account.verified, account.website)

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
           "http", "01", "youâ", "breitbart.com", "8")
remove <- tibble(text_dumps=dumps) %>% 
  unnest_tokens(word, text_dumps) 
text <- text %>% 
  anti_join(remove, by = "word") 

# Stemming 
text <- text %>%
  mutate(across(.cols = "word", .fns = ~wordStem(., language = "en")))

