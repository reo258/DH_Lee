# Filtering
text_a <- text %>%
  filter(!is.na(party), time >= as.Date("2024-01-01"), lang == "en")
df_a <- df %>%
  filter(!is.na(party), time >= as.Date("2024-01-01"), lang == "en")   

# Metadata
meta <- df_a %>% select(time, party)

# preprocessing
processed <- textProcessor(df_a$truth, 
                           metadata = meta,
                           customstopwords = dump_words$word)
output <- prepDocuments(processed$documents, 
                        processed$vocab, 
                        processed$meta, 
                        lower.thresh = 10)

# Finding K
search.results <- searchK(output$documents, output$vocab,
                          K = c(5,10,15,20,30,40),
                          data = output$meta,
                          proportion=0.2) 
plot(search.results, type = "summary")

# STM
K <- 20 
fit <- stm(documents = output$documents, vocab = output$vocab, 
           K=K,
           data = output$meta, 
           prevalence = ~ party + s(time),
           content = ~ party, 
           verbose = TRUE)
plot(fit, topics = 1:20, type = "summary")

# Reviewing documents
doc.props <- make.dt(fit) 
doc.count <- dim(doc.props)[1] 
top10 <- doc.props %>% summarize_if(is.numeric, sum, na.rm = TRUE) %>% 
  select_if(str_detect(names(.), "Topic")) %>%
  divide_by(doc.count) %>% t() %>% as.data.frame() %>%
  top_n(10)
print(top10)

# Topic words
z <- c(1:20)
labelTopics(fit, topics=z, n=20)

# Finding actual posts
thoughts <- findThoughts(fit, texts = as.character(df_a[as.numeric(rownames(output$meta)),]$truth), topic=z, n = 50)
  thoughts$docs$`Topic 17`[1:10]
  
# By political parties
plot(fit, type="perspectives", topics=5)
  
