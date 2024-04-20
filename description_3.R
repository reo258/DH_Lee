# Figure 1 - counting posts by time
daily_count <- df_a %>%
  mutate(date = date(time)) %>%
  group_by(date) %>%
  filter(date >= as.Date("2024-01-16")) %>% 
  summarise(count = n())

ggplot(daily_count, aes(x = date, y = count)) +
  geom_line() +
  labs(title = "Daily Data Count(2024)", x = "Date", y = "Count") +
  theme_minimal()


# Figure 2 - top 20 words
text_a %>%
  count(word, sort = TRUE) %>%
  slice(1:20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(color='blue') +
  labs(y = NULL, x='Term frequency', title="20 most frequent terms", size = 20)


# Figure 3 - words statistics
text_a %>%
  group_by(id) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  summarise(mean = mean(total_count), sd = sd(total_count)) %>% 
  ggplot(aes(x = factor(1), y = mean)) +
  geom_col(width = 0.3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + 2*sd),
                width = 0.2, position = position_dodge(width = 0.25)) +
  labs(title = "Mean and Standard Deviation of Word Counts",
       y = "Word Count") +
  theme_minimal()
  
text_a %>%
  group_by(id) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  summarise(mean = mean(total_count), sd = sd(total_count)) 
