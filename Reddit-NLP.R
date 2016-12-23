

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(viridis)
library(igraph)
library(widyr)
library(ggraph)
library(scales)
library(lubridate)
library(purrr)
library(broom)




require(RSQLite)
require(dplyr)
library(reshape2)
dataframe<- function(subreddit){
           sub<- subreddit
           db <- src_sqlite('database.sqlite', create = F)
           db_subset <- db %>%
              tbl('May2015')
            
            db_subset <- db %>% 
                tbl('May2015') %>% 
                filter(subreddit == sub)
            df <- data.frame(db_subset)  
            return(df)
                 }
                 
                 
                 
### Function for seeing the trending words in any subreddit through May 2015                 

reddit_words_may2015<- function(subreddit){
    df1<- dataframe(subreddit) 
    df1 <- df1 %>%
    mutate(created_utc = as.POSIXct(created_utc, 
                                    origin = "1970-01-01", 
                                    tz = "UTC")) %>%
    filter(created_utc > as.POSIXct("2015-05-01 01:00:00", tz = "UTC"))
tidy_df1<-unnest_tokens(df1,word,body)
dim(tidy_df1)
words_by_days <- tidy_df1 %>%
    filter(str_detect(word, "[a-z]")) %>%
    anti_join(data_frame(word = c("ref"))) %>%
    mutate( origin = "1970-01-01",created = floor_date(created_utc, unit = "24 hours")) %>%
    distinct(id, word, .keep_all = TRUE) %>%
    count(created, word) %>%
    ungroup() %>%
    group_by(created) %>%
    mutate(day_total = sum(n)) %>%
    group_by(word) %>%
    mutate(word_total = sum(n)) %>%
    ungroup() %>%
    rename(count = n) %>%
    filter(word_total > 500)
tail(words_by_days) 
nested_models <- words_by_days %>%
    nest(-word) %>%
    mutate(models = map(data, ~ glm(cbind(count, day_total) ~ created, ., 
                                    family = "binomial")))
slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "created")
slopes %>% 
    arrange(estimate)
slopes %>% 
    arrange(desc(estimate))
top_slopes <- slopes %>%
    top_n(10, estimate) 
words_by_days %>%
    inner_join(top_slopes, by = "word") %>%
    mutate(created = with_tz(created, tz = "America/Chicago")) %>%
    ggplot(aes(created, count/day_total, color = word)) +
    geom_line(alpha = 0.8, size = 1.3) +
    labs(x = "Time (Central Time Zone)", y = "Word frequency",
         subtitle = "lets see",
         title = "Trending words on the subreddit through May 2015 ")    
low_slopes <- slopes %>%
    top_n(-10, estimate)

words_by_days %>%
    inner_join(low_slopes, by = "word") %>%
    mutate(created = with_tz(created, tz = "America/Chicago")) %>%
    ggplot(aes(created, count/day_total, color = word)) +
    geom_line(alpha = 0.8, size = 1.3) +
    labs(x = "Time (Central Time Zone)", y = "Word frequency",
         subtitle = "lets see",
         title = "Trending words on the through May 2015 ")
         }
### Example with a subreddit         
reddit_words_may2015('soccer')



### Function for seeing the overall sentiment in a subreddit

sentiment_in_subreddit<-function(subreddit){
df1<-dataframe(subreddit)
data('stop_words')
tidy_df1<-unnest_tokens(df1,word,body)
tidy_df1 <- tidy_df1 %>%
        anti_join(stop_words)
tidy_df1 %>%
        count(word, sort = TRUE)        
bing <- sentiments %>%
        filter(lexicon == "bing") %>%
        select(-score)

commentsentiment <- tidy_df1 %>%
        inner_join(bing) %>% 
        count(subreddit, index =id , sentiment) %>% 
        spread(sentiment, n, fill = 0) %>% 
        mutate(sentiment = positive - negative)
ggplot(commentsentiment, aes(index, sentiment, fill = subreddit)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~subreddit, scales = "free_x") +
        theme_minimal(base_size = 13) +
        labs(title = "Sentiment in the subreddit",
            y = "Sentiment") +
        scale_fill_viridis(end = 0.025, discrete=TRUE, direction = -1) +
        scale_x_discrete(expand=c(0.02,0)) +
        theme(strip.text=element_text(hjust=0)) +
        theme(strip.text = element_text(face = "italic")) +
        theme(axis.title.x=element_blank()) +
        theme(axis.ticks.x=element_blank()) +
        theme(axis.text.x=element_blank())
        }
# example with a subreddit        
sentiment_in_subreddit('gameofthrones')


### Function to see which of two subreddits is more closely related to a given subreddit.

comparison_of_subs<-function(sub1,sub2,sub3){
df1<-dataframe(sub1)
tidy_df1<-unnest_tokens(df1,word,body)
tidy_df1 <- tidy_df1 %>%
       anti_join(stop_words) 
df2<- dataframe(sub2) 
tidy_df2<-unnest_tokens(df2,word,body)
data('stop_words')
tidy_df2 <- tidy_df2 %>%
       anti_join(stop_words)        
df3<- dataframe(sub3) 
tidy_df3<-unnest_tokens(df3,word,body)
data('stop_words')
tidy_df3 <- tidy_df3 %>%
        anti_join(stop_words)         
tidy_both <- bind_rows(
        mutate(tidy_df2, sub = sub2),
        mutate(tidy_df3, sub = sub3))
frequency <- tidy_both %>%
        mutate(word = str_extract(word, "[a-z]+")) %>%
        count(sub, word) %>%
        rename(other = n) %>%
        inner_join(count(tidy_df1, word)) %>%
        rename(sub1 = n) %>%
        mutate(other = other / sum(other),
               sub1 = sub1 / sum(sub1)) %>%
        ungroup()
ggplot(frequency, aes(x = other, y = sub1, color = abs(sub1 - other))) +
        geom_abline(color = "gray40") +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.4, height = 0.4) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), low = "chartreuse3", high = "CadetBlue3") +
        facet_wrap(~sub, ncol = 2) +
        theme_minimal(base_size = 14) +
        theme(legend.position="none") +
        labs(title = "Comparing Word Frequencies",
             subtitle = "Word frequencies in sub1 in relation to sub2 than to sub3 ",
             y = "sub1", x = NULL)
             }
# example with three subreddits
comparison_of_subs('MachineLearning','dataisbeautiful','tifu')






