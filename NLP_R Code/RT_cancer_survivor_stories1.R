
#Word Counts In Cancer Survivor Stories - A tidytext approach

### 2) Word Counts In Survivor Stories - A tidytext approach
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(stringr)
library(tm)
library(tidyverse)
library(readtext)
survivor_stories <- readLines("RT_cancer_survivor_stories.txt")


can_surv_df <- data_frame(Text = survivor_stories)

# Preview the stories:
head(can_surv_df, n = 20)


#library(purrr)
#can_surv_df <- map_df(can_surv_df, ~ gsub("[^A-Za-z]", "", .x))


survivor_words <- can_surv_df %>% 
  unnest_tokens(output = word, input = Text)

head(survivor_words, n = 10)

custom_stop_words <- tibble(word =  c("cancer", "who", "other", "show", "â", "â·","this", "and", 
                                      "the","with", "for", "was","were",
                                      "from", "had", "that", "not", "all", "has",
                                      "are",  "â©",  "have","than",  "â·", "most",
                                      "these", "those", "but", "â·â·","been", "which",
                                      "can","say","one","way","use","also","howev","tell","will",
                                      "much","need","take","tend","even","particular","rather","said"
                              ))
# the cancer has >34000 so added to stopwords list

# join sentiments
survivor_words <- survivor_words %>%
  inner_join(survivor_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word")

survivor_words <- survivor_words %>%
  anti_join(stop_words)

# Word Counts:

survivor_wordcounts <- survivor_words %>% count(word, sort = TRUE)

head(survivor_wordcounts, n = 15)

print(survivor_wordcounts[16:30, ])
graphics.off()

survivor_wordcounts[1:30, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Top 30 Most Common Words In \n Cancer Survivor Stories \n") +
  geom_text(aes(label = n), hjust = 1, colour = "white", fontface = "bold", size = 3.5) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))



### 3) Bigrams (Two-Word Phrases) in Remember The Name:

survivor_bigrams <- can_surv_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

# Look at the bigrams:

survivor_bigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

survivor_bigrams_sep <- survivor_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

survivor_bigrams_filt <- survivor_bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# Filtered bigram counts:
survivor_bigrams_counts <- survivor_bigrams_filt %>% 
  count(word1, word2, sort = TRUE)

head(survivor_bigrams_counts, n = 15)

survivor_bigrams_counts <- survivor_bigrams_counts %>%
  unite(bigram, word1, word2, sep = " ")

survivor_bigrams_counts

# We can now make a plot of the word counts.
# ggplot2 Plot Of Top 20 Bigrams From Cancer Stories:

survivor_bigrams_counts[1:20, ] %>% 
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "darkgray") +
  coord_flip() +
  labs(x = "Bigram \n", y = "\n Count ", title = "Top 20 Bigrams In \n Cancer Survivor Stories \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))

# 4) Sentiment Analysis 
# Are the stories positive, negative, neutral?

# Using nrc, bing and AFINN lexicons

word_labels_nrc <- c(
  `negative` = "Negative Words",
  `positive` = "Positive Words"
)

### nrc lexicons:
get_sentiments("nrc")

survivor_words_nrc <- survivor_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(survivor_words_nrc)

# Sentiment Plot with nrc Lexicon (Word Count over 70)

survivor_words_nrc %>%
  filter(n > 10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3) +
  facet_wrap(~sentiment, nrow = 2, scales = "free_y", labeller = as_labeller(word_labels_nrc)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In The Cancer Survivor Stories \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12),
        strip.background = element_rect(fill = "#8DECC7"),
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.text.y = element_text(size = 11, face = "bold")) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide="none") + 
  coord_flip()



AFINN <- get_sentiments("afinn")
AFINN

negation_words <- c("not", "no", "never", "without")

#use bigrams separated data

not_words <-  survivor_bigrams_sep %>%
  filter(word1 %in% negation_words) %>%
  #filter(word1 == c("not" | "with out"|"no"| "never")) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")



























