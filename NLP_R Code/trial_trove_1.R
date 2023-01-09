library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(stringr)
library(tm)
library(readr)
library(readxl)
library(tm)


fileName <- read_excel("TrialTrove_NSCLC.xlsx")
trials_result <- fileName
colnames(trials_result)

trials_result <- trials_result[,c(1,3,8,17,25,27,28,48:50,67,68,70:72,74,75)]
head(trials_result)
colnames(trials_result)
trials_result <- trials_result %>% 
  pivot_longer(cols = 2:16, names_to = "grp", values_to = "descript")
head(trials_result)

trials_result <- as_tibble(trials_result)

trials_result <- unnest_tokens(trials_result,
                               bigram,
                               descript,
                               token = "ngrams",
                               n=2)
head(trials_result %>% 
       count(bigram,sort=T))

head(trials_result %>% 
       count(bigram,sort=T),10)

#there are so many useless words, these may imp in grammer
# but not in the semantic or meaning of the text

# remove stopwords
library(tidyr)

# but to remove stopwords first we need to separate bigram
# to single word

trials_result_separate <- trials_result %>% 
  separate(bigram,c("word1","word2"),sep = " ")

#now see the DF word1 word2 separated

#Now remove stopwords from word1 column and word2 column
trials_result_separate <- trials_result_separate %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) 
dim(trials_result_separate)

trials_result_counts <- trials_result_separate %>% 
  count(word1,word2)
head(trials_result_counts)

#need to use sort=T
trials_result_counts <- trials_result_separate %>% 
  count(word1,word2, sort=T)
head(trials_result_counts)

#now combine as bigram after removng stop words
trials_result_united <- trials_result_separate %>% 
  unite(bigram, word1, word2, sep = " ")

head(trials_result_united %>% 
       count(bigram,sort=T),10)

#visualization

library(wordcloud)
pal <- brewer.pal(8,"Dark2")

trials_result_united %>% 
  count(bigram) %>% 
  with(wordcloud(bigram,n,
                 max.words = 1000,
                 colors = pal))
# brewer is not working, no diff colors showing...in the pic
graphics.off()

trials_result_united %>% 
  count(bigram) %>% 
  filter(n>1000) %>% 
  ggplot(aes(x=bigram,y=n))+
  geom_col(fill="darkgreen")+coord_flip()

#use mutate

trials_result_united %>% 
  group_by(grp) %>% 
  count(bigram) %>% 
  filter(n>500) %>% 
  #ungroup() %>%
  mutate(bigram=reorder(bigram,n)) %>%
  ggplot(aes(x=bigram,y=n))+
  facet_wrap(~grp, ncol = 2, scales = "free")+
  geom_col(fill="darkgreen")+coord_flip()

  
#network graphs##########################################################

library(ggraph)
library(igraph)

graphics.off()
bigram_graph <- trials_result_united %>% 
  count(bigram) %>% 
  filter(n>250) %>%   #you can try 5/10/15/20 etc based on visibility of cluster
  graph_from_data_frame()

ggraph(bigram_graph,layout = "fr")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),
                 vjust=1,hjust=1,
                 color="turquoise4")
  
 ####END**************************************************** 















## TF-IDF#################################

# work on it no results

fileName <- read_excel("TrialTrove_NSCLC.xlsx")
tt_tf_idf <- fileName
colnames(tt_tf_idf)

tt_tf_idf <- tt_tf_idf[,c(1,3,8,17,25,27,28,48:50,67,68,70:72,74,75)]
head(tt_tf_idf)
colnames(tt_tf_idf)

tt_tf_idf <- tt_tf_idf %>% 
  pivot_longer(cols = 2:17, names_to = "grp", values_to = "descript")
head(tt_tf_idf)
colnames(tt_tf_idf)

tt_tf_idf1 <- tt_tf_idf %>% 
  unnest_tokens(input = descript, output = word)


tt_tf_idf1 <- tt_tf_idf1  %>% 
  count(word, sort = TRUE)

tt_tf_idf1 

tt_tf_idf1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))

###*********************************************************




book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)
book_tf_idf








tt_tf_idf1 <- tt_tf_idf$grp

head(tt_tf_idf1)



total_words <- tt_tf_idf1 %>%
  group_by() %>%
  mutate(tt_tf_idf$grp=reorder(word,n))
   summarize(total = sum(n))
   
   
   
   
   
   
book_words <- left_join(tt_tf_idf1, total_words)
book_words



covid_bigram_tfidf <- tidy_covid_united %>% 
  count(TITLE, bigram) %>% 
  bind_tf_idf(bigram,TITLE,n) %>% 
  arrange(desc(tf_idf))

head(covid_bigram_tfidf )



























library(dplyr)
library(janeaustenr)
library(tidytext)
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>%
  group_by(book) %>%
  mutate(=reorder(bigram,n))
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)
book_words
#> # A tibble:













  
