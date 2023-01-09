# https://www.youtube.com/watch?v=dNcIaEHoCmw&ab_channel=R_PyDataScience


library(bibliometrix)
library(dplyr)
library(ggplot2)
library(tidytext)
library(bib2df)
library(wordcloud)
#url <- "https://github.com/ravitejatarun/textmine/blob/main/covid19.bib"

url <- "https://raw.githubusercontent.com/ravitejatarun/textmine/main/covid19.bib?token=GHSAT0AAAAAABRKWJCGRWYJJT6W74ZAL6GIYQYXZVA"
#since you are using raw file need to copy everytime and paste i think to avoid erros

?convert2df #Import and Convert bibliographic export files and API objects

covid_analysis <- convert2df(url,dbsource= "isi",format="bibtex")

View(covid_analysis)

?biblioAnalysis

results <- biblioAnalysis(covid_analysis,sep=",")
options(width = 100)  # 100 number of docs

s <- summary(object = results,k=10,pase=FALSE) #10 prominent authors, will rank authors; 

?bibliometrix
# performing bibliometric analysis and building networks for co-citation, coupling, scientific collaboration
# and co-word analysis.

# you can see prominent protals in the fields; one authors published more than few articles
#most of the articles in 2020 few in 2021


#co-occurances, cluster dendrogram will be explored

#PART2 VIDEO####################
#  DE/TI/AB

CS <- conceptualStructure(covid_analysis,
                          field="DE",
                          method = "CA",
                          minDegree = 4,
                          stemming = FALSE,
                          labelsize = 10,
                          documents = 2)
?conceptualStructure

# The function conceptualStructure creates a conceptual structure map of a 
# scientific field performing Correspondence Analysis (CA), Multiple 
# Correspondence Analysis (MCA) or Metric Multidimensional Scaling (MDS) and
# Clustering of a bipartite network of terms extracted from keyword, title 
# or abstract fields.

#Sanky plots ----- explore in google....

threeFieldsPlot(covid_analysis,
                fields = c("AU","DE", "SO"))
threeFieldsPlot(covid_analysis,
                fields = c("AU","DE", "SO"),n=c(10,10,10))

threeFieldsPlot(covid_analysis,
                fields = c("DE","AB", "SO"),n=c(10,10,10))

# threeFieldPlot   https://www.youtube.com/watch?v=jBb1iha6-sg

?threeFieldsPlot

biblioshiny() # it installs and opens in the browse. 
###********************
threeFieldsPlot(trials_result,
                fields = c("Patient Segment","Primary Tested Drug", "Oncology Biomarker"))




?threeFieldsPlot()
#  threeFieldsPlot(M, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))

# Visualize the main items of three fields (e.g. authors, keywords, journals), and how they are 
# related through a Sankey diagram.

# part3########################################



library(bibliometrix)
library(dplyr)
library(ggplot2)
library(tidytext)
library(bib2df)
library(wordcloud)


library(bib2df)
url <- "https://raw.githubusercontent.com/ravitejatarun/textmine/main/covid19.bib?token=GHSAT0AAAAAABRKWJCHJA4VNJUKB57HR2POYQVI7LQ"
#  use fresh link all the time

covid19 <- bib2df(url)
str(covid19)
glimpse(covid19)  # we may not use all the variable, may use author keyword, abstract

#DOI 1990 add of doi started recently, can not find with older publications

which(!complete.cases(covid19$DOI))
# number  87 551 articles dont have DOI

sum(is.na(covid19$DOI))
# 2 articles dont have DOI

sum(is.na(covid19$ABSTRACT))
# 159 articles dont have DOI

table(covid19$CATEGORY)

covid19new <- covid19 %>% 
  filter(!is.na(ABSTRACT)) #remove recrods with missing abstracts

sum(!is.na(covid19new$ABSTRACT))

table(covid19new$CATEGORY) # we interested only in the articles not misc (these could be books, news, online info etc.)

# so filter out articles

covid19new <- covid19new %>% 
  filter(CATEGORY=="ARTICLE")
covid19new

#  once data is ready:  Text Analysis
#tidytext, qunateda, text2vwx, tm

#tokenization  

covid_data <- covid19new %>% 
  select("TITLE","ABSTRACT", "KEYWORDS","AUTHOR")

library(tidytext)

?tidytext
#tidytext: Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools


# PART 4  ########################

tidy_cocid <- covid_data %>% 
  unnest_tokens(input = ABSTRACT, output = word)
tidy_cocid

tidy_cocid$word

?unnest_tokens

?stop_words  #A data frame with 1149 rows and 2 variables:

tidy_covid <- tidy_cocid %>% 
  anti_join(stop_words)
tidy_covid

# custom_stop_words <- c("background","methods","results", "conclusion")
# 
# 
# stop_words_update <- c(custom_stop_words, stop_words)
# 
# tidy_cocid <- tidy_cocid %>% 
#   anti_join(stop_words_update)mydata_df
# tidy_covid

#you need to figure out.....

library(ggplot2)

tidy_covid %>% 
  count(word,sort = T) %>% 
  filter(n>100) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(x=word,y=n)) +
  geom_col(fill="darkgreen")+
  coord_flip()

library(wordcloud)
pal <- brewer.pal(8,"Dark2")

tidy_covid %>% 
  count(word) %>% 
  with(wordcloud(word,n,
                 max.words = 200,colors = pal))

graphics.off()

#Part 5: Text analysis, bigrams and visualizing word relationships in network graphs####

# bigram
tidy_covid <- unnest_tokens(covid_data,
                            bigram,
                            ABSTRACT,
                            token = "ngrams",
                            n=2)
head(tidy_covid %>% 
       count(bigram,sort=T))

head(tidy_covid %>% 
       count(bigram,sort=T),10)

#there are so many useless words, these may imp in grammer
# but not in the semantic or meaning of the text

# remove stopwords

library(tidyr)

# but to remove stopwords first we need to separate bigram
# to single word

tidy_covid_separate <- tidy_covid %>% 
  separate(bigram,c("word1","word2"),sep = " ")

#now see the DF word1 word2 separated

#Now remove stopwords from word1 column and word2 column

tidy_covid_separate <- tidy_covid_separate %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) 

head(tidy_covid_separate)
dim(tidy_covid_separate)

covid_bigram_counts <- tidy_covid_separate %>% 
  count(word1,word2)

head(covid_bigram_counts)

#need to use sort=T
covid_bigram_counts <- tidy_covid_separate %>% 
  count(word1,word2, sort=T)

head(covid_bigram_counts)

dim(tidy_covid_separate)
dim(covid_bigram_counts)  

#now combine as bigram after removng stop words
tidy_covid_united <- tidy_covid_separate %>% 
  unite(bigram, word1, word2, sep = " ")

head(tidy_covid_united %>% 
  count(bigram,sort=T),10)

#visualization

library(wordcloud)
pal <- brewer.pal(8,"Dark2")

tidy_covid_united %>% 
  count(bigram) %>% 
  with(wordcloud(bigram,n,
                 max.words = 200,
                 colors = pal))
# brewer is not working, no diff colors showing...in the pic
graphics.off()

tidy_covid_united %>% 
  count(bigram) %>% 
  filter(n>50) %>% 
  ggplot(aes(x=bigram,y=n))+
  geom_col(fill="darkgreen")+coord_flip()

#use mutate

tidy_covid_united %>% 
  count(bigram) %>% 
  filter(n>50) %>% 
  mutate(bigram=reorder(bigram,n)) %>% 
  ggplot(aes(x=bigram,y=n))+
  geom_col(fill="darkgreen")+coord_flip()

#network graphs

library(igraph)

bigram_graph <- tidy_covid_united %>% 
  count(bigram) %>% 
  filter(n>5) %>%   #you can try 5/10/15/20 etc based on visibility of cluster
  graph_from_data_frame()

library(ggraph)
set.seed(2021)

ggraph(bigram_graph,layout = "fr")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),
                 vjust=1,hjust=1,
                 color="turquoise4")

#you can see how words related each other; ex; air quality with mers cov etc.
#there are many numbers which wont give any sense, if case genetic analysis give some sense.
# you need to filter out some of the use less text as well.
# you can also use this graph in case of sentiment analysis with positive and negative sentiment association
graphics.off()

######********END************************

#Part 6: Text Analysis, TF-IDF (term freq, inverse document frequency) intuition and examples in R ###################

# 232 words in one abstract and 9 are covid19 terms 

(TF <- 9/232)  # 0.0388
9/232

#382 total documents
#300 documents have COVID19 term

(IDF <- log(382/300)) # 0.2416

#382/300 = 1.27
log(1.27)

(TF_IDF <- TF*IDF) # 0.00937 which is very low number; i.e.almost each document contains the term COVID19
0.0388*0.2416   #  = 0.00937408


#Part 7: Bigrams, Tf-Idf and visualizing the most influential bigrams in R ###########

#TF-IDF

covid_bigram_tfidf <- tidy_covid_united %>% 
  count(TITLE, bigram) %>% 
  bind_tf_idf(bigram,TITLE,n) %>% 
  arrange(desc(tf_idf))

head(covid_bigram_tfidf )

# GRAPH

covid_bigram_tfidf %>% 
  top_n(10) %>% 
  ggplot(aes(bigram,tf_idf)) +
  geom_col(fill="turquoise4")


#********************************************************************************

tidy_covid_united %>% 
  count(bigram) %>% 
  filter(n>50) %>% 
  mutate(bigram=reorder(bigram,n)) %>% 
  ggplot(aes(x=bigram,y=n))+
  geom_col(fill="darkgreen")+coord_flip()


# covid_bigram_tfidf %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(bigram=factor(bigram,
#                      levels(rev(unique(bigram))))) %>%
#   top_n(10) %>%
#   ggplot(aes(bigram,tf_idf)) +
#   geom_col(fill="turquoise4")

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(50) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")



#********************************************************************************

































