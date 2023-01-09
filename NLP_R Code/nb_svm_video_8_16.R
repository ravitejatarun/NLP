#Part 8: Biomedical literature text analysis using Quanteda ###############

library(dplyr)
library(ggplot2)
library(caret)
library(bib2df) #read BibTex data
library(quanteda)

co_cancer <- bib2df("covid_CanPts.bib")
co_immuC <- bib2df("covid_immucomp.bib")

#Part 9: Splitting text data to pre-process in Quanteda  ############

#we are interested only in abstract, so removing/filtering articles w/o abstracts
# then selecting articles/dataset with only 6 imp variables

co_cancer <- bib2df("covid_CanPts.bib") %>% 
  filter(!is.na(ABSTRACT)) %>% 
  select("YEAR","TITLE","ABSTRACT", 
         "KEYWORDS","AUTHOR","BIBTEXKEY") 


co_immuC <- bib2df("covid_immucomp.bib")%>% 
  filter(!is.na(ABSTRACT)) %>% 
  select("YEAR","TITLE","ABSTRACT", 
         "KEYWORDS","AUTHOR","BIBTEXKEY")

#SMOT  we are not using now
# co_caner: Include; co_immuC: Exclude
co_cancer$Include <- 1
co_immuC$Include <- 0

co_can_immu <- rbind(co_cancer,co_immuC) 

table(co_can_immu$Include) %>% prop.table()

table(co_can_immu$Include) #distribution
 
# mydata$v1 <- factor(mydata$v1,
#                     levels = c(1,2),
#                     labels = c("label1", "label2"))

#assign values 0 or 1 as factor
co_can_immu$Include <- factor(co_can_immu$Include, 
                              levels = c(0,1),
                              labels=c("co_immuC","co_cancer"))




library(caret)
set.seed(1234) 

trainIndex <- createDataPartition(co_can_immu$Include,
                                  p=0.8,
                                  list=FALSE,
                                  times=1)

?createDataPartition

#here co_can_immu$Include is dependent variable i.e. y axis

head(trainIndex)

train <- co_can_immu[trainIndex,]
test <- co_can_immu[-trainIndex,] #complement of train = -

head(train)
head(test)

library(quanteda)
#Part 10: Text Analysis using Quanteda ############

names(test)
# we interested in ABSTRACT AND BIBTEXKEY (AS DOC id)

train_data <- train[,7]
test_data <- test[,7]
# column 7 will be used as label for variables or outcome variables


# Corpus ; is structured text data
#we have make corpus on abstract since we have text used for the analysis
train_corpus <- corpus(train$ABSTRACT,
                       docvars = data.frame(abstract_label=names(
                         train_data)))
test_corpus <- corpus(test$ABSTRACT,
                      docvars = data.frame(abstract_label=names(
                        test_data)))

#Bibtexkey is last name of first author as a unique identifier used in meta analysis 
# if same 1st author publishes same year it will become 2021.1: test_data)))

docid <- paste(train$BIBTEXKEY)
docnames(train_corpus) <- docid
print(train_corpus)

docid_test <- paste(test$BIBTEXKEY)
docnames(test_corpus) <- docid_test
print(test_corpus)

#  Tokenization
# Abstract, Intro/background, obje, methods, results, discussion, conclusions
# very rare
# stopwords: is, a, of, the...so that reduces the size of doc for analysis part

custom_stop_words <- c("background","introduction","aims",
                       "objectives","methods","results","conclusions",
                       "textless","study")
# textless removes alpha, beta, gama type of symbols

#train tokens: remove punctuation, remove numbers, stopwords, custom stopwords

#creating tokens
train_tokens <- tokens(train_corpus, remove_punct=TRUE,
                       remove_numbers=TRUE)
#Removing stopwords
train_tokens <- tokens_select(train_tokens,
                              pattern=stopwords('en'),
                              selection='remove')
#Removing custom stopwords
train_tokens <- tokens_select(train_tokens,
                              pattern=custom_stop_words,
                              selection='remove')

# test tokens *******************************************

#creating tokens
test_tokens <- tokens(test_corpus, remove_punct=TRUE,
                      remove_numbers=TRUE)
#Removing stopwords
test_tokens <- tokens_select(test_tokens,
                             pattern=stopwords('en'),
                             selection='remove')
#Removing custom stopwords
test_tokens <- tokens_select(test_tokens,
                             pattern=custom_stop_words,
                             selection='remove')

# Document Feature Matrix  ###############

train_dfmat <- dfm(train_tokens)
test_dfmat <- dfm(test_tokens)

#tfidf
train_dfmat_tfidf <- dfm_tfidf(train_dfmat)
test_dfmat_tfidf <- dfm_tfidf(test_dfmat)


head(train_dfmat)
head(test_dfmat)
dim(train_dfmat)

head(train_dfmat_tfidf)
head(test_dfmat_tfidf)
dim(train_dfmat_tfidf)


#Part 11: Constructing Document Feature Matrices and visualization of the features in Quanteda #################


dim(train_dfmat)

# 1113 rows vs 1782 columns (these are features/variables in dfm)
# need at least with ratio of: 20:1; i.e 20 obs vs 1 variable column
# 20 variables vs 400 obs rows
# this is a short and fat dataframe common in genetics and text data; this kind of data set can not be used ex. regression analysis
# 98% sparsity means lot of zero values

nfeat(train_dfmat)  # of features in an object.i.e 1887 features (variables)
nfeat(train_dfmat_tfidf)

# Visualization: wordcloud***

library(RColorBrewer)  
library(quanteda.textplots)

pal <- brewer.pal(5,"Dark2") #5 color matrix

textplot_wordcloud(train_dfmat,
                   min_count=5,
                   max_words = 500,
                   min_size = 1.5,
                   color=pal)
graphics.off()

?textplot_wordcloud()

# Prediciton; Bayes, SVM, quanteda.textmodels; e1071; GLMNET: Reg


#Part 12: Bayesian Classifier/Naive Bayes######################

# supervised text clasificn is imp in classification of text data i.e. labelled data that means we the data, which abstract is which 
# which abstract is covid (covid cancer) and which abstract is SARS (covid immunocompramised)

# other ex is if the person +ve of the disease or -ve; if you know the person which one which = labelled data 

# clustering comes under unsupervised

#NAIVE BAYES (BAYESIAN CLASSFICATION) PROBABILITY CLASSIFICATION: our brain is conditional probability machine.
# every decision will be done reasoning. what makes human diff from machines out ability to solve equation


#Part 13: Training Naive Bayes/Bayesian Classifier algorithm in R using Quanteda.textmodels package ##################

# @ 8:00  Naive Bayes
library(quanteda.textmodels)

y <- train$Include
x <- train_dfmat


naive_bayes <- textmodel_nb(x=train_dfmat,
                            y=train$Include)

summary(naive_bayes) # you get all the probability values


#Naivebayes tfidf#################

naive_bayes_tfidf <- textmodel_nb(x=train_dfmat_tfidf,
                                  y=train$Include)

summary(naive_bayes_tfidf)


table(test$Include)
nfeat(test_dfmat) # has low number
nfeat(train_dfmat) # has large number

# the training set got training terms should match with test data, other algorithm may not predict correctly.

# predict test data   ##################

matched_dfmat <- dfm_match(test_dfmat,
                           features = featnames(train_dfmat))
#tfidf
matched_dfmat_tfidf <- dfm_match(test_dfmat_tfidf,
                                 features = featnames(train_dfmat_tfidf))

# need to match features of trained dataset wit test data

nfeat(train_dfmat)
nfeat(matched_dfmat) #both are matching now

nfeat(matched_dfmat_tfidf)

pred_clas <- predict(naive_bayes,
                     newdata=matched_dfmat)

table(pred_clas) # gives prediction of test data


#tfidf
pred_clas_tfidf <- predict(naive_bayes_tfidf,
                           newdata=matched_dfmat_tfidf)
table(pred_clas_tfidf)


# contingency table 2x2 table

tab_class <- table(pred_clas,test$Include)
tab_class

tab_class_tfidf <- table(pred_clas_tfidf,test$Include)
tab_class_tfidf

#confusion matrix @24:00 prediction description

conf_mat <- confusionMatrix(tab_class,mode="everything",
                            positive="co_cancer")


conf_mat  #learn about confusionmatrix

#  @35:00 
conf_mat1 <- confusionMatrix(tab_class_tfidf,mode="everything",
                             positive="co_cancer")
conf_mat1
names(conf_mat1)
names(conf_mat1)
#compare values between dfmat vs tfidf ; this NB is good for simple frequency not TFIDF wrf to experience
#the naive bayes: precision is good for simple frequency, not that much for tfidf dta


#Part 14: Support Vector Machines#####################
# is used for continuous data such as height weight etc.



#Part 15: training SVM text classification model using quanteda text models in R #############

#SVM using DFM

svm_1 <- textmodel_svm(x=train_dfmat,
                       y=train$Include,
                       weight = "uniform")
#SVM using dfm_tfidf

svm_tfidf <- textmodel_svm(x=train_dfmat_tfidf,
                           y=train$Include,
                           weight = "uniform")
?textmodel_svm
#prediction
pred_clas <- predict(svm_1,
                     newdata=matched_dfmat)
table(pred_clas)

#contingency table/confusion matrix
table(pred_clas,test$Include)

cont_table <- table(pred_clas,test$Include)

#caret
confmat_svm <- confusionMatrix(cont_table,
                               mode="everything")
confmat_svm

#Tfidf

pred_clas_tfidf <- predict(svm_tfidf,
                           newdata=matched_dfmat_tfidf)

cont_table_tfidf <- table(pred_clas_tfidf,
                          test$Include)

confmat_svm_tfidf <- confusionMatrix(cont_table_tfidf,
                                     mode="everything")
confmat_svm_tfidf
names(confmat_svm_tfidf)

#Part 16: SVM for text classification using the e1071 R package ##################

#cost, gamma,mold, poly, sig, linear, rbt

library(e1071)
# e1071: works will with data frame so convert in to DF
train_df <- convert(train_dfmat,
                    to="data.frame")

test_df <- convert(matched_dfmat,
                   to="data.frame")
View(train_df)
# remove 1st column ie docID, actually this is coming from quanteda
# we dont need in the e1071
train_df <- train_df[,-1] 
test_df <- test_df[,-1]
View(train_df)

  #SVM
?SVM  #type in the console

table(train$Include)

time <- Sys.time()
x <- train_df
y <- as.factor(train$Include)  # as per video #17 used as.factor
#y <- train$Include

svm_linear <- svm(x=x,
                  y=y,
                  kernal=linear,
                  cost=10)
Sys.time() -time

summary(svm_linear)

nrow(train_df)

# the nrow are 1272 vs support vectors are 1183 so it's kind of OK model
# less No of support vectors are to bew good model

pred_linear <- predict(svm_linear,
                       newdata = test_df)
tab_class <- table(pred_linear,test$Include)
confmat_svmlinear <- confusionMatrix(tab_class,
                                     mode="everything")

confmat_svmlinear

# Explain upto part 16 this is enough######################










#Part 17: Tuning Support Vector Machines for biomedical text classification in R##############

# convert dfm to data frame using convert

#explore drm_trim with regards to sparse cleaning  in svm gamma, cost parameters etc..
?svm

 #if you use kernal = linear you dont need gamma variable
# 	parameter needed for all kernels except linear (default: 1/(data dimension))

?tune
rm(svm) # if any svm object will be removed


time <- Sys.time()
set.seed(123)

svm_tune_radial<- tune(svm, train.x=train_dfmat,train.y = as.factor(train$Include),
                       kernal="radial",
                       type="C-classification",
                       parallel=TRUE,
                       ranges = list(cost=c(0.001,0.01,0.1,0.2,0.3,
                                            0.4,0.5,1,5,6,7,8,10,15),
                                     gamma=c(0.0009,0.001,0.002,0.003,
                                             0.0035,0.004,0.0045,0.005)),
                       validation.x = tune.control(sampling = "cross",
                                                   cross=10))
                       
                    
Sys.time() -time

#  range is for decision boundary


time <- Sys.time()
set.seed(123)

svm_tune_radial_tfidf<- tune(svm, 
                       train.x=train_dfmat_tfidf,train.y = as.factor(train$Include),
                       kernal="radial",
                       type="C-classification",
                       parallel=TRUE,
                       ranges = list(cost=c(0.001,0.01,0.1,0.2,0.3,
                                            0.4,0.5,1,5,6,7,8,10,15),
                                     gamma=c(0.0009,0.001,0.002,0.003,
                                             0.0035,0.004,0.0045,0.005)),
                       validation.x = tune.control(sampling = "cross",
                                                   cross=10))


Sys.time() -time
svm_tune_radial
summary(svm_tune_radial)
names(svm_tune_radial)
svm_tune_radial$best.parameters
svm_tune_radial$best.model
dim(train_df)  # out of 1113 support vectors are 1021 (its high number not a good tune)

pred_svm_radial <- predict(svm_tune_radial$best.model, newdata = matched_dfmat) # use best model to predict
table(pred_svm_radial, test$Include) # prediction is not good

confusionMatrix(table(pred_svm_radial, test$Include), mode="everything")

# do same thing for tf_idf
pred_svm_radial_tfidf <- predict(svm_tune_radial_tfidf$best.model, newdata = matched_dfmat_tfidf) # use best model to predict
table(pred_svm_radial_tfidf, test$Include) # prediction is not good

confusionMatrix(table(pred_svm_radial_tfidf, test$Include), mode="everything")

# IMP create function to avoid everytime writing code#####################

pred_conf <- function(trained_model,test_data){
  predict_class <- predict(trained_model,test_data)
  tabulated_class <- table(predict_class,test$Include)
  conf_mat <- confusionMatrix(tabulated_class, mode="everything")
  return(list(tabulated_class, conf_mat))
}


pred_conf(svm_tune_radial$best.model,matched_dfmat) #use function here

pred_conf(svm_tune_radial_tfidf$best.model,matched_dfmat) #use function here

#next will test using linear, sigmoid, polynominal models work on the predicton



#   SVM linear##################

time <- Sys.time()
set.seed(123)

svm_tune_linear<- tune(svm, train.x=train_dfmat,train.y = as.factor(train$Include),
                       kernal="linear",
                       type="C-classification",
                       parallel=TRUE,
                       ranges = list(cost=c(0.001,0.01,0.1,0.2,0.3,
                                            0.4,0.5,1,5,6,7,8,10,15)),
                                     # gamma=c(0.0009,0.001,0.002,0.003,
                                     #         0.0035,0.004,0.0045,0.005)),
                       validation.x = tune.control(sampling = "cross",
                                                   cross=10))
#for linear dont need gamma parameter

Sys.time() -time



time <- Sys.time()
set.seed(123)

svm_tune_linear_tfidf<- tune(svm, train.x=train_dfmat_tfidf,train.y = as.factor(train$Include),
                       kernal="linear",
                       type="C-classification",
                       parallel=TRUE,
                       ranges = list(cost=c(0.001,0.01,0.1,0.2,0.3,
                                            0.4,0.5,1,5,6,7,8,10,15)),
                       # gamma=c(0.0009,0.001,0.002,0.003,
                       #         0.0035,0.004,0.0045,0.005)),
                       validation.x = tune.control(sampling = "cross",
                                                   cross=10))
#for linear dont need gamma parameter

Sys.time() -time


# this time took less b/c not many parameters such as gamma



pred_conf(svm_tune_linear$best.model,matched_dfmat) #use function here

pred_conf(svm_tune_linear_tfidf$best.model,matched_dfmat) #use function here


#Sensitivity : 0.3378        can be balanced in the next video  
# Specificity : 0.8367   

# you can also change linear to sigmoid, or polynominal, but polynominal did not work well in his case
# you can alos ranges of cost to better fit 


#Part 18: Tuning Sigmoid SVM for text classification  ###########


time <- Sys.time()
set.seed(123)

svm_tune_sigmoid<- tune(svm, train.x=train_dfmat,train.y = as.factor(train$Include),
                       kernal="sigmoid",
                       type="C-classification",
                       parallel=TRUE,
                       ranges = list(cost=c(0.001,0.01,0.1,0.2,0.3,
                                            0.4,0.5,1,5,6,7,8,10,15),
                                     gamma=c(0.0009,0.001,0.002,0.003,
                                             0.0035,0.004,0.0045,0.005)),
                       validation.x = tune.control(sampling = "cross",
                                                   cross=10))


Sys.time() -time

#  range is for decision boundary


time <- Sys.time()
set.seed(123)

svm_tune_sigmoid_tfidf<- tune(svm, 
                             train.x=train_dfmat_tfidf,train.y = as.factor(train$Include),
                             kernal="sigmoid",
                             type="C-classification",
                             parallel=TRUE,
                             ranges = list(cost=c(0.001,0.01,0.1,0.2,0.3,
                                                  0.4,0.5,1,5,6,7,8,10,15),
                                           gamma=c(0.0009,0.001,0.002,0.003,
                                                   0.0035,0.004,0.0045,0.005)),
                             validation.x = tune.control(sampling = "cross",
                                                         cross=10))


Sys.time() -time




svm_tune_sigmoid
summary(svm_tune_sigmoid)
names(svm_tune_sigmoid$best.model) # to find the nSV # of support vectors
svm_tune_sigmoid$best.model$nSV
svm_tune_sigmoid$best.model$tot.nSV

svm_tune_sigmoid_tfidf$best.model$tot.nSV  # in this tf_idf not doing well compared to tf b/c the # of svm should be low
# otherwise model is over fitted


number_sv <- rbind(lin_svm=svm_tune_linear$best.model$tot.nSV,
                   lin_svm_tfidf= svm_tune_linear_tfidf$best.model$tot.nSV,
                   
                   rad_svm=svm_tune_radial$best.model$tot.nSV,
                   rad_svm_tfidf=svm_tune_radial_tfidf$best.model$tot.nSV,
                   
                   sig_svm=svm_tune_radial$best.model$tot.nSV,
                   sig_svm_tfidf=svm_tune_radial_tfidf$best.model$tot.nSV) %>% as.data.frame()



number_sv

names(number_sv) <- "No of Support Vectors"

number_sv

pred_conf(svm_tune_sigmoid$best.model,matched_dfmat) #use function here

pred_conf(svm_tune_sigmoid_tfidf$best.model,matched_dfmat) #use function here



cost <- svm_tune_sigmoid$performances

names( svm_tune_sigmoid$performances)

svm_tune_sigmoid$performances$cost

svm_tune_sigmoid$performances$gamma
svm_tune_sigmoid$performances$dispersion


cost <- svm_tune_sigmoid$performances$cost
gamma <- svm_tune_sigmoid$performances$gamma
error <- svm_tune_sigmoid$performances$error
disperson <- svm_tune_sigmoid$performances$dispersion

accuracy <- 1-error


svm_models_df <- cbind(cost, gamma,error,
                       disperson,accuracy) %>% as.data.frame()
head(svm_models_df)
dim(svm_models_df)



p_svm <- ggplot(svm_models_df, aes(cost,
                                   error, col=gamma))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.3576014, col="darkgreen",
             linetype="dashed", size=0.7)+
  geom_segment(aes(x=4, y=0.35, xend=5, yend=0.37),
               arrow = arrow(length = unit(0.2,"cm")),color="red",
               size=1.2)
  

p_svm
  
  
min(svm_tune_sigmoid$performances$error)

#tfidf


cost_tfidf <- svm_tune_sigmoid_tfidf$performances$cost
gamma_tfidf <- svm_tune_sigmoid_tfidf$performances$gamma
error_tfidf <- svm_tune_sigmoid_tfidf$performances$error
disperson_tfidf <- svm_tune_sigmoid_tfidf$performances$dispersion


accuracy_tfidf <- 1-error_tfidf


svm_models_tfidf <- cbind(cost_tfidf, gamma_tfidf,error_tfidf,
                       disperson_tfidf,accuracy) %>% as.data.frame()
head(svm_models_tfidf)
dim(svm_models_tfidf)
min(svm_tune_sigmoid_tfidf$performances$error)


p_svm_tfidf <-  ggplot(svm_models_tfidf, aes(cost_tfidf,
                                          error_tfidf, col=gamma_tfidf))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.3576014, col="darkgreen",
             linetype="dashed", size=0.7)+
  geom_segment(aes(x=4, y=0.35, xend=5, yend=0.37),
               arrow = arrow(length = unit(0.2,"cm")),color="red",
               size=1.2)


p_svm_tfidf

library(patchwork) # to show graphs side by side
p_svm+p_svm_tfidf

























