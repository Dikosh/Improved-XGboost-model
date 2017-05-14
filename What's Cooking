# Загружаем необходимые для нас библиотеки

# Если захотите запустить на своем сервере то надо скомпилировать нижеуказанный код убрав "#"
# Данные ПК: ОЗУ выше 32Гб, и если хотите запустить на своем ПК, то нужно будет скачать дополнительные библиотеки
# Многие библиотеки собирались из архивных источников CRAN
# Если возникнут проблемы 87072964488
# F-score 0.80-0.84
# install_version("xgboost", version = "0.4-3", repos = "http://cran.us.r-project.org")
# install.packages("jsonlite")
# install.packages("tm")
# install.packages("readr")
# install.packages("tokenizers")
# install.packages("caret")
# install.packages("Matrix")
# install.packages("mlr")
getwd()
setwd("/resources/data")
  library(jsonlite)
  library(tm)
  library(readr)
  require(tokenizers)
  library(tm)
  library(caret)
  library(xgboost)
  library(Matrix)
  
  getwd()
  
# Импортируем данные
    train <- fromJSON("train2.json")
    test <- fromJSON("test1.json")
    ########################    
      dat <- Corpus(VectorSource(train$ingredients))
      dat <- tm_map(dat, tolower)
      dat <- tm_map(dat, removePunctuation)
      dat <- tm_map(dat, removeWords, c(stopwords="english"))
      dat <- tm_map(dat, stripWhitespace)
      dat <- tm_map(dat, stemDocument)
          dat[[1]]
          #document matrix
         frequencies <- DocumentTermMatrix(dat) 
         rm(dat)
    sparse <- removeSparseTerms(frequencies, 1 - 2/nrow(frequencies))
    newsparse <- as.data.frame(as.matrix(sparse))
#################
    gc() #- do it now
    gcinfo(TRUE) #-- in the future, show when R does it
    x <- integer(100000); for(i in 1:18) x <- c(x, i)
    gcinfo(verbose = FALSE) #-- don't show it anymore
    
    gc(TRUE)
    
    gc(reset = TRUE)
    
    
    
    
    corpus_test <- Corpus(VectorSource(test$ingredients))
    corpus_test <- tm_map(corpus_test, tolower)
    corpus_test <- tm_map(corpus_test, removePunctuation)
    corpus_test <- tm_map(corpus_test, removeWords, c(stopwords="en"))
    corpus_test <- tm_map(corpus_test, removeNumbers)
    corpus_test <- tm_map(corpus_test, stripWhitespace)
    corpus_test <- tm_map(corpus_test, stemDocument)
    corpus_test[[1]]
    #document matrix
    frequencies_test <- DocumentTermMatrix(corpus_test) 
    rm(corpus_test)
    sparse_test <- removeSparseTerms(frequencies_test, 1 - 2/nrow(frequencies_test))
    newsparse_test <- as.data.frame(as.matrix(sparse_test))
    #######################
    dim(newsparse_test)
    

# Метим
colnames(newsparse) <- make.names(colnames(newsparse))
newsparse$cuisine <- as.factor(c(train$cuisine))
# Смотрим пропорции слов
base::table(newsparse$cuisine)
prop.table(base::table(newsparse$cuisine))*100
##################

new_brazillian<- newsparse[newsparse$cuisine=="brazilian",]
intrain<-createDataPartition(y=new_brazillian$cuisine,p=0.1,list=FALSE)
new_brazillian1<-new_brazillian[-intrain,]
new_brazillian <- rbind(new_brazillian,new_brazillian1)

new_brazillian<- newsparse[newsparse$cuisine=="brazilian",]
intrain<-createDataPartition(y=new_brazillian$cuisine,p=0.1,list=FALSE)
new_brazillian1<-new_brazillian[-intrain,]
new_brazillian <- rbind(new_brazillian,new_brazillian1)


new_british <-   newsparse[newsparse$cuisine=="british",]
intrain<-createDataPartition(y=new_british$cuisine,p=0.1,list=FALSE)
new_british1<-new_british[-intrain,]
new_british <- rbind(new_british,new_british1)

new_cajun <-     newsparse[newsparse$cuisine=="cajun_creole",]
intrain<-createDataPartition(y=new_cajun$cuisine,p=0.7,list=FALSE)
new_cajun1<-new_cajun[-intrain,]
new_cajun <- rbind(new_cajun,new_cajun1)


new_chinese <-   newsparse[newsparse$cuisine=="chinese",]
intrain<-createDataPartition(y=new_chinese$cuisine,p=0.3,list=FALSE)
new_chinese<-new_chinese[-intrain,]


new_fill <-      newsparse[newsparse$cuisine=="filipino",]
intrain<-createDataPartition(y=new_fill$cuisine,p=0.1,list=FALSE)
new_fill1<-new_fill[-intrain,]
new_fill <- rbind(new_fill,new_fill1)


new_french <-    newsparse[newsparse$cuisine=="french",]
intrain<-createDataPartition(y=new_french$cuisine,p=0.3,list=FALSE)
new_french<-new_french[-intrain,]


new_grekk <-     newsparse[newsparse$cuisine=="greek",]
intrain<-createDataPartition(y=new_grekk$cuisine,p=0.3,list=FALSE)
new_grekk1<-new_grekk[-intrain,]
new_grekk <- rbind(new_grekk,new_grekk1)


new_indian <-    newsparse[newsparse$cuisine=="indian",]
intrain<-createDataPartition(y=new_indian$cuisine,p=0.4,list=FALSE)
new_indian<-new_indian[-intrain,]

new_irish <-     newsparse[newsparse$cuisine=="irish",]
intrain<-createDataPartition(y=new_irish$cuisine,p=0.1,list=FALSE)
new_irish1<-new_irish[-intrain,]
new_irish <- rbind(new_irish,new_irish1)
intrain<-createDataPartition(y=new_irish$cuisine,p=0.5,list=FALSE)
new_irish1<-new_irish[-intrain,]
new_irish <- rbind(new_irish,new_irish1)


new_italian <-   newsparse[newsparse$cuisine=="italian",]
intrain<-createDataPartition(y=new_italian$cuisine,p=0.8,list=FALSE)
new_italian<-new_italian[-intrain,]

new_jamaican <-  newsparse[newsparse$cuisine=="jamaican",]
intrain<-createDataPartition(y=new_jamaican$cuisine,p=0.1,list=FALSE)
new_jamaican1<-new_jamaican[-intrain,]
new_jamaican <- rbind(new_jamaican,new_jamaican1)
intrain<-createDataPartition(y=new_jamaican$cuisine,p=0.1,list=FALSE)
new_jamaican1<-new_jamaican[-intrain,]
new_jamaican <- rbind(new_jamaican,new_jamaican1)


new_japanese <-  newsparse[newsparse$cuisine=="japanese",]

new_korean <-    newsparse[newsparse$cuisine=="korean",]
intrain<-createDataPartition(y=new_korean$cuisine,p=0.1,list=FALSE)
new_korean1<-new_korean[-intrain,]
new_korean <- rbind(new_korean,new_korean1)


new_mexican <-   newsparse[newsparse$cuisine=="mexican",]
intrain<-createDataPartition(y=new_mexican$cuisine,p=0.7,list=FALSE)
new_mexican<-new_mexican[-intrain,]


new_maroccan <-  newsparse[newsparse$cuisine=="moroccan",]
intrain<-createDataPartition(y=new_maroccan$cuisine,p=0.1,list=FALSE)
new_maroccan1<-new_maroccan[-intrain,]
new_maroccan <- rbind(new_maroccan,new_maroccan1)


new_russian <-   newsparse[newsparse$cuisine=="russian",]
intrain<-createDataPartition(y=new_russian$cuisine,p=0.1,list=FALSE)
new_russian1<-new_russian[-intrain,]
new_russian <- rbind(new_russian,new_russian1)
intrain<-createDataPartition(y=new_russian$cuisine,p=0.1,list=FALSE)
new_russian1<-new_russian[-intrain,]
new_russian <- rbind(new_russian,new_russian1)


new_usa <-       newsparse[newsparse$cuisine=="southern_us",]
intrain<-createDataPartition(y=new_usa$cuisine,p=0.64,list=FALSE)
new_usa<-new_usa[-intrain,]


new_span <-      newsparse[newsparse$cuisine=="spanish",]
intrain<-createDataPartition(y=new_span$cuisine,p=0.1,list=FALSE)
new_span1<-new_span[-intrain,]
new_span <- rbind(new_span1,new_span)

new_thai <-      newsparse[newsparse$cuisine=="thai",]


new_viet <-      newsparse[newsparse$cuisine=="vietnamese",]
intrain<-createDataPartition(y=new_viet$cuisine,p=0.1,list=FALSE)
new_viet1<-new_viet[-intrain,]
new_viet <- rbind(new_viet1,new_viet)



ggg <- rbind(new_british,new_brazillian,new_cajun,new_chinese,new_fill,new_french,new_grekk,new_indian,new_irish,new_italian,
             new_jamaican,new_japanese,new_korean, new_maroccan, new_mexican,new_russian,new_span,new_thai,new_usa,new_viet)
base::table(ggg$cuisine)



##################


#Делим данные на тренировочные и тестируемые 

intrain<-createDataPartition(y=ggg$cuisine,p=0.6,list=FALSE)
mytrain<-ggg[intrain,]
mytest<-ggg[-intrain,]
mytest_final <- newsparse_test

gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x, i)
gcinfo(verbose = FALSE) #-- don't show it anymore

gc(TRUE)

gc(reset = TRUE)


base::table(ggg$cuisine)

# Делаем матрицу из train test
ctrain <- xgb.DMatrix(Matrix(data.matrix(mytrain[,!colnames(mytrain) %in% c('cuisine')])), label = as.numeric(mytrain$cuisine)-1)
#advanced data set preparation
dtest <- xgb.DMatrix(Matrix(data.matrix(mytest[,!colnames(mytest) %in% c('cuisine')]))) 
watchlist <- list(train = ctrain, test = dtest)


# multiclass model using softmax
#first model
xgbmodel <- xgboost(data = ctrain, max.depth = 25, eta = 0.7,eval_metric="mlogloss", nround = 200, objective = "multi:softmax", num_class = 21, verbose = 1, watchlist = watchlist)
#second model
xgbmodel2 <- xgboost(data = ctrain, max.depth = 20, eta = 0.2, nrounds = 250, objective = "multi:softmax", num_class = 20, watchlist = watchlist)

#third model
xgbmodel3 <- xgboost(data = ctrain, max.depth = 25, gamma = 2, min_child_weight = 2, eta = 0.1, nround = 250, objective = "multi:softmax", num_class = 20, verbose = 2,watchlist = watchlist)

#xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')]))
#xgbmodel.predict.text <- levels(mytrain$cuisine)[xgbmodel.predict + 1]
#predict 2
#xgbmodel.predict2 <- predict(xgbmodel2, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
#xgbmodel.predict2.text <- levels(mytrain$cuisine)[xgbmodel.predict2 + 1]
#predict 3
#xgbmodel.predict3 <- predict(xgbmodel3, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
#xgbmodel.predict3.text <- levels(mytrain$cuisine)[xgbmodel.predict3 + 1]

xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(mytest_final))
xgbmodel.predict.text <- levels(mytrain$cuisine)[xgbmodel.predict + 1]
#predict 2
xgbmodel.predict2 <- predict(xgbmodel2, newdata = data.matrix(mytest_final)) 
xgbmodel.predict2.text <- levels(mytrain$cuisine)[xgbmodel.predict2 + 1]
#predict 3
xgbmodel.predict3 <- predict(xgbmodel3, newdata = data.matrix(mytest_final)) 
xgbmodel.predict3.text <- levels(mytrain$cuisine)[xgbmodel.predict3 + 1]
###
library(data.table)
#data frame for predict 1
submit_match1 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict.text))
colnames(submit_match1) <- c('id','cuisine')
submit_match1 <- data.table(submit_match1, key = 'id')
#data frame for predict 2
submit_match2 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict2.text))
colnames(submit_match2) <- c('id','cuisine')
submit_match2 <- data.table(submit_match2, key = 'id')
#data frame for predict 3
submit_match3 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict3.text))
colnames(submit_match3) <- c('id','cuisine')
submit_match3 <- data.table(submit_match3, key = 'id')

#ensembling 
submit_match3$cuisine2 <- submit_match2$cuisine 
submit_match3$cuisine1 <- submit_match1$cuisine

#function to find the maximum value row wise
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
x <- Mode(submit_match3[,c("cuisine","cuisine2","cuisine1")])
y <- apply(submit_match3,1,Mode)
final_submit <- data.frame(id= submit_match3$id, cuisine = y)
#view submission file
data.table(final_submit)
#final submission
write.csv(final_submit, '~/ensemble.csv', row.names = FALSE)
base::table(final_submit$cuisine)
###


  # Смотрим на Accuracy
 #   sum(diag(base::table(mytest$cuisine, xgbmodel.predict)))/nrow(mytest)
#sum(diag(table(mytest$cuisine, xgbmodel.predict3)))/nrow(mytest)
#sum(diag(table(mytest$cuisine, xgbmodel.predict2)))/nrow(mytest) 
# Model 1
#confusionMatrix(xgbmodel.predict.text,mytest$cuisine) # confusion matrix
#mat <-base::table(mytest$cuisine, xgbmodel.predict) # table TP TN FP FN
#(precision <- diag(mat) / rowSums(mat)) # Precision
#recall <- (diag(mat) / colSums(mat)) # Recall
#f_meas <- 2*precision*recall/(precision+recall) # F-measure
#mean(f_meas) # F measure средний общий
#f_meas # F measure по отдельности

