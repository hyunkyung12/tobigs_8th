rm(list=ls())


###패키지설치### 
install.packages("tm")
install.packages(" wordcloud")
install.packages("e1071")
install.packages("NLP")


###데이터### 
setwd("C:/Users/USER/Desktop/투빅스/2주차 나이브베이즈")

sms_data <- read.csv("ham_spam.csv", stringsAsFactors = F) #변수 factor형으로 불러오지 않음 : F , char형으로 받음
str(sms_data) #type, text 두가지 변수

View(sms_data)

table(sms_data$type)

sms_data$type <- as.factor(sms_data$type) #type변수 factor형으로 변환
str(sms_data) #5559x2 dimension, type변수: factor형, text변수: chr형


##corpus형 데이터 생성##
#corpus는 text를 나타내는 말뭉치 자료형
library(tm)

sms_data$text <- iconv(enc2utf8(sms_data$text), sub="byte") #UTF-8 encoding(오류 방지)
sms_corpus <- Corpus(VectorSource(sms_data$text)) #corpus 생성 : 단어를 다루는 자료형

print(sms_corpus)
inspect(sms_corpus) #list 형태와 유사

sms_corpus[[1]]
sms_corpus[[1]]$content #내용
sms_corpus[[1]]$meta #정보


##전처리 : 함수 tm_map ##
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #소문자로 통일
corpus_clean <- tm_map(corpus_clean, removeNumbers) #숫자 제거 
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords()) #기존에 존재하는 stopwords() 이용해 stopwords 제거
corpus_clean <- tm_map(corpus_clean, removePunctuation) #구두점 제거
corpus_clean <- tm_map(corpus_clean, stripWhitespace) #한칸 이상의 띄어쓰기 한칸으로 변환

sms_corpus[[1]]$content #전처리 전
corpus_clean[[1]]$content #전처리 후


##document term matrix(sparse matrix) 생성##
#단어 frequency 행렬 (row: document, column: 단어)
sms_dtm <- DocumentTermMatrix(corpus_clean) 
inspect(sms_dtm[1:10,1:10])
dim(sms_dtm) #서로 다른 8283개의 단어 존재


##data split : train , test로##
#train:test = 0.7:0.3
set.seed(0727)
index <- sample(1:dim(sms_data)[1],round(dim(sms_data)[1]*0.7))

sms_data_train <- sms_data[index,] 
sms_data_test <- sms_data[-index,] 
sms_dtm_train <- sms_dtm[index,] 
sms_dtm_test <- sms_dtm[-index,] 
sms_corpus_train <- corpus_clean[index] 
sms_corpus_test <- corpus_clean[-index]

table(sms_data_train$type)/table(sms_data_test$type) #type 비율 유사

## 데이터셋의 크기를 줄이기 위해 sparse matrix의 8283개 단어 중 빈도수 5이상인 단어만 이용##
sms_dict<- findFreqTerms(sms_dtm_train, 5) #빈도수 5이상인 단어만 저장
length(sms_dict) #1203개

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict)) #빈도수 5이상인 단어로 최종 dtm생성
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))


##단어 빈도수 데이터를 단어 존재유무 변수로 변환##
convert_counts <- function(x){
  x <- ifelse(x>0, 1, 0) #x가 0보다 크면 1 아니면 0
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
} 

sms_train <- apply(sms_train, 2, convert_counts) #column별 convert_counts 함수 적용 
sms_test <- apply(sms_test, 2, convert_counts)



###모델 적합###
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_data_train$type)

sms_classifier$tables[1:3] #단어별 분류 확률
sms_classifier$tables$call #call 단어의 분류 확률

###예측###
sms_test_pred <- predict(sms_classifier, sms_test)
table <- table(sms_test_pred, sms_data_test$type)
table

sum(diag(table))/sum(table) #accuarcy = 0.8111511

###라플라스 방법 사용###
sms_classifier <- naiveBayes(sms_train, sms_data_train$type,laplace=1) # laplace 방법 사용
# 위와 동일하게 하면 됨
## 이 경우 라플라스를 하나 안하나 accuarcy 비슷함, 굳이 할필요 X


