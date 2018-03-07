#http://statkclee.github.io/ml/ml-market-basket.html

rm(list=ls())
#install.packages(c("arules","arulesViz")) 
library(arules)
library(arulesViz)

data(Epub) #a lot of 0 : density of 0.001758755
#sparse format 형식으로 저장된 itemMatrix의 거래 *transactions) 데이터셋
#밀도(density)가 0.1758755% 라고 나오는데 전체 셀중에서 약 0.1758%의 셀에 거래가 발생해서 숫자가 차있다.
summary(Epub)
#most frequent items는 거래 빈도가 가장 많은 top 5의 품목명과 거래빈도를 제시
#element는 품목의 개수별로 몇번의 거래가 있었는지 
class(Epub)

#transaction 데이터 보기
inspect(Epub[1:10])
image(sample(Epub,500,replace=FALSE))
itemFrequency(Epub) #상대도수
itemFrequency(Epub,type="absolute") #도수
itemFrequency(Epub[,1:10]) #거래품목(item)별로 거래에서 차지하는 비율(support) - 앞의 10개만 확인
itemFrequencyPlot(Epub) #도수 막대그래프,#type="absolute"
itemFrequencyPlot(Epub,support=0.01,main="item frequency plot support") #지지도 1% 이상의 item에 대한 막대그래프
itemFrequencyPlot(Epub, topN=30, main="support top 30 item") #support 상위 30개의 막대그래프
Epub@itemInfo 
Epub@data  #sparse matrix 


#Epub데이터를 가지고 연관성분석을 해보자
#함수형태 apriori(data,parameter=list(support=0.1, confidence=0.8, minlen=1, maxlen=10, smax=1)) -각 값들은 default
#support=최소지지도, confidence=최소신뢰도, minlen=최소물품수(lhs+rhs), maxlen=최대물품수(lhs+rhs), smax=최대지지도
#parameter를 따로 지정안하면 default값을 기준으로 계산함
Epub_rule1<-apriori(Epub, parameter=list(support=0.01,confidence=0.2,minlen=1)) 
Epub_rule1 
Epub_rule2 <-apriori(Epub, parameter=list(support=0.001,confidence=0.2,minlen=1))
summary(Epub_rule2) #2개가 62개, 3개가 3개

Epub_rule2[quality(Epub_rule2)$confidence>0.2]


#연관규칙 평가
inspect(Epub_rule2[1:20]) 
inspect(sort(Epub_rule2[1:20],by="lift"))
inspect(sort(Epub_rule2,by="support"))

##관심있는 item이 포함된 연관규칙
#cf)
#%in% (select itemsets matching any given item) 조건은 적어도 하나의 제품이라도 존재하면 연관규칙을 indexing
#%pin% (partial matching)는 부분 일치만 하더라도 indexing
#%ain% (select only itemsets matching all given item)는 완전한 일치를 할 때만 indexing 

#%in%
#subset이라는 함수를 이용해서 관심이 있는 item이 포함된 연관규칙만 선별 
rule_interest <- subset(Epub_rule2, items %in% c("doc_72f", "doc_4ac"))
inspect(rule_interest)
#왼쪽 lhs, 오른쪽 rhs 
rule_interest_lhs <- subset(Epub_rule2, lhs %in% c("doc_72f", "doc_4ac"))
inspect(rule_interest_lhs)
#여기서 %in% 조건은 적어도 하나의 제품이라도 존재하면 연관규칙을 인덱싱 
#%pin%
rule_interest_pin <- subset(Epub_rule2, items %pin% c("60e"))
inspect(rule_interest_pin)
#%ain%
rule_interest_lhs_ain <- subset(Epub_rule2, lhs %ain% c("doc_6e8", "doc_6e9"))
inspect(rule_interest_lhs_ain)

##시각화
library(arulesViz)
plot(Epub_rule2) 
plot(sort(Epub_rule2, by = "support"), method = "grouped")
#상위 20개만 따로 그려보자 
plot(sort(Epub_rule2, by = "support")[1:20], method = "grouped")
#빨갈수록 lift값이 크고, 원이 클수록 support값이 크다.
plot(Epub_rule2, method = "graph", control = list(type="items"))
#{item}->{item} 연관규칙의 지지도는 원의 크기, 색깔은 향상도 
plot(Epub_rule2,method="graph",interactive = T)  
plot(Epub_rule2,method="paracoord")

#예시 - mushroom data로 

data(Mushroom)
inspect(Mushroom[1:10])
itemFrequencyPlot(Mushroom, support=0.3)
itemFrequencyPlot(Mushroom, topN=20)

rule_poison<-apriori(Mushroom,parameter=list(support=0.25,confidence=0.8,minlen=1),appearance=list(rhs=c("Class=poisonous"), 
                                                                                                   default="lhs"))
summary(rule_poison)   #208rules
inspect(rule_poison)
inspect(sort(rule_poison,by='lift')[1:20])
#odor=foul, gillspace=close, gillattached=free, veilcolor=white, veiltype=partial, surfacebelowring=silky, ringnumber=one 포함된 규칙

rule_edible<-apriori(Mushroom,parameter=list(support=0.3,confidence=0.8,minlen=1),appearance=list(rhs=c("Class=edible"),default="lhs"))
summary(rule_edible)  #528rules
inspect(rule_poison)[1:10]
inspect(sort(rule_edible,by='lift')[1:20])
#odor=none, stalkshape=tapering, RingNumber=one, GillAttached=free, VeilColor=white, VeilType=partial ,GillSize=broad 포함


####################################################################
# transactions class는 arules 패키지내에 연관성 분석을 위한 class
# 기존의 데이터를 transactions class로 변환
# 모든 요소들이 팩터형이여야 한다.
## 1. matrix -> transactions class
matrix <- matrix(c(1,1,0,0,0,
                   1,0,1,0,0,
                   0,1,1,1,0,
                   1,0,0,0,1,
                   0,0,1,1,1), ncol=5, byrow=T)

dimnames(matrix)<-list(paste0("trans",1:5),letters[1:5])
trans.matrix <-as(matrix,"transactions")
summary(trans.matrix)
inspect(trans.matrix)
## 2. data.frame -> transactions class
df <- as.data.frame(matrix)
str(df)
df <- as.data.frame(sapply(df,as.logical))
df.trans <-as(df,"transactions")
summary(df.trans)
inspect(df.trans)
## 3. list -> transactions class
list <- list(tr1=c("a","b","c"),
             tr2=c("a","d"),
             tr3=c("b","e"),
             tr4=c("a","d","e"),
             tr5=c("b","c","d"))
list
trans.list <-as(list,"transactions")
summary(trans.list)
inspect(trans.list)

#범주형 자료를 이진화시켜서 분석 
cust_id <- c(1, 2, 3, 4, 5, 6)
gender <- c("FEMALE", "MALE", "FEMALE", "FEMALE", "MALE", "FEMALE")
age <- c(23, 28, 42, 34, 45, 36)
child_prd_yn <- c("NO", "NO", "NO", "YES", "NO", "YES")
mobile_app_use <- c("YES", "YES", "NO", "YES", "NO", "YES")
re_order <- c("YES", "NO", "NO", "YES", "NO", "YES")

cust_mart <- cbind(cust_id, gender, age, child_prd_yn, mobile_app_use, re_order)
cust_mart <- as.data.frame(cust_mart)
sapply(cust_mart, class) #각각의 class를 확인 
str(cust_mart)

cust_mart <- transform(cust_mart, cust_id = as.character(cust_id),
                       age=as.numeric(age))
sapply(cust_mart,class)

# age : custinuous data -> discretization
cust_mart <- within(cust_mart, { #with랑 비슷한 역할을 하는 함수 
  age_cd = character(0)
  age_cd[ age <= 29 ] = "age_20"
  age_cd[ age > 29 & age <= 39 ] = "age_30"
  age_cd[ age > 39 ] = "age_40"
  age_cd = factor(age_cd, level = c("age_20", "age_30", "age_40"))
})
cust_mart_ar <- subset(cust_mart, select=-c(cust_id, age))
summary(cust_mart_ar)
#only factors
cust_mart_trans <-as(cust_mart_ar, "transactions")
inspect(cust_mart_trans)

########################################################################

#eclat 알고리즘 - FP-growth
#Apriori랑 다른 점으로 깊이 우선 탐색을 하는 방법으로
#먼저 transaction을 세는 것이 아니라 itemset을 찾고 최소 support를 만족하는 규칙을 찾는다.
#장점은 apriori 알고리즘 보다 빠른 속도로 구현하지만, Transaction ID set이 꽤 길어질 수 있다는 단점이 있다. 
library(arules)
data(Adult)
#eclat에서는 parameter가 오직 support와 maxlen, minlen 밖에 없다. 
eclat.rules<-eclat(Adult, parameter=list(support=0.1,maxlen=15))
eclat.rules<-eclat(Adult, parameter=list(support=0.5,maxlen=15))
summary(eclat.rules)
inspect(sort(eclat.rules))

as(items(eclat.rules),"matrix")
as(items(eclat.rules),"ngCMatrix") #sparse matrix의 형태 

