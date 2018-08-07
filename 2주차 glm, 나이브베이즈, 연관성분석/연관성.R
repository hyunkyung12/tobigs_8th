rm(list=ls())
library(arules)
data("AdultUCI")
adu <- AdultUCI
adu = na.omit(adu)

attach(adu)
names(adu)
adu = adu[,c(-2,-3,-4,-14)]
head(adu)
### 데이터 파악하기
str(adu)
hist(adu$`capital-gain`)
hist(adu$`capital-loss`)
hist(adu$`hours-per-week`)

# age : int => factor => 0/1
age = ifelse(adu$age<=39,"청장년층","중노년층") # 청장년층 / 중노년층
adu$age = factor(age,levels=c("중노년층","청장년층")) 

# education-num : factor => 0/1
edunum = ifelse(adu$`education-num`<=9,"고졸","고졸이상") # 고졸 / 고졸이상
adu$`education-num` = factor(edunum, levels=c("고졸","고졸이상"))

# marital-status : factor => 0/1 
plot(adu$marital,adu$income) # married-civ-spouse 과 아닌것의 차이가 큼
marital = ifelse(adu$`marital-status`=="Married-civ-spouse","Married-civ-spouse O","Married-civ-spouse X") # "Married-civ-spouse O" / "Married-civ-spouse X"
adu$`marital-status` = factor(marital, levels=c("Married-civ-spouse X","Married-civ-spouse O"))
table(adu$`marital-status`)

# occupation : factor => 0/1
# 소득을 기준으로 임의로 나눔
occupation = character(0)
occupation[adu$occupation %in% c("Exec-managerial","Craft-repair","Prof-speciality","Sales","Tech-support","Protective-serv")]="고소득직종"
occupation[is.na(occupation)]="저소득직종"
adu$occupation = factor(occupation, levels=c("저소득직종","고소득직종"))

# relationship : factor => 0/1
relationship = character(0) 
relationship[adu$relationship %in% c("Husband","Wife")]="배우자 O" # 배우자가 있는경우 / 없는경우
relationship[is.na(relationship)]="배우자 X"
adu$relationship = factor(relationship,levels=c("배우자 X","배우자 O"))
table(adu$relationship)

# race : factor
race = ifelse(adu$race=="White","백인","백인X") # 백인인경우 / 아닌경우
adu$race = factor(race, levels=c("백인x","백인"))

# sex
plot(adu$sex, adu$income)
sex = ifelse(adu$sex=="Male","남","여")
adu$sex = factor(sex, levels=c("여","남"))
# capitalgain 
gain = ifelse(adu$`capital-gain`==0,"무","유") # capital이 있는경우/ 없는경우
adu$`capital-gain` = factor(gain, levels=c("무","유"))

# capitalloss 
loss = ifelse(adu$`capital-loss`==0,"무","유")
adu$`capital-loss` = factor(loss, levels=c("무","유"))

# hours-per-week
summary(adu$`hours-per-week`)
hist(adu$`hours-per-week`)
table(adu$`hours-per-week`)
hours = c()
hours[adu$`hours-per-week`<=39]="40시간 미만"
hours[adu$`hours-per-week`>39 & adu$`hours-per-week`<45]="40시간 이상 45시간 미만"
hours[adu$`hours-per-week`>=45]="45시간 이상"
adu$`hours-per-week` = factor(hours, levels=c("40시간 미만","40시간 이상 45시간 미만","45시간 이상"))

### transaction 데이터 만들기
library(arules)
library(arulesViz)

str(adu)  
adu = as(adu, 'transactions')

### 연관성분석
adu_rule1 = apriori(adu, parameter=list(support=0.01,confidence=0.2,minlen=3,maxlen=11))
summary(adu_rule1)
adu_rule2 = apriori(adu, parameter=list(support=0.05,confidence=0.2,minlen=3,maxlen=11))
summary(adu_rule2) 
itemFrequencyPlot(adu, support=0.05)

### 연관규칙평가
rule_large = subset(adu_rule2, rhs %pin% c("income=large"))
large_bylift = inspect(sort(rule_large[1:20]),by="lift")
large_bysupport = inspect(sort(rule_large[1:20]),by="support") ## age=중노년층, hours 45시간 이상 정도가 많이 나옴을 확인
rule_large2 = subset(rule_large, lhs %ain% c("age=중노년층","hours-per-week=45시간 이상"))
large2_bylift = inspect(sort(rule_large2),by="lift") ## race=백인, capital-loss=무, sex=남 이 많이 나옴을 확인

# income-large <= age=중노년층, hours=45시간 이상, sex=남, race=백인, capital-loss=무, relationship=배우자O

rule_small = subset(adu_rule2, rhs %pin% c("income=small"))
small_bylift = inspect(sort(rule_small[1:20]),by="lift")
small_bysupport = inspect(sort(rule_small[1:20]),by="support") ## hours 40시간 미만 정도가 많이 나옴을 확인
rule_small2 = subset(rule_small, lhs %in% c("hours-per-week=40시간 미만"))
rule_small3 = subset(rule_small2, lhs %ain% c("age=청장년층","sex=여")) ## age=청장년층, sex=여 가 많이 나옴을 확인
small2_bylift = inspect(sort(rule_small3),by="lift")

# income-small <= age=청장년층, hours=40시간 미만, sex=여, capital=gain=무, realtionship=배우자X

### 시각화
library(arulesViz)
plot(rule_large2)
plot(sort(rule_large2, by="support"),method="grouped")
plot(rule_small3)
plot(sort(rule_small3, by="support"),method="grouped")
plot(rule_large2,method="paracoord")
plot(rule_small3,method="paracoord")
