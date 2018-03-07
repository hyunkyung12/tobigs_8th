install.packages("stringr")
library("stringr")

whiteSpace<-function(str){
  temp<-str_trim(str)
  eval.parent(substitute(str<-temp))
}

removeSpecialText<-function(str){
  temp<-gsub("<.*?>","",str)
  eval.parent(substitute(str<-temp))
}

###EDIYA INFO.###
# URL : http://www.ediya.com/board/listing/brd/store/page/1
# page: 1~203
# symbol : <td class="left">

url = "http://www.ediya.com/board/listing/brd/store/page/"
url = paste0(url,1:203)
cont = readLines(url,encoding="UTF-8")

result = NULL
for(i in 1:length(url)){
  cont = readLines(url[i],encoding="UTF-8")
  
  index = which(str_detect(cont,"<td class=\"left\">")==TRUE)
  loca = cont[index-2]
  branch = cont[index-1]
  add = cont[index]
  num = cont[index+2] # \t는 tab \n은 엔터

  whiteSpace(loca)
  whiteSpace(branch)
  whiteSpace(add)
  whiteSpace(num)
  removeSpecialText(loca)
  removeSpecialText(branch)
  removeSpecialText(add)
  removeSpecialText(num)
  
  temp = cbind(loca,branch,add,num)
  result = rbind(result,temp)
}
# meta charset = utf-8 or euc-kr 에 따라 encoding

index = which(str_detect(cont,"<td class=\"left\">")==TRUE)
loca = cont[index-2]
branch = cont[index-1]
add = cont[index]
num = cont[index+2] # \t는 tab \n은 엔터

loca = gsub(" ","",loca) # gsub(현재문자,교체문자,대상문자열)
loca = str_trim(loca) # gsub에서 안지워졌던 \t까지 지워짐
loca = gsub("<.*?>","",loca)

# "[0-9]{3}-[0-9]{4}-[0-9]{4}" 핸드폰번호
whiteSpace(loca)
whiteSpace(branch)
whiteSpace(add)
whiteSpace(num)
removeSpecialText(loca)
removeSpecialText(branch)
removeSpecialText(add)
removeSpecialText(num)

result = cbind(loca,branch,add,num)
setwd("C:/Users/USER/Desktop/투빅스/3주차 crawling")
write.csv(result,file="test.csv")


##########################jtbc########################
# STEP1
# URL : http://news.jtbc.joins.com/section/list.aspx?pdate=20170731&scode=10©right=&pgi=1

# http://news.jtbc.joins.com/section/index.aspx?scode=10
# http://news.jtbc.joins.com/article/article.aspx?news_id=NB11502647
# http://news.jtbc.joins.com/section/{앞에 /가 없는 주소 입력}?scode=10

cont = readLines("http://news.jtbc.joins.com/section/index.aspx?scode=10",encoding = "UTF-8")
index = which(str_detect(cont,"<dt class=\"title_cr\">")==TRUE)
url = cont[index+1]
url = str_trim(url)
url = gsub("<a href=\"","",url)
url = gsub("</a>","",url)
url = str_split(url,"\">")
url = unlist(url)

sub_url = url[seq(1,40,2)] # start,end,step
sub_url = paste0("http://news.jtbc.joins.com",sub_url)
sub_cont = readLines(sub_url[1],encoding = "UTF-8")
index = which(str_detect(sub_cont, "itemprop=\"articleBody\"")==TRUE)
url = sub_cont[index+1]
url = str_trim(url)
url = gsub("<.*?>","",url) # <> 안의 속성값 전부 지우기
date_index = which(str_detect(sub_cont, "<span class=\"i_date\">")==TRUE)
date = sub_cont[date_index]
date = str_trim(date)
date = gsub("<.*?>","",date) # <> 안의 속성값 전부 지우기

