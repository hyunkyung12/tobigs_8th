library("stringr")

url = "http://www.ediya.com/board/listing/brd/store/page/"
url = paste0(url,1:5)

suburl = 

clean = function(str){
  temp = gsub("<.*?>","",str)
  temp = str_trim(temp)
  eval.parent(substitute(str<-temp))
}

result = NULL
time = c()
for(i in 1:length(url)){
  ## 첫번째 페이지의 크롤링
  cont = readLines(url[i],encoding = "UTF-8") # 첫번째 페이지 소스 전체
  index = which(str_detect(cont,"<td class=\"left\">")==TRUE) # 원하는 정보가 있는 곳의 index

  location = cont[index-2]
  branch = cont[index-1]
  address = cont[index]
  phone = cont[index+2]
  
  clean(location)
  clean(branch)
  clean(address)
  clean(phone)
  
  ## 두번째 페이지의 크롤링
  sub_index = which(str_detect(cont, "<a class=\"btn navybtn\"")==TRUE) # 두번째로 원하는 정보가 있는 곳의 index
  sub = cont[sub_index]
  sub = str_trim(sub)
  sub = gsub("<a class=\"btn navybtn\" href=\"","",sub)
  sub = gsub("\">상세보기</a>","",sub)
  url2 = paste0("http://www.ediya.com",sub)
  
  
  for(j in 1:length(url2)){
    cont2 = readLines(url2[j],encoding="UTF-8") # 두번째 페이지의 속성 전체
    index2 = which(str_detect(cont2, "<td class=\"td_address\">")==TRUE) # 원하는 정보가 있는곳의 index
    
    time[j] = cont2[index2+4]
    clean(time)
    time = gsub("주말 및 공휴일에는 변경될 수 있습니다.","",time) # 주말 및 공휴일~ 은 공통인것 같아서 삭제
  }
  
  temp = cbind(location, branch, address, phone, time)
  result = rbind(result,temp)
}

setwd("C:/Users/USER/Desktop/투빅스/3주차 과제")
write.csv(result,file="크롤링.csv")

