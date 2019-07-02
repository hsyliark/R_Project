setwd("D:/Workplace/R_Project/project1")

library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)


### Naver news 속보


categories <- c("정치","경제","사회","생활_문화","세계","IT_과학","오피니언")
base_url <- 'https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1='
number <- c(100,101,102,103,104,105,110)

category <- c() 
for (i in 1:length(categories)) {
  category <- c(category,rep(categories[i],10))
}

naver_headline1 <- data.frame(titles=c(), writing=c(), articles=c())

for (i in 1:length(number)) {
  
  url <- paste0(base_url,number[i])
  html <- read_html(url)
  
  # 제목
  lis1 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('dt') %>%
    html_text()
  lis1 <- trim(lis1)
  lis1 <- ifelse(lis1 %in% c("","동영상기사"),"",lis1)
  titles <- c()
  for (i in 1:length(lis1)) {
    if (lis1[i] %in% c("","동영상기사")) {
      titles <- titles }
    else {
      a <- gsub('\\"',"",lis1[i])
      titles <- c(titles,a)
    }
  }
  
  # 제공회사
  lis2 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('.writing') %>%
    html_text()
  writing <- lis2
  
  # 기사
  lis3 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('.lede') %>%
    html_text()
  lis3 <- trim(lis3)
  lis3 <- ifelse(lis3 %in% c("","동영상기사"),"",lis3)
  articles <- c()
  for (i in 1:length(lis3)) {
    if (lis3[i] %in% c("","동영상기사")) {
      articles <- articles }
    else {
      a <- gsub('\\"',"",lis3[i])
      articles <- c(articles,a)
    }
  }
  
  headline1 <- data.frame(titles=titles, writing=writing, articles=articles)
  naver_headline1 <- rbind.data.frame(naver_headline1, headline1)
  
}

naver_headline1 <- cbind.data.frame(category,naver_headline1)

View(naver_headline1)



naver_headline2 <- data.frame(titles=c(), writing=c(), articles=c())

for (i in 1:length(number)) {
  
  url <- paste0(base_url,number[i])
  html <- read_html(url)
  
  # 제목
  lis1 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06') %>%
    html_nodes('dt') %>%
    html_text()
  lis1 <- trim(lis1)
  lis1 <- ifelse(lis1 %in% c("","동영상기사"),"",lis1)
  titles <- c()
  for (i in 1:length(lis1)) {
    if (lis1[i] %in% c("","동영상기사")) {
      titles <- titles }
    else {
      a <- gsub('\\"',"",lis1[i])
      titles <- c(titles,a)
    }
  }
  
  # 제공회사
  lis2 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('.writing') %>%
    html_text()
  writing <- lis2
  
  # 기사
  lis3 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('.lede') %>%
    html_text()
  lis3 <- trim(lis3)
  lis3 <- ifelse(lis3 %in% c("","동영상기사"),"",lis3)
  articles <- c()
  for (i in 1:length(lis3)) {
    if (lis3[i] %in% c("","동영상기사")) {
      articles <- articles }
    else {
      a <- gsub('\\"',"",lis3[i])
      articles <- c(articles,a)
    }
  }
  
  headline2 <- data.frame(titles=titles, writing=writing, articles=articles)
  naver_headline2 <- rbind.data.frame(naver_headline2, headline2)
  
}

naver_headline2 <- cbind.data.frame(category,naver_headline2)

View(naver_headline2)



naver_headline <- rbind.data.frame(naver_headline1,naver_headline2)

View(naver_headline)

  
  












