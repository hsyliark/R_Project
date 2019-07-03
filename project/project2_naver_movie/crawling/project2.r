setwd("D:/Workplace/R_Project/project/project2_naver_movie/crawling")

library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
library(abind)



### 영화 '기생충' 평점, 리뷰


trim <- function(x) gsub("^\\s+|\\s+$", "", x)

base_url <- "https://movie.naver.com"
url_sub <- "/movie/bi/mi/point.nhn?code=161967"
url <- paste0(base_url,url_sub)

html <- read_html(url)

url2 <- html %>% html_node('iframe.ifr') %>% html_attr('src')
url_add <- "&page=" 
url_ifr <- paste0(base_url,url2,url_add)

ems_html <- read_html(url_ifr)
ems <- ems_html %>%
  html_node("div.score_total") %>%
  html_nodes("em") %>%
  html_text()
ems <- as.numeric(gsub(",","",ems[2]))
pages <- 1:ceiling(ems/10)

naver_movie <- data.frame(score=c(),review=c(),writer=c(),time=c())

for (n in 1:length(pages)) {
  
  if (n %% 50 == 0)
    print(n)
  
  url_pages <- paste0(url_ifr,pages[n])
  html2 <- read_html(url_pages)  
  
  lis <- html2 %>% html_node('div.score_result') %>% html_nodes('li') 
  
  score <- c()
  review <- c()
  writer <- c()
  time <- c()
  
  for (li in lis) {
    # score
    score <- c(score,html_node(li,'.star_score') %>% html_text('em') %>% trim()) 
    # review
    tmp <- li %>% html_node('.score_reple') %>% html_text('p') %>% trim()
    idx <- str_locate(tmp,'\r')
    review <- c(review,str_sub(tmp,1,idx[1]-1))
    # writer
    tmp <- trim(str_sub(tmp,idx[1],-1))
    idx <- str_locate(tmp,'\r')
    writer <- c(writer,str_sub(tmp,1,idx[1]-1))
    # time
    tmp <- trim(str_sub(tmp,idx[1],-1))
    idx <- str_locate(tmp,'\r')
    time <- c(time,str_sub(tmp,1,idx[1]-1))
  }
  
  movie <- data.frame(score=score,review=review,writer=writer,time=time)
  naver_movie <- rbind.data.frame(naver_movie,movie)  

}

View(naver_movie)

write.csv(naver_movie,"naver_movie.csv")
