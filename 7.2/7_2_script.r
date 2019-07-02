setwd("D:/Workplace/R_Project/7.2")

## Crawling 방법
# html_nodes() -> tag, 구별자 (class,id)
# html_attr() -> src, 속성이름 검색
# html_text() -> h1, 텍스트 가져올 때

# &ndsp; -> 빈칸 

## CSS 적용 방법
# 1. tag에서 attribute 적용
# 2. <head> style </head>
# 3. 별도 파일

### list 만들기

# 순서 있는 리스트 - <ol> </ol>
# 순서 없는 리스트 - <ul> </ul>
# 정의 리스트 - <dl> </dl>

## list item
# <li> ... </li>
# <li> 생략 가능

### table
# <table> : 표 전체를 담는 컨테이너

###
# id : # --> javascript
# class : . 
# <caption> : 표 제목
# <thead> : 헤딩 셀 그룹
# <tfoot> : 바닥 셀 그룹
# <tbody> : 데이터 셀 그룹
# <tr> : 행, 여러 셀로 구성 <th>, <td>
# <th> : 제목 셀
# <td> : 데이터 셀

### anchor <a>





# 한빛아카데미 도서 crawling

install.packages("rvest")
library(rvest)
library(dplyr)
library(stringr)
install.packages("openxlsx")
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# createWorkbook()
# addWorksheet()
# writeDataTable()

base_url <- 'http://www.hanbit.co.kr/store/books/category_list.html?'
page <- 'page='
category <- c('&cate_cd=004007&srt=p_pub_date','&cate_cd=004008&srt=p_pub_date',
              '&cate_cd=004003&srt=p_pub_date','&cate_cd=004004&srt=p_pub_date',
              '&cate_cd=004005&srt=p_pub_date','&cate_cd=004006&srt=p_pub_date',
              '&cate_cd=005005&srt=p_pub_date','&cate_cd=005001&srt=p_pub_date',
              '&cate_cd=005002&srt=p_pub_date','&cate_cd=005003&srt=p_pub_date',
              '&cate_cd=005004&srt=p_pub_date')
category_length <- c(6,4,3,3,1,2,2,2,1,1,1)
              

hanbit_books <- data.frame(title=c(),writer=c(),price=c()) 

for (n in 1:length(category)) {

    for (j in 1:length(category_length)) {  
  
        for (i in 1:category_length[j]) {
  
             url <- paste0(base_url,page,i,category[n])

             html <- read_html(url)
             # book_list <- html_node(html,'.sub_book_list_area')
             # lis <- html_nodes(book_list,"li")

             lis <- html %>%
               html_node('.sub_book_list_area') %>%
               html_nodes("li") 
  
             price <- c()
             title <- c()
             writer <- c()

             for (li in lis) {
                  pr <- html_node(li,'.price') %>% html_text()
                  pr <- gsub("\\\\","",pr)
                  price <- c(price,pr)
                  title <- c(title,html_node(li,'.book_tit') %>% html_text())
                  writer <- c(writer,html_node(li,'.book_writer') %>% html_text()) 
                  # cat(title,writer,price,'\n')
             }

             books <- data.frame(title=title,writer=writer,price=price)
             hanbit_books <- rbind.data.frame(hanbit_books,books)

        }
 
    }  
   
}

hanbit_books





# 한빛아카데미 도서 크롤링
library(rvest)
library(dplyr)
library(stringr)
#library(xlsx)
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

cat_names = c('컴퓨터공학', '정보통신_전기_전자', '수학_과학_공학', '프로그래밍_웹',
              '그래픽_디자인', 'OA_활용', '전기기본서', '전기기사',
              '전기산업기사', '전기공사기사', '전기공사산업기사')
categories = c('004007', '004008', '004003', '004004', '004005', '004006',
               '005005', '005001', '005002', '005003', '005004')
pages = c(6, 4, 3, 3, 1, 2, 2, 2, 1, 1, 1)

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page <- 'page='
category <- '&cate_cd='
sort <- '&srt=p_pub_date'

wb <- createWorkbook()
for (cat_no in 1:11) {
  df_books <- data.frame(title=c(), writer=c(), price=c())
  print(cat_no)
  
  for (i in 1:pages[cat_no]) {
    url <- paste0(base_url, page, i, category, categories[cat_no], sort)
    html <- read_html(url)
    
    html %>%
      html_node('.sub_book_list_area') %>%
      html_nodes('li') -> lis
    lis
    
    price <- c()
    title <- c()
    writer <- c()
    for (li in lis) {
      pr <- html_node(li, '.price') %>% html_text()
      pr <- gsub("\\\\", "", pr)
      price <- c(price, pr)
      title <- c(title, html_node(li, '.book_tit') %>% html_text())
      writer <- c(writer, html_node(li, '.book_writer') %>% html_text())
    }
    books <- data.frame(title=title, writer=writer, price=price)
    df_books <- rbind.data.frame(df_books, books)
  }
  # filename <- paste0("D:/Workspace/R_Project/01_Crawling/books/", cat_no, ".xlsx")
  # write.xlsx(df_books, file=filename, 
  #            sheetName=cat_names[cat_no],
  #            col.names=TRUE, row.names=FALSE, append=TRUE)
  
  addWorksheet(wb, cat_names[cat_no])
  writeDataTable(wb, cat_names[cat_no], df_books)
} 
saveWorkbook(wb, file="D:/Workspace/R_Project/01_Crawling/books.xlsx")






