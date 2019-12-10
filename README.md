# Dcinside 대학 분위기 분석
www.dcinside.com 사이트를 R를 이용하여 분석을 해본다.
****
<h2> 개요 </h2>
Dcinside는 국내 커뮤니티중에 가장 거대한 사이트이다.
2019년 대한민국의 사이트 접속률 8위를 기록하고 있는 자타공인 국내 최대 커뮤니티 사이트.
대한민국 최대의 커뮤니티답게 무려 2,000여 개의 갤러리, 1만여 개의 마이너 갤러리가 있으며 하루에 200만 명이 방문하고, 일일 페이지뷰가 1억 회에 달하는 사이트다.


<h2> 수집방법 </h2>

* https://gall.dcinside.com 파싱
* 각 5000개의 데이터를 분석하였다
* DC 갤러리의 제목들을 크롤링
* 워드클라우드2를 활용하여 대학교 성향 및 분위기 분석

 ****
 
 <h2> 결과 예상 </h2>
 
 <h3> 셀레니움 연결 </h3>
 
~~~
 
useSejongDic() #단어 사전 업로드

### 셀레니움 연결
remDr <- remoteDriver(remoteServerAddr = 'localhost', 
                      port = 4445L, # 포트번호 입력 
                      browserName = "chrome") 
remDr$open() 

 
 ~~~
 
<h3> 메인코드 </h3>
 
~~~
 
### 제목 벡터 생성

all.title <- c()


####100개씩 5페이지를 가져옴 
for(page in 1:5){
  
  url = 'https://gall.dcinside.com/board/lists/?id=koreauniversity&list_num=100&sort_type=N&search_head=&page='
  url <- paste(url,page,sep='')
  # read_html 함수를 사용하여 html 페이지를 htxt 변수에 저장
  remDr$navigate(url)
  
  page_source = remDr$getPageSource()[[1]]
  
  title <-read_html(page_source) %>%
    html_nodes(xpath='//*[@id="container"]/section[1]/article[2]/div[2]/table/tbody/tr[*]/td[2]/a[1]/text()')%>%
    html_text()
  

  
  all.title <- c(all.title, title)
 
}

~~~

<h3> 워드클라우드2 </h3>
 
~~~

###문자 분리하기 

reviews.word <- sapply(all.title, extractNoun, USE.NAMES = F)
word_vector = unlist(reviews.word)
word_vector <- Filter(function(x){nchar(x)>=2 && nchar(x)<=5}, word_vector)
name = table(word_vector)

###워드클라우드2
wordcount <- head(sort(name,decreasing=T),200)
wordcloud2(wordcount,fontFamily = '나눔바른고딕')


~~~
 
 ****
 
<h2> 실행 및 분석 결과 </h2>

<h3> 전북대 </h3>

![Alt text](/img/JBNU.JPG)

<h3> 서울대 </h3>

![Alt text](/img/SU.JPG)

<h3> 연세대 </h3>

![Alt text](/img/YU.JPG)

<h3> 고려대 </h3>

![Alt text](/img/KU.JPG)

<h3> 전남대 </h3>

![Alt text](/img/JNNU.JPG)
