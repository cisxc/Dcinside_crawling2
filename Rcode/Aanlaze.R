install.packages('wordcloud2') 
install.packages("KoNLP")
install.packages("igraph")
install.packages("arules")
install.packages("combinat")
install.packages('reshape2')
install.packages('tm')
install.packages('qgraph')
##################################################
library(wordcloud2)
library("KoNLP")  
library(dplyr)
library("tidyverse")   
library('igraph')
library(arules)
library(combinat)
library('reshape2')
library(tm)
library(qgraph)
useSejongDic() #단어 사전 업로드
dir = ('C:/R_data/Dcinside')
file_list = list.files(dir)

#변수 초기화 
data = data.frame()
all.list = list()
all.str.sm = vector()
all.str = vector()
all.rank = vector()
all.rank.str = vector()
#####################

###자주 쓰인 단어, 갤러리 랭킹
for(file in file_list){
  all.str.sm = vector()
  temp = read.csv(paste(dir,file,sep="/"),header=TRUE,sep=",")
  all.rank = cbind(all.rank,names(temp)[2:11])
  for(rnk in 2:11){
    all.str.sm = c(all.str.sm,as.vector(temp[[rnk]][!is.na(temp[[rnk]])]))
  }
  all.str = c(all.str,all.str.sm)
  ##시간별 순위 매기기
  reviews.word <- sapply(all.str.sm, extractNoun, USE.NAMES = F)
  word_vector = unlist(reviews.word)
  word_vector = gsub('[ㄱ-ㅎ]','',word_vector)
  word_vector = gsub('jp','',word_vector)
  word_vector = gsub('[~!@#$%&*()_+=?<>^]','',word_vector)
  word_vector <- Filter(function(x){nchar(x)>=2}, word_vector)
  name = table(word_vector)
  wordcount <- head(sort(name,decreasing=T),10)
  #####
  all.rank.str= cbind(all.rank.str,names(wordcount))
}


## 명사/형용사 추출 함수 생성
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

options(mc.cores=1)    # 단일 Core 만 활용하도록 변경 (옵션)


cps <- VCorpus(VectorSource(all.str))  
tdm <- TermDocumentMatrix(cps,control=list(tokenize=ko.words,   ## token 분류시 활용할 함수명 지정
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 6),  
                                       weighting=weightBin))  

#최종결과 확인
dim(tdm)
tdm.matrix <- as.matrix(tdm)
#Encoding(rownames(tdm.matrix)) <- "UTF-8"
rownames(tdm.matrix)[1:100]


#자주 쓰이는 단어 순으로 order 처리
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함
word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬
freq.words <- tdm.matrix[word.order[1:20], ] #Term Document Matrix에서 자주 쓰인 단어 상위 20개에 해당하는 것만 추출
co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       edge.color='blue',
       vsize=log(diag(co.matrix))*2) ##diag는 matrix에서 대각선만 뽑는 것임. 즉 그 단어가 얼마나 나왔는지를 알 수 있음. vsize는 그 크기를 결정하는데 여기 인자값으로 단어가 나온 숫자를 넘겨주는 것임. log를 취한것은 단어 크기의 차이가 너무 커서 log를 통해서 그 차이를 좀 줄여준것임. 




###종합 문자 분리하기 
reviews.word <- sapply(all.str, extractNoun, USE.NAMES = F)
word_vector = unlist(reviews.word)
word_vector = gsub('[ㄱ-ㅎ]','',word_vector)
word_vector = gsub('jp','',word_vector)
word_vector = gsub('[~!@#$%&*()_+=?<>^]','',word_vector)
word_vector <- Filter(function(x){nchar(x)>=2 && nchar(x)<=5}, word_vector)
name = table(word_vector)

#자주 쓰이는 단어 순으로 order 처리
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함
word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬
freq.words <- tdm.matrix[word.order[1:20], ] #Term Document Matrix에서 자주 쓰인 단어 상위 20개에 해당하는 것만 추출
co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경
co.matrix



###워드클라우드2
wordcount <- head(sort(name,decreasing=T),100)
wordcloud2(wordcount,fontFamily = '나눔바른고딕')

##단어
wordcount <- head(sort(name,decreasing=T),15)
wc.df = data.frame(wordcount)
Region = wc.df$word_vector
ggplot(data.frame(wordcount), aes(y = wc.df$Freq, x = wc.df$word_vector,color = Region)) + geom_point()


