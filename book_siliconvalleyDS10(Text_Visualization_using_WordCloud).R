# 1. 제퍼디! 질문 데이터
# (1) Show Number : 방송 횟수
# (2) Air Date : 방송 날짜
# (3) Round : 주로 'Jeopardy!', 'Double Jeopardy!', 'Final Jeopardy!'가 있다.
# (4) Category : 문제 카테고리
# (5) Value : 문제를 맞췄을 때의 상금($)
# (6) Question : 질문
# (7) Answer : 정답


# 2. 자연어 처리와 텍스트 마이닝 환경 준비
# url : https://goo.gl/r39eIp
# (1) tm : 텍스트 마이닝을 위한 R Frameword
# (2) SnowballC : 어간추출을 위한 library. C의 libstemmer에 기반을 둔다.
# (3) wordcloud : 단어 구름을 생성하기 위한 library
install.packages(c("tm", "SnowballC", "wordcloud"))


# 3. 단어 구름 그리기
library(tm)
library(SnowballC)
library(wordcloud)

data <- read.csv("JEOPARDY_CSV.csv", stringsAsFactors = F, nrows = 10000)
glimpse(data)

(data_corpus <- Corpus(VectorSource(data$Question)))

data_corpus <- tm_map(data_corpus, content_transformer(tolower)) # 소문자 변환
data_corpus <- tm_map(data_corpus, removePunctuation) # 쉼표 제거
data_corpus <- tm_map(data_corpus, removeWords, stopwords('english')) # for, of, his 등의 불용어 제거
data_corpus <- tm_map(data_corpus, stemDocument) # 어간 추출

wordcloud(data_corpus, max.words = 100, random.order = F, colors = brewer.pal(8, "Dark2"))
# http://cs224d.stanford.edu
# 한국어 자연어 처리 패키지 : KoNLP / url : https://goo.gl/p5kA5L