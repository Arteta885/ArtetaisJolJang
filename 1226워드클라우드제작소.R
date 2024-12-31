# 필요한 패키지 불러오기
library(tm)
library(tidyverse)
library(topicmodels)
library(stm)
library(tidytext)
library(reshape2)
library(igraph)
library(ggraph)
library(pdftools)
library(stringr)
library(SnowballC)
library(quanteda)
library(widyr)
library(tidygraph)
library(wordcloud)
library(syuzhet)
library(udpipe)
library(ggwordcloud)

# PDF 파일이 저장된 경로 설정
folder_path <- "C:/Users/dlsdn/Desktop/paper/nuclear"

# PDF 파일 목록 읽기
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# 텍스트 저장을 위한 리스트
all_text <- list()

# 사용자 정의 스테밍 규칙
stem_rules <- c("leader" = "kim", "leadership" = "kim", "supreme" = "kim",
                "escapees" = "refugees", "escapee" = "refugees", "refugee" = "refugees",
                "report" = "reports", "woman" = "women", "child" = "children", "hrc" = "ohchr",
                "girl" = "women", "girls" = "women", "female" = "women", "disability" = "disabl",
                "disabilities" = "disabl", "camp" = "camps", "disappearance" = "disappearances")

# 사용자 정의 스테밍 함수
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

# 텍스트에 사용자 정의 스테밍 적용
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# PDF 파일에서 텍스트 추출 및 전처리 함수
process_text <- function(pdf_file) {
  cat(sprintf("Processing file: %s\n", pdf_file))
  
  # PDF 텍스트 추출
  text <- pdf_text(pdf_file)
  
  # 여러 페이지를 하나로 합치고 소문자로 변환
  text <- tolower(paste(text, collapse = " "))
  
  # 사용자 정의 스테밍 적용
  text <- apply_custom_stemming(text)
  
  # 특수문자와 숫자 제거 (마침표와 쉼표 제외)
  text <- str_replace_all(text, "[^a-zA-Z.,\\s]", " ")
  text <- str_replace_all(text, "\\b\\d+\\b", " ")
  
  # 특정 단어 결합
  text <- str_replace_all(text, c("c prk" = "cprk", "per cent" = "percent",
                                  "special rapporteur" = "specialrapporteur", 
                                  "para graph" = "paragraph", "third committee" = "thirdcommittee", 
                                  "treaty bodies" = "treatybodies", "independent experts" = "independentexperts"))
  
  # 불용어 및 특정 단어 제거
  text <- removeWords(text, c(stopwords("en"), "democratic", "people", "s", "republic", "korea",
                              "korean", "north", "human", "rights", "right", "dprk", "united",
                              "nations", "commissioner", "resolution", "situation", "also",
                              "high", "draft", "paragraph", "council", "international",
                              "government", "para", "mr", "ms", "mrs", "reports", "e", "commission",
                              "said", "assembly", "committee", "general", "p", "http", "php",
                              "specialrapporteur", "independentexperts", "WWW.", "dailynk", ".com",
                              "cprk", "percent", "per", "including", "one", "english", "hrc", "ohchr",
                              "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "n", "m", "o", "p",
                              "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "www.theelders.org", "res", "spa",
                              "organ", "country"))
  
  # 월 이름 제거
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  # 공백 정리
  text <- str_squish(text)
  
  return(text)
}

# PDF 파일별로 텍스트를 처리하여 리스트에 저장
for (pdf_file in pdf_files) {
  processed_text <- process_text(pdf_file)
  all_text <- c(all_text, processed_text)
}

# 전체 텍스트를 하나의 벡터로 결합
all_text_combined <- paste(unlist(all_text), collapse = " ")

# 문장을 마침표와 쉼표 기준으로 분리
sentences <- unlist(strsplit(all_text_combined, "[.,]"))

# 문장별 단어 수 계산
word_counts <- sapply(sentences, function(sentence) length(strsplit(sentence, "\\s+")[[1]]))

# 단어 수가 5개 이상인 문장만 선택
selected_sentences <- sentences[word_counts >= 5]

# 선택된 문장을 다시 결합
all_text_combined <- paste(selected_sentences, collapse = " ")

# 단어 리스트 생성 및 빈도 계산
words <- unlist(str_split(all_text_combined, "\\s+"))
word_freq <- table(words)
sorted_word_freq <- sort(word_freq, decreasing = TRUE)

# 상위 30개 단어 추출
top_words <- head(sorted_word_freq, 30)

# ggwordcloud를 사용한 워드클라우드 생성
df <- data.frame(word = names(top_words), freq = as.numeric(top_words))

ggplot(df, aes(label = word, size = freq, color = word)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = rep(brewer.pal(8, "Dark2"), length.out = length(df$word))) +
  theme_minimal() +
  theme(aspect.ratio = 1)

# 상위 30개 단어 출력
print(top_words)



##############################어간추출############################3

# 패키지
library(tm)
library(tidyverse)
library(topicmodels)
library(stm)
library(tidytext)
library(reshape2)
library(igraph)
library(ggraph)
library(pdftools)
library(stringr)
library(SnowballC)
library(quanteda)
library(widyr)
library(tidygraph)
library(wordcloud)
library(syuzhet)
library(udpipe)

# 경로(folder_path)
folder_path <- "C:/Users/dlsdn/Desktop/paper/peace"

# PDF파일목록(pdf_files)
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# PDF에서 TEXT 추출(이름지정:all_text)
all_text <- list()

##### Custom stemming list #####
stem_rules <- list(
  "leader" = "kim",
  "leadership" = "kim"
)

# 어간추출함수정의(ChatGPT활용)
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

# 어간추출적용함수정의(ChatGPT활용)
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# PDF 파일에서 TEXT 추출 및 어간 추출 적용
for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  # 텍스트 추출
  text <- pdftools::pdf_text(pdf_file)
  
  # 소문자 변환
  text <- tolower(text)
  
  # 어간 추출 적용
  text <- apply_custom_stemming(text)
  
  
  # 전처리 수행 (마침표, 쉼표 제외)
  text <- gsub("[^a-zA-Z\\s\\.,]", " ", text)  # 특수문자 제거 (마침표와 쉼표 제외)
  text <- gsub("\\b\\d+\\b", " ", text)     # 숫자 제거
  text <- removeWords(text, stopwords("en"))  # 불용어 제거
  text <- gsub("\\bdisabilities\\b", "disabl", text) #disabilities -> disabl
  
  # 특수한 구문을 한 단어로 묶기
  text <- gsub("c prk", "cprk", text)
  text <- gsub("per cent", "percent", text)
  text <- gsub("special rapporteur", "specialrapporteur", text)
  
  # 특정 단어 제거 (24 07 13 update)
  remove_words <- c("human", "rights", "international", "situation", "un", 
                    "united", "nations", "dprk", "democratic", "people",
                    "s", "republic", "korea", "korean", "also", "commission",
                    "country", "council", "north", "ohchr", "hrc", "will", "report", 
                    "para", "state", "statement", "states", "resolution", "commissioner", 
                    "committee","session", "mr", "mrs", "ms")
  text <- removeWords(text, remove_words)
  
  # 월 제거
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  # 소문자로 변환된 텍스트를 문장 단위로 분리
  sentences <- strsplit(text, "[.,]")[[1]]
  
  # 각 문장을 단어로 분리하여 단어의 개수를 세기
  word_counts <- sapply(sentences, function(sentence) length(strsplit(sentence, "\\s+")[[1]]))
  
  # 단어의 개수가 5개 이상인 문장만 선택
  selected_sentences <- sentences[word_counts >= 5]
  
  # 선택된 문장을 리스트에 추가
  all_text <- c(all_text, selected_sentences)  
}

##### 추출된 텍스트를 하나의 벡터로 결합 #####
all_text_combined <- paste(unlist(all_text), collapse = " ")

##### 단어 리스트 생성 #####
words <- unlist(str_split(all_text_combined, "\\s+"))

##### 어간추출 수행 #####
stemmed_words <- wordStem(words, language = "en")

##### 단어 빈도 계산 #####
word_freq <- table(stemmed_words)
sorted_word_freq <- sort(word_freq, decreasing = TRUE)

##### 상위 30개 단어 추출 #####
top_words <- head(sorted_word_freq, 30)
##### ggwordcloud를 사용한 워드클라우드 생성 #####
# 데이터 프레임 생성
df <- data.frame(word = names(top_words), freq = as.numeric(top_words))

# ggwordcloud 사용 (단어에 색상 추가 - 색상 반복 사용)
ggplot(df, aes(label = word, size = freq, color = word)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = rep(brewer.pal(8, "Dark2"), length.out = length(df$word))) +  # 색상 반복
  theme_minimal() +
  theme(aspect.ratio = 1)  # 정사각형 비율 유지

print(top_words)
