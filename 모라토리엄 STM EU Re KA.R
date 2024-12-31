##### 패키지 로드 #####
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
library(dplyr)
library(textmineR)
library(ggplot2)
library(proxy)

##### 경로 설정 #####
folder_path <- "C:/Users/dlsdn/Desktop/paper/peace"

##### PDF 파일 목록 #####
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

##### Custom stemming list #####
stem_rules <- c("leader" = "kim", "leadership" = "kim", "supreme" = "kim",
                "escapees" = "refugees", "escapee" = "refugees", "refugee" = "refugees",
                "report" = "reports", "woman" = "women", "child" = "children", "hrc" = "ohchr",
                "girl" = "women", "girls" = "women", "female" = "women", "disability" = "disabl",
                "disabilities" = "disabl", "camp" = "camps", "disappearance" = "disappearances")

##### 전처리 및 사용자 정의 스테밍 적용 함수 #####
apply_custom_stemming <- function(text) {
  words <- str_split(text, "\\s+")[[1]]
  stemmed_words <- ifelse(words %in% names(stem_rules), stem_rules[words], words)
  paste(stemmed_words, collapse = " ")
}

##### 문장을 분리하고 단어 수가 5개 이상인 문장만 채택하는 함수 #####
split_into_sentences <- function(text) {
  sentences <- unlist(strsplit(text, "(?<=[.,])\\s+", perl = TRUE))
  sentences <- sentences[sapply(sentences, function(s) str_count(s, "\\w+") >= 5)]
  return(sentences)
}

##### 텍스트 전처리 및 필터링을 한 번에 적용 #####
process_text <- function(pdf_file) {
  cat(sprintf("Extracting and processing text from file: %s\n", pdf_file))
  
  # 텍스트 추출
  text <- pdf_text(pdf_file)
  
  # 소문자 변환
  text <- tolower(paste(text, collapse = " "))
  
  # 사용자 정의 스테밍 적용
  text <- apply_custom_stemming(text)
  
  # 특수문자와 숫자 제거, 마침표와 쉼표 제외
  text <- str_replace_all(text, "[^a-zA-Z.,\\s]", " ")
  text <- str_replace_all(text, "\\b\\d+\\b", " ")
  
  # 중복 공백 제거
  text <- str_squish(text)
  
  # 특정 단어 묶기
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
                              "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "www.theelders.org", "res", "country",
                              "spa", "th"))
  
  # 월 제거
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  # 문장 분리 및 단어 수 5개 이상인 문장만 선택
  split_into_sentences(text)
}

##### 모든 PDF 파일에 대해 텍스트 처리 수행 #####
all_text <- lapply(pdf_files, process_text)
names(all_text) <- basename(pdf_files)

##### 추출된 텍스트를 데이터 프레임으로 변환 #####
pdf_texts_df <- data.frame(
  file_name = rep(names(all_text), sapply(all_text, length)),
  sentences = unlist(all_text),
  stringsAsFactors = FALSE
)

##### "security" 포함 여부 열 추가 #####
pdf_texts_df$contains_security <- factor(ifelse(grepl("\\bsecurity\\b", pdf_texts_df$sentences), "yes", "no"))

##### 전처리된 텍스트로 DTM 생성 #####
# 전처리된 텍스트를 기반으로 corpus 생성
corpus <- VCorpus(VectorSource(pdf_texts_df$sentences))  # 이미 전처리된 텍스트 사용

# DTM 생성 (단어 길이, 빈도수에 따라 설정)
dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(1, Inf), 
  bounds = list(global = c(5, Inf))
))

# DTM을 STM 형식으로 변환
dtm_matrix <- as.matrix(dtm)
vocab <- colnames(dtm_matrix)
documents <- apply(dtm_matrix, 1, function(x) {
  rbind(as.integer(which(x > 0)), as.integer(x[x > 0]))
})

##### STM 모델 생성 #####
K <- 4  # 주제 수 설정
stm_model <- stm(documents, vocab, K = K, data = pdf_texts_df, max.em.its = 75)

##### STM 모델 요약 #####
print(summary(stm_model))

##### 주제 비율 계산 #####
topic_proportions <- colMeans(stm_model$theta)

##### STM 모델 요약 플롯 (비율 반영) #####
plot(stm_model, 
     type = "summary", 
     labeltype = "prob", 
     custom.labels = paste0(round(topic_proportions, 2)), 
     main = "STM model and top 10 Highest Prob (Moratorium)")

# 주제별 주요 단어 플롯
plot(stm_model, type = "labels", n = 12, main = "Top 10(+2) Words for Each Topic")



#####################################################
##### 예시 문장 추출출 #####
#####################################################
# 각 토픽별 예시 문장 3개씩 무작위 추출 (중복 제거 포함)
example_sentences <- list()

for (k in 1:K) {
  # 각 토픽에서 대표적인 문장 3개 찾기
  example <- findThoughts(stm_model, texts = pdf_texts_df$sentences, topics = k, n = 10)
  
  # 문장 길이 필터 및 중복 제거
  filtered_examples <- unique(unlist(lapply(example$docs[[1]], function(sentence) {
    # 문장 길이 필터 (단어 수 5개 이상인 문장만 선택)
    if (str_count(sentence, "\\w+") >= 5) sentence else NA
  })))
  
  # NA 값 제거 후 고유한 문장만 남김
  filtered_examples <- filtered_examples[!is.na(filtered_examples)]
  
  # 고유한 문장이 3개 이상일 경우, 중복 없이 3개 문장만 무작위 선택
  example_sentences[[paste("Topic", k)]] <- if (length(filtered_examples) >= 3) {
    sample(filtered_examples, 3, replace = FALSE)
  } else {
    # 3개보다 적은 경우, 가능한 문장만 선택
    filtered_examples
  }
}

# 각 토픽에 대한 예시 문장 출력
example_sentences
