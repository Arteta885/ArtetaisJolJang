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
folder_path <- "C:/Users/dlsdn/Desktop/paper/nuclear"

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

##### "kim" 포함 여부 열 추가 #####
pdf_texts_df$contains_kim <- factor(ifelse(grepl("\\bkim\\b", pdf_texts_df$sentences), "yes", "no"))

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
K <- 5  # 주제 수 설정
stm_model <- stm(documents, vocab, K = K, data = pdf_texts_df, max.em.its = 75)

##### STM 모델 요약 #####
print(summary(stm_model))

##### 주제 비율 계산 #####
# 주제 비율 계산
topic_proportions <- colMeans(stm_model$theta)

# 데이터프레임 생성
topic_df <- data.frame(
  Topic = paste0("Topic ", 1:length(topic_proportions)),
  Proportion = topic_proportions
)

# Topic 번호가 위에서 아래로 1 → 5 순서로 보이도록 factor 수준 조정
topic_df$Topic <- factor(topic_df$Topic, levels = rev(paste0("Topic ", 1:length(topic_proportions))))

# ggplot으로 논문 스타일 그래프 생성
p <- ggplot(topic_df, aes(x = Topic, y = Proportion)) +
  geom_col(fill = "white", color = "black", linewidth = 0.6, width = 0.5) +
  geom_text(aes(label = round(Proportion, 2)), hjust = -0.3, size = 3.5) +
  coord_flip() +
  labs(title = "Topic Proportions in Nuclear/Missile Documents",
       x = NULL, y = "Expected Topic Proportions") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# 출력
p  # 또는 print(p)


# 주제별 주요 단어 플롯
plot(stm_model, type = "labels", n = 12, main = "Top 10(+2) Words for Each Topic")



#####################################################
##### 예시 문장 추출출 #####
#####################################################
# 각 토픽별 예시 문장 3개씩 무작위 추출 (중복 제거 포함)
# 사용자 정의 코사인 유사도 함수
compute_cosine_similarity <- function(dtm_matrix) {
  norm_matrix <- dtm_matrix / sqrt(rowSums(dtm_matrix^2)) # 각 행을 벡터 정규화
  similarity_matrix <- norm_matrix %*% t(norm_matrix)     # 코사인 유사도 계산
  return(similarity_matrix)
}

remove_similar_sentences <- function(sentences, threshold = 0.5) {
  if (length(sentences) < 2) return(sentences)  # 문장이 1개 이하라면 바로 반환
  
  # DTM 생성
  dtm <- CreateDtm(sentences, doc_names = seq_along(sentences))
  dtm_matrix <- as.matrix(dtm)
  
  # 코사인 유사도 계산
  similarity_matrix <- compute_cosine_similarity(dtm_matrix)
  
  # 유사도가 threshold를 초과하는 문장의 인덱스 제거
  to_remove <- c()
  for (i in 1:(nrow(similarity_matrix) - 1)) {
    for (j in (i + 1):ncol(similarity_matrix)) {
      if (similarity_matrix[i, j] > threshold) {
        to_remove <- c(to_remove, j)
      }
    }
  }
  
  # 중복 문장 제거: to_remove가 비어있지 않은 경우에만 제거 수행
  if (length(to_remove) > 0) {
    return(sentences[-unique(to_remove)])
  } else {
    return(sentences)  # 제거할 문장이 없으면 원본 반환
  }
}
# 코사인 유사도 계산 함수 사용
example_sentences <- list()

for (k in 1:K) {
  # 각 토픽에서 대표적인 문장 10개 찾기
  example <- findThoughts(stm_model, texts = pdf_texts_df$sentences, topics = k, n = 30)
  
  # 문장 길이 필터 및 중복 제거
  filtered_examples <- unique(unlist(lapply(example$docs[[1]], function(sentence) {
    # 문장 길이 필터 (단어 수 5개 이상인 문장만 선택)
    if (str_count(sentence, "\\w+") >= 5) sentence else NA
  })))
  
  # NA 값 제거 후 고유한 문장만 남김
  filtered_examples <- filtered_examples[!is.na(filtered_examples)]
  
  # 고유한 문장이 3개 이상일 경우, 중복 없이 3개 문장만 무작위 선택
  if (length(filtered_examples) >= 3) {
    sampled_sentences <- sample(filtered_examples, 3, replace = FALSE)
  } else {
    # 3개보다 적은 경우, 가능한 문장만 선택
    sampled_sentences <- filtered_examples
  }
  
  # 유사 문장 제거
  filtered_sentences <- remove_similar_sentences(sampled_sentences, threshold = 0.5)
  example_sentences[[paste("Topic", k)]] <- filtered_sentences
}

# 각 토픽에 대한 예시 문장 출력
print(example_sentences)
