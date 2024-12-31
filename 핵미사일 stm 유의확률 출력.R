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

###########################
#################################
####################################

##### "kim"이 포함된 문서 비율 계산 #####
# 문서별로 "kim" 포함 여부를 확인
kim_inclusion <- sapply(all_text, function(sentences) {
  any(grepl("\\bkim\\b", sentences, ignore.case = TRUE))
})

# 포함된 문서 수와 비율 계산
kim_included_count <- sum(kim_inclusion)
total_docs <- length(all_text)
kim_included_ratio <- kim_included_count / total_docs * 100

# 결과 출력
cat(sprintf("Total documents: %d\n", total_docs))
cat(sprintf("Documents containing 'kim': %d\n", kim_included_count))
cat(sprintf("Percentage of documents containing 'kim': %.2f%%\n", kim_included_ratio))


########################
##########################

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



##############3
################
#################


##### "kim" 포함 여부가 주제 분포에 미치는 영향 평가 #####
# "contains_kim" 열 생성
if (!"text" %in% colnames(pdf_texts_df)) {
  pdf_texts_df$text <- pdf_texts_df$sentences
}
pdf_texts_df$contains_kim <- ifelse(grepl("\\bkim\\b", pdf_texts_df$text), "yes", "no")
pdf_texts_df$contains_kim <- as.factor(pdf_texts_df$contains_kim)

# 메타데이터와 DTM 일치 확인
if (nrow(dtm) != nrow(pdf_texts_df)) {
  pdf_texts_df <- pdf_texts_df[seq_len(nrow(dtm)), ]
}

##### "kim" 포함 여부가 주제 분포에 미치는 영향 평가 #####
effect <- estimateEffect(1:K ~ contains_kim, stm_model, meta = pdf_texts_df)

##### 효과 요약 #####
print(summary(effect))

#######################################################################


############################################3


# 분석 결과를 데이터프레임으로 준비 (Intercept 제외)
results_kim <- data.frame(
  Topic = factor(rep(1:5, each=1), levels=1:5),
  Variable = rep("contains_kimyes", times=5),
  Estimate = c(-0.0091421, -0.0090316, -0.0053497, 0.0299222, -0.006467),
  Std.Error = c(0.0025945, 0.0023426, 0.0026228, 0.0026614, 0.002667),
  p.value = c(0.000426, 0.000116, 0.0414, 2e-16, 0.0153)
)

# p-value 범위에 따라 색상 설정
results_kim <- results_kim %>%
  mutate(Significance = case_when(
    p.value <= 0.001 ~ "Very Significant (***)",
    p.value <= 0.01 ~ "Significant (**)",
    p.value <= 0.05 ~ "Marginally Significant (*)",
    p.value <= 0.1  ~ "Borderline Significant (.)",
    TRUE ~ "Not Significant"
  ))

# 막대그래프 생성
library(ggplot2)
ggplot(results_kim, aes(x=Topic, y=Estimate, fill=Significance)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=Estimate - Std.Error, ymax=Estimate + Std.Error), 
                position=position_dodge(width=0.9), width=0.25) +
  geom_text(aes(label=sprintf("%.3f", Estimate)), vjust=-0.5, position=position_dodge(width=0.9)) +
  geom_text(aes(label=sprintf("p=%.3f", p.value)), vjust=1.5, position=position_dodge(width=0.9)) +
  labs(title="Effect of 'kim' on Each Topic",
       x="Topic",
       y="Estimate") +
  scale_fill_manual(values=c(
    "Very Significant (***)" = "darkred",
    "Significant (**)" = "red",
    "Marginally Significant (*)" = "lightcoral",
    "Borderline Significant (.)" = "lightpink",
    "Not Significant" = "lightgray"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom" # 범례 아래로 이동
  )