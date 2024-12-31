##### 패키지 로드 #####
library(tm)
library(tidyverse)
library(pdftools)
library(stringr)
library(udpipe)       # 품사 태깅을 위해 사용
library(tidytext)
library(ggplot2)

# 경로 설정
pdf_path <- "C:/Users/dlsdn/Desktop/paper/segment"

# 파일 목록 불러오기
pdf_files <- list.files(pdf_path, pattern = "\\.pdf$", full.names = TRUE)

# 텍스트 추출 및 문장 단위로 나누기
extract_sentences_from_pdf <- function(file) {
  text <- pdf_text(file) %>%
    paste(collapse = " ") %>%
    tolower() %>%                    # 소문자 통합
    str_remove_all("[0-9]+") %>%     # 숫자 제거
    removeWords(stopwords("en"))     # 불용어 제거
  
  # 문장 단위 분할: 마침표(.)와 쉼표(,) 기준
  sentences <- unlist(strsplit(text, "(\\.|,)+")) %>%
    str_trim() %>%
    .[. != ""]
  return(sentences)
}

# 모든 파일에서 연도와 문장 단위 텍스트 추출
all_sentences <- lapply(pdf_files, extract_sentences_from_pdf)
years <- as.numeric(str_extract(basename(pdf_files), "^[0-9]+"))

# 문장 수에 맞게 연도 반복
year_list <- rep(years, sapply(all_sentences, length))

# 데이터 프레임 생성
pdf_data <- data.frame(
  year = year_list,
  sentences = unlist(all_sentences),
  stringsAsFactors = FALSE
)

# -------------------------------
# 품사 태깅 (형용사, 부사, 동사만 사용)
# -------------------------------
# 언어 모델 다운로드 및 로드
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# 품사 태깅 수행 (문장에 고유 ID 할당)
pdf_data$doc_id <- seq_len(nrow(pdf_data))  # 각 문장에 고유 번호 할당
tagged_data <- udpipe_annotate(ud_model, x = pdf_data$sentences, doc_id = pdf_data$doc_id)
tagged_df <- as.data.frame(tagged_data)

# 형용사(JJ), 부사(RB), 동사(VB)만 필터링
filtered_words <- tagged_df %>%
  filter(upos %in% c("ADJ", "ADV", "VERB")) %>%  # 형용사, 부사, 동사만 추출
  select(doc_id, lemma) %>%   # lemmatized 단어를 사용
  rename(word = lemma)

# 감성 사전 불러오기 (긍정: +1, 부정: -1, 중립: 0)
bing_lexicon <- get_sentiments("bing")

# 중립 문장 추가 (긍정, 부정에 해당하지 않는 경우)
neutral_words <- setdiff(unique(filtered_words$word), bing_lexicon$word)

# 중립적인 단어 처리: 중립인 단어에는 sentiment == "neutral"을 할당
neutral_sentiment <- data.frame(word = neutral_words, sentiment = "neutral")

# 감성 사전 업데이트: 긍정(1), 부정(-1), 중립(0)
updated_bing_lexicon <- bing_lexicon %>%
  bind_rows(neutral_sentiment) %>%
  mutate(sentiment_value = case_when(
    sentiment == "positive" ~ 1,
    sentiment == "negative" ~ -1,
    sentiment == "neutral" ~ 0
  ))

# 감성 분석 수행: 필터링된 단어와 감성 사전 결합
filtered_words$doc_id <- as.numeric(filtered_words$doc_id)  # doc_id를 숫자로 변환
sentiment_analysis <- filtered_words %>%
  inner_join(updated_bing_lexicon, by = "word") %>%
  left_join(pdf_data, by = c("doc_id" = "doc_id")) %>%  # 원본 데이터에서 year 추가
  group_by(year) %>%
  summarise(
    positive_count = sum(sentiment_value == 1),
    negative_count = sum(sentiment_value == -1),
    neutral_count = sum(sentiment_value == 0),
    net_sentiment = sum(sentiment_value)  # net_sentiment는 +1, -1, 0의 합계
  )


#----------------------------------------


# 수동으로 연도 목록 설정 (97, 98을 첫 번째로, 00 이후 순차적으로)
manual_years <- c(97, 98, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

# 강조할 연도 설정
highlight_years <- c(6, 9, 13, 14, 15, 16, 17, 19, 21, 22, 23, 24)

# 연도를 숫자형으로 처리하고 수동으로 연도 순서 조정
sentiment_analysis$year <- factor(sentiment_analysis$year, 
                                  levels = manual_years)


#------------------------------------------

# 연도별 감정 점수 시각화 (각 연도 연결하는 선)
ggplot(sentiment_analysis, aes(x = year, y = net_sentiment, group = 1)) +
  geom_line(color = "steelblue", size = 1) +  # 연도별 값을 잇는 선
  geom_point(color = "red", size = 2) +  # 데이터 포인트 강조
  # x축 표시 연도 설정
  scale_x_discrete(
    breaks = manual_years,  # 수동 설정된 연도
    labels = as.character(manual_years)  # 연도 레이블
  ) +
  # 강조할 연도 텍스트 추가
  geom_text(data = subset(sentiment_analysis, year %in% highlight_years), 
            aes(label = as.character(year)), 
            color = "red", size = 5, vjust = -0.5) +
  labs(
    title = "Sentiment Analysis",
    x = "Year",
    y = "Sentiment Score (AVA)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
