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
folder_path <- "C:/Users/dlsdn/Desktop/paper/nuclear"

# PDF파일목록(pdf_files)
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# PDF에서 TEXT 추출(이름지정:all_text)
all_text <- list()

##### Custom stemming list #####
stem_rules <- list(
  "leader" = "kim",
  "leadership" = "kim",
  "supreme" = "kim",
  "escapees" = "refugees",
  "escapee" = "refugees",
  "refugee" = "refugees",
  "report" = "reports",
  "woman" = "women",
  "female" = "women",
  "child" = "children",
  "girl" = "women",
  "girls" = "women",
  "disability" = "disabl",
  "disabilities" = "disabl",
  "camp" = "camps",
  "disappearance" = "disappearances")

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
  
  # 특수한 구문을 한 단어로 묶기 (문제X)
  text <- gsub("c prk", "cprk", text)
  text <- gsub("per cent", "percent", text)
  text <- gsub("special rapporteur", "specialrapporteur", text)
  text <- gsub("para graph", "paragraph", text)
  text <- gsub("third committee", "thirdcommittee", text)
  text <- gsub("treaty bodies", "treatybodies", text)
  text <- gsub("independent experts", "independentexperts", text)
  
  # 특정 단어 제거 (24 11 13 update 문제X)
  remove_words <- c(stopwords("en"), "democratic", "people", "s", "republic", "korea",
                    "korean", "north", "human", "rights", "right", "dprk", "united",
                    "nations", "commissioner", "resolution", "situation", "also",
                    "high", "draft", "paragraph", "council", "international",
                    "government", "para", "mr", "ms", "mrs", "reports", "e", "commission",
                    "said", "assembly", "committee", "general", "p", "http", "php",
                    "specialrapporteur", "independentexperts", "WWW.", "dailynk", ".com",
                    "cprk", "percent", "per", "including", "one", "english", "hrc", "ohchr",
                    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "n", "m", "o", "p",
                    "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "www.theelders.org", "res", "country",
                    "spa", "th")
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

###########################전 처 리############################

# 정제된 텍스트를 data_frame(tibble)로 변환
text_df <- tibble(text = unlist(all_text))

text_df <- text_df %>% rownames_to_column(var = "rowid")

# 단어 간의 동시 출현 빈도 계산
pairwise_counts <- text_df %>%
  unnest_tokens(word, text) %>%
  pairwise_count(item = word, feature = rowid, sort = TRUE)

pairwise_counts <- pairwise_counts %>%
  mutate(item1 = pmin(item1, item2),
         item2 = pmax(item1, item2)) %>%
  distinct(item1, item2, .keep_all = TRUE)

# 쌍의 순서를 바꾸어 중복을 제거하고 item1과 item2가 동일한 경우 제외
pairwise_counts <- pairwise_counts %>%
  mutate(pair = paste(pmin(item1, item2), pmax(item1, item2), sep = " ")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  filter(item1 != item2)  # item1과 item2가 동일한 경우 제외

# 결과 출력
print(pairwise_counts)

# 노드 빈도 계산
node_frequency <- pairwise_counts %>%
  group_by(item1) %>%
  summarise(n = sum(n))

# 그래프 데이터 생성
graph_pair <- pairwise_counts %>%
  filter(n >= 165) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  left_join(node_frequency, by = c("name" = "item1"))



# 시각화 (범례 삭제하고, 자주 등장하는 단어 쌍 강조, 엣지 두께 조정)
set.seed(9710324)
ggraph(graph_pair, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE) +  # 엣지 투명도 및 두께 조정
  scale_edge_alpha(range = c(0.3, 1)) +  # 엣지 투명도 범위 설정 (빈도에 따라)
  scale_edge_width(range = c(1, 3)) +  # 엣지 두께 범위 설정 (빈도에 따라)
  geom_node_point(aes(size = n, color = n), show.legend = FALSE) +  # 노드 크기 및 색상 적용, 범례 제거
  scale_size(range = c(3, 15), guide = "none") +  # 노드 크기 조정 (빈도에 따라)
  scale_color_gradient(low = "yellow", high = "red") +  # 색상 스케일 설정
  geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface = "bold") +  # 노드 텍스트 크기 및 굵기 조정
  labs(title = "Pairwise Co-occurrence Network Nuclear/Missile (n>=165)") +  # 범례 없이 제목만 표시
  theme_void()  # 테마 설정


##############################################################33

# 특정단어랑 사용많이되는 단어찾아보기
pairwise_counts %>% filter(item1 == "kim"|item2=="kim")

# kim이 포함된 문장 중 함께 사용되는 단어 추출
kim_co_occurrence <- pairwise_counts %>%
  filter(item1 == "kim" | item2 == "kim")

# kim과 함께 사용된 단어의 빈도를 카운트하여 데이터프레임으로 출력하고 내림차순으로 정렬
kim_word_counts <- kim_co_occurrence %>%
  group_by(word = ifelse(item1 == "kim", item2, item1)) %>%  # kim과 함께 사용된 단어를 word로 설정
  summarise(count = sum(n)) %>%
  arrange(desc(count))  # 내림차순으로 정렬

# 상위 20개의 단어 추출
top_20_words <- kim_word_counts %>%
  top_n(20) %>%
  arrange(desc(count))  # 내림차순으로 다시 정렬

# 막대그래프 생성
ggplot(top_20_words, aes(x = factor(word, levels = rev(top_20_words$word)), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "단어", y = "출현 빈도", title = "kim과 함께 많이 출현한 상위 20개의 단어") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# 네트워크 그래프 생성
kim_graph <- kim_co_occurrence %>%
  filter(item1 %in% top_20_words$word | item2 %in% top_20_words$word) %>%
  as_tbl_graph()

# 노드의 크기를 단어의 등장 빈도에 비례하여 조정
node_size <- kim_word_counts$count[match(V(kim_graph)$name, kim_word_counts$word)]
node_size <- ifelse(is.na(node_size), 0, node_size)  # NA 값을 0으로 대체
node_size <- node_size / max(node_size, na.rm = TRUE) * 30  # 최대 크기를 30으로 조정

# 그래프 시각화
set.seed(2849)
ggraph(kim_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "yellow", size = node_size) +  # 노드의 크기 조정
  geom_node_text(aes(label = name)) +
  theme_void()
