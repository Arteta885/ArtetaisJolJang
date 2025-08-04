# packages
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

# folder_path
folder_path <- "C:/Users/dlsdn/Desktop/paper/peace"

# pdf_files
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# TEXT collection : all_text
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

# Define function for custom stemming
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# Extracting text from file...PDF
for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  # Read text
  text <- pdftools::pdf_text(pdf_file)
  
  # Convert to lowercase
  text <- tolower(text)
  
  # Apply custom stemming
  text <- apply_custom_stemming(text)
  
  # Preprocessing
  text <- gsub("[^a-zA-Z\\s\\.,]", " ", text)  # Remove special characters
  text <- gsub("\\b\\d+\\b", " ", text)        # Remove numbers
  text <- removeWords(text, stopwords("en"))  # Remove common stopwords
  
  # Standardize specific expressions
  text <- gsub("c prk", "cprk", text)
  text <- gsub("per cent", "percent", text)
  text <- gsub("special rapporteur", "specialrapporteur", text)
  text <- gsub("para graph", "paragraph", text)
  text <- gsub("third committee", "thirdcommittee", text)
  text <- gsub("treaty bodies", "treatybodies", text)
  text <- gsub("independent experts", "independentexperts", text)
  
  # Remove high-frequency irrelevant terms
  remove_words <- c("democratic", "people", "s", "republic", "korea",
                    "korean", "north", "human", "rights", "right", "dprk", "united",
                    "nations", "commissioner", "resolution", "situation", "also",
                    "high", "draft", "paragraph", "council", "international",
                    "government", "para", "mr", "ms", "mrs", "reports", "e", "commission",
                    "said", "assembly", "committee", "general", "p", "http", "php",
                    "specialrapporteur", "independentexperts", "WWW.", "dailynk", ".com",
                    "cprk", "percent", "per", "including", "one", "english", "hrc", "ohchr",
                    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "n", "m", "o", "p",
                    "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "www.theelders.org", "res", "spa",
                    "organ", "country")
  text <- removeWords(text, remove_words)
  
  # Remove month names
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  # Split text into sentences
  sentences <- strsplit(text, "[.,]")[[1]]
  
  # Count words per sentence
  word_counts <- sapply(sentences, function(sentence) length(strsplit(sentence, "\\s+")[[1]]))
  
  # Select sentences with at least 5 words
  selected_sentences <- sentences[word_counts >= 5]
  
  # Append to the main text list
  all_text <- c(all_text, selected_sentences)  
}

#######################################################
#######################################################

# Convert text into data_frame (tibble)
text_df <- tibble(text = unlist(all_text))

text_df <- text_df %>% rownames_to_column(var = "rowid")

# Calculate pairwise co-occurrence counts
pairwise_counts <- text_df %>%
  unnest_tokens(word, text) %>%
  pairwise_count(item = word, feature = rowid, sort = TRUE)

# Standardize pairwise ordering and remove duplicates
pairwise_counts <- pairwise_counts %>%
  mutate(item1 = pmin(item1, item2),
         item2 = pmax(item1, item2)) %>%
  distinct(item1, item2, .keep_all = TRUE)

# Remove self-pairs and final duplicates
pairwise_counts <- pairwise_counts %>%
  mutate(pair = paste(pmin(item1, item2), pmax(item1, item2), sep = " ")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  filter(item1 != item2)  # Remove self-loops

# Print co-occurrence result
print(pairwise_counts)

# Calculate node frequency
node_frequency <- pairwise_counts %>%
  group_by(item1) %>%
  summarise(n = sum(n))

# Construct co-occurrence network graph
graph_pair <- pairwise_counts %>%
  filter(n >= 75) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  left_join(node_frequency, by = c("name" = "item1")) %>%
  filter(!is.na(name))  # Remove NA values

# Plot the network
set.seed(970324)
ggraph(graph_pair, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE) +  # Adjust edge transparency and thickness
  scale_edge_alpha(range = c(0.3, 1)) +  # Edge transparency range by frequency
  scale_edge_width(range = c(1, 3)) +    # Edge thickness range by frequency
  geom_node_point(aes(size = n, color = n), show.legend = FALSE) +  # Node size and color by frequency
  scale_size(range = c(3, 15), guide = "none") +  # Node size scale
  scale_color_gradient(low = "yellow", high = "red") +  # Node color gradient
  geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface = "bold") +  # Label size and style
  labs(title = "Pairwise Co-occurrence Network (Moratorium, n>=75)") +  # Add title only
  theme_void()  # Use void theme


##############################################################
