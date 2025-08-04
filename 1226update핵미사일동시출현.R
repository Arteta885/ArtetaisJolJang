# Packages
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

# Path to folder
folder_path <- "C:/Users/dlsdn/Desktop/paper/nuclear"

# List of PDF files
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# Initialize text collection from PDFs
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

# Define custom stemming function (ChatGPT-assisted)
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

# Apply custom stemming to text (ChatGPT-assisted)
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# Extract text from PDFs and apply stemming
for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  # Extract text
  text <- pdftools::pdf_text(pdf_file)
  
  # Convert to lowercase
  text <- tolower(text)
  
  # Apply custom stemming
  text <- apply_custom_stemming(text)
  
  # Basic preprocessing (excluding periods and commas)
  text <- gsub("[^a-zA-Z\\s\\.,]", " ", text)  # Remove special characters (excluding period and comma)
  text <- gsub("\\b\\d+\\b", " ", text)        # Remove numbers
  text <- removeWords(text, stopwords("en"))  # Remove English stopwords
  
  # Combine specific multi-word phrases into single tokens
  text <- gsub("c prk", "cprk", text)
  text <- gsub("per cent", "percent", text)
  text <- gsub("special rapporteur", "specialrapporteur", text)
  text <- gsub("para graph", "paragraph", text)
  text <- gsub("third committee", "thirdcommittee", text)
  text <- gsub("treaty bodies", "treatybodies", text)
  text <- gsub("independent experts", "independentexperts", text)
  
  # Remove high-frequency or irrelevant terms (updated 24-11-13)
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
  
  # Remove month names
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  # Split text into sentences
  sentences <- strsplit(text, "[.,]")[[1]]
  
  # Count words in each sentence
  word_counts <- sapply(sentences, function(sentence) length(strsplit(sentence, "\\s+")[[1]]))
  
  # Keep only sentences with 5 or more words
  selected_sentences <- sentences[word_counts >= 5]
  
  # Add selected sentences to the list
  all_text <- c(all_text, selected_sentences)  
}

########################### Preprocessing ############################

# Convert cleaned text into a tibble
text_df <- tibble(text = unlist(all_text))

text_df <- text_df %>% rownames_to_column(var = "rowid")

# Calculate pairwise word co-occurrence
pairwise_counts <- text_df %>%
  unnest_tokens(word, text) %>%
  pairwise_count(item = word, feature = rowid, sort = TRUE)

# Reorder word pairs and remove duplicates
pairwise_counts <- pairwise_counts %>%
  mutate(item1 = pmin(item1, item2),
         item2 = pmax(item1, item2)) %>%
  distinct(item1, item2, .keep_all = TRUE)

# Remove identical word pairs (self-loops) and ensure unique pairs
pairwise_counts <- pairwise_counts %>%
  mutate(pair = paste(pmin(item1, item2), pmax(item1, item2), sep = " ")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  filter(item1 != item2)  # Exclude identical word pairs

# Output result
print(pairwise_counts)

# Calculate node frequencies
node_frequency <- pairwise_counts %>%
  group_by(item1) %>%
  summarise(n = sum(n))

# Create network graph data
graph_pair <- pairwise_counts %>%
  filter(n >= 165) %>%
  as_tbl_graph(directed = FALSE) %>%
  activate(nodes) %>%
  left_join(node_frequency, by = c("name" = "item1"))

# Visualization (without legend, highlight frequent pairs, adjust edge width)
set.seed(9710324)
ggraph(graph_pair, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE) +  # Adjust edge transparency and width
  scale_edge_alpha(range = c(0.3, 1)) +  # Transparency based on frequency
  scale_edge_width(range = c(1, 3)) +    # Edge width based on frequency
  geom_node_point(aes(size = n, color = n), show.legend = FALSE) +  # Node size and color without legend
  scale_size(range = c(3, 15), guide = "none") +  # Scale node size
  scale_color_gradient(low = "yellow", high = "red") +  # Color gradient
  geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface = "bold") +  # Node label styling
  labs(title = "Pairwise Co-occurrence Network Nuclear/Missile (n>=165)") +  # Title only
  theme_void()  # Empty theme

