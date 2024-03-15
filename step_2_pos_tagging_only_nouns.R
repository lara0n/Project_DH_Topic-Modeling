options(stringsAsFactors = FALSE)

library(dplyr)
library(stringr)
library(udpipe)

books_data <- read.csv("books_in_paragraphs.csv", sep = ",", encoding = "UTF-8")

m_eng <- udpipe::udpipe_download_model(language = "english-ewt")

m_eng <- udpipe_load_model(file = m_eng$file_model)

tagged_paragraphs <- udpipe::udpipe_annotate(m_eng, x = books_data$paragraph)

tagged_pargraphs <- as.data.frame(tagged_paragraphs)

doc_ids <- data.frame(doc_id = unique(tagged_pargraphs$doc_id))

noun_rows <- tagged_pargraphs %>%
  filter(upos %in% c("NOUN"))

nouns_each_paragraph <- noun_rows %>% group_by(doc_id) %>% 
  summarise(paragraphs_lemma = str_c(lemma, collapse = " "))

nouns_each_paragraph <- left_join(doc_ids, nouns_each_paragraph, by = "doc_id")

paragraphs_lowercase <- tolower(nouns_each_paragraph$paragraphs_lemma)

paragraphs_no_sc <- str_replace_all(paragraphs_lowercase, "[[:punct:]]", " ")
  
paragraphs_no_sc_and_num <- str_replace_all(paragraphs_no_sc, "\\d", "")

paragraphs_cleaned <- str_squish(paragraphs_no_sc_and_num)

books_data_new <- books_data %>% 
  mutate(paragraphs_cleaned = paragraphs_cleaned)

books_data_new <- books_data_new[,-c(1,2)]

books_data_new <- books_data_new %>%
  filter(paragraphs_cleaned != "") %>% 
  mutate(paragraph_ID = row_number())

write.csv(books_data_new, file = "books_noun_lemmas.csv", row.names = FALSE)

        