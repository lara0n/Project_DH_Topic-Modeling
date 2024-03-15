library(gutenbergr)
library(dplyr)

gb_ids <- c(13439, 1905, 30274, 503, 36132, 29725, 6629, 19337, 701, 1860, 
            19033, 17772, 708, 15569, 1904, 902, 3006, 16891, 37106, 1948, 2770,
            964, 5314, 31820, 12116, 28847, 33697)

all_book_paragraphs_df <- lapply(gb_ids, function(gb_id){
  
  book <- gutenberg_download(gb_id, meta_fields = c("title", "author"), 
                             strip = TRUE)
  title <- unique(book$title)
  author <- unique(book$author)
 
  book_paragraphs <- book %>%
    mutate(paragraph_ID = cumsum(text == "")) %>%
    filter(text != "") %>%
    group_by(paragraph_ID) %>%
    summarize(paragraph = paste(text, collapse = " ")) %>%
    ungroup() %>%
    mutate(author = author[1], title = title[1], gb_id = gb_id)
  
 
   return(book_paragraphs)
})

all_book_paragraphs_df <- bind_rows(all_book_paragraphs_df)

print(all_book_paragraphs_df)

write.csv(all_book_paragraphs_df, file = "books_in_paragraphs.csv", 
          row.names = FALSE)


