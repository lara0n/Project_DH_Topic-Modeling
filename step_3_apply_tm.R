options(stringsAsFactors = FALSE)
library(quanteda)
library(quanteda.textstats)
library(topicmodels)

booksdata <- read.csv("books_noun_lemmas.csv", sep = ",", encoding = "UTF-8")

gb_id_ordered_by_year <- c(13439,1905,503,30274,36132,5314,29725,31820,6629,
                           16891,19337,12116,701,1860,19033,28847,37106,17772,
                           1948,708,33697,15569,2770,964,902,1904,3006)

booksdata$gb_id <- factor(booksdata$gb_id, levels = gb_id_ordered_by_year)

booksdata <- booksdata[order(booksdata$gb_id), ]

paragraphs_corpus <- corpus(booksdata$paragraphs_cleaned, 
                            docnames = booksdata$X)

corpus_tokens <- paragraphs_corpus %>% tokens()

paragraphs_collocations <- textstat_collocations(corpus_tokens, min_count = 30)

paragraphs_collocations <- paragraphs_collocations[1:250, ]

corpus_tokens <- tokens_compound(corpus_tokens, paragraphs_collocations)

DFM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 5)

term_frequency <- textstat_frequency(DFM, n = 50)

print(term_frequency$feature)

DFM <- DFM[, !(colnames(DFM) %in% term_frequency$feature)]

sel_idx <- rowSums(DFM) > 0

DFM <- DFM[sel_idx, ]

booksdata <- booksdata[sel_idx,]

K <- 20

topicModel <- LDA(DFM, K, method="Gibbs", control=list(
  iter = 700,
  seed = 1,
  verbose = 50,
  alpha = 0.02), 
  group = booksdata$author)


tmResult <- posterior(topicModel)

attributes(tmResult)

beta <- tmResult$terms

theta <- tmResult$topics

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), 
                   mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)

library(reshape2)
library(ggplot2)

booksdata$author <- factor(booksdata$author, levels = unique(booksdata$author))

topic_proportion_per_author <- aggregate(theta,
                                         by = list(author = booksdata$author), 
                                         mean)

colnames(topic_proportion_per_author)[2:(K+1)] <- topicNames

vizDataFrame <- melt(topic_proportion_per_author, id.vars = "author")

ggplot(vizDataFrame,
       aes(x=author, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(25), "FF"), name = "topics") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

