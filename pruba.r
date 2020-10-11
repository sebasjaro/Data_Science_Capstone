file_b <- file("final/en_US/en_US.blogs.txt","rb")
blogs <- readLines(file_b, encoding = "UTF-8", skipNul = TRUE)
close(file_b)

file_n <- file("final/en_US/en_US.news.txt","rb")
news <- readLines(file_n, encoding = "UTF-8", skipNul = TRUE)
close(file_n)

file_t <- file("final/en_US/en_US.twitter.txt","rb")
twitter <- readLines(file_t, encoding = "UTF-8", skipNul = TRUE)
close(file_t)

####### summary
library(stringi)
library(kableExtra)

# Files sizes
files <- c("final/en_US/en_US.blogs.txt","final/en_US/en_US.news.txt","final/en_US/en_US.twitter.txt")
file_names <- c("Blogs","News","Twitter")
file_sizes <- file.info(files)$size/1024^2

# Number of lines per file
n_lines <- c(length(blogs),length(news),length(twitter))
n_words <- c(sum(stri_count_words(blogs)), sum(stri_count_words(news)), sum(stri_count_words(twitter)))
summary_df <- data.frame(File = file_names, Size = file_sizes, Lines = n_lines, Words = n_words)
#kable(summary_df)
summary_df %>% 
        kbl() %>%
        kable_paper(bootstrap_options = "striped", full_width = F)

####### sampling
set.seed(31415)
percent <- .01
sample_blogs <- sample(blogs, length(blogs) * percent)
sample_news <- sample(news, length(news) * percent)
sample_twitter <- sample(twitter, length(twitter) * percent)
sample <- c(sample_blogs, sample_news, sample_twitter)
lines_sample <- length(sample)
words_sample <- sum(stri_count_words(sample))

####### corpus
library(tm)
library(SnowballC)
sample_corpus <- Corpus(VectorSource(sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
sample_corpus <- tm_map(sample_corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
sample_corpus <- tm_map(sample_corpus, toSpace, "@[^\\s]+")
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
sample_corpus <- tm_map(sample_corpus, content_transformer(removeURL)) 
sample_corpus <- tm_map(sample_corpus, removeNumbers)
sample_corpus <- tm_map(sample_corpus, tolower)
sample_corpus <- tm_map(sample_corpus, removePunctuation)
sample_corpus <- tm_map(sample_corpus, removeWords, stopwords("en"))
sample_corpus <- tm_map(sample_corpus, stripWhitespace)
sample_corpus <- tm_map(sample_corpus, stemDocument)

saveRDS(sample_corpus, file = "./sample_corpus.rdat")

library(wordcloud)
library(RColorBrewer)

