install.packages("tidyverse")
library(tidyverse)
library(quanteda)
library(readtext)
library(readr)
library(purr)
library(quanteda.textplots)
library(quanteda.textstats)

files = list.files(pattern="* .csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read_csv(x)))

tweet=distinct(myfiles, text, .keep_all = TRUE)
corpustweet=corpus(tweet$text, docvars = tweet %>% 
                     select(created_at, screen_name))
summary(corpustweet,1)


kwic(corpustweet,pattern= "negr*")

stp=stopwords(language = "it")
tk=tokens(corpustweet,
          remove_punct = TRUE, 
          remove_numbers = TRUE,
          remove_symbols = TRUE,
          remove_url = TRUE,
          padding = TRUE) %>% 
  tokens_remove(stp,padding = TRUE)

coll <- textstat_collocations(tk, tolower = FALSE, size = 3)

corpdfm <- dfm(tk)

featnames(corpdfm)
textstat_frequency(corpdfm, 
                   n = 1, groups = screen_name)
summary(corpus_subset(corpustweet, names(corpustweet) =="text21438"))


