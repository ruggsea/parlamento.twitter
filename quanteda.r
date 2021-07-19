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
tk=tokens(corpustweet) %>% 
  tokens_remove(stp,padding = TRUE)

coll <- textstat_collocations(tk, tolower = FALSE, size = 3)


summary(corpus_subset(corpustweet, names(corpustweet) =="text21438"))


