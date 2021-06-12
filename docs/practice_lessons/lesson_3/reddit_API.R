library(tidyverse)
library(jsonlite)
library(stringi)
library(quanteda)

rm(list=ls())

## simple comment-query on reddit
#################################
## formulate query
search.term <- 'phenomonology'
query <- paste0('https://api.pushshift.io/reddit/search/comment/?q=', search.term)
query
response <- fromJSON(query, flatten = TRUE)
?str
str(response, max.level = 2)
## extract data
df <- response$data
View(df)
## have a look at the texts
cat(df$body)

## AND: phenomenology AND Hegel
################################
search.term <- 'phenomonology+hegel'
query <- paste0('https://api.pushshift.io/reddit/search/comment/?q=', search.term)
query
response <- fromJSON(query, flatten = TRUE)
df <- response$data
table(grepl('(hegel.*phenomonology)|(phenomonology.*hegel)', tolower(df$body)))

## OR: phenomenology OR logicism
################################
search.term <- 'phenomonology|logicism'
query <- paste0('https://api.pushshift.io/reddit/search/comment/?q=', search.term)
query
response <- fromJSON(query, flatten = TRUE)
df <- response$data
table(grepl('phenomonology|logicism', tolower(df$body)))

## Phrases: "constructive criticism"
###################################
search.term <- '%22constructive%20criticism%22'
query <- paste0('https://api.pushshift.io/reddit/search/comment/?q=', search.term)
query
response <- fromJSON(query, flatten = TRUE)
df <- response$data
df

## all possible parameters here: https://reddit-api.readthedocs.io/en/latest/#using-the-https-api-pushshift-io-endpoints


#######################################
## From the query to a corpus object ##
#######################################
search.terms <- c('"awful holiday"', 'basejumping', 'supervenience')
search.terms
search.terms.coll <- paste0(search.terms, collapse = '|')
search.terms.coll
## replace space with its URL-encoding
query_tail <- gsub('\\s', '%20', search.terms.coll)
query_tail
## replace quote with URL-encoding
query_tail <- gsub('\\"', '%22', query_tail)
query_tail
## query
query <- paste0('https://api.pushshift.io/reddit/search/comment/?q=', query_tail)
response <- fromJSON(query, flatten = TRUE)
## response is a list containing vectors and lists_
str(response, max.level = 2)
## so:
## the response is a list and must be unpacked
## some of the columns in $data are list themselves and must be dealt with
response <- response$data # unpack $data
sapply(response, is.list) # check which columns are lists
## remove list elements
listless <- response[!sapply(response, is.list)]
listless <- as_tibble(listless)
listless
## rename 
df <- listless
## check out the text data
head(df$body)
## annotate which of your search term actually occurs in each response
## coerce text data to lower case
df$body <- tolower(df$body)
## create regex
search.terms.coll
search.regex <- gsub('\\"', '', search.terms.coll) # remove quotes that were needed for API query
search.regex
## extract matches
regmatch <- stri_extract_all(df$body, regex = search.regex)
regmatch # you might also have NAs
## inspect them
df$body[sapply(regmatch, is.na)] # I think this one we can safely drop
## now we join the match-annotation to the data
## check that the match-vector is same length as the number of rows in the data
if(nrow(df) == length(unlist(regmatch))){
  ## IF TRUE:
  df$rmatch <- unlist(regmatch)
}else{
  ## IF FALSE:
  #regmatch[[1]] <- c(regmatch[[1]], 'falsepositive')
  #nrow(df) == length(unlist(regmatch)) # now this turns out false
  head(regmatch)
  lengths(regmatch)
  # data inflation
  df <- df[rep(1:nrow(df), times = lengths(regmatch)), ]
  df$rmatch <- unlist(regmatch)
}
## filter out data without matches
#df <- filter(df, !rmatch == 'falsepositive')
df <- filter(df, !is.na(rmatch))

### TADAAAAA now you have your data
## check how many times each search word occurs
table(df$rmatch)
## create quanteda-corpus
corp <- corpus(df, text_field = 'body', docid_field = 'id')
corp
## subset documents for supervenience
docvars(corp)
sup <- corpus_subset(corp, rmatch == 'supervenience')
sup


# https://api.pushshift.io/reddit/comment/search/?q=universe&after=1620848893&before=24h