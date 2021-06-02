library(rvest)
library(jsonlite)
library(stringi)
library(tidyverse)
library(quanteda)
library(readtext)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

refpage <- read_html('https://plato.stanford.edu/contents.html')
hrefs <- refpage %>% html_nodes('div#content ul li a') %>% html_attr('href')
hrefs <- hrefs[grepl('entries', hrefs)]
hrefs <- paste0('https://plato.stanford.edu/', hrefs)

for(i in hrefs){
  #i = hrefs[2]
  cat(i)
  title <- stri_extract(i, regex = '(?<=entries\\/).*[^/]')
  page <- read_html(i)
  preamble <- page %>% html_node('div#preamble') %>% html_text()
  bibliography <- page %>% html_nodes('div#bibliography ul li') %>% sapply(., html_text)
  #ody.titles <- page %>% html_nodes('div#main-text h2, div#main-text h3') %>% html_text()
  body.text <- page %>% html_nodes('div#main-text p') %>% html_text() %>% paste0(., collapse = ' ')
  body.text <- paste0(preamble, body.text, collapse = ' ')
  pubinfo <- page %>% html_nodes('div#pubinfo') %>%  html_text()
  pubinfo <- stri_extract_all(pubinfo, regex = '\\b[A-z]{3}\\b\\s[0-9]+\\,\\s[0-9]+') %>% unlist
  pubinfo <- as.Date(pubinfo, format = '%b %d, %Y')
  authorship <- page %>% html_node('div#article-copyright') %>% html_text()
  authorship <- unlist(str_split(authorship, '\\n'))
  authorship <- gsub('\\s+', ' ', authorship)
  authorship <- authorship[grepl('[A-z]', authorship)]
  author <- authorship[!grepl('Copyright|@', authorship)]
  contact.mail <- stri_extract(authorship[grepl('@', authorship)], regex='(?<=<).*(?=>)')
  if(identical(character(0), contact.mail)) contact.mail <- NA
  contact.domain <- stri_extract(contact.mail, regex='(?<=@).*')
  if(identical(character(0), contact.domain)) contact.domain <- NA
  author.info <- page %>% html_nodes('div#article-copyright a[target="other"]') %>% html_attr('href')
  related <- page %>% html_nodes('div#related-entries p a') %>% html_attr('href')
  related <- gsub('\\.\\.\\/', 'https://plato.stanford.edu/entries/', related)
  df <- data.frame(
    date.published = pubinfo[1],
    date.changed = pubinfo[2],
    url = i,
    author1 = author[1],
    author2 = author[2],
    author1.info = author.info[1],
    author2.info = author.info[2],
    entry = title,
    body.text,
    contact.mail,
    contact.domain
  )
  bib <- data.frame(bibliography)
  rel <- data.frame(related)
  outMain <- paste0('../output/stanfordEnc/txt/', title, '.json')
  outBib <- paste0('../output/stanfordEnc/bib/', title, '.json')
  outRel <- paste0('../output/stanfordEnc/rel/', title, '.json')
  write_json(df, outMain, auto_unbox = T)
  write_json(bib, outBib, auto_unbox = T)
  write_json(rel, outRel, auto_unbox = T)
}


df <- readtext('../output/stanfordEnc/txt/*.json', text_field = "body.text", verbosity = 3)
df <- corpus(df)
summary(df)
