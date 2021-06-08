library(jsonlite)
library(stringi)
library(tidyverse)
library(quanteda)
library(readtext)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## the stanford corpus
df <- readtext('../output/stanfordEnc/txt/*.json', text_field = "body.text", verbosity = 3)
df <- corpus(df)
docvars(df) <- mutate(docvars(df), doc = docnames(df))
df <- corpus_subset(df, !duplicated(docvars(df)$url))
docvars(df) <- mutate(docvars(df), doc = gsub('\\.[0-9]', '', doc))

## the university metadata
path <- "../output/philpeople"
files <- dir(path, pattern = "*.json")

meta <- files %>%
  map_df(~mutate(fromJSON(file.path(path, .), flatten = TRUE), doc = .))
sort(table(meta$geo.type))
length(unique(meta$doc))
meta <- 
  meta %>% 
  group_by(doc) %>% 
  filter(geo.type%in%c('university', 'college', 'community_centre', 'dormitory') | (geo.type%in%c('station', 'information', 'residential', 'bus_stop', 'tram_stop', 'administrative') & grepl('university', tolower(uni))), .preserve = T) %>% 
  slice(1, .preserve = T)

## join
table(meta$doc%in%df$doc)
docvars(df) <- left_join(docvars(df), meta, by='doc')
docvars(df) <- mutate(docvars(df), lat = as.numeric(lat), lon = as.numeric(lon))
sfe <- df
sfe <- as.data.frame(sfe)

save(sfe, file = '../output/stanfordEnc/fin/stanford-encyclopedia.RDS')
