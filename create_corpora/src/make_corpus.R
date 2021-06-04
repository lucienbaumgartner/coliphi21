library(jsonlite)
library(stringi)
library(tidyverse)
library(quanteda)
library(readtext)
library(pbmcapply)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## the stanford corpus
df <- readtext('../output/stanfordEnc/txt/*.json', text_field = "body.text", verbosity = 3, encoding = 'utf8')
#df <- readtext('../output/stanfordEnc/txt/18thGerman-preKant.json', text_field = "body.text", verbosity = 3, encoding = 'ascii')
df$text <- unlist(pbmclapply(df$text, function(x) gsub('\\\\\\((.*?)\\\\\\)|\\\\\\[(.*?)\\\\\\]', '', x), mc.cores = 4))
df$text <- unlist(pbmclapply(df$text, function(x) gsub("((?<=[A-z])\\’s)|“|”|\\'|‘|’|(e\\.g\\.)|(i\\.e\\.)", '', x, perl = T), mc.cores = 4))
#df$text
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
#meta$doc[!meta$doc%in%df$doc]
docvars(df) <- left_join(docvars(df), meta, by='doc')
docvars(df) <- mutate(docvars(df), lat = as.numeric(lat), lon = as.numeric(lon))
sfe <- df

## remove latex stuff
#colnames(sfe)
#p <- sfe[['logic-algebraic-propositional.json']]
#gsub('\\\\\\((.*?)\\\\\\)|\\\\\\[(.*?)\\\\\\]', '', p)

save(sfe, file = '../output/stanfordEnc/fin/stanford-encyclopedia.RDS')
