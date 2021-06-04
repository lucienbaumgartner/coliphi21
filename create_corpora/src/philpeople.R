library(rvest)
library(jsonlite)
library(stringi)
library(tidyverse)
library(quanteda)
library(readtext)
library(RecordLinkage)
library(foreach)
library(doParallel)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- readtext('../output/stanfordEnc/txt/*.json', text_field = "body.text", verbosity = 3)
df <- corpus(df)
df <- corpus_subset(df, !duplicated(docvars(df)$url))
nms <- docvars(df)$author1
nms <- gsub('\\s', '-', tolower(nms))
urls <- paste0('https://philpeople.org/profiles/', nms)
nms <- gsub('\\-', '+', nms)
uncert <- paste0('https://philpeople.org/find-philosopher/search?utf8=%E2%9C%93&include_nonpro=true&keywords=', nms)

iterator <- c(1:length(urls))[!list.files('../output/stanfordEnc/txt/') %in% list.files('../output/philpeople/')]


registerDoParallel(cl <- makeCluster(4))

#res <- foreach(i = 1:length(urls), .packages = .packages()) %dopar% {
for(i in iterator){
  cat(paste0(i, ' '))
  philpage <- urls[i]
  dfile <- docnames(df)[i]
  philsearch <- uncert[i]
  author <- docvars(df)$author1[i]
  
  page <- try(read_html(philpage))
  if(!'try-error'%in%class(page)){
    uni <- page %>% html_node('div.profile-header__text--secondary a')
    if(!is.na(uni)){
      uni <- data.frame(uni = html_text(uni), 
                        uni.url = paste0('https://philpeople.org', html_attr(uni, 'href')))
      loc.query <- paste0('https://nominatim.openstreetmap.org/search?q=',
                          gsub('\\s', '+', gsub('\\(.*\\)', '', tolower(uni$uni))),
                          '&format=json')
      loc <- jsonlite::fromJSON(loc.query)
      if(!identical(loc, list())){
        if(nrow(loc)>1) loc <- filter(loc, type%in%c('college', 'university', 'community_centre', 'dormitory'))
        if(!nrow(loc)==0){
          loc <- select(loc, place_id, osm_id, lat, lon, display_name, importance, type)
          loc <- rename(loc, uni.place_id = place_id, uni.osm_id = osm_id, uni.address = display_name, uni.importance_osm = importance, geo.type = type)
          info <- cbind(uni, loc)
        }else{
          info <- uni
        }
      }else{
        info <- uni
      }
      
      psearch <- read_html(philsearch)
      hits <- psearch %>% html_nodes('div#results div.profile-name') %>% html_attr('title')
      if(!identical(hits, character(0))){
        sim.prob <- levenshteinSim(author, hits)
        certainty <- length(sim.prob[sim.prob>.7])/length(sim.prob)
        if(!certainty==1) certainty <- 1-certainty
      }else{
        certainty <- NA
      }
      info <- cbind(info, certainty)
    }else{
      info <- data.frame(uni = NA)
    }
  }else{
    info <- data.frame(uni = NA)
  }
  
  outpp <- paste0('../output/philpeople/', gsub('(\\.[0-9])?\\.json', '', dfile), '.json')
  write_json(info, outpp, auto_unbox = T)
}

#stopCluster(cl)
