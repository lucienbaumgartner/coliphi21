library(rvest)
library(tidyverse)
library(quanteda)
library(readtext)
library(httr)
library(pdftools)

rm(list=ls())
setwd('~/Desktop/')

## get a user agent here: https://www.whatismybrowser.com/guides/the-latest-user-agent/chrome
useragent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.101 Safari/537.36"
## override default user agent
httr::user_agent(useragent)

## downloading pdfs from google scholar
#######################################
## URL from which you want the pdfs
URL <- "https://scholar.google.com/scholar?hl=en&q=normative+generics"
## parse HTML
page <- read_html(URL)
## extract a-elements with 'pdf' in their link
pdf_nodes <- html_nodes(page, xpath='//a[contains(@href, "pdf")]')
pdf_nodes
## mmh but we have more pdfs available..
## we will have to be less specific, because some links apparently don't contain the word 'pdf'
pdf_nodes_2 <- html_nodes(page, xpath='//a[@data-clk and @data-clk-atid and not(@id)]')
pdf_nodes_2
## better!!
## extract actual links
links <- html_attr(pdf_nodes_2, 'href') 
links
for(i in links){
  i
}
## download a file
?download.file
download.file(links[1], destfile = 'testfile.pdf')
## make corpus
txt <- readtext('testfile.pdf')
txt
df <- corpus(txt)
df
## add metadata
pdftxt <- pdf_text('testfile.pdf')
as_tibble(pdftxt)
pdfmeta <- pdf_info('testfile.pdf')
pdfmeta
metadata <- tibble(title = pdfmeta$keys$Title, author = pdfmeta$keys$Author, 
                   created = pdfmeta$created, modified = pdfmeta$modified)
metadata
## corpus has no metadata
docvars(df)
## add metadata
docvars(df) <- metadata
df
docvars(df)
