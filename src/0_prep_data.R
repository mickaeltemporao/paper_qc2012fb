#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Preprocess Data
# Filename:     0_prep_data.R
# Description:  Preprocess Data for QC 2012 FB Analysis
# Version:      0.0.0.000
# Created:      2016-04-20 15:29:26
# Modified:     2016-05-10 13:02:35
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(dplyr)

# Functions --------------------------------------------------------------------
unaccent <- function(text) {
#YD Unaccent function
  text <- gsub("['`^~\"]", "  ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# Loading Yannick Preprocessed Data frame --------------------------------------
db <- data.table::fread("data/2012_fb_yd.csv", na.strings = "None") %>%
  dplyr::filter(!is.na(message)) %>% select(-V1, -fid) %>%
  as.data.frame()
names(db) <- c('primary_post', 'username', 'from_username', 'message','likes','comments','picture', 'link', 'from_name')
db$message <- unaccent(db$message)

# Trying to find date variable in the other 200k posts...
# filenames <- list.files(path='~/dropbox/grcp/web/aspira/elections_qc_2012_fb/', full.names=TRUE)[1:3]
# datalist <- lapply(filenames, read.csv, header=T, stringsAsFactors=F)
# db_full <- rbindlist(datalist,fill=T) %>% select(fid, created_time)
# names(db_full) <- c('id','created')
# match(db$id, db_full$id)

# Loading dictionaries & ignored features --------------------------------------
# Loads yd_features; mt_features; dict_issues_fr
source('src/dictionaries_features.R')

## Lexicoder French Dictionary
dict_fr <- dictionary(file = "frlsd.cat", format = "wordstat")

# Generating the FB posts 2012 Corpus
qc_corpus <- corpus(db, textField = "message")
summary(qc_corpus, 5)

## Test for word cloud
wc_dfm <- dfm(qc_corpus, ignoredFeatures = c(stopwords("french"), stopwords("english"),
  yd_features, mt_features), stem = F, language = "french")

topfeatures(wc_dfm, 20)

# Test plot
# pdf("../../Figures/2012_qc_wc.pdf")
# if (require(RColorBrewer)) plot(wc_dfm, max.words = 100, colors = brewer.pal(6, "Dark2"))
# dev.off()

# Lexicode French Sentiment analysis
qc_dfm <- dfm(
  qc_corpus,
  dictionary = c(dict_fr, dict_issues_fr), # other dictionaries can be added ex. dict_parties
  # ignoredFeatures = c(stopwords("french"),
  #                     stopwords("english"),
  #                     yd_features,
  #                     mt_features),
  stem = F, language = "french"
  )

topfeatures(qc_dfm, 27)
df_sent <- as.data.frame(qc_dfm)

db <- db %>% bind_cols(df_sent)

write.csv(db, 'data/2012_qc_lexicoder.csv')
