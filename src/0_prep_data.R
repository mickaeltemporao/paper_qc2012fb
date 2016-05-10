#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Preprocess Data
# Filename:     0_prep_data.R
# Description:  Preprocess Data for QC 2012 FB Analysis
# Version:      0.0.0.000
# Created:      2016-04-20 15:29:26
# Modified:     2016-05-10 15:57:54
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(dplyr)

path <- "../../../../../grcp/web/aspira/Elections_qc_2012_fb/Elections_Quebec_2012_FBPosts.csv"
posts <- read.csv(path, encoding='UTF-8', stringsAsFactors=F, na.strings="None", sep=',')

# Functions -------------------------------------------------------------------
rm_diacritics <- function(text) {
  text <- gsub("['`^~\"]", "  ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

mv_lowercase <- function(x){
  y <- NA
  try_error <- tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, "error"))
    y <- tolower(x)
  return(y)
}

# Preprocess ------------------------------------------------------------------

# Official Posts by Parties - Official=1; Non-Official=0
posts$official <- 0
posts$official[posts$user__username==posts$ffrom__username] <- 1

# Keep only required variables and posts on 6 majors FB Pages
posts <- posts %>%
  dplyr::filter(user__username %in% c("coalitionavenir", "LiberalQuebec", "OptionNationale.QC",
    "lepartiquebecois", "partivert", "Quebecsolidaire")) %>%
  dplyr::select(user__name, user__username, ffrom__name, ffrom__username, ffrom__gender, message,
    official, picture, link, ftype, likes_count, comments_count, shares_count) %>%
  dplyr::filter(message!="None") %>% rename(page=user__name, page_id=user__username,
    from=ffrom__name, from_id=ffrom__username, gender=ffrom__gender, type=ftype)
posts$message <- mv_lowercase(posts$message)
posts$message <- rm_diacritics(posts$message)

# Trying to find date variable in the other 200k posts...
# filenames <- list.files(path='~/dropbox/grcp/web/aspira/elections_qc_2012_fb/', full.names=TRUE)[1:3]
# datalist <- lapply(filenames, read.csv, header=T, stringsAsFactors=F)
# posts_full <- rbindlist(datalist,fill=T) %>% select(fid, created_time)
# names(posts_full) <- c('id','created')
# match(posts$id, posts_full$id)

# Loading dictionaries & ignored features --------------------------------------
# Loads yd_features; mt_features; dict_issues_fr
source('src/dictionaries_features.R')

## Lexicoder French Dictionary
dict_fr <- dictionary(file = "frlsd.cat", format = "wordstat")

# Generating the FB posts 2012 Corpus
qc_corpus <- corpus(posts, textField = "message")
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
  ignoredFeatures = c(stopwords("french"),
    stopwords("english"),
    yd_features,
    mt_features),
  stem = F, language = "french"
)

topfeatures(qc_dfm, 5)
df_sent <- as.data.frame(qc_dfm)

posts <- posts %>% bind_cols(df_sent)
write.csv(posts, 'data/2012_qc_fb_posts.csv')
