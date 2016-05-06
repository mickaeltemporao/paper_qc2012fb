#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     Lexicoder.R
# Description:  Lexicoder script for QC 2012 Elections
# Version:      0.0.0.000
# Created:      2016-04-20 15:29:26
# Modified:     2016-05-05 14:40:28
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

library(dplyr)
library(quanteda)

db <- data.table::fread("../Data/Elections_Quebec_2012_FBPosts.csv", na.strings = "None") %>%
    dplyr::filter(!is.na(message)) %>% dplyr::select(1:2, likes_count:shares_count) %>%
    as.data.frame()

# Loading dictionaries & ignored features --------------------------------------

source('src/dictionaries_features.R')
# Loads yd_features; mt_features; dict_issues_fr

## Lexicoder French Dictionary
dict_fr <- dictionary(file = "frlsd.cat", format = "wordstat")

# Generating the FB posts 2012 Corpus
qc_corpus <- corpus(db, textField = "message")

summary(qc_corpus, 5)

## Test for word cloud
qc_dfm <- dfm(qc_corpus, ignoredFeatures = c(stopwords("french"), stopwords("english"),
  yd_features, mt_features), stem = F, language = "french")

topfeatures(qc_dfm, 20)

# Test plot
# pdf("../../Figures/2012_qc_wc.pdf")
# if (require(RColorBrewer)) plot(qc_dfm, max.words = 100, colors = brewer.pal(6, "Dark2"))
# dev.off()

# Lexicode French Sentiment analysis
sentiment_qc_dfm <- dfm(
  qc_corpus,
  dictionary = c(dict_fr, dict_issues_fr), # other dictionaries can be added ex. dict_parties
  ignoredFeatures = c(stopwords("french"),
                      stopwords("english"),
                      yd_features,
                      mt_features),
  stem = F, language = "french"
  )

df_sent <- as.data.frame(sentiment_qc_dfm)

base_df <- data.table::fread("../../Data/Elections_Quebec_2012_FBPosts.csv", na.strings = "None") %>%
    dplyr::filter(!is.na(message)) %>%
    as.data.frame() %>% bind_cols(df_sent)

write.csv(base_df, '../../Data/Quebec_2012_lexicoder_results.csv')
