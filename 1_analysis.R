#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     1_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-10 13:01:26
# Modified:     2016-05-10 16:13:56
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

db <- read.csv('data/2012_qc_fb_posts.csv', stringsAsFactors=F) %>%
  group_by(from_id) %>% select(-X)

#1 Number of Posts (Official vs Non-Official) by Party
plot1 <- db %>% dplyr::count(page_id, official)
ggplot(plot1, aes(x=page_id, y=n, fill=official))+
  geom_bar(stat='identity') +
  scale_x_discrete(labels=c("coalitionavenir"="CAQ","lepartiquebecois"="PQ","LiberalQuebec"="PLQ",
    "OptionNationale.QC"="ON","partivert"="PVQ","Quebecsolidaire"="QS")) +
  scale_y_continuous("Nombre de posts")

