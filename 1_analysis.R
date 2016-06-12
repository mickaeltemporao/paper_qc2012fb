#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     1_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-10 13:01:26
# Modified:     2016-06-12 15:45:32
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

db <- read.csv('data/2012_qc_fb_posts.csv', stringsAsFactors=F) %>%
  mutate(sentiment= POSITIVE - NEGATIVE)
db[grep('topic', names(db))] <- ifelse(db[grep('topic', names(db))] !=0, 1, 0)

db[db=="coalitionavenir"] <- "CAQ"
db[db=="lepartiquebecois"] <- "PQ"
db[db=="LiberalQuebec"] <- "PLQ"
db[db=="OptionNationale.QC"] <- "ON"
db[db=="partivert"] <- "PVQ"
db[db=="Quebecsolidaire"] <- "QS"

#1 Number of Posts (Official vs Non-Official) by Party
plot1 <- db %>% count(page_id, primary)

ggplot(plot1, aes(x=reorder(page_id, -n), y=n, fill=factor(primary)))+
  geom_bar(stat='identity') +
  scale_fill_discrete('', labels=c('Secondary', 'Primary')) +
  ylab('Number of Posts') + xlab('')

ggsave(paste0('figs/plot1','.pdf'), width = 7, height = 7)

#2 Number of Posts by Category
plot2 <- db %>% select(page_id, contains('topic')) %>%
  group_by(page_id) %>% summarise_each(funs(sum)) %>%
  gather(topic, count, topic_economy:topic_partisan)

ggplot(plot2, aes(x=reorder(topic, count), y=count)) +
  geom_bar(stat='identity') +
  xlab('') + ylab('') +
  coord_flip()

ggsave(paste0('figs/plot2','.pdf'), width = 10, height = 6)

#3 Number of Posts by Category by Party
ggplot(plot2, aes(x=reorder(topic, count), y=count)) +
  geom_bar(stat='identity') +
  coord_flip() +
  xlab('') + ylab('') +
  facet_grid(.~page_id,)

ggsave(paste0('figs/plot3', '.pdf'), width=18, height = 6)


