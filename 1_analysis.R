#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     1_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-10 13:01:26
# Modified:     2016-06-12 18:34:37
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

# Outliers
db$sentiment[db$sentiment > 16] <- 16
db$sentiment[db$sentiment < -16] <- -16

db[db=="coalitionavenir"] <- "CAQ"
db[db=="lepartiquebecois"] <- "PQ"
db[db=="LiberalQuebec"] <- "PLQ"
db[db=="OptionNationale.QC"] <- "ON"
db[db=="partivert"] <- "PVQ"
db[db=="Quebecsolidaire"] <- "QS"

#1 Number of Posts (Official vs Non-Official) by Party
plot1 <- db %>% group_by(page_id, primary) %>%
  summarise(n = n()) %>% mutate(freq = round(n/ sum(n),2))

ggplot(plot1, aes(x=reorder(page_id, -n), y=n, fill=factor(primary)))+
  geom_bar(stat='identity') +
  scale_fill_discrete('', labels=c('Secondary', 'Primary')) +
  geom_text(aes(y = n, label = ifelse(primary ==0, paste0(freq*100, ' %'), '')),
            vjust=2, color="white", size=3) +
  ylab('Number of Posts') + xlab('')

ggsave(paste0('figs/plot1','.pdf'), width = 7, height = 7)

#2 Number of Posts by Category
plot2 <- db %>% select(page_id, contains('topic')) %>%
  group_by(page_id) %>% summarise_each(funs(sum)) %>%
  gather(topic, count, topic_economy:topic_partisan) %>%
  group_by(page_id) %>% mutate(freq = round(count/sum(count),2))

ggplot(plot2, aes(x=reorder(topic, count), y=count)) +
  geom_bar(stat='identity') +
  xlab('') + ylab('') +
  coord_flip()

ggsave(paste0('figs/plot2','.pdf'), width = 10, height = 6)

#3 Proportion of Posts by Category for each party
ggplot(plot2, aes(x=reorder(topic, count), y=count, fill = page_id)) +
  geom_bar(stat='identity', position = 'fill') +
  xlab('') + ylab('') +
  coord_flip()

ggsave(paste0('figs/plot3','.pdf'), width = 6, height = 10)

#4 Number of Posts by Category by Party
ggplot(plot2, aes(x=reorder(topic, count), y=count)) +
  geom_bar(stat='identity') +
  coord_flip() +
  xlab('') + ylab('') +
  facet_grid(.~page_id,)

ggsave(paste0('figs/plot4', '.pdf'), width=18, height = 6)

#5 Tone by party
plot5 <- db %>% select(page_id, sentiment, primary) %>% filter(sentiment !=0) %>%
  mutate(sent = sign(sentiment)) %>% group_by(page_id, sent) %>%
  summarise(n = n()) %>% mutate(freq = round(n/ sum(n),2))

ggplot(plot5, aes(x = reorder(page_id, -n), y = n, fill = as.factor(sent))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_discrete('', labels=c('Negative', 'Positive')) +
  geom_text(aes(label=paste0(freq*100, ' %')), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3)+
  xlab('') + ylab('')

ggsave(paste0('figs/plot5', '.pdf'), width=7, height = 7)

#6 Types of posts
plot6 <- db %>% filter(primary == 1) %>% filter(type !='swf') %>%
  select(page_id, type) %>% group_by(page_id, type) %>%
  summarise(n = n()) %>% mutate(freq = round(n/sum(n),2))

ggplot(plot6, aes(x=page_id, y =freq, fill=type)) +
  geom_bar(stat='identity', position = 'dodge') +
  xlab('') + ylab('%') +
  geom_text(aes(label=paste0(round(freq*100,2), '%')),
            hjust=1,color="white", size=3,
            position = position_dodge(width = 1)) +
  coord_flip()

ggsave(paste0('figs/plot6', '.pdf'), width=7, height = 10)
