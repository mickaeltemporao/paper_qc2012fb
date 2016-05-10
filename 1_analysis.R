#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     1_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-10 13:01:26
# Modified:     2016-05-10 13:06:04
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(dplyr)
db <- read.csv('data/2012_qc_lexicoder.csv') %>%
  group_by(username)

