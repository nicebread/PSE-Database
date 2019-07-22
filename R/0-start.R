library(xtable)
library(tidyverse)
library(stringr)
library(yhat)
library(MuMIn)
library(dplyr)
library(lmerTest)
library(brms)
library(robustbase) 
library(rstanarm)
source("0-helpers.R")


PSE_file <- "database_releases/PSE_0.2.RData"

# PSE is the database of stories and codings
load(file=PSE_file)

# ensure integrity/correct version of file: MD5 checksum from 
# 0.2 version of database.
# md5sum("data/PSE.RData") == "7b1d1ce62498402f9b176d7573ec53a3"
