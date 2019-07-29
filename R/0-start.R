library(rio)
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


PSE_file <- "database_releases/PSE_0.2_redacted.tsv"

# ensure integrity/correct version of file: MD5 checksum from 
# 0.2 version of database.
md5sum(PSE_file) == "312140eb6d077880be8b15374eed6732"

# PSE is the database of stories and codings
PSE <- import(PSE_file)

