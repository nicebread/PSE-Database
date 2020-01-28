## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2019 Felix Schönbrodt
##-----------------------------------------------------------------------------
## Purpose: Load all necessary libraries nand helper functions, load central data file
## Source this file when you start working with the project
##-----------------------------------------------------------------------------

library(rio)
library(knitr)
library(xtable)
library(tidyverse)
library(stringr)
library(yhat)
library(MuMIn)
library(dplyr)
library(lmerTest)
library(brms)
library(robustbase) 
library(ggtern)
library(metafor)
library(pwr)
#library(rstanarm)
source("0-helpers.R")

# load the central data file
PSE_file <- "../story_database/PSE_1.0_redacted_data.tsv"

# ensure integrity/correct version of file: MD5 checksum from 
# 0.2 version of database.
if (tools::md5sum(PSE_file) != "9acc8e74a45e2651a96ceb371b227ade") {
	warning("ERROR: checksum of data file does not match!")
} else {
	print("SUCCESS: Integrity of data set verified by checksum.")
}

# PSE is the database of stories and codings
PSE <- import(PSE_file)
