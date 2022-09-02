setwd(dir= "~/Documents/skola/master/work/analysis")

# input Stata file
library(haven)
library(TraMineR)
library(ggplot2)
library(cluster)
library(fastDummies)
library(dplyr)
library(margins)
library(foreign)


stateseqs <- read_dta("01_data/00_ggs/01_temp/state_sequence_set.dta") 
grep("FAMILY_STATE0", colnames(stateseqs))
grep("FAMILY_STATE167", colnames(stateseqs))

# Define sequence
family.seq <- seqdef(stateseqs, 619:786)

# Find DSS

family.dss <- seqdss(family.seq)

### Export Data to Stata
idnew <- stateseqs$childID


newseq <-data.frame(idnew,family.dss, row.names = idnew)
write.foreign(newseq,  datafile = "01_data/00_ggs/01_temp/dss.csv", 
              codefile = "02_do-files/import_dss.do", package = "Stata")    

