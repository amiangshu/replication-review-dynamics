setwd('~/replication_package/')
args = commandArgs(trailingOnly=TRUE)
project = args[1]


source('./misc.R')
library(lme4)

dep_var = 'IsVotePositive'
load(paste0("models/",project,"_mixed_model.Rdata"))
print(project)
print(fit)
val_results = ComputeOptimism(fit)
save(val_results, file=paste0("models/",project,"_bootResults.Rdata"))