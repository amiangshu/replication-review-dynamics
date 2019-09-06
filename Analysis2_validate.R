setwd('~/replication_package/')
args = commandArgs(trailingOnly=TRUE)
project = args[1]


source('./misc.R')
library(lme4)

dep_var = 'IsVotePositive'
load(paste0("models/",project,"_JIT_model_RQ2.Rdata"))
print(project)
print(fit)
val_results = ComputeOptimism(fit)
save(val_results, file=paste0("models/",project,"_bootResults_RQ2.Rdata"))
print(val_results$Optimism)