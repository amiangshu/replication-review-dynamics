setwd('~/replication_package/')
library(tidyverse)
library(Rnalytica)
source('./misc.R')



args = commandArgs(trailingOnly=TRUE)
project = args[1]
analysis = args[2]

data = read.csv(paste0("studied_data/",project,"_dynamic_commits.csv"),stringsAsFactors = F) 
data$Fix = factor(data$Fix ==1)
data$Author_AuthoringExp = factor(data$Author_AuthoringExp ==T)
data$Author_ReviewingExp = factor(data$Author_ReviewingExp ==T)

### Preliminary Analysis
commit_vars = c("NF","ND","Entropy","LA","LD","LT","Fix","NDEV",'Age',"NUC","PriorChanges","PriorBugFixes","AvgComplexity","Author_AuthoringExp","Author_ReviewingExp")
review_vars = c("NumReviewers","NumComments","positive_voters","ReviewingTime")
if(analysis == "prelim"){
  dynamic_vars = c("consistentPositiveVote","changeVoteFinal")
}else if(analysis == "all_metrics"){
  dynamic_vars = c("consistentPositiveVote","inconsistentNegativeVote","changeVoteFinal","consistentWithPriorComments","strongRelationshipAuthor","consistentCorePositiveVotes","coreAuthor")
}

surived_vars = AutoSpearman(data, metrics = c(commit_vars[!(commit_vars %in% c("Author_AuthoringExp","Author_ReviewingExp","Fix"))],review_vars,dynamic_vars), verbose = T)
surived_vars = c(surived_vars,c("Author_AuthoringExp","Author_ReviewingExp","Fix"))


data$response = ifelse(data$IsFixIntroducing, 1,0) 
dep_var = "response"
fit = glm(formula(paste(dep_var," ~", paste0(surived_vars,collapse = "+"))), data = data, family = "binomial")
save(fit, file= paste0("models/",project,"_JIT_model_",analysis,".Rdata"))
fit.noSocial = glm(formula(paste(dep_var," ~", paste0(surived_vars[!(surived_vars %in% dynamic_vars)],collapse = "+"))), data = data, family = "binomial")
fit.noReview = glm(formula(paste(dep_var," ~", paste0(surived_vars[!(surived_vars %in% review_vars)],collapse = "+"))), data = data, family = "binomial")
fit.noPatch = glm(formula(paste(dep_var," ~", paste0(surived_vars[!(surived_vars %in% commit_vars)],collapse = "+"))), data = data, family = "binomial")

print(summary(fit))
a = printModel(fit,normalize = F)

# print(summary(data[,surived_vars]))

if(analysis != "prelim"){
  fit_eval = EvaluateModel(fit)
  LRT_SocialVars = LRT(fit.noSocial,fit)
  LRT_SocialVars =  data.frame(Variable="Delta Social LR Chisq.",Chisq=paste0(round(LRT_SocialVars[1],digits = 2)," (",round(LRT_SocialVars[1]*100/fit_eval[2],digits = 0),"%)"),SigCode= ifelse(LRT_SocialVars[3] < 0.001, "$^{***}$", ifelse(LRT_SocialVars[3] < 0.01,"$^{**}$",ifelse(LRT_SocialVars[3] < 0.05,"$^{*}$","$\\circ$"))), coef="& ")
  names(LRT_SocialVars) = names(a)
  a = rbind(a, LRT_SocialVars)
  
  LRT_PatchVars = LRT(fit.noPatch,fit)
  LRT_PatchVars =  data.frame(Variable="Delta Patch LR Chisq.",Chisq=paste0(round(LRT_PatchVars[1],digits = 2)," (",round(LRT_PatchVars[1]*100/fit_eval[2],digits = 0),"%)"),SigCode= ifelse(LRT_PatchVars[3] < 0.001, "$^{***}$", ifelse(LRT_PatchVars[3] < 0.01,"$^{**}$",ifelse(LRT_PatchVars[3] < 0.05,"$^{*}$","$\\circ$"))), coef="& ")
  names(LRT_PatchVars) = names(a)
  a = rbind(a, LRT_PatchVars)
  
  LRT_ReviewVars = LRT(fit.noReview,fit)
  LRT_ReviewVars =  data.frame(Variable="Delta Review LR Chisq.",Chisq=paste0(round(LRT_ReviewVars[1],digits = 2)," (",round(LRT_ReviewVars[1]*100/fit_eval[2],digits = 0),"%)"),SigCode= ifelse(LRT_ReviewVars[3] < 0.001, "$^{***}$", ifelse(LRT_ReviewVars[3] < 0.01,"$^{**}$",ifelse(LRT_ReviewVars[3] < 0.05,"$^{*}$","$\\circ$"))), coef="& ")
  names(LRT_ReviewVars) = names(a)
  a = rbind(a, LRT_ReviewVars) 
}



print(a)

