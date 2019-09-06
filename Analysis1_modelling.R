setwd('~/replication_package/')

args = commandArgs(trailingOnly=TRUE)
project = args[1]

library(tidyverse)
library(Rnalytica)
source('./misc.R')


data = read.csv(paste0("studied_data/",project,"_dynamic_data.csv"),stringsAsFactors = F)
dep_var = 'IsVotePositive'

dynamic_vars = c('PositiveVoters_Norm','NegativeVoters_Norm','NumberOfCorePositiveVoters','RemainingOtherComments','InteractionFreqWithAuthor','PostiveVoteFreqWithAuthor','Avg_InteractionFreqWithPositiveVoters','isAuthorCore')
patch_vars = c("AddedLines", "DeletedLines","Churn", "NumberOfModifiedDirectories", "Entropy",'DescriptionLength','Author_AuthoringExp','Author_ReviewingExp')
reviewer_vars = c('StdReviewer_InvolvementLevel','StudiedReviewer_AuthoringExp','StudiedReviewer_ReviewingExp')

output = RescaleData(data, c(dynamic_vars,patch_vars,reviewer_vars))
data = output$scaled_data

data$isAuthorCore = data$isAuthorCore == "True"

# summary(data[,c(patch_vars,dynamic_vars,reviewer_vars)])

survived_vars = AutoSpearman(dataset = data, metrics = c(dynamic_vars[dynamic_vars != 'isAuthorCore'],patch_vars[!(patch_vars %in% c("Author_AuthoringExp","Author_ReviewingExp"))],reviewer_vars),verbose = T)
survived_vars = c(survived_vars,"isAuthorCore")
survived_vars = c(survived_vars,"Author_AuthoringExp")
survived_vars = c(survived_vars,"Author_ReviewingExp")
dynamic.survived = dynamic_vars[dynamic_vars %in% survived_vars]
patch.survived = patch_vars[patch_vars %in% survived_vars]
reviewer.survived = reviewer_vars[reviewer_vars %in% survived_vars]


base_model.form = paste0(dep_var,' ~ ',paste0(c(dynamic.survived,patch.survived),collapse = " + "))
patch_form = paste0(dep_var,' ~ ',paste0(c(patch.survived),collapse = " + "))
social_form = paste0(dep_var,' ~ ',paste0(c(dynamic.survived),collapse = " + "))

#Build mixed-effect model
library(lme4)
  print(paste0(base_model.form,"+ (1|ReviewerId) "))
  fit = glmer(formula(paste0(base_model.form,"+ (1|ReviewerId) ")), family = 'binomial', data=data, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e6)))
  save(fit, file = paste0("models/",project,"_mixed_model.Rdata"))


  print(paste0(patch_form,"+ (1|ReviewerId) "))
  fit.patch = glmer(formula(paste0(patch_form,"+ (1|ReviewerId) ")), family = 'binomial', data=data, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e6)))
  save(fit.patch, file = paste0("models/",project,"_mixed_patch_model.Rdata"))



  print(paste0(social_form,"+ (1|ReviewerId) "))
  fit.social = glmer(formula(paste0(social_form,"+ (1|ReviewerId) ")), family = 'binomial', data=data, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e6)))
  save(fit.social, file = paste0("models/",project,"_mixed_social_model.Rdata"))



#Build model with interaction
  library(car)
  var_imp = data.frame(Anova(fit, type=3))

  var_imp$var = row.names(var_imp)
  sig.dynamic_vars = var_imp %>% arrange(-Chisq) %>% filter(var != "(Intercept)") %>% slice(0:3) %>% filter(var %in% dynamic_vars)
  sig.dynamic_vars = sig.dynamic_vars$var

  interaction.form = paste(apply(expand.grid(sig.dynamic_vars, reviewer.survived),1, paste, collapse=":"),collapse = " + ")
  reviewer.form = paste(reviewer.survived, collapse = " + ")
  base.form = paste(c(sig.dynamic_vars,patch.survived), collapse = " + ")

  print(paste0(dep_var," ~ ",interaction.form,'+',reviewer.form,'+', base.form,"+ (1|ReviewerId) "))
  fit.interact = glmer(formula(paste0(dep_var," ~ ",interaction.form,'+',reviewer.form,'+', base.form,"+ (1|ReviewerId) ")), family = 'binomial', data=data, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e6)))
  save(fit.interact, file = paste0("models/",project,"_interact_model.Rdata"))

  print(paste0(dep_var," ~ ",reviewer.form,'+', base.form,"+ (1|ReviewerId) "))
  fit.reviewer = glmer(formula(paste0(dep_var," ~ ",reviewer.form,'+', base.form,"+ (1|ReviewerId) ")), family = 'binomial', data=data, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e6)))
  save(fit.reviewer, file = paste0("models/",project,"_mixed_reviewer_model.Rdata"))
