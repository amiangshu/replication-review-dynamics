setwd('~/replication_package/')

library(lme4)
library(car)
source("./misc.R")
dep_var = 'IsVotePositive'


dynamic_vars = c('PositiveVoters_Norm','NumberOfCorePositiveVoters','RemainingOtherComments','InteractionFreqWithAuthor','PostiveVoteFreqWithAuthor','Avg_InteractionFreqWithPositiveVoters','Avg_AgreementLevelWithPositiveVoters','isAuthorCore')

patch_vars = c("AddedLines", "DeletedLines", "NumberOfModifiedDirectories", "Entropy",'DescriptionLength','Author_AuthoringExp','Author_ReviewingExp')

reviewer_vars = c('StdReviewer_InvolvementLevel','StudiedReviewer_AuthoringExp','StudiedReviewer_ReviewingExp')

# "Chisq", "AUC","Variance", "Dev",""
variable_names = data.frame(Variable = c("Chisq","AUC","Variance","Intercept",
                                         "Delta Patch LR Chisq.", "AddedLines", "DeletedLines","NumberOfModifiedDirectories", "Entropy",'Author_AuthoringExp','Author_ReviewingExp','DescriptionLength',
                                         "Delta Social LR Chisq.",'PositiveVoters_Norm','NegativeVoters_Norm','RemainingOtherComments','NumberOfCorePositiveVoters','isAuthorCore','InteractionFreqWithAuthor','Avg_InteractionFreqWithPositiveVoters','PostiveVoteFreqWithAuthor','Avg_AgreementLevelWithPositiveVoters','StdReviewer_InvolvementLevel','StudiedReviewer_AuthoringExp','StudiedReviewer_ReviewingExp'),
                            MetricNames = c("Overall LR Chisq","AUC","Variance of Random Effect","Intercept",
                                            "Delta Patch LR Chisq.","#Added Lines", "#Deleted Lines","#Modified Directories", "Entropy",'Authoring Exp.\n of Patch Author',"Reviewing Exp.\n of Patch Author","Patch Description Length",
                                            "Delta Social LR Chisq.",'%Prior Positive Votes','%Prior Negative Votes','%Prior Comments','%Prior Positive Vote\n of Core','is Patch Author Core','Co-working Freq.\n with Patch Author','Avg. Co-working Freq. \n with Positive Voters','Postive Vote Freq. for Patch Author','Avg. Agreement Level \n with Positive Voters','Involvement Level \n in the patch','Authoring Exp.','Reviewing Exp.'),
                            Characteristics=c("","","","","","Patch", "Patch","Patch", "Patch",'Patch','Patch','Patch',"",'Peer','Peer','Peer','Peer','Peer','Peer','Peer','Peer','Peer','Reviewer','Reviewer','Reviewer'))




### Convert the anova results of the mixed model into latex
projects <- c('openStack','qt')
anova_model = data.frame()
for(project in projects){
  load(paste0('models/',project,'_mixed_model.Rdata'))
  load(paste0('models/',project,'_mixed_social_model.Rdata'))
  load(paste0('models/',project,'_mixed_patch_model.Rdata'))
  
  print(fit)
  print(table(fit@frame$IsVotePositive))

  a = printModel(fit,normalize = F)
  
  #Add optimism reduced
  if(file.exists(paste0('models/',project,'_bootResults.Rdata'))){
    load(paste0('models/',project,'_bootResults.Rdata'))
    val_results$Optimism
    a[a$Variable == "AUC",]$Chisq = paste0(a[a$Variable == "AUC",]$Chisq, "(",round(val_results$Optimism$Optimism_AUC, digits = 3),")")
    a[a$Variable == "Chisq",]$Chisq = paste0(a[a$Variable == "Chisq",]$Chisq,"(", round(val_results$Optimism$Optimism_L.Ratio, digits = 2),")")
  }

  
  fit_eval = EvaluateModel(fit)
  
  
  
  LRT_SocialVars = LRT(fit.patch,fit)
  LRT_SocialVars =  data.frame(Variable="Delta Social LR Chisq.",Chisq=paste0(round(LRT_SocialVars[1],digits = 2)," (",round(LRT_SocialVars[1]*100/fit_eval[2],digits = 0),"%)"),SigCode= ifelse(LRT_SocialVars[3] < 0.001, "$^{***}$", ifelse(LRT_SocialVars[3] < 0.01,"$^{**}$",ifelse(LRT_SocialVars[3] < 0.05,"$^{*}$","$\\circ$"))), coef="& ")
  names(LRT_SocialVars) = names(a)
  a = rbind(a, LRT_SocialVars)
  
  LRT_PatchVars = LRT(fit.social,fit)
  LRT_PatchVars =  data.frame(Variable="Delta Patch LR Chisq.",Chisq=paste0(round(LRT_PatchVars[1],digits = 2)," (",round(LRT_PatchVars[1]*100/fit_eval[2],digits = 0),"%)"),SigCode= ifelse(LRT_PatchVars[3] < 0.001, "$^{***}$", ifelse(LRT_PatchVars[3] < 0.01,"$^{**}$",ifelse(LRT_PatchVars[3] < 0.05,"$^{*}$","$\\circ$"))), coef="& ")
  names(LRT_PatchVars) = names(a)
  a = rbind(a, LRT_PatchVars)
  
  colnames(a)[which(colnames(a) != "Variable")] = paste0(project,"_",colnames(a[colnames(a) != "Variable"]))
  if(nrow(anova_model) == 0){
    anova_model = a
  }else{
    anova_model = merge(anova_model, a, by="Variable",all.x=T,all.y=T)
  }
  
}
result_table = merge(variable_names,anova_model, by="Variable", all.y=T)
result_table = result_table[match(variable_names$Variable,result_table$Variable),]
result_table = result_table[!is.na(result_table$Variable),]
result_table = result_table[,colnames(result_table) != "Variable"]
print(result_table)


# 
# #### Show the wald of the models with interaction terms
# projects <- c('openstack','qt')
# anova_interact = data.frame()
# reviewer_vars = c('StdReviewer_InvolvementLevel','StudiedReviewer_AuthoringExp','StudiedReviewer_ReviewingExp')
# 
# for(project in projects){
#   load(paste0('models/',project,'_interact_model.Rdata'))
#   load(paste0('models/',project,'_mixed_reviewer_model.Rdata'))
#   
#   
#   print(summary(fit.interact))
#   
#   
#   load(paste0('models/',project,'_mixed_model.Rdata'))
#   
#   a = printModel(fit.interact,normalize = FALSE)
#   fit_eval = EvaluateModel(fit.interact)
#   
#   # fit_eval = EvaluateModel(fit)
#   
#   report = function(VarSet, set_name){
#     return(data.frame(Variable=paste0("Delta ",set_name," LR Chisq."),Chisq=paste0(round(VarSet[1],digits = 2)," (",round(VarSet[1]*100/fit_eval[2],digits = 0),"%)"),SigCode= ifelse(VarSet[3] < 0.001, "$^{***}$", ifelse(VarSet[3] < 0.01,"$^{**}$",ifelse(VarSet[3] < 0.05,"$^{*}$","$\\circ$"))), coef="& "))
#   }
#   
#   Delta_test = rbind(report(LRT(fit.reviewer,fit.interact), "Interact"), report(LRT(fit,fit.reviewer), "Reviewer"),report(LRT(fit,fit.interact),"Two full models"))
#   names(Delta_test) = names(a)
#   a = rbind(a, Delta_test)
#   
#   
#   for(i in which(grepl(":",a$Variable))){
#     vars = unlist(strsplit(a$Variable[i], ':'))
#     if(vars[1] %in% reviewer_vars){
#       a$Variable[i] = paste0(vars[2],':',vars[1])
#     }
#   }
#   
#   wald_interact = a[which(grepl(":",a$Variable)),]
#   
#   library(stringr)
#   terms = data.frame(str_split_fixed(wald_interact$Variable, ":", 2))
#   colnames(terms) = c('Peer_vars','Reviewer_vars')
#   wald_interact =  cbind(wald_interact,terms)
#   wald_interact$Peer_vars_label = NA
#   wald_interact$Reviewer_vars_label = NA
#   for(i in 1:nrow(wald_interact)){
#     wald_interact[i,]$Peer_vars_label = as.character(variable_names[as.character(variable_names$Variable) == as.character(wald_interact[i,]$Peer_vars),]$MetricNames)
#     wald_interact[i,]$Reviewer_vars_label = as.character(variable_names[as.character(variable_names$Variable) == as.character(wald_interact[i,]$Reviewer_vars),]$MetricNames)
#   }
#   
#   wald_interact$Reviewer_vars_label = factor(wald_interact$Reviewer_vars_label, levels=variable_names[variable_names$Characteristics == "Reviewer",]$MetricNames)
#   wald_interact$Peer_vars_label = factor(wald_interact$Peer_vars_label, levels = rev(variable_names[variable_names$Characteristics == 'Peer' & variable_names$MetricNames %in% wald_interact$Peer_vars_label,]$MetricNames))
#   
#   wald_interact$Chisq = as.numeric(gsub("\\\\%","",wald_interact$Chisq))
#   wald_interact$label = paste0(wald_interact$Chisq,ifelse(wald_interact$SigCode == "$\\circ$","'",gsub("[$^{]","",gsub("}[$]","",wald_interact$SigCode))))
#   
#   if(nrow(anova_interact) == 0){
#     anova_interact = a
#   }else{
#     anova_interact = merge(anova_interact, a, by="Variable",all.x=T,all.y=T)
#   }
# }

# print(anova_interact)