#Perform Log Likelihood ratio test
LRT <- function (model1, model2) { 
  L0 <- logLik(model1) #Get LogLikelihood of model1
  L1 <- logLik(model2) #Get LogLikelihood of model2
  LR <- as.vector(- 2 * (L0 - L1)) #Get -2LogLik ratio
  df <- attr(L1, "df") - attr(L0, "df") #Difference of D.F.
  p_value <- pchisq(LR, df, lower.tail = FALSE)
  return(data.frame(L.Ratio = LR, Diff_D.F. = df, 
                    "p-value" =  p_value) )
} 

#Calculate Deviance explain
DevianceExplain  <- function(model.null, model.1){
  expl = 1 - (deviance(model.1)/deviance(model.null))
  return(expl)
}

EvaluateModel <- function(model){
  
  type = attr(model,"class")[1]
  library(lme4)
  if(type == 'glmerMod'){
    dep_var = colnames(model@frame)[1]
    m.data=model@frame
    print(paste0("From EvaluateModel function: dep_var = ",dep_var))
    null.model = glmer(formula(paste0(dep_var," ~ 1 + (1|ReviewerId)")), family='binomial', data=m.data, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e5)))
    #Measure AUC
    library(pROC)
    prediction = predict(model)
    pred.auc = auc(m.data[,dep_var], prediction)
  }else if(type == 'glm') {
    dep_var = colnames(model$model)[1]
    m.data = model$data
    print(paste0("From EvaluateModel function: dep_var = ",dep_var))
    null.model = glm(formula(paste0(dep_var," ~ 1")), family = 'binomial', data=m.data)
    #Measure AUC
    library(pROC)
    prediction = predict(model,data=m.data, type='response')
    pred.auc = auc(m.data[,dep_var], prediction)
  }
 
  #Measure deviance Explain
  dev_expl = round( 1 - (stats::deviance(model)/stats::deviance(null.model)),digits = 2)
  #Loglikelihood ratio test
  l.ratio = LRT(null.model, model)

  #Degree of freedom spent
  DF = attr(logLik(model),'df')
  
  model_measures = c(DevianceExplain=dev_expl, L.Ratio=l.ratio$L.Ratio, L.Ratio_p=l.ratio$p.value, AUC=as.numeric(pred.auc), D.F. = DF)
  return(model_measures)
}

my.comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

my.boot <- function(model, eval_func, runs = 100){
  library(doParallel)
  # library(doSNOW)
  
  cl <- makeCluster(5)
  # registerDoSNOW(cl)
  registerDoParallel(cl)
  
  print("bootstrap running")
  performance <- foreach(i=1:runs, .packages = c("lme4","pROC","stats","car"),.export=c("EvaluateModel","DevianceExplain","LRT"), .combine='my.comb', .multicombine=TRUE, .init=list(list(), list())) %dopar% {
    set.seed(i)
  
    m.type = attr(model,"class")[1]
    if(m.type == 'glmerMod'){
      indices <- sample(nrow(model@frame), replace= T)
      boot.sample <- model@frame[indices, ]
      boot.model <- glmer(formula(model@call[[2]]), family='binomial', data=boot.sample, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=1e6)))
    }else if(m.type == 'glm'){
      indices <- sample(nrow(model$data), replace= T)
      boot.sample <- model$data[indices, ]
      boot.model <- glm(formula(model$formula), family='binomial', data=boot.sample)
    }
    output = EvaluateModel(boot.model)
    a = data.frame(Anova(boot.model, type=3))
    a$Variables = row.names(a)
    a$Iter = i
    return(list(output, a))
  }
  stopCluster(cl)
  return(performance)
}

ComputeOptimism <- function(model){
  library(lme4)
  perf = as.data.frame(t(EvaluateModel(model)))
  model.boot <- my.boot(model, EvaluateModel)
  
  var_imp = do.call("rbind",model.boot[[2]])
  row.names(var_imp) <- c()
  
  t_diff = do.call("rbind",model.boot[[1]])
  for(r in 1:nrow(t_diff)){
    for(c in colnames(t_diff)){
      t_diff[r, c] = perf[,c]- t_diff[r, c]
    }
  }
  output = data.frame(t(apply(t_diff, 2, mean)))[,c('DevianceExplain','L.Ratio','AUC')]
  colnames(output) = paste0("Optimism_",colnames(output))
  output= cbind(perf,output)
 
  return(list(Optimism=output, boot_results = model.boot, boot_anova = var_imp))
}

printModel <- function(model, normalize = TRUE){
  library(car)
  
  if(attr(model,"class")[1] == "glm"){
    a = data.frame(Anova(model, type=3, test.statistic = 'Wald'))
    test_name = "Chisq"
    pr_name = "Pr..Chisq."
    dir = data.frame(coef = coef(model))
  }else if(attr(model,"class")[1] == "glmerMod"){
    a = data.frame(Anova(model, type=3))
    test_name = "Chisq"
    pr_name = "Pr..Chisq."
    dir = data.frame(coef = fixef(model))
    
  }

  a$Variable = row.names(a)
  row.names(a) = c()
  intercept = a[a$Variable == "(Intercept)",]
  a = a[a$Variable != "(Intercept)",]
  total_Chisq = round(sum(a[,test_name]),digits = 2)
  # total_Chisq = 1
  eval = data.frame(t(EvaluateModel(model)))
  # total_Chisq = round(eval$L.Ratio,digits = 2)
  if(normalize){
    a[,test_name] = paste0(round(a[,test_name]*100/total_Chisq, digits = 0),"\\%")
  }else{
    a[,test_name] = round(a[,test_name], digits = 0)
  }
  a$SigCode = "$\\circ$"
  a$SigCode[a[,pr_name] < 0.05] = "$^{*}$ "
  a$SigCode[a[,pr_name] < 0.01] = "$^{**}$"
  a$SigCode[a[,pr_name] < 0.001] = "$^{***}$"
  

  dir$Variable = gsub("True","",row.names(dir))
  dir$coef = ifelse(dir$coef < 0, "(-) &","(+) &")
  row.names(dir) = c()
  a = merge(a, dir[,c('Variable','coef')], by='Variable', all.x = T)
  
  
  total = a[1,]
  total$Variable = "Chisq"
  total[,test_name] = round(eval$L.Ratio,digits = 2)
  total$SigCode = ifelse(eval$L.Ratio_p < 0.001, "$^{***}$", ifelse(eval$L.Ratio_p < 0.01,"$^{**}$",ifelse(eval$L.Ratio_p < 0.05,"$^{*}$","$\\circ$")))
  total$coef = "&"
  output = rbind(total[,c("Variable",test_name,"SigCode","coef")],a[,c("Variable",test_name,"SigCode","coef")])
  
  dev = total
  dev$Variable = "Dev"
  dev[,test_name] = round(eval$DevianceExplain,digits = 2)
  dev$SigCode = ""
  output = rbind(dev[,c("Variable",test_name,"SigCode","coef")],output)
  
  c = total
  c$Variable = "AUC"
  c[,test_name] = round(eval$AUC,digits = 2)
  c$SigCode = ""
  output = rbind(c[,c("Variable",test_name,"SigCode","coef")],output)
  
  if(attr(model,"class")[1] == "glmerMod"){
    var_cor = data.frame(VarCorr(model))
    v = total
    v$Variable = "Variance"
    v[,test_name] = round(var_cor$vcov,digits = 2)
    v$SigCode = ""
    output = rbind(v[,c("Variable",test_name,"SigCode","coef")],output)
  }
  
  intercept$SigCode = "$\\circ$"
  intercept$SigCode[intercept$Pr..Chisq. < 0.05] = "$^{*}$ "
  intercept$SigCode[intercept$Pr..Chisq. < 0.01] = "$^{**}$"
  intercept$SigCode[intercept$Pr..Chisq. < 0.001] = "$^{***}$"
  
  output = rbind(output, data.frame("Variable" = "Intercept","Chisq" = round(intercept$Chisq,digits=0), "SigCode" = intercept$SigCode, "coef" = " &"))
  
  return(output)
}

getChisqProportion <- function(model){
  library(car)
  a = data.frame(Anova(model, type=3))
  if(attr(model,"class")[1] == "glm"){
    test_name = "LR.Chisq"
    pr_name = "Pr..Chisq."
  }else if(attr(model,"class")[1] == "glmerMod"){
    test_name = "Chisq"
    pr_name = "Pr..Chisq."
  }
  
  a$Variable = row.names(a)
  row.names(a) = c()
  a = a[a$Variable != "(Intercept)",]
  total_Chisq = round(sum(a[,test_name]),digits = 2)
  
  
  a[,test_name] = a[,test_name]*100/total_Chisq
  
  return(a)
}

RescaleData <- function(data, variables){
  scaled_variables = data.frame()
  for(c in c(variables)){
    if(is.numeric(data[,c])){
      #Using min-max method https://math.stackexchange.com/questions/362918/value-range-of-normalization-methods-min-max-z-score-decimal-scaling
      xmin = min(data[,c])
      xmax = max(data[,c])
      data[,c] = (data[,c]-xmin)/(xmax - xmin)
      scaled_variables = rbind(scaled_variables, data.frame(variable = c, xmax = xmax, xmin = xmin))
    }
  }
  return(list(scaled_data=data, scaled_variables = scaled_variables))
}