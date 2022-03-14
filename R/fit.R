############# produces R2  ##########

fit.R2 <- function(model,obj) {
  
  # r2 and tests for the model
  r2list  <-  r2.est(model,obj)
  
  # check if model comparisons are required
  if (obj$option("comparison")) {
    ### r2 for the nested model
    r2nested<-r2.est(obj$nested_model,obj)
    ### compare the two models
    comp<-stats::anova(obj$nested_model,model,test=obj$options$omnibus)
    r2comp<-as.list(comp[2,])

    .names<-list(df2=c("Res.Df","Resid. Df"),
                 df1="Df",p=c("Pr(>Chi)","Pr(>F)"),
                 f="F",
                 test="Deviance")
    
    names(r2comp)<-transnames(names(r2comp),.names)

    ## sometimes the chi-square is not printed out    
    if (obj$option("omnibus","LRT") & !hasName(r2comp,"lrt"))
          r2comp$lrt<-as.numeric(2*(stats::logLik(model)-stats::logLik(obj$nested_model)))
    ### adjust
    r2comp$r2<-r2list[[1]]$r2-r2nested[[1]]$r2
    r2comp$ar2<-r2list[[1]]$ar2-r2nested[[1]]$ar2
    
    r2list<-c(r2list,r2nested,list(r2comp))
    
  }
  r2list
}




r2.est<- function(model,...) UseMethod(".r2") 


.r2.default<-function(model) {
   
  performance::r2(model,tolerance =0)

}

.r2.glm<-function(model,obj) {

  alist       <-  list()
  # mcFadden and adjusted
  alist$r2    <-  1-(model$deviance/model$null.deviance)
  alist$ar2   <-  1-((model$deviance+2*length(model$coefficients))/model$null.deviance)
  
  if (alist$ar2<0)
    alist$ar2 <- 0
  
  if (any(sapply(alist,is.null)))
    warning("R-squared cannot be computed")
  
  results     <-  fit.compare_null_model(model)
  alist$test  <-  results$test
  alist$df1    <-  results$df1
  alist$p     <-  results$p
  list(alist)
  
}

.r2.polr<-function(model) {

  alist       <-  list()
  results     <-  fit.compare_null_model(model)

  # mcFadden 

  alist$r2    <-  1-(results$deviance/results$null.deviance)
  alist$ar2   <-  ""

  if (is.null(alist$r2))
    obj$warnings  <-  list(topic="tab_r2",message="R-squared cannot be computed")
  
  alist$test  <-  results$test
  alist$df1    <-  results$df1
  alist$p     <-  results$p
  return(list(alist))
  
}
  
.r2.multinom<-function(model) {

  
  llfull<-stats::logLik(model)  ### model loglikelihood
  data<-mf.getModelData(model)
  nullmodel <- stats::update(model, ~ 1, data=data,  evaluate = T)
#  nullmodel <- eval.parent(nullmodel)
  llnull<-stats::logLik(nullmodel)
  r2<-as.numeric(1-(llfull/llnull))  
  compare<-anova(nullmodel,model)
  

  alist       <-  list()
  # mcFadden 
  alist$r2    <-  as.numeric(1-(llfull/llnull))
  alist$test  <-  compare$`LR stat.`[2]
  alist$df1    <-  compare$`   Df`[2]
  alist$p     <-  compare$`Pr(Chi)`[2]
  return(list(alist))
}

.r2.lm<-function(model,obj) {
  
  ss<-summary(model)
  results<-list()
  results$df1<-ss$fstatistic[["numdf"]]
  results$df2<-ss$fstatistic[["dendf"]]
  results$r2<-ss$r.squared
  results$ar2<-ss$adj.r.squared
  if (hasName(ss,"fstatistic")) {

    if (obj$option("omnibus","LRT")) {
      
      ssres <- stats::sigma(model)^2*model$df.residual
      ### here we estimate the sum of squares of the null model
      ssnull<-(ss$fstatistic[[1]]*ss$fstatistic[[2]]*ssres/ss$fstatistic[[3]])+ssres
      n<-sum(ss$df[1:2])
      ### compute the loglik of the null model
      loglik0<- -0.5*n*(log(2*pi) + log( ssnull/n ) + 1)
      loglik1<-as.numeric(stats::logLik(model))
      results$lrt<-2*(loglik1-loglik0)
      results$p<-stats::pchisq(results$lrt,results$df1,lower.tail = FALSE)

    } else {    
      results$f<-ss$fstatistic[["value"]]
      results$p<-stats::pf(results$f,results$df1,results$df2, lower.tail = FALSE)
    }
  } else {
    warning("R-squared test cannot be computed")
    
  }
  list(results)
  
}


.r2.lmerModLmerTest<-function(model) {
  
  alist<-list()
  r2<-.r2.default(model,obj)
  if (is.null(r2))
      return(NULL)
  results<-fit.compare_null_model(model,type="m")
  
  tests<-results$obj
  tests$r2<-r2$R2_marginal
  alist[[1]]<-tests
  if (!is.na(r2$R2_conditional)) {
      tests<-fit.compare_null_model(model,type="c")
      tests$r2<-r2$R2_conditional
      alist[[2]]<-tests
  } else
      tests<-list(r2="")
  alist[[2]]<-tests
  alist

  
}



######### model comparisons for one model ########

fit.compare_null_model<- function(x,...) UseMethod(".compare_null_model")

.compare_null_model.default<-function(model) {
  
  data    <-  insight::get_data(model)
  int<-attr(terms(model),"intercept")
  form<- as.formula(paste("~",int))
  model0  <-  stats::update(model,form ,data=data,evaluate=T)
  results <-  stats::anova(model0,model,test = "LRT")
  results$test  <-  results$Deviance
  results$df1    <-  results$Df
  results$p     <-  results$`Pr(>Chi)`
  results[2,]
}


.compare_null_model.polr<-function(model) {

  form<- as.formula("~1")
  data<-mf.getModelData(model)
  model0  <-  stats::update(model,form ,data=data,evaluate=T)
  .results <-  stats::anova(model0,model)
  results<-.results[2,]
  results$deviance<--2*as.numeric(stats::logLik(model))
  results$null.deviance<--2*as.numeric(stats::logLik(model0))
  results$test  <-  results$`LR stat.`
  results$df1    <-  results$`   Df`
  results$p     <-  results$`Pr(Chi)`
  results
}


.compare_null_model.negbin<-function(model) {
  
  data    <-  mf.getModelData(model)
  int<-attr(terms(model),"intercept")
  form<- as.formula(paste("~",int))
  model0  <-  stats::update(model,form ,data=data,evaluate=T)
  results <-  stats::anova(model0,model)
  results$test  <- results$`LR stat.`
  results$df1    <- results$`   df`
  results$p    <- results$`Pr(Chi)`
  results[2,]
}

.compare_null_model.lmerModLmerTest<-function(model,type="c") {
  
  data    <-  mf.getModelData(model)

  int<-attr(terms(model),"intercept")
  
  if (type=="c") {

          form<-as.formula(paste(formula(model)[[2]],"~",int))
          model0<-stats::lm(form,data=data)
          
  } else  {
    
          re<-lme4::findbars(formula(model))
          re<-paste("(",re,")",collapse = "+")
          form<-update(formula(model),paste("~",int," + ",re))
          model0  <-  stats::update(model, form ,data=data)
          
  }
  ### please note that here we use performance::test_likelihoodratio, which compute the LRT 
  ### on the esimated models, no matter what REML is. If one compares the results with 
  ### lmerTest::anova() they are slightly different because the latter re-estimate the models
  ### with ML, not REML. We do not see why re-estimaing is necessary, given these results: .https://www.jstor.org/stable/2533680

  results <-  as.data.frame(performance::test_likelihoodratio(model0,model))
  names(results)<-c("nothing1","nothing2","nothing3","df","test","p")
  results[2,c("df","test","p")]
}

null.deviance<-function(model) {
      int<-attr(terms(model),"intercept")
      form<- as.formula(paste("~",int))
      model0  <-  stats::update(model,form ,evaluate=T)
      stats::deviance(model0)
}


deviance<- function(object,...) UseMethod("stats::deviance")

deviance.clm<-function(object) as.numeric(-2*stats::logLik(object))

#### additional fit indices

fit.indices<- function(model,...) UseMethod(".fit") 

.fit.default<-function(model,obj) {
  return(NULL)
}

.fit.glm<-function(model,obj) {

  alist<-list()
  alist[[length(alist)+1]]<-list(value=as.numeric(stats::logLik(model)))
  alist[[length(alist)+1]]<-list(value=stats::extractAIC(model)[2])
  alist[[length(alist)+1]]<-list(value=stats::BIC(model))
  alist[[length(alist)+1]]<-list(value=model$deviance)
  alist[[length(alist)+1]]<-list(value=model$df.residual)
  value <- sum(stats::residuals(model, type = "pearson")^2)
  result <- value/stats::df.residual(model)
  alist[[length(alist)+1]]<-list(value=result)
  alist

}

.fit.multinom<-function(model,obj) {
  
  alist<-list()
  alist[[length(alist)+1]]<-list(value=as.numeric(stats::logLik(model)))
  alist[[length(alist)+1]]<-list(value=stats::extractAIC(model)[2])
  alist[[length(alist)+1]]<-list(value=stats::BIC(model))
  alist[[length(alist)+1]]<-list(value=model$deviance)
  alist
  
}
