############# produces R2  ##########

fit.R2 <- function(model,obj) {

  # r2 and tests for the model
  r2list  <-  r2.est(model,obj)
  # check if model comparisons are required
  if (obj$option("comparison")) {
    ### r2 for the nested model
    r2nested<-r2.est(obj$nested_model,obj)

    ### compare the two models
    if (obj$option(".caller",c("lmer","glmer")))
        comp <-  as.data.frame(performance::test_likelihoodratio(obj$nested_model,model))
    else  
        comp<-stats::anova(obj$nested_model,model,test=obj$options$omnibus)

    r2comp<-as.list(comp[2,])
    .names<-list(df2=c("Res.Df","Resid. Df"),
                 df1=c("Df","df_diff"),p=c("Pr(>Chi)","Pr(>F)"),
                 f="F",
                 test=c("Deviance","Chi2"))
    
    names(r2comp)<-transnames(names(r2comp),.names)
    
    ### give some warning

    if (r2comp$df1<0)
        obj$dispatcher$warnings<-list(topic="main_r2",message="Nested model is not actually nested in the full model ")

    if (r2comp$df1==0)
       obj$dispatcher$warnings<-list(topic="main_r2",message="Nested and full models are identical, try removing some term from the nested model")
    
    ### if two different estimation (mixed vs lm) are compared, be sure
    ### all rows are filled
    if (length(r2list)>length(r2nested))
          r2nested[[2]]<-r2nested[[1]]
    
    
    ## sometimes the chi-square is not printed out    
#    if (obj$option("omnibus","LRT") & !hasName(r2comp,"lrt"))
#          r2comp$lrt<-as.numeric(2*(stats::logLik(model)-stats::logLik(obj$nested_model)))
    ### adjust

    r2comp$r2<-r2list[[1]]$r2-r2nested[[1]]$r2
    if (length(r2comp$r2)==0)
      r2comp$r2<-NA
    
    if (hasName(r2list[[1]],"ar2"))
          r2comp$ar2<-r2list[[1]]$ar2-r2nested[[1]]$ar2
      r2list<-c(r2list,r2nested,list(r2comp))
  }
  for (r in r2list) {
    if (is.na(r$r2)) {
      mark("r2 na for", r)
    }
        
  }
  r2list
}

r2.est<- function(model,...) UseMethod(".r2") 


.r2.default<-function(model,obj,model="Full") {
a<- performance::r2(model,tolerance =0)

}

.r2.glm<-function(model,obj) {

  alist       <-  list()
  # mcFadden and adjusted
  alist$r2    <-  1-(model$deviance/model$null.deviance)
  alist$ar2   <-  1-((model$deviance+2*length(model$coefficients))/model$null.deviance)
  
  if (alist$ar2<0)
    alist$ar2 <- 0
  
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
      results$test<-results$lrt
      
      results$p<-stats::pchisq(results$lrt,results$df1,lower.tail = FALSE)

    } else {    
      results$f<-ss$fstatistic[["value"]]
      results$test<-results$f
      results$p<-stats::pf(results$f,results$df1,results$df2, lower.tail = FALSE)
    }
  } else {
     results$test<-NA
  }
  list(results)
  
}


.r2.lmerModLmerTest<-function(model,obj) {
  
  r2<-.r2.default(model,obj)
  if (is.null(r2) || is.na(r2)) 
      r2<-list(R2_conditional=NA,R2_marginal=NA)
  
  cond<-fit.compare_null_model(model,type="c")
  cond$type<-"Conditional"
  cond$r2<-r2$R2_conditional
  
  .marg<-fit.compare_null_model(model,type="m")
   if (is.null(.marg))
          warning("Marginal R'\u00B2' tests cannot be computed for the marginal model")
   else 
          marg<-.marg
  marg$type="Marginal"
  marg$r2<-r2$R2_marginal
  list(cond,marg)
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
          if (int==0) 
              return(NULL)
    
          re<-lme4::findbars(formula(model))
          re<-paste("(",re,")",collapse = "+")
          dep<-insight::model_info(model)$model_terms$response
          form<-paste(dep,"~",int," + ",re)
          model0  <-  stats::update(model, formula=form)

  }
  ### please note that here we use performance::test_likelihoodratio, which compute the LRT 
  ### on the estimated models, no matter what REML is. If one compares the results with 
  ### lmerTest::anova() they are slightly different because the latter re-estimate the models
  ### with ML, not REML. We do not see why re-estimaing is necessary, given these results: .https://www.jstor.org/stable/2533680

  results <-  as.data.frame(performance::test_likelihoodratio(model0,model))
  names(results)<-c("nothing1","nothing2","nothing3","df1","test","p")
  results[2,c("df1","test","p")]
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
