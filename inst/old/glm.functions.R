
local.getModelTerms<-function(model) names(coefficients(model))

local.getModelData<-function(model) model$model

local.getModelDummies<-function(variable,model) {
  nn<-local.getModelTerms(model)
  nn<-nn[grep(variable,nn)]
  nn[grep(":",nn,invert = T,fixed = T)]     
}

#### remove this ##
local.getModelFactors<-function(model) 
  names(model$contrasts)

#### remove this ##
local.getFormulaFactors<-function(formula,data) {
  fake<-model.matrix(formula,data)
  names(attr(fake,"contrasts"))
}

##### function to check results ####

local.aliased<-function(model) {
  aliased<-alias(model)
  if (!is.null(aliased$Complete))
    return(TRUE)
  return(FALSE)
}


#### functions for simple effects and plots #########

local.anova=function(model) {
  car::Anova(model,type=3)
}
local.summary=function(model) {
  ss<-summary(model)
  as.data.frame(ss$coefficients)
}

local.estimate=function(form,data) {
  stats::lm(form,data)
}



##### some function to produce results for anovatable

anovaTable<-function(model,anovaframe) {
  dss<-anovaframe
  sumr<-summary(model)
  errDF<-sumr$fstatistic[3]
  modDF<-sumr$fstatistic[2]
  modF<-sumr$fstatistic[1]
  errSS<-dss$ss[length(dss$ss)]
  errMS<-errSS/errDF
  r2<-sumr$r.squared
  totalSS<-1/((1-r2)*(1/errSS))
  modSS<-totalSS-errSS
  modp<-1 - pf(modF, modDF, errDF) 
  modRow<-c(ss=modSS,df=modDF,F=modF,p=modp)
  ss<-rbind(modRow,dss)
  ss$ms<-ss$ss/ss$df
  ss$etaSq<-ss$ss/(totalSS)
  ss$etaSqP <- ss$ss / (ss$ss + errSS)
  ss$omegaSq <- (ss$ss - (ss$df * errMS)) / (totalSS + errMS)
  ss
}

cleanAnova<-function(anovares,type) {
  res<-as.data.frame(anovares)
  if (type=='1') 
    res<-res[,c(2,1,4,5)]
  
  if (type=='3') 
    res<-res[-1,]
  colnames(res)<-c("ss","df","F","p")
  res
}

r2indices<-function(model) {
  sumr<-summary(model)
  list(r2=round(sumr$r.squared,digits = 3),ar2=round(sumr$adj.r.squared,digits=3))
}