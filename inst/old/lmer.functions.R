local.getModelTerms<-function(model) attr(terms(model),"term.labels")

local.getModelData<-function(model) model@frame

local.getModelDummies<-function(variable,model) {
  nn<-local.getModelTerms(model)
  nn<-nn[grep(variable,nn)]
  nn[grep(":",nn,invert = T,fixed = T)]     
}

local.getModelFactors<-function(model) names(attr(model@pp$X,"contrast"))

local.guessFixedEffects<-function(dep,modelTerms,data) {
  if ((length(modelTerms)==0))
    return("(Intercept)")
  formula<-as.formula(jmvcore::constructFormula(dep, modelTerms))
  colnames(stats::model.matrix(formula,data))  
}

local.predict<-function(model,data=NULL) {
                        predict(model,data,re.form=~0)
}


local.aliased<-function(model) {
  rank<-attr(model@pp$X,"msgRankdrop")
  if (!is.null(rank))
          return(TRUE)
  return(FALSE)
}

local.getModelMessages<-function(model) {
  message<-list()
  eigen<-model@optinfo$conv$lme4$messages[[1]]
  if (!is.null(eigen))
     message["eigen"]<-eigen
  message
}


local.summary=function(model) {
  ss<-lmerTest::summary(model)
  return(as.data.frame(ss$coefficients))
}

local.summary<-function(model) {
  ss<-lmerTest::summary(model)
  as.data.frame(ss$coefficients)
}

local.anova<-function(model)
                     car::Anova(model,test="F",type=3)

local.estimate<-function(form,data,REML=TRUE)
    do.call(lmerTest::lmer, list(formula=form, data=data,REML=REML))


