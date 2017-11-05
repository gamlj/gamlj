
##### function to check results ####

.which.class<-function(model) {
  if (class(model)[[1]]=="glm")
    return("glm")
  if ("lm" %in% class(model))
      return("lm")
  if ("merModLmerTest" %in% class(model))
      return("lmer")
  if ("lmerMod" %in% class(model))
      return("lmer")
  if ("multinom" %in% class(model))
    return("multinomial")
  
}

mf.aliased<-function(model) {
  if (.which.class(model)=="lm" || .which.class(model)=="glm") {  
  aliased<-alias(model)
  if (!is.null(aliased$Complete))
    return(TRUE)
  return(FALSE)
  }
  if (.which.class(model)=="lmer") {  
     rank<-attr(model@pp$X,"msgRankdrop")
     if (!is.null(rank))
        return(TRUE)
     return(FALSE)
  }
  FALSE
}

mf.predict_response<-function(model) {
  if (.which.class(model)=="glm" || .which.class(model)=="lm") {
  return(  predict(model,type="response"))    
  }
  if (.which.class(model)=="lmer") {
    return(  predict(model,re.form=~0))    
  }
} 
  
##### predictions for the plots #######

mf.predict<- function(x,...) UseMethod(".predict")


.predict.lm<-function(model,data=NULL,bars="none") {
    preds<-predict(model,data,se.fit = T,interval="confidence",type="response")
    if (bars=="none")
      return(data.frame(fit=preds$fit[,1]))
    if (bars=="ci") {
      fit<-preds$fit[,1]
      lwr<-preds$fit[,2]
      upr<-preds$fit[,3]
    }
    if (bars=="se") {
      fit<-preds$fit[,1]
      lwr<-preds$fit[,1]-preds$se.fit
      upr<-preds$fit[,1]+preds$se.fit
    }  
    return(as.data.frame(cbind(fit=fit,lwr=lwr,upr=upr)))
  }
  
.predict.glm<-function(model,data=NULL,bars="none") {
  
    preds<-predict(model,data,se.fit = T,type="link")
    if (bars=="none")
      return(data.frame(fit=model$family$linkinv(preds$fit)))
    if (bars=="ci")
        critval<-qnorm(0.975)
    if (bars=="se")
        critval<-1
    upr <- preds$fit + (critval * preds$se.fit)
    lwr <- preds$fit - (critval * preds$se.fit)
    fit<-preds$fit
    fit <- model$family$linkinv(fit)
    upr <- model$family$linkinv(upr)
    lwr <- model$family$linkinv(lwr)
    return(as.data.frame(cbind(fit,lwr,upr)))
  }

.predict.merModLmerTest<-function(model,data=NULL,bars="none") 
             return(.predict.lmer(model,data,bars))
  
.predict.lmer<-function(model,data=NULL,bars="none") {

      return(data.frame(fit=predict(model,data,re.form=~0)))
}

.predict.multinom<-function(model,data=NULL,bars="none") {
  
  return(data.frame(fit=predict(model,data,type="probs")))
}

##### get model data #########

mf.getModelData<- function(x,...) UseMethod(".getModelData")

.getModelData.default<-function(model) 
     return(model$model)

.getModelData.merModLmerTest<-function(model) 
     return(.getModelData.lmer(model))

.getModelData.lmer<-function(model) 
  return(model@frame)




#### those function are needed for simple effects and plots. They get a model as imput and gives back a function
#### to reestimate the same kind of model

mf.estimate<-function(model) {
  if (.which.class(model)=="lm") {  
     return(function(form,data) stats::lm(form,data))
  }  

  if (.which.class(model)=="lmer") {  
    return(function(form,data)
      do.call(lmerTest::lmer, list(formula=form, data=data,REML=!is.na(model@devcomp$cmp["REML"]))))
  }
  if (.which.class(model)=="glm") {  
    return(function(form,data)
      stats::glm(form,data,family=model$family))
  }  
  if (.which.class(model)=="multinomial") {  
    return(function(form,data)
       nnet::multinom(form,data,model = T))
  }  
  
}

############# produces summary in a somehow stadard format ##########

mf.summary<- function(x,...) UseMethod(".mf.summary")

.mf.summary.default<-function(model) {
      return(FALSE)
}
  
.mf.summary.lm<-function(model){
  ss<-summary(model)$coefficients
  colnames(ss)<-c("estimate","se","t","p")
  as.data.frame(ss,stringsAsFactors = F)
}

.mf.summary.merModLmerTest<-function(model)
       .mf.summary.lmer(model)

.mf.summary.lmer<-function(model) {
  
       ss<-lmerTest::summary(model)$coefficients
      if (dim(ss)[2]==3) {
          colnames(ss)<-c("estimate","se","t")
          if (dim(ss)[1]==1)
               attr(ss,"warning")<-"lmer.df"
          else
              attr(ss,"warning")<-"lmer.zerovariance"
      }
       else
          colnames(ss)<-c("estimate","se","df","t","p")
       as.data.frame(ss,stringsAsFactors = F)
}

.mf.summary.glm<-function(model) {
     
     ss<-summary(model)$coefficients
     expb<-exp(ss[,"Estimate"])  
     ss<-cbind(ss,expb)
     colnames(ss)<-c("estimate","se","z","p","expb")
     as.data.frame(ss,stringsAsFactors = F)
}

.mf.summary.multinom<-function(model) {
  
     sumr<-summary(model)
     rcof<-sumr$coefficients
     cof<-as.data.frame(matrix(rcof,ncol=1))
     names(cof)<-"estimate"
     cof$dep<-rownames(rcof)
     cof$variable<-rep(colnames(rcof),each=nrow(rcof))
     se<-matrix(sumr$standard.errors,ncol=1)
     cof$se<-as.numeric(se)
     cof$expb<-exp(cof$estimate)  
     cof$z<-cof$estimate/cof$se
     cof$p<-(1 - pnorm(abs(cof$z), 0, 1)) * 2
     ss<-as.data.frame(cof,stringsAsFactors = F)
     ss<-cof[order(ss$dep),]
     ss
}
  
############# produces anova/deviance table in a somehow stadard format ##########
mf.anova<- function(x,...) UseMethod(".anova")

.anova.glm<-function(model) {

        ano<-car::Anova(model,test="LR",type=3,singular.ok=T)
        colnames(ano)<-c("test","df","p")
        as.data.frame(ano, stringsAsFactors = F)
}

.anova.multinom<-function(model) {

      ano<-car::Anova(model,test="LR",type=3,singular.ok=T)
      colnames(ano)<-c("test","df","p")
      as.data.frame(ano, stringsAsFactors = F)
      
  }
  
.anova.lm<-function(model) {
  
    ano<-car::Anova(model,test="F",type=3, singular.ok=T)
    colnames(ano)<-c("ss","df","f","p")
    ano
}


.anova.merModLmerTest<-function(model) {
  
  ano<-car::Anova(model,test="F",type=3, singular.ok=T)
  colnames(ano)<-c("test","df1","df2","p")
  ano
  
}

mf.lmeranova=function(model) {

  if (.which.class(model)=="lmer") {   
    
  ano<-lmerTest::anova(model)
  if (dim(ano)[1]==0)
    return(ano)
  if (dim(ano)[2]==4) {
    print("lmer anava uses car::Anova")
    ano<-car::Anova(model,type=3,test="F")
    ano<-ano[-1,]
    names(ano)<-c("F","df1","df2","p")
    attr(ano,"method")<-"Kenward-Roger"
    return(ano)
  }
  ano<-ano[,c(5,3,4,6)]
  names(ano)<-c("F","df1","df2","p")
  attr(ano,"method")<-"Satterthwaite"
  if (!all(is.na(ano$df2)==F)) {
    print("lmer anava uses car::Anova")
    ano<-car::Anova(model,type=3,test="F")
    ano<-ano[-1,]
    names(ano)<-c("F","df1","df2","p")
    attr(ano,"method")<-"Kenward-Roger"
    return(ano)
  }
  
  return(ano)
  }
}


mf.getModelFactors<-function(model) {
  names(attr(model.matrix(model),"contrasts"))
} 


mf.getModelMessages<-function(model) {
  message<-list()
  if (.which.class(model)=="lmer") {
  eigen<-model@optinfo$conv$lme4$messages[[1]]
  if (!is.null(eigen))
      message["eigen"]<-eigen
  }
  message
}


mf.give_family<-function(modelSelection) {
  
  if (modelSelection=="linear")
       return(gaussian())
  if (modelSelection=="logistic")
       return(binomial())
  if (modelSelection=="poisson")
    return(poisson())
  if (modelSelection=="multinomial")
    return("multinomial")
  
  
  NULL  
}

mf.checkData<-function(dep,data,modelType) {
  
     if (modelType=="linear")
       ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))  
     
     if  (modelType=="poisson") {
         ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))
       if (any(data[[dep]]<0))
             return("Poisson model requires all positive numbers in the dependent variable")
     }
      if (modelType=="logistic") {
          data[[dep]] <- factor(data[[dep]])
          if (length(levels(data[[dep]]))!=2)
               return("Logistic model requires two levels in the dependent variable")
      } 
     if (modelType=="multinomial")
            data[[dep]] <- factor(data[[dep]])

     data
}


###### confidence intervals ##########
mf.confint<- function(x,...) UseMethod(".confint")

.confint.default<-function(model,level) 
  return(FALSE)

.confint.lm<-function(model,level) 
    return(confint(model,level = level))
  
.confint.glm<-function(model,level) {  
    ci<-confint(model,level = level)
    return(ci)
}

.confint.merModLmerTest<-function(model,level) 
                                return(.confint.lmer(model,level))

.confint.lmer<-function(model,level)  {

      ci<-confint(model,method="Wald")
      ci<-ci[!is.na(ci[,1]),]
      if (is.null(dim(ci)))
          ci<-matrix(ci,ncol=2)
      return(ci)
  }

.confint.multinom <- function (object, level = 0.95, ...) 
  {
  ci<-confint(object,level=level)
  cim<-NULL
  for (i in seq_len(dim(ci)[3]))
     cim<-rbind(cim,(ci[,,i]))
  return(cim)
}

###### post hoc ##########
mf.posthoc<- function(x,...) UseMethod(".posthoc")

.posthoc.default<-function(model,term,adjust) {
  referenceGrid<-lsmeans::lsmeans(model,term,transform = "response")
  summary(pairs(referenceGrid, adjust=adjust))
}

.posthoc.multinom<-function(model,term,adjust) {
  
  dep<-names(attr(terms(model),"dataClass"))[1]
  terms<-paste(term,collapse = ":")
  tterm<-as.formula(paste("~",paste(dep,terms,sep = "|")))  
  referenceGrid<-lsmeans::lsmeans(model,tterm,transform = "response")
  summary(pairs(referenceGrid, by=dep, adjust=adjust))
  }

###### means tabòe ##########
mf.means<- function(x,...) UseMethod(".means")

.means.default<-function(model,term) {
  table<-lsmeans::lsmeans(model,term,transform = "response")
  table<-as.data.frame(summary(table))
  table<-table[,-(1:length(term))]
  colnames(table)<-c("lsmean","se","df","lower","upper")
  table
}

.means.multinom<-function(model,term) {

  dep<-names(attr(terms(model),"dataClass"))[1]
  terms<-paste(term,collapse = ":")
  tterm<-as.formula(paste("~",paste(dep,terms,sep = "|")))  
  table<-lsmeans::lsmeans(model,tterm,transform = "response")
  table<-as.data.frame(summary(table))
  table<-table[,-(2:(1+length(term)))]
  colnames(table)<-c("dep","lsmean","se","df","lower","upper")
  table<-table[order(table$dep),]
  table$dep<-as.character(table$dep)
  table
}