
##### function to check results ####

.which.class<-function(model) {
  if (class(model)[[1]]=="glm")
    return("glm")
  if ("lm" %in% class(model))
      return("lm")
  if ("lmerModLmerTest" %in% class(model))
    return("lmer")
  if ("merModLmerTest" %in% class(model))
      return("lmer")
  if ("lmerMod" %in% class(model))
      return("lmer")
  if ("glmerMod" %in% class(model))
    return("lmer")
  
  if ("multinom" %in% class(model))
    return("multinomial")
  
}

###### model estimation and checks ##############



##### get model data #########

mf.getModelData<- function(x,...) UseMethod(".getModelData")

.getModelData.default<-function(model) {
     return(model$model)
}

.getModelData.glmerMod<-function(model) 
  return(.getModelData.lmer(model))

.getModelData.glmer<-function(model) 
  return(.getModelData.lmer(model))

.getModelData.merModLmerTest<-function(model) 
     return(.getModelData.lmer(model))

.getModelData.lmerModLmerTest<-function(model) 
  return(.getModelData.lmer(model))

.getModelData.lmer<-function(model) 
  return(model@frame)





############# produces summary in a somehow standard format ##########

mf.summary<- function(x,...) UseMethod(".mf.summary")

.mf.summary.default<-function(model) {
      return(FALSE)
}

.fix_coeffs<-function(parameters, coeffs) {

  if (length(coeffs)==dim(parameters)[1]) {
    return(as.data.frame(parameters))
  }
  
  ### fix missing coefficients ########
  all_coefs<-data.frame(all=coeffs)
  rownames(all_coefs)<-names(coeffs)
  all_coefs$order<-1:length(all_coefs$all)
  res<-merge(parameters,all_coefs,by="row.names",all=T)
  res<-res[order(res$order),]
  rownames(res)<-res$Row.names
  res[,c("Row.names", "order","all")]<-NULL
  ########################
  res
}
  
.mf.summary.lm<-function(model){
  smr<-summary(model)
  params<-smr$coefficients
  params<-.fix_coeffs(params,smr$aliased)
  if (nrow(params)[1]==0)
    return(as.data.frame(NULL))
  params$df<-smr$df[2]
  colnames(params)<-c("estimate","se","t","p","df")
  params
  ###########################à
  as.data.frame(params,stringsAsFactors = F)
}

.mf.summary.merModLmerTest<-function(model)
       .mf.summary.lmer(model)

.mf.summary.lmerModLmerTest<-function(model)
  .mf.summary.lmer(model)

.mf.summary.glmerMod<-function(model) {
  smr<-summary(model)
  params<-stats::coef(smr)
  all_coefs<-lme4::fixef(model,add.dropped=T)
  params<-.fix_coeffs(params,all_coefs)
  if (nrow(params)[1]==0)
    return(as.data.frame(NULL))
  expb<-exp(params[,"Estimate"])  
  params<-cbind(params,expb)
  colnames(params)<-c("estimate","se","z","p","expb")
  as.data.frame(params,stringsAsFactors = F)
}
.mf.summary.lmerMod<-function(model)
  .mf.summary.lmer(model)

.mf.summary.lmer<-function(model) {
       ss<-summary(model)$coefficients
       ss<-as.data.frame(ss,stringsAsFactors = F)
       
      if (dim(ss)[2]==3) {
        
          ano<-car::Anova(model,test="F",type=3)
          lnames<-rownames(ss)
          matching<-which(lnames %in% rownames(ss))
          ss$df<-NA
          ss$df[matching]<-ano$Df.res[matching]
          ss$p<-NA
          ss$p[matching]<-ano$`Pr(>F)`[matching]
          ss<-ss[,c(1,2,4,3,5)]       
          if (any(is.null(ss$df)))
               attr(ss,"warning")<-"lmer.df"
      }
      colnames(ss)<-c("estimate","se","df","t","p")
      ss
}

.mf.summary.glm<-function(model) {
     smr<-summary(model)
     params<-smr$coefficients
     params<-.fix_coeffs(params,smr$aliased)
     if (nrow(params)[1]==0)
        return(as.data.frame(NULL))
     expb<-exp(params[,"Estimate"])  
     params<-cbind(params,expb)
     colnames(params)<-c("estimate","se","z","p","expb")
     as.data.frame(params,stringsAsFactors = F)
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
     cof$p<-(1 - stats::pnorm(abs(cof$z), 0, 1)) * 2
     ss<-as.data.frame(cof,stringsAsFactors = F)
     ss<-cof[order(ss$dep),]
     lab<-model$lab[1]
     ss$dep<-sapply(ss$dep, function(a) paste(a,"-",lab))
     ss
}
  
############# produces anova/deviance table in a somehow stadard format ##########
mf.anova<- function(x,...) UseMethod(".anova")

.anova.default<-function(model) {
 stop("no suitable model found") 
}
  
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
    if (length(grep("Intercept",rownames(ano),fixed=T))>0)
        dss<-ano[-1,]
    else
        dss<-ano
    
    sumr<-summary(model)
    errDF<-sumr$fstatistic[3]
    modDF<-sumr$fstatistic[2]
    modF<-sumr$fstatistic[1]
    errSS<-dss$ss[length(dss$ss)]
    errMS<-errSS/errDF
    r2<-sumr$r.squared
    totalSS<-1/((1-r2)*(1/errSS))
    modSS<-totalSS-errSS
    modp<-1 - stats::pf(modF, modDF, errDF) 
    modRow<-c(ss=modSS,df=modDF,F=modF,p=modp)
    ss<-rbind(modRow,dss)
    ss$ms<-ss$ss/ss$df
    ss$etaSq<-ss$ss/(totalSS)
    ss$etaSqP <- ss$ss / (ss$ss + errSS)
    ss$omegaSq <- (ss$ss - (ss$df * errMS)) / (totalSS + errMS)
    lt<-length(ss$df)
    ss[lt,c("f","p","etaSq","etaSqP","omegaSq")]<-NA
    ss
}

.anova.glmerMod<-function(model) {
  ginfo(".anova.glmerMod")
  ano<-.car.anova(model)
  names(ano)<-c("test","df1","p")
  ano  
}
  

.anova.lmerModLmerTest<-function(model,df="Satterthwaite") 
   .anova.merModLmerTest(model,df) 
     
.anova.lmerMod<-function(model,df) 
       .anova.merModLmerTest(model,df) 
         
.anova.merMod<-function(model,df="Satterthwaite") 
           .anova.merModLmerTest(model,df) 
             
.anova.merModLmerTest<-function(model,df="Satterthwaite") {

  ano<-stats::anova(model,ddf=df)
        
  if (dim(ano)[1]==0)
    return(ano)
  
  if (dim(ano)[2]==4) {
    ano<-.car.anova(model,df)
  } else {
  ano<-ano[,c(5,3,4,6)]
  attr(ano,"method")<-df
  attr(ano,"statistic")<-"F"
  }
  if (!all(is.na(ano[,3])==F)) {
    # lmerTest 2.0-33 does not produce the F-test for continuous IV if they appear in the model before
    # the factors. Here we try to fix it.
    ginfo("lmerTest problems with df: try to fix it")
    whichone<-is.na(ano[,3])
    rn<-rownames(ano[whichone,])
    smr<-summary(model,ddf=df)
    coefz<-as.data.frame(smr$coefficients)
    coefz<-coefz[rownames(smr$coefficients) %in% rn,3:5]

    if (dim(coefz)[1]!=length(rn))
      ano<-.car.anova(model,df)
    else {
        coefz<-data.frame(coefz)
        
        if (dim(coefz)[2]==1)
          coefz<-as.data.frame(t(coefz))
        rownames(coefz)<-rn
        coefz[,2]<-coefz[,2]^2
        coefz$df1<-1
        coefz<-coefz[,c(2,4,1,3)]
        ano[rownames(ano) %in% rownames(coefz),]<-coefz
        attr(ano,"method")<-df
        attr(ano,"statistic")<-"F"
    }
  }
  # here we fix the column names depending on the estimation that succeeded
  if (attr(ano,"statistic")=="Chisq")
     names(ano)<-c("test","df1","p")
  else 
    names(ano)<-c("test","df1","df2","p")
  
  return(ano)
  
}

.car.anova<-function(model,df) {
  ginfo("mf.anova uses car::Anova")
  if (model@devcomp$dims["REML"]==0) 
    test<-"Chisq"
  else test<-"F"
  ano<-car::Anova(model,type=3,test=test)
  if (attr(stats::terms(model),"intercept")==1)
    ano<-ano[-1,]
  attr(ano,"method")<-"Kenward-Roger"
  attr(ano,"statistic")<-test
  ano
}


mf.getModelFactors<-function(model) {
  names(attr(stats::model.matrix(model),"contrasts"))
} 


mf.getModelMessages<-function(model) {
  message<-list(conv=TRUE,msg=NULL,singular=FALSE)
  if (.which.class(model)=="lmer") {
      message['msg']=model@optinfo$conv$lme4$messages
      message["singular"]<-lme4::isSingular(model,tol = 1e-04)
      if (message["singular"]==TRUE)
         message['msg']="The fit is singular"
  }
  message
}


mf.give_family<-function(modelSelection,custom_family=NULL,custom_link=NULL) {
  if (modelSelection=="linear")
       return(stats::gaussian())
  if (modelSelection=="logistic")
       return(stats::binomial())
  if (modelSelection=="poisson")
    return(stats::poisson())
  if (modelSelection=="multinomial")
    return(list(family="multinomial",link="slogit"))
  if (modelSelection=="nb")
    return(list(family="nb",link="logit"))
  
    if (modelSelection=="poiover")
      return(stats::quasipoisson())
  if (modelSelection=="probit")
    return(stats::binomial("probit"))
  if (modelSelection=="custom")
    return(do.call(custom_family,list(custom_link)))
  
  NULL  
}

mf.checkData<-function(options,data,cluster=NULL,modelType="linear") {
     
     if (!is.data.frame(data))
         jmvcore::reject(data)
  
     dep=jmvcore::toB64(options$dep)
     ########## check the dependent variable ##########
     if (modelType %in% c("linear","mixed"))
       ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))  
     
       if ( any(is.na(data[[dep]])) ) {
          nice=paste0(toupper(substring(modelType,1,1)),substring(modelType,2,nchar(modelType)))
          return(paste(nice,"model requires a numeric dependent variable"))
       }
               
     if  (modelType=="poisson" || modelType=="nb" || modelType=="poiover") {
         ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))
       if (any(data[[dep]]<0))
             return("Poisson-like models require all positive numbers in the dependent variable")
     }
     ####### check the covariates usability and center them##########
     covs=options$covs
     scaling=options$scaling
     scalingVars<-sapply(scaling,function(a) a$var)
     clusterdata<-NULL
     if (is.something(cluster))
           clusterdata<-data[[jmvcore::toB64(cluster)]]
     for (cov in covs) {
       cov64<-jmvcore::toB64(cov) 
       data[[cov64]]<-as.numeric(as.character(data[[cov64]]))
       if (any(is.na(data[[cov64]])))
          return(paste("Covariate",cov, "cannot be converted to a numeric variable"))
       w<-which(scalingVars==cov)
       type<-ifelse(is.something(w),scaling[[w]][['type']],"centered")
       data[[cov64]]<-lf.scaleContinuous(data[[cov64]],type,by=cluster)  

     }
     # check factors
     factors=options$factors
     
     for (factorName in factors) {
       factorName<-jmvcore::toB64(factorName)
       lvls <- base::levels(data[[factorName]])
       if (length(lvls) == 1)
         return(paste("Factor ",jmvcore::fromB64(factorName),"contains only a single level"))
       else if (length(lvls) == 0)
         return(paste("Factor ",jmvcore::fromB64(factorName),"contains no data"))
     }
     
     data
}


###### confidence intervals ##########

mf.confint<- function(x,...) UseMethod(".confint")

.confint.default<-function(model,level,parameters,method=NULL) {
  ginfo("CI model unknown",class(model))  
  return(FALSE)
  
}

.confint.format<-function(ci,parameters) {
  att<-attr(parameters,"warning")
  if (length(ci)==0) {
    att<-append(att,"C.I. cannot be computed")
    attr(parameters,"warning")<-att
    return(parameters)
  }
  if (jmvcore::isError(ci)) {
    message <- jmvcore::extractErrorMessage(ci)
    att<-append(att,paste(message,"C.I. cannot be computed"))
    ci<-NULL
  } else {
    if (any(is.na(ci)))
       att<-append(att,paste("Some C.I. cannot be computed"))
    if (nrow(ci)==nrow(parameters)) {
         parameters<-cbind(parameters,ci)
    } else {
         parameters$order<-1:dim(parameters)[1]
         parameters<-merge(ci,parameters,by="row.names",all=TRUE)
         parameters<-parameters[order(parameters$order),]
         parameters$order<-NULL
    }
#    parameters<-cbind(parameters,ci)
    attr(parameters,"warning")<-att
  }
  return(parameters)
  
}
.confint.lm<-function(model,level,parameters)  {
    ci<-try(stats::confint(model,level=level))
    ci<-data.frame(ci)
    colnames(ci)<-c("cilow","cihig")
   .confint.format(ci,parameters)
}
.confint.glm<-function(model,level,parameters) {  
     ci<-try(stats::confint(model,level = level))
     if (jmvcore::isError(ci)) 
       return(.confint.format(ci,parameters))
     if (is.null(dim(ci)))
       ci<-matrix(ci,ncol=2)
     ci<-data.frame(ci)
     colnames(ci)<-c("cilow","cihig")
     ci["ecilow"]<-exp(ci["cilow"])     
     ci["ecihig"]<-exp(ci["cihig"])
     .confint.format(ci,parameters)
    
}

.confint.merModLmerTest<-function(model,level,parameters) 
                                return(.confint.lmer(model,level,parameters))

.confint.lmerModLmerTest<-function(model,level,parameters) 
  return(.confint.lmer(model,level,parameters))

.confint.lmer<-function(model,level,parameters)  {
      ci<-try(stats::confint(model,method="Wald",level=level))
      if (jmvcore::isError(ci)) 
        return(.confint.format(ci,parameters))
      
      ci<-ci[!is.na(ci[,1]),]
      if (is.null(dim(ci)))
          ci<-matrix(ci,ncol=2)
      ci<-data.frame(ci)
      colnames(ci)<-c("cilow","cihig")
      .confint.format(ci,parameters)
  }

.confint.glmerMod<-function(model,level,parameters,method="wald")  {
  if (method=="wald")
        method="Wald"
  ci<-stats::confint(model,level=level,method=method)
  w<-grep(".sig",rownames(ci),fixed=T,invert = T)
  ci<-ci[w,]
  if (is.null(dim(ci)))
    ci<-matrix(ci,ncol=2)
  ci<-data.frame(ci)
  colnames(ci)<-c("cilow","cihig")
  ci["ecilow"]<-exp(ci["cilow"])     
  ci["ecihig"]<-exp(ci["cihig"])
  .confint.format(ci,parameters)
}


.confint.multinom <- function (object, level = 0.95, ...) 
  {
  ci<-stats::confint(object,level=level)
  cim<-NULL
  for (i in seq_len(dim(ci)[3]))
     cim<-rbind(cim,(ci[,,i]))
  return(cim)
}



########### to be removed and update with mi.xxx #########


mf.aliased<- function(x,...) UseMethod(".aliased")

.aliased.default<-function(model) {
  aliased<-stats::alias(model)
  (!is.null(aliased$Complete))
}

.aliased.glmerMod<-function(model) {
  rank<-attr(model@pp$X,"msgRankdrop")
  return((!is.null(rank)))
}

.aliased.lmerMod<-function(model) {
  rank<-attr(model@pp$X,"msgRankdrop")
  return((!is.null(rank)))
}
.aliased.multinom<-function(model) {
  ### to do 
  FALSE
  
}
