
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
        .anova<-car::Anova(model,test="F",type=3, singular.ok=T)
        .anova<-.anova[!(rownames(.anova) %in% c("(Intercept)")),]
        anovatab<-.anova
        colnames(anovatab)<-c("ss","df","f","p")
        effss<-anovatab[!(rownames(anovatab) %in% c("Residuals")),]
        reds<-list(ss=anovatab$ss[rownames(anovatab)=="Residuals"],df=anovatab$df[rownames(anovatab)=="Residuals"])
        sumr<-summary(model)

        ### whole model ###
        f<-sumr$fstatistic[[1]]
        edf<-sumr$fstatistic[[3]]
        mdf<-sumr$fstatistic[[2]]
        p<-stats::pf(f,mdf,edf,lower.tail = F)
        modeta<-effectsize::F_to_eta2(f,mdf,edf)
        modomega<-effectsize::F_to_omega2(f,mdf,edf)
        modepsilon<-effectsize::F_to_epsilon2(f,mdf,edf)
        modss<-f*reds$ss*mdf/edf
        mods<-list(ss=modss,
               df= mdf,
               f=f,
               p=p,
               etaSq=modeta[[1]],etaSqP=modeta[[1]],
               omegaSq=modomega[[1]],
               omegaSqP=modomega[[1]],
               epsilonSq=modepsilon[[1]],
               epsilonSqP=modepsilon[[1]])

        tots<-list(ss=mods$ss+reds$ss,df=mdf)

        #####
        # Here we need a correct to the computation of the effect sizes. To compute the non-partial indeces
        ## effectsize:: package uses as total sum of squares the sum of the effects SS (plus residuals)
        ## In unbalanced designs, the sum does not necessarely correspond to the model SS (plus residuals)
        ## so the estimation is biased. Eta-squared does not correspond to semi-partial r^2 any more
        ## and many properties of the non-partial indices are broken. 
        ## Thus, we fixed it by adding a bogus effect whose SS is exactly the discrepancy betweem
        ## the table SS and the model+error SS. In this way, the estimation uses the correct total SS
        #####
        diff<-mods$ss-sum(effss$ss)
        add<-data.frame(diff,1,1,0)
        names(add)<-names(.anova)
        .anova<-rbind(.anova,add)
        last<-dim(effss)[1]+1
        etap<-effectsize::eta_squared(.anova,partial = T,verbose = F)
        eta<-effectsize::eta_squared(.anova,partial = F,verbose = F)
        eps<-effectsize::epsilon_squared(.anova,partial = T,verbose = F)
        omegap<-effectsize::omega_squared(.anova,partial = T,verbose = F)
        omega<-effectsize::omega_squared(.anova,partial = F,verbose = F)
        epsilonp<-effectsize::epsilon_squared(.anova,partial = T,verbose = F)
        epsilon<-effectsize::epsilon_squared(.anova,partial = F,verbose = F)
        
        effss$etaSq<-eta[-last,2]
        effss$etaSqP<-etap[-last,2]
        effss$omegaSq<-omega[-last,2]
        effss$omegaSqP<-omegap[-last,2]
        effss$epsilonSq<-epsilon[-last,2]
        effss$epsilonSqP<-epsilonp[-last,2]
        
        reslist<-listify(effss)
        reslist<-append_list(reslist,reds,"Residuals")
        reslist<-append_list(reslist,tots,"Totals")
        reslist<-prepend_list(reslist,mods,"Model")
    reslist
}

.anova.glmerMod<-function(model,df="Satterthwaite") {
  ginfo("using .anova.glmerMod")
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
    return(list(family="nb",link="log"))

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
  
     clusterdata<-NULL
     if (is.something(cluster)) {
         clusterdata<-data[[jmvcore::toB64(cluster)]]
     }
     
     dep=jmvcore::toB64(options$dep)
     ########## check the dependent variable ##########
     if (modelType %in% c("linear","mixed")) {
       ### here I need the dv to be really numeric
       data[[dep]] <- as.numeric(as.character(data[[dep]]))

       if ("dep_scale" %in% names(options)) 
           data[[dep]]<-lf.scaleContinuous(data[[dep]],options$dep_scale,by=clusterdata)
     }
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
     
     for (cov in covs) {
       cov64<-jmvcore::toB64(cov) 
       data[[cov64]]<-as.numeric(as.character(data[[cov64]]))
       if (any(is.na(data[[cov64]])))
          return(paste("Covariate",cov, "cannot be converted to a numeric variable"))
       w<-which(scalingVars==cov)
       type<-ifelse(is.something(w),scaling[[w]][['type']],"centered")
       if (length(data[[cov64]])>0)
            data[[cov64]]<-lf.scaleContinuous(data[[cov64]],type,by=clusterdata)  

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
     ### this is a check for the clusterbase bug
#     if (length(cluster)>0 & length(covs)> 0) {
#         check<-round(tapply(data[[jmvcore::toB64(covs[[1]])]], data[[jmvcore::toB64(cluster)]], mean),digits=5)
#         mark(check)
#     }
     
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
         parameters<-merge(ci,parameters,by="row.names",all.y=TRUE)
         parameters<-parameters[order(parameters$order),]
         parameters$order<-NULL
    }
    attr(parameters,"warning")<-att
  }
  return(parameters)
  
}
.confint.lm<-function(model,level,parameters)  {
    ci<-try(stats::confint(model,level=level))
    ci<-data.frame(ci)
    colnames(ci)<-c("lower.CL","upper.CL")
   .confint.format(ci,parameters)
}
.confint.glm<-function(model,level,parameters) {  
     ci<-try(stats::confint(model,level = level))
     if (jmvcore::isError(ci)) 
       return(.confint.format(ci,parameters))
     if (is.null(dim(ci)))
       ci<-matrix(ci,ncol=2)
     ci<-data.frame(ci)
     colnames(ci)<-c("lower.CL","upper.CL")
     ci["lower.ECL"]<-exp(ci["lower.CL"])     
     ci["upper.ECL"]<-exp(ci["upper.CL"])
     .confint.format(ci,parameters)
    
}

.confint.merModLmerTest<-function(model,level,parameters,method="wald") 
                                return(.confint.lmer(model,level,parameters,method=method))

.confint.lmerModLmerTest<-function(model,level,parameters,method="wald") 
  return(.confint.lmer(model,level,parameters,method=method))

.confint.lmer<-function(model,level,parameters,method="wald")  {
     if (method=="wald")
               method<-"Wald"
     
      ci<-try(stats::confint(model,method=method,level=level))
      if (jmvcore::isError(ci)) 
        return(.confint.format(ci,parameters))
      
      ci<-ci[!is.na(ci[,1]),]
      if (is.null(dim(ci)))
          ci<-matrix(ci,ncol=2)
      ci<-data.frame(ci)
      colnames(ci)<-c("lower.CL","upper.CL")
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
  colnames(ci)<-c("lower.CL","upper.CL")
  ci["lower.ECL"]<-exp(ci["lower.CL"])     
  ci["upper.ECL"]<-exp(ci["upper.CL"])
  .confint.format(ci,parameters)
}


.confint.multinom <- function (object, level, parameters) 
  {
  ci<-stats::confint(object,level=level)
  cim<-NULL
  for (i in seq_len(dim(ci)[3]))
     cim<-rbind(cim,(ci[,,i]))
  cim<-as.data.frame(cim)
  rownames(cim)<-1:length(cim[,1])
  rownames(parameters)<-1:length(cim[,1])
  colnames(cim)<-c("lower.CL","upper.CL")
  cim["lower.ECL"]<-exp(cim["lower.CL"])     
  cim["upper.ECL"]<-exp(cim["upper.CL"])
  .confint.format(cim,parameters)

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


########### set the model call for R in a reasonable way #########

mf.setModelCall<- function(x,...) UseMethod(".setModelCall")

.setModelCall.default<-function(model,info) {
  coptions<-paste(names(info$coptions),info$coptions,sep="=",collapse = ",")
  call<-paste0(info$command,"(",coptions,", data=data)")
  model$call<-call
  model
}

.setModelCall.lmerMod<-function(model,info) {
  coptions<-paste(names(info$coptions),info$coptions,sep="=",collapse = ",")
  call<-paste0(info$command,"(",coptions,", data=data)")
  model@call<-as.call(str2lang(call))
  model
}
.setModelCall.glmerMod<-function(model,info) 
        .setModelCall.lmerMod(model,info)



#######################

mf.savePredRes<-function(options,results,model) {

        if (options$predicted && results$predicted$isNotFilled()) {
            ginfo("Saving predicted")
            if ("multinom" %in% class(model))  type="probs" else type="response"
            p<-stats::predict(model,type=type)
  # we need the rownames in case there are missing in the datasheet
            pdf <- data.frame(predicted=p, row.names=rownames(mf.getModelData(model)))
            results$predicted$setValues(p)
              }
        if (options$residuals && results$residuals$isNotFilled()) {
            ginfo("Saving residuals")
            p<-stats::resid(model)
  # we need the rownames in case there are missing in the datasheet
            pdf <- data.frame(residuals=p, row.names=rownames(mf.getModelData(model)))
            results$residuals$setValues(pdf)
        }
}


