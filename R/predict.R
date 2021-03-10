
### some conditioning function for the jamovi interface, plus numeric values and custom function for R interface

.fixLabels<-function(table,preds,cov_conditioning) {

  aList<-list()
  if ("dep" %in% names(table)) {
    aList[["dep"]]<-levels(table[,"dep"])
  }

  if ("contrast" %in% names(table))
      aList[["contrast"]]<-levels(table[,"contrast"])
  for (p in preds) {
    if (p %in% jmvcore::toB64(cov_conditioning$vars))
      aList[[p]]<-cov_conditioning$labels(p,decode=T)
    else {
      aList[[p]]<-levels(factor(table[,p]))
    }    
  }
  levs<-expand.grid(aList)
  table[,names(levs)]<-levs
  table
}


#### simple Effects estimates: parameters  table and anova table
#### conditioning expects an object of R6 class "conditioning"

rawMeans<- function(x,...) UseMethod(".rawMeans")

.rawMeans.default<-function(model,terms,cov_conditioning=conditioning$new(),interval=95,type="link") {
  nterms<-jmvcore::fromB64(terms) 
  data<-mf.getModelData(model)
  interval=as.numeric(interval)/100
  condlist<-cov_conditioning$values(nterms)
  names(condlist)<-jmvcore::toB64(names(condlist))
  suppressMessages({
    est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type,nesting=NULL)
  })
#  est<-.fixLabels(as.data.frame(est),terms,cov_conditioning)
  est
}

.rawMeans.lmerModLmerTest<-function(model,terms,cov_conditioning=conditioning$new(),interval=95,type="link",df="Satterthwaite") 
               .rawMeans.merModLmerTest(model,terms,cov_conditioning,df) 
    
    
.rawMeans.merModLmerTest<-function(model,terms,cov_conditioning=conditioning$new(),interval=95,type="link",df="Satterthwaite") {
  
  nterms<-jmvcore::fromB64(terms) 
  df<-tolower(df)
  data<-mf.getModelData(model)
  oldw <- getOption("warn")
  options(warn = -1)
  interval=as.numeric(interval)/100
  condlist<-cov_conditioning$values(nterms)
  names(condlist)<-jmvcore::toB64(names(condlist))
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type,lmer.df = df,nesting=NULL)
#  est<-.fixLabels(as.data.frame(est),terms,cov_conditioning)
  options(warn = oldw)
  est
}



pred.means<-function(model,terms,cov_conditioning=conditioning$new(),interval=95,type="response") {
         suppressWarnings(
         est<-rawMeans(model,terms,cov_conditioning,interval=interval,type=type)
         )
         dest<-as.data.frame(est)
         old<-names(dest)
         ## reorder the variable independently of emmeans output
         wide<-dim(dest)[2]
         new<-c(terms,old[(wide-4):wide])
         dest<-dest[,new]
         dd<-.fixLabels(dest,terms,cov_conditioning)
         names(dd)<-c(terms,c("emmean", "SE","df", "lower.CL", "upper.CL"))
         for (nn in names(dd))
            if (is.factor(dd[[nn]]))
               dd[[nn]]<-as.character(dd[[nn]])
         dd
}


##### this is needed to change the contrast in pred.simpleEstimates

.internal.emmc<-function(levs,data=data,variable=variable) {
  # remove possible average covariates values in the levels passed by emmeans
  levs<-sapply(levs,function(b) {
    v<-strsplit(b,",")[[1]]
    v[length(v)]
  })
  # get the contrast weights
  
  codes<-stats::contrasts(data[[variable]])
  # transform the model matrix into the contrast matrix
  n<-length(levs)
  if (attr(data[[variable]],"jcontrast")=="dummy")
    codes<-lf.createContrasts(levs,"simple")  
  M <- as.data.frame(MASS::ginv(t(codes)))
  # set some labels
  names(M) <- lf.contrastLabels(levs,attr(data[[variable]],"jcontrast"))
  attr(M, "desc") <- attr(data[[variable]],"jcontrast")
  M
}



pred.simpleEstimates<- function(x,...) UseMethod(".simpleEstimates")

.simpleEstimates.glmerMod<-function(model,
                                    variable,
                                    moderator,
                                    threeway=NULL,
                                    cov_conditioning=conditioning$new(),
                                    interval=95) {
                                                 .simpleEstimates.glm(model,
                                                                      variable,
                                                                      moderator,
                                                                      threeway=threeway,
                                                                      cov_conditioning=cov_conditioning,
                                                                      interval=interval)
                                    }

.simpleEstimates.glm<-function(model,
                               variable,
                               moderator,
                               threeway=NULL,
                               cov_conditioning=conditioning$new(),
                               interval=95) {
             
           tables<-.simpleEstimates.default(model,
                                            variable,
                                            moderator,
                                            threeway=threeway,
                                            cov_conditioning=cov_conditioning,
                                            interval=interval)
           params<-tables[[1]]
           params$expb<-exp(params$estimate)
           params$lower.ECL<-exp(params$lower.CL) 
           params$upper.ECL<-exp(params$upper.CL)
           tables[[1]]<-params
           tables
}

.simpleEstimates.default<-function(model,
                                   variable,
                                   moderator,
                                   threeway=NULL,
                                   cov_conditioning=conditioning$new(),
                                   interval=95) {
 
               ginfo(paste("simple effects estimation for generic model on",paste(class(model),collapse = " ") ))
               data<-mf.getModelData(model)
               preds<-unlist(c(moderator,threeway))
               lnames<-c("moderator","threeway")[1:length(preds)]
  
        if (is.factor(data[[variable]])) {
                   emm<-rawMeans(model,
                   unlist(c(variable,moderator,threeway)),
                   cov_conditioning = cov_conditioning,
                   interval=interval)
                   est<-emmeans::contrast(emm,method = .internal.emmc,by = preds,data=data,variable=variable)
                   ci<-stats::confint(est,level = interval/100)
    
    ####### rename ci variables because emmeans changes them for different models
                   wci<-dim(ci)[2]
                   ci<-ci[,c(wci-1,wci)]
                   names(ci)<-c("lower.CL","upper.CL")  
    ################################
    
                   params<-cbind(as.data.frame(est),ci)
                   params<-.fixLabels(params,preds,cov_conditioning)
                   params$contrast<-as.character(params$contrast)
                   names(params)[2:(1+length(lnames))]<-lnames

        } else {
                   condlist<-cov_conditioning$values(preds,decode=T)
                   lev<-interval/100
#    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=lev))
#    emmeans::emm_options(ref_grid = list(level = .90))
                   args<-list(model,specs=preds,var=variable,at=condlist,options=list(level=lev))
                   est<-do.call(emmeans::emtrends,args)
                   ci<-as.data.frame(est)
    ####### rename ci variables because emmeans changes them for different models
                   wci<-dim(ci)[2]
                   ci<-ci[,c(wci-1,wci)]
                   names(ci)<-c("lower.CL","upper.CL")  
    ################################
                   params<-cbind(emmeans::test(est),ci)
                   params<-.fixLabels(params,preds,cov_conditioning)
                   names(params)[1:(length(lnames)+1)]<-c(lnames,"estimate")
    }
  

                   ff<-emmeans::test(est,joint=T,by=preds)
                   ff<-.fixLabels(ff,preds,cov_conditioning)
  ### For some model emmeans returns F with df2=Inf, which means Chi-squared/df. Let's make it explicit
                   if (ff$df2[1]==Inf) {
                        ff$df<-ff$df1
                        ff$chisq<-ff$F.ratio*ff$df1
                        ff$df1<-NULL
                        ff$df2<-NULL
                        ff$F.ratio<-NULL
                   }
      

  names(ff)[1:length(lnames)]<-lnames
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  if (.which.class(model)=="lm") {
    ff$etaSqP<-ff$F.ratio*ff$df1/(ff$F.ratio*ff$df1+ff$df2)
    ff$omegaSq<-(ff$F.ratio-1)*ff$df1/(ff$F.ratio*ff$df1+ff$df2+1)
    ff$omegaSq[ff$omegaSq<0]<-0  
    ff$epsilonSq<-(ff$F.ratio-1)*ff$df1/(ff$F.ratio*ff$df1+ff$df2)
    ff$epsilonSq[ff$epsilonSq<0]<-0  
    
    ## eta squared
    SS<-stats::anova(model)
    SSerr<-stats::sigma(model)^2*model$df.residual
    SStot<-sum(SS$`Sum Sq`)
    SSe<-(ff$etaSqP/(1-ff$etaSqP))*SSerr
    ff$etaSq<-SSe/SStot
    

  }
    
  list(params,ff)
}

.simpleEstimates.multinom<-function(model,variable,moderator,threeway=NULL,
                                   cov_conditioning=conditioning$new(),
                                   interval=95) {
 
  
  ginfo(paste("simple effects estimation for multinomial model on",paste(class(model),collapse = " ") ))
  data<-mf.getModelData(model)
  preds<-unlist(c(moderator,threeway))
  vars<-unlist(c(variable,moderator,threeway))
  lnames<-c("moderator","threeway")[1:length(preds)]
  ciWidth<-interval/100
  dep<-names(attr(stats::terms(model),"dataClasses"))[1]


    preds_int<-jmvcore::composeTerm(vars)
    preds_form<-stats::as.formula(paste("~",dep,"|",preds_int))
    condlist<-cov_conditioning$values(vars,decode = T)
    params<-data.frame()
    ######## for multinom emmeans::test does not work well, so 
    ####### we use the centering method
    .vals<-list()
    .levs<-list()
    for (pred in preds)
      if (pred %in% jmvcore::toB64(cov_conditioning$vars)) {
        .vals[[pred]]<-unlist(cov_conditioning$values(pred,decode = T))
        .levs[[pred]]<-cov_conditioning$labels(pred,decode = T)
      }
      else {
        .vals[[pred]]<-1:length(levels(data[[pred]]))
        .levs[[pred]]<-levels(data[[pred]])
      }

    vals<-expand.grid(.vals)
    form<-model$call$formula
    results<-NULL
    params<-NULL
    if (is.factor(data[[variable]]))
       contrast<-paste(variable,1:(length(levels(data[[variable]]))-1),sep="_._._")
    else
       contrast<-variable
    for (l in 1:nrow(vals)) {
      .data<-data
      for (name in preds)
        if (is.factor(.data[[name]]))
          stats::contrasts(.data[[name]])<-stats::contr.treatment(length(unique(vals[[name]])),base=vals[[name]][l])
        else
          .data[[name]]<-.data[[name]]-vals[[name]][l]
        
        .model<-nnet::multinom(form,data=.data,model = T)
        ano<-car::Anova(.model,type=3)
        ano<-ano[rownames(ano)==variable,]
        results<-rbind(results,ano)
        param<-mf.summary(.model)
        citry<-try({
          ci<-mf.confint(.model,level=ciWidth)
          colnames(ci)<-c("lower.CL","upper.CL")
          param<-cbind(ci,param) 
        })
        param<-param[param$variable %in% contrast,]
        params<-rbind(params,param)
    }
    
    results<-as.data.frame(results)
    names(results)<-c("chisq","df","p.value")
    results<-cbind(vals,results)
    ff<-.fixLabels(results,preds,cov_conditioning)
    .levs[["dep"]]<-levels(factor(params[["dep"]]))
    .levs<-.levs[c("dep",preds)]
    levs<-expand.grid(.levs)
    params<-cbind(levs,params)
    names(ff)[1:length(lnames)]<-lnames
    names(params)[2:(length(lnames)+1)]<-lnames
    names(params)[-(1:(length(lnames)+1))]<-c("lower.CL","upper.CL", "estimate","dep","variable","SE","expb","z.ratio","p.value")
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  params[,"dep"]<-as.character(params[,"dep"])
  list(params,ff)
  
}

pred.predictRandom<-function(model,
                             dep64,
                             groupName64,
                             cluster64,
                             linesName64=NULL,
                             plotsName64=NULL,
                             type="response") {
  
  data<-model@frame
  preds64<-c(groupName64,linesName64,plotsName64)
  gclass<-class(data[[groupName64]])
  # here we set all model predictors but the x-axis variable to zero
  # to smooth random effects predicted values
  mvars<-names(data)
  tozero<-setdiff(mvars,c(groupName64,cluster64,dep64))
  newdata<-data
  for(v in tozero)
    if (!is.factor(newdata[,v]))
      newdata[,v]<-0
  
  pd<-stats::predict(model,type="response",newdata=newdata)
  # end of zeroing 
    
  randomData<-as.data.frame(cbind(pd,data[,c(cluster64,groupName64)]))
  pnames<-c("group","lines","plots")
  names(randomData)<-c("y","cluster",pnames[1:length(preds64)])
  randomData  
}

.predictRandom_continuous<-function(model,
                                    groupName,
                                    cluster,
                                    linesName=NULL,
                                    plotsName=NULL,
                                    type="response") {
  
  preds<-c(groupName,linesName,plotsName)
  data<-model@frame
     
  .coefs<-stats::coefficients(model)
  .ccoefs<-.coefs[[cluster]]
  .where<-grep(groupName,names(.ccoefs),fixed = T)
  .num_coefs<-.ccoefs[,c(1,.where)]
  nterms<-length(.num_coefs)-1
  FUN<-stats::family(model)$linkinv
  if (type!="response")
    FUN<-identity
  res<-lapply(rownames(.num_coefs),function(row) {
    .data<-data[data[[cluster]]==row,]
    .x<-.data[,groupName]
    .m<-matrix(rep(.x,nterms),ncol = nterms)
     x<-cbind(1,.m)
     pr<-as.numeric(x%*%.num_coefs)
    .newdata<-.data[,preds]
    .newdata$y<-FUN(pr)
    .newdata$cluster=row
    .newdata[,c("y","cluster",preds)]
  })
  do.call("rbind",res)
  
}
