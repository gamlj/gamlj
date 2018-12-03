

### some conditioning function for the jamovi interface, plus numeric values and custom function for R interface

.fixLabels<-function(table,preds,cov_conditioning) {

  aList<-list()
  if ("dep" %in% names(table))
    aList[["dep"]]<-levels(table[,"dep"])
  
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
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type,nesting=NULL)
#  est<-.fixLabels(as.data.frame(est),terms,cov_conditioning)
  est
}

.rawMeans.lmerModLmerTest<-function(model,terms,cov_conditioning=conditioning$new(),interval=95,type="link",df="Satterthwaite") 
               .rawMeans.merModLmerTest(model,terms,cov_conditioning,df) 
    
    
.rawMeans.merModLmerTest<-function(model,terms,cov_conditioning=conditioning$new,interval=95,type="link",df="Satterthwaite") {
  
  nterms<-jmvcore::fromB64(terms) 
  df<-tolower(df)
  data<-mf.getModelData(model)
  interval=as.numeric(interval)/100
  condlist<-cov_conditioning$values(nterms)
  names(condlist)<-jmvcore::toB64(names(condlist))
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type,lmer.df = df,nesting=NULL)
#  est<-.fixLabels(as.data.frame(est),terms,cov_conditioning)
  est
}



pred.means<-function(model,terms,cov_conditioning=conditioning$new(),interval=95) {
         est<-rawMeans(model,terms,cov_conditioning,interval=interval,type="response")
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




pred.simpleEstimates<- function(x,...) UseMethod(".simpleEstimates")

.simpleEstimates.default<-function(model,variable,moderator,threeway=NULL,
           cov_conditioning=conditioning$new(),
           interval=95) {
  
  mark(paste("simple effects estimation for generic model on",paste(class(model),collapse = " ") ))
  data<-mf.getModelData(model)
  preds<-unlist(c(moderator,threeway))
  lnames<-c("moderator","threeway")[1:length(preds)]
  
  if (is.factor(data[[variable]])) {
    
    .internal.emmc<<-function(levs) {
      # remove possible average covariates values in the levels passed by emmeans
      levs<-sapply(levs,function(b) {
        v<-strsplit(b,",")[[1]]
        v[length(v)]
      })
      # get the contrast weights
                
      codes<-contrasts(data[[variable]])
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
    emm<-rawMeans(model,
                   unlist(c(variable,moderator,threeway)),
                   cov_conditioning = cov_conditioning,
                   interval=interval)
    est<-emmeans::contrast(emm,method = ".internal",by = preds)
    ci<-confint(est,level = interval/100)
    
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
    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=interval/100))
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
  list(params,ff)
}

.simpleEstimates.multinom<-function(model,variable,moderator,threeway=NULL,
                                   cov_conditioning=conditioning$new(),
                                   interval=95) {
 
  
  mark(paste("simple effects estimation for multinomial model on",paste(class(model),collapse = " ") ))
  data<-mf.getModelData(model)
  preds<-unlist(c(moderator,threeway))
  vars<-unlist(c(variable,moderator,threeway))
  lnames<-c("moderator","threeway")[1:length(preds)]
  
  dep<-names(attr(terms(model),"dataClasses"))[1]
  

    .internal.emmc<<-function(levs) {
      # remove possible average covariates values in the levels passed by emmeans
      levs<-sapply(levs,function(b) {
        v<-strsplit(b,",")[[1]]
        v[length(v)]
      })
      if (!is.factor(data[[variable]])) {
        vsd<-sd(data[[variable]])
        M<-as.data.frame(c(-.5/vsd,0,.5/vsd))
        names(M)<-"Slope"
      } else {
      # get the contrast weights
      codes<-contrasts(data[[variable]])
      # transform the model matrix into the contrast matrix
      n<-length(levs)
      M <- as.data.frame(MASS::ginv(t(codes)))
      # set some labels
      names(M) <- lf.contrastLabels(levs,attr(data[[variable]],"jcontrast"))
      attr(M, "desc") <- attr(data[[variable]],"jcontrast")
      }
      M
    }
    preds_int<-jmvcore::composeTerm(list(vars))
    preds_form<-as.formula(paste("~",dep,"|",preds_int))
    condlist<-cov_conditioning$values(vars,decode = T)
    emm<-emmeans::emmeans(model,specs=preds_form,mode="latent",at=condlist,nesting=NULL)
    emm<-update(emm,df=Inf)
    est<-emmeans::contrast(emm,interaction = c("trt.vs.ctrl1",".internal") ,by = preds)
    ci<-confint(est,level = interval/100,df=Inf)
    
    ####### rename ci variables because emmeans changes them for different models
    wci<-dim(ci)[2]
    ci<-ci[,c(wci-1,wci)]
    names(ci)<-c("lower.CL","upper.CL")  
    ################################
    
    params<-cbind(as.data.frame(est),ci)
    levs<-levels(data[[dep]])
    names(params)[1:2]<-c("dep","contrast")
    names(params)[3:(2+length(preds))]<-preds
    params<-.fixLabels(params,preds,cov_conditioning)
    params$contrast<-as.character(params$contrast)
    names(params)[3:(2+length(lnames))]<-lnames

    ######## for multinom emmeans::test does not work well, so 
    ####### we use the centering method
    .vals<-list()
    for (pred in preds)
      if (pred %in% jmvcore::toB64(cov_conditioning$vars))
        .vals[[pred]]<-unlist(cov_conditioning$values(pred,decode = T))
      else
        .vals[[pred]]<-1:length(levels(data[[pred]]))
    vals<-expand.grid(.vals)
    form<-model$call$formula
    results<-NULL
    for (l in 1:nrow(vals)) {
      .data<-data
      for (name in preds)
        if (is.factor(.data[[name]]))
          contrasts(.data[[name]])<-contr.treatment(length(unique(vals[[name]])),base=vals[[name]][l])
        else
          .data[[name]]<-.data[[name]]-vals[[name]][l]
        
        .model<-nnet::multinom(form,data=.data,model = T)
        ano<-car::Anova(.model,type=3)
        ano<-ano[rownames(ano)==variable,]
        results<-rbind(results,ano)
    }
    results<-as.data.frame(results)
    names(results)<-c("chisq","df","p.value")
    results<-cbind(vals,results)    
    ff<-.fixLabels(results,preds,cov_conditioning)

  names(ff)[1:length(lnames)]<-lnames
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  params[,"dep"]<-as.character(params[,"dep"])

  #### return both tables ##
  list(params,ff)
  
}




