

### some conditioning function for the jamovi interface, plus numeric values and custom function for R interface

.fixLabels<-function(table,preds,cov_conditioning,repeated=F) {

  aList<-list()
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
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type)
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
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type,lmer.df = df)
#  est<-.fixLabels(as.data.frame(est),terms,cov_conditioning)
  est
}



pred.means<-function(model,terms,cov_conditioning=conditioning$new(),interval=95) {
         est<-rawMeans(model,terms,cov_conditioning,interval=interval,type="response")
         dd<-.fixLabels(as.data.frame(est),terms,cov_conditioning)
         for (nn in names(dd))
            if (is.factor(dd[[nn]]))
               dd[[nn]]<-as.character(dd[[nn]])
         dd
}




pred.simpleEstimates<- function(x,...) UseMethod(".simpleEstimates")

.simpleEstimates.default<-function(model,variable,moderator,threeway=NULL,
           cov_conditioning=conditioning$new(),
           interval=95) {
  
  mark("simple effects estimation for generic model")
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
    params<-cbind(as.data.frame(est),ci[,c("lower.CL","upper.CL")])
    params<-.fixLabels(params,preds,cov_conditioning,repeated = T)
    params$contrast<-as.character(params$contrast)
    names(params)[2:(1+length(lnames))]<-lnames
    
  } else {
    condlist<-cov_conditioning$values(preds,decode=T)
    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=interval/100))
    estf<-as.data.frame(est)
    ci<-estf[,c(dim(estf)[2]-1,dim(estf)[2])]
    params<-cbind(emmeans::test(est),ci)
    params<-.fixLabels(params,preds,cov_conditioning)
    names(params)[1:(length(lnames)+1)]<-c(lnames,"estimate")
    
  }
  ff<-emmeans::test(est,joint=T,by=preds)
  ff<-.fixLabels(ff,preds,cov_conditioning)
  
  names(ff)[1:length(lnames)]<-lnames
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  list(params,ff)
}
