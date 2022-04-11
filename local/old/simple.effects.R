

### some conditioning function for the jamovi interface, plus numeric values and custom function for R interface

.fixGridNames<-function(grid,lablist,labels) {
    mark(labels)
    if (labels=="labels") { 
         for (labvar in names(lablist)) {
              onevar<-lablist[[labvar]]
              for (i in seq_along(onevar)) {
              grid[grid[,labvar]==onevar[i],labvar]<-names(onevar)[i]
              }
         }
    }
  if (labels=="values_labels") { 
    for (labvar in names(lablist)) {
      onevar<-lablist[[labvar]]
      for (i in seq_along(onevar)) {
        grid[grid[,labvar]==onevar[i],labvar]<-paste0(names(onevar)[i],"=",onevar[i])
      }
    }
  }
  
  return(grid)
  
}    


    .condition_values<-function(datavar,conditioning,span) {
      
           if (is.numeric(conditioning))
                       return(conditioning)
      
           fun<-try(get(conditioning),silent = T)
           if (!isError(fun)) 
             return(fun(datavar))
           
           if (conditioning=="mean") {
             .mean<-mean(datavar)
             return(round(c("Mean"=.mean),digits = 3))
           }
           
           if (conditioning=="mean_sd") {
               .mean<-mean(datavar)
               .sd<-sd(datavar)
               return(round(c("Mean-SD"=.mean-.sd,"Mean"=.mean,"Mean+SD"=.mean+.sd),digits = 3))
           }
           
           if (conditioning=="mean_offset") {
             .mean<-mean(datavar)
             vec<-round(c(.mean-span,.mean,.mean+span),digits = 3)
             vecnames<-c(paste0("Mean-",vec[1]),"Mean",paste0("Mean+",vec[3]))
             names(vec)<-vecnames
             return(vec)
           }
           
           if (conditioning=="percent") 
             return(round(quantile(datavar,c(.25,.5,.75)),digits=3))

           if (conditioning=="percent_offset") {
             return(round(quantile(datavar,c(.5-span,.5,.5+span)),digits=3))
           }
  
}

    
.condition_list<-function(preds,data,conditioning,span) {    
    for (name in preds)
      if (!is.factor(data[[name]])) {
        .cond<-conditioning
        if (is.list(conditioning) && (name %in% names(conditioning)))
                .cond<-conditioning[[name]]
        mlevels<-.condition_values(data[[name]],conditioning=.cond,span=span)
        condlist[[name]]<-as.numeric(mlevels)
        lablist[[name]]<-mlevels
      }  
  return(list(condlist,lablist))
}
#### simple Effects estimates: parameters  table and anova table


simpleEstimates<-function(model,variable,moderator,threeway=NULL,
                           conditioning="mean_sd",
                           span=0,
                           interval=95,
                           labels="numbers") {
  data<-mf.getModelData(model)
  interval=as.numeric(interval)/100
  ### prepare the conditioning values
  condlist<-list()
  lablist<-list()
  
  lnames<-"moderator"
  if (!is.null(unlist(threeway)))
     lnames<-c("moderator","threeway")
  
  preds<-unlist(c(moderator,threeway))
  temp<-.condition_list(preds,data,conditioning,span) 
  condlist<-temp[[1]]
  lablist<-temp[[2]]
  
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
    est<-emmeans::emmeans(model,specs=variable,by=preds,at=condlist)
    est<-emmeans::contrast(est,method=".internal")
    
    est@grid<-.fixGridNames(est@grid,lablist,labels)    
    
    ci<-as.data.frame(confint(est,level = interval))
    params<-cbind(as.data.frame(est),ci[,c("lower.CL","upper.CL")])
    params$contrast<-as.character(params$contrast)
    names(params)[2:(length(lnames)+1)]<-lnames
    
  } else {
    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=.80))
    estf<-as.data.frame(est)
    ci<-estf[,c("lower.CL","upper.CL")]
    params<-cbind(emmeans::test(est),ci)
    names(params)[1:length(lnames)]<-lnames
    names(params)[length(lnames)+1]<-"estimate"
  }

  ff<-emmeans::test(est,joint=T,by=preds)
  names(ff)[1:length(lnames)]<-lnames
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  list(params,ff)
}
