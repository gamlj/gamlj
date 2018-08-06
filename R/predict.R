

### some conditioning function for the jamovi interface, plus numeric values and custom function for R interface

.fixGridNames<-function(grid,lablist,type) {
    # here we replace labels from values if labels are available: if a variable in lablist is a named list, 
    # names(variables) represents the labels. Otherwise, values are returned 
    if (type=="labels") { 
         for (labvar in names(lablist)) {
              onevar<-lablist[[labvar]]
              if (!is.null(names(onevar))) {
                for (i in seq_along(onevar)) {
                grid[grid[,labvar]==onevar[i],labvar]<-names(onevar)[i]
                }
              }
         }
    }
  # here we add labels to values if labels are available: if a variable in lablist is a named list, 
  # names(variables) represents the labels. Otherwise, values are returned 
  
  if (type=="values_labels") { 
    for (labvar in names(lablist)) {
      onevar<-lablist[[labvar]]
      if (!is.null(names(onevar))) {
              for (i in seq_along(onevar)) {
                grid[grid[,labvar]==onevar[i],labvar]<-paste0(names(onevar)[i],"=",onevar[i])
              }
      }
    }
  }
  return(grid)
  
}    


    .condition_values<-function(datavar,conditioning,span) {

      #conditioning of covariates can come in different forms:
      # 1. can be a list of numbers, thus are passed as they are as the "at" levels 
      # 2. a list variable, each with labels and values: such as list(x=list("mean-sd"=-10,"mean"=0,"mean+sd"=+10)) 
      # 3. one of standard conditioning: mean_sd, mean_offset, percent, percent_offset
           if (is.list(conditioning))
                   return(conditioning)
           if (is.numeric(conditioning))
                   return(conditioning)

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
           
           if (conditioning=="percent") {
             vec<-quantile(datavar,c(.25,.5,.75))
             return(vec)
            }
           if (conditioning=="percent_offset") {
             span<-span/100
             return(round(quantile(datavar,c(.5-span,.5,.5+span)),digits=3))
           }
  
}

    
.condition_list<-function(terms,data,conditioning,span) {    
    condlist<-list()
    lablist<-list()
    for (name in terms)
      if (!is.factor(data[[name]])) {
        if (is.list(conditioning)) {
           if (name %in% names(conditioning))
                .cond<-conditioning[[name]]
           else 
                .cond<-"mean_sd"
        }
        else
                .cond<-conditioning
        mlevels<-.condition_values(data[[name]],conditioning=.cond,span=span)
        if (length(mlevels)>1 & (max(mlevels)-min(mlevels)==0))
            stop(paste("Error in covariate scaling: conditioning levels for", 
                       jmvcore::fromB64(name), 
                       "are all equal. Conditioning was ",
                       .cond, "and values were", 
                       paste(mlevels,collapse = ",")))
        condlist[[name]]<-as.numeric(mlevels)
        lablist[[name]]<-mlevels
      }  
  return(list(condlist,lablist))
}
#### simple Effects estimates: parameters  table and anova table

rawMeans<- function(x,...) UseMethod(".rawMeans")

.rawMeans.default<-function(model,terms,conditioning="mean_sd",span=0,interval=95,labels="values",type="link") {
  data<-mf.getModelData(model)
  interval=as.numeric(interval)/100
  temp<-.condition_list(terms,data,conditioning,span) 
  condlist<-temp[[1]]
  lablist<-temp[[2]]
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type)
  est@grid<-.fixGridNames(est@grid,lablist,labels)
  est
}

.rawMeans.lmerModLmerTest<-function(model,terms,conditioning="mean_sd",span=0,interval=95,labels="values",type="link",df="Satterthwaite") 
               .rawMeans.merModLmerTest(model,terms,conditioning,span,interval,labels,df) 
    
    
.rawMeans.merModLmerTest<-function(model,terms,conditioning="mean_sd",span=0,interval=95,labels="values",type="link",df="Satterthwaite") {

  df<-tolower(df)
  data<-mf.getModelData(model)
  interval=as.numeric(interval)/100
  temp<-.condition_list(terms,data,conditioning,span) 
  condlist<-temp[[1]]
  lablist<-temp[[2]]
  est<-emmeans::emmeans(model,specs=terms,at=condlist,type=type,lmer.df = df)
  est@grid<-.fixGridNames(est@grid,lablist,labels)
  est
}



pred.means<-function(model,terms,conditioning="mean_sd",span=0,interval=95,labels="values") {
         est<-rawMeans(model,terms,conditioning,span = span ,labels = labels,interval=interval,type="response")
         dd<-as.data.frame(est)
         for (nn in names(dd))
            if (is.factor(dd[[nn]]))
               dd[[nn]]<-as.character(dd[[nn]])
#         names(dd)[seq_along(terms)]<-paste0("c",length(terms):1)
#         dd[,c(length(terms):1,(length(terms)+1):dim(dd)[2])]
         dd
}




pred.simpleEstimates<- function(x,...) UseMethod(".simpleEstimates")

.simpleEstimates.default<-function(model,variable,moderator,threeway=NULL,
           conditioning="mean_sd",
           span=0,
           interval=95,
           labels="values") {
  
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
                          conditioning = conditioning,
                          interval=interval,df=df)
       
      
       est<-emmeans::contrast(emm,method = ".internal",by = preds)
       ci<-confint(est,level = interval/100)
       params<-cbind(as.data.frame(est),ci[,c("lower.CL","upper.CL")])
       params$contrast<-as.character(params$contrast)
       names(params)[2:(1+length(lnames))]<-lnames
       
  } else {
    temp<-.condition_list(preds,data,conditioning,span) 
    condlist<-temp[[1]]
    lablist<-temp[[2]]
    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=interval/100))
    estf<-as.data.frame(est)
    ci<-estf[,c(dim(estf)[2]-1,dim(estf)[2])]
    params<-cbind(emmeans::test(est),ci)
    names(params)[1:(length(lnames)+1)]<-c(lnames,"estimate")
  }
  ff<-emmeans::test(est,joint=T,by=preds)
  names(ff)[1:length(lnames)]<-lnames
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  list(params,ff)
}

.simpleEstimates.lmerModLmerTest<-function(model,variable,moderator,threeway=NULL,conditioning="mean_sd",span=0,interval=95,labels="values")
       .simpleEstimates.merModLmerTest(model,variable,moderator,threeway, conditioning, span, interval, labels)
    
    
    
.simpleEstimates.merModLmerTest<-function(model,variable,moderator,threeway=NULL,
                                   conditioning="mean_sd",
                                   span=0,
                                   interval=95,
                                   labels="values") {
  mark("simple effects estimation for merMod")
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
                   conditioning = conditioning,
                   interval=interval,labels = labels,span=span)
    
    est<-emmeans::contrast(emm,method = ".internal",by = preds)
    ci<-confint(est,level = interval/100)
    params<-cbind(as.data.frame(est),ci[,c("lower.CL","upper.CL")])
    params$contrast<-as.character(params$contrast)
    names(params)[2:(1+length(lnames))]<-lnames
    
  } else {
    temp<-.condition_list(preds,data,conditioning,span) 
    condlist<-temp[[1]]
    lablist<-temp[[2]]
    est<-emmeans::emtrends(model,specs=preds,var=variable,at=condlist,options=list(level=interval/100))
    estf<-as.data.frame(est)
    ci<-estf[,c(dim(estf)[2]-1,dim(estf)[2])]
    params<-cbind(emmeans::test(est),ci)
    names(params)[1:(length(lnames)+1)]<-c(lnames,"estimate")
  }
  ff<-emmeans::test(est,joint=T,by=preds)
  names(ff)[1:length(lnames)]<-lnames
  for (name in lnames) {
    ff[,name]<-as.character(ff[,name])
    params[,name]<-as.character(params[,name])
  }
  list(params,ff)
}
