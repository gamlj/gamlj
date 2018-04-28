#### "image" should be from jomovi image object
#### this function assumes that the y is called "mean", the x is "group" and the
#### tracing factor "lines" (in any)
#### errorType is an option for plotting error bars or CI. It can either  "" (empty) which
#### means no bars or CI or any other string. The string will be used as
#### the legend name and the function assumes that the data contains "lwr" and "upr" varianbles
#### "theme" is passed from jmv plot function

.twoWaysPlot<-function(image,theme,depName,groupName,linesName,errorType="none",title=NULL) {
    if (errorType != 'none') {
    dodge <- ggplot2::position_dodge(0.2)
    clabel<-paste(linesName, paste0("(",toupper(errorType),")"),sep="\n")
  }
  else {
    errorType<-""
    clabel<-linesName
    dodge <- ggplot2::position_dodge(0)
  }
    p <- ggplot2::ggplot(data=image$state$data, aes(x=group, y=fit, group=factor(lines), colour=lines)) +
       geom_line(size=.8, position=dodge) +
       labs(x=groupName, y=depName, colour=clabel) +
       scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) 

    if (!is.null(image$state$raw)) {
      rawData=image$state$raw
      p <- p + geom_point(data=rawData,aes(x=x, y=y, colour=factor(z), group=factor(z)), alpha=.5,show.legend=F,shape=16, size=2)
    }
    
    
    if (is.factor(image$state$data$group)) {
      p <- p + geom_point(shape=16, size=3, position=dodge)
      if (errorType != '')
          p <- p + geom_errorbar(aes(x=group, ymin=lwr, ymax=upr, width=.1, group=lines), size=.8, position=dodge)
   } else {
    if (errorType != '')
     p <- p + geom_ribbon(aes(x=group, ymin=lwr, ymax=upr,group=lines,colour=lines,fill = lines),linetype = 0,show.legend=F, alpha=.2)          
   }
    
    if (!is.null(title))
      p<-p+ ggtitle(title)
    
 p   
}

.oneWayPlot<-function(image,theme,depName,groupName,errorType="none") {
  
  if (errorType != 'none') {
    dodge <- ggplot2::position_dodge(0.2)
    clabel<-toupper(errorType)
  }
  else {
    errorType<-""
    clabel<-""
    dodge <- ggplot2::position_dodge(0)
  }

     p <- ggplot2::ggplot(data=image$state$data) +
        labs(x=groupName, y=depName, colour=errorType) +
        scale_colour_manual(name=clabel, values=c(colour=theme$color[1]), labels='') +
        scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) 

     if (!is.null(image$state$raw)) {
       rawData=image$state$raw
       p <- p + geom_point(data=rawData,aes(x=x, y=y), colour="grey42", shape=16,alpha=.5, size=2)
     }
     

  if (is.factor(image$state$data$group)) {
      p <- p + geom_point(aes(x=group, y=fit, colour='colour'), shape=16,  size=4,show.legend=F)
      p <- p+geom_line(aes(x=group,y=fit,group = 1)) 
      if (errorType != '')
           p <- p + geom_errorbar(aes(x=group, ymin=lwr, ymax=upr, colour='colour', width=.1), size=.8)
  }  else { 
      p <- p+geom_line(aes(x=group,y=fit),size=1.5) 
      if (errorType != '')
      p <- p + geom_ribbon(aes(x=group, ymin=lwr, ymax=upr),show.legend=T, alpha=.3)
  
  }
     
   p
}


.linesPlot<-function(data,theme,depName,groupName,title=NULL) {
  
  depLabs<-paste(depName,"category",sep="\n")
  yLab<-paste("prob. of",depName,"category")
  dodge <- ggplot2::position_dodge(0)
  p <- ggplot2::ggplot(data=long, aes(x=group, y=fit, group=dep,colour=dep)) +
    geom_line(size=.8, position=dodge)+
    scale_y_continuous(limits=c(0,1)) 
  if (is.factor(data$group)) 
    p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)
  
  p<-p+labs(x=groupName, y=yLab, colour=depLabs)   
  if (!is.null(title))
    p<-p+ ggtitle(title)
  p
}


lp.linesMultiPlot<-function(image,theme,depName,groupName,linesName=NULL,errorType="none",title=NULL) {
  
  data<-image$state$data
  depLabs<-paste("Prob.",depName,"categories")
  if (!is.null(linesName)) {
    plots<-list()
    levels<-levels(data[["plots"]])
    for (level in levels) {
      sdata<-subset(data,plots==level)
      sdata$plots<-NULL
      title<-paste(linesName,"=",level)
      image$setState(list(data=sdata,range=c(0,1)))
      aplot<-.twoWaysPlot(image,theme,depLabs,groupName,depName,errorType=errorType,title = title)
      aplot<-aplot+theme
      plots[[level]]<-aplot
    }
    thegrid<-do.call(gridExtra::grid.arrange,plots)
    return(thegrid)
  } else
    return(.twoWaysPlot(image,theme,depLabs,groupName,depName,errorType=errorType))
}

###### gives the appropriated range of the plot  ############

lp.range<- function(x,...) UseMethod(".range")

.range.default<-function(model,depName,predictions,rawData=NULL) {

  if (!is.null(rawData)) {
    rawRange <- pretty(rawData$y)
  } else {
    rawRange<-NULL
  }

  if ("lwr" %in% names(predictions))
       yAxisRange <- pretty(c(predictions$lwr, predictions$upr,rawRange))
  else 
       yAxisRange <- pretty(c(predictions$fit,rawRange))


  return(yAxisRange)
}

.range.glm<-function(model,depName=NULL,predictions=NULL,rawData=NULL) {

    if (model$family[1] %in% c("quasipoisson","poisson","gaussian"))
        return(.range.default(model,depName,predictions,rawData))

  return(c(0,1))
}

.range.negbin<-function(model,depName=NULL,predictions=NULL,rawData=NULL) {
  
    return(.range.default(model,depName,predictions,rawData))
  
}

       
##### This function prepares the plot predicted data #########

lp.preparePlotData<- function(x,...) UseMethod(".lp.preparePlotData")

.lp.preparePlotData.default=function(model,
                                     groupName,
                                     linesName=NULL,
                                     plotsName=NULL,
                                     bars="none",
                                     ciwidth=95,
                                     conditioning="mean_sd",span=0,labels="values") {
      selected<-c(groupName,linesName,plotsName)  
      selected64<-toB64(selected)
      preds<-unlist(toB64(c(linesName,plotsName)))
      nsel<-length(selected)
      varnames<-c("group","lines","plots")[1:nsel]
      data<-mf.getModelData(model)
      temp<-.condition_list(preds,data,conditioning,span) 
      condlist<-temp[[1]]
      lablist<-temp[[2]]
      
      if (!is.factor(data[[toB64(groupName)]])) {
        lablist[[toB64(groupName)]]<-pretty(c(min(data[[toB64(groupName)]]),max(data[[toB64(groupName)]])),25) 
      }

            
  pdata<-try({
      pred.means(model,selected64,lablist,span = span,labels = labels )
  })

    
  if (isError(pdata)) {
    mark(paste("problems with emmeans in plot data",jmvcore::extractErrorMessage(pdata)))
      jmvcore::reject("Plot estimated values cannot be computed. Refine the model or the covariates conditioning (if any)", code='error')
    
  }

  names(pdata)<-c(varnames,c("fit","SE","df","lwr","upr"))  
  
  if (bars=="se") {
    pdata$lwr<-pdata$fit-pdata$SE
    pdata$upr<-pdata$fit+pdata$SE
  }
  if (bars=="none") {
    pdata$lwr<-NULL
    pdata$upr<-NULL
  } else
       if ("glm" %in% class(model) && "binomial" %in% family(model)["family"]) {
         pdata$lwr<-ifelse(pdata$lwr<0,0,ssdf$lwr)
         pdata$lwr<-ifelse(pdata$lwr>1,1,ssdf$lwr)
         pdata$upr<-ifelse(pdata$upr<0,0,ssdf$upr)
         pdata$upr<-ifelse(pdata$upr>1,1,ssdf$upr)
         }
  return(pdata)
}

.lp.preparePlotData.multinom=function(model,groupName,linesName=NULL,plotsName=NULL,bars="none",ciwidth=95) {
  
  
  depName<-names(attr(terms(model),"dataClass"))[1]
  selected<-c(depName,groupName,linesName,plotsName)  
  nsel<-length(selected)
  varnames<-c("lines","group","plots","plots2")[1:nsel]
  data<-mf.getModelData(model)  
  covs<-NULL
  FUN=list()
  if (!is.factor(data[[groupName]])) {
    FUN[[groupName]]<-function(x)  pretty(x,25) 
  } 
  if (!is.null(linesName) && !is.factor(data[[linesName]])) {
    FUN[[linesName]]<-function(x)  c(mean(x)+sd(x),mean(x),mean(x)-sd(x))
    covs<-linesName
    covslevels<-c("-SD","Mean","+SD")
  } 
  if (!is.null(plotsName) && !is.factor(data[[plotsName]])) {
    FUN[[plotsName]]<-function(x)  c(mean(x)+sd(x),mean(x),mean(x)-sd(x))
    covs<-c(covs,plotsName)
    covslevels<-c("-SD","Mean","+SD")
  } 
  
  term<-c(depName,groupName,linesName)
  cilevel<-ciwidth/100
  ss<-try({
    mm<-emmeans::emmeans(model,term,by = plotsName,cov.reduce=FUN,type="response",options=list(level=cilevel))
    summary(mm)
  })
  if (isError(ss)) {
    print("problems with emmeans in plot data")
  }
  ssdf<-as.data.frame(ss)
  for (aname in covs) {
    ssdf[[aname]]<-factor(ssdf[[aname]])
    levels(ssdf[[aname]])<-covslevels
  }
  names(ssdf)<-c(varnames,c("fit","SE","df","lwr","upr"))  
  if (bars=="se") {
    ssdf$lwr<-ssdf$fit-ssdf$SE
    ssdf$upr<-ssdf$fit+ssdf$SE
  }
  
    ssdf$lwr<-ifelse(ssdf$lwr<0,0,ssdf$lwr)
    ssdf$lwr<-ifelse(ssdf$lwr>1,1,ssdf$lwr)
    ssdf$upr<-ifelse(ssdf$upr<0,0,ssdf$upr)
    ssdf$upr<-ifelse(ssdf$upr>1,1,ssdf$upr)
    return(ssdf)
}


###### gives the raw date for the plot  ############

lp.rawData<- function(x,...) UseMethod(".rawData")

.rawData.default<-function(model,depName,groupName,linesName=NULL) {
  data=mf.getModelData(model)
  data$y<-data[[toB64(depName)]]
  data$x<-data[[toB64(groupName)]]
  
  if (is.null(linesName) || !is.factor(data[[toB64(linesName)]]))
    data$z<-NA
  else
    data$z<-data[,toB64(linesName)]
  
  return(data[,c("y","x","z")])

  }  

.rawData.glm<-function(model,depName,groupName,linesName=NULL) {
  
  
  if (model$family[1] %in% c("binomial")) {
      data=mf.getModelData(model)
      data$y<-ifelse(data[[toB64(depName)]]==levels(data[[toB64(depName)]])[1],1,0)
      data$x<-data[[toB64(groupName)]]
      if (is.null(linesName) || !is.factor(data[[toB64(linesName)]]))
           data$z<-NA
      else
           data$z<-data[,toB64(linesName)]
      
      return(data[,c("y","x","z")])
  }
  
  
  return(.rawData.default(model,depName,groupName,linesName))
  
  } 

.rawData.multinom<-function(model,depName,groupName,linesName=NULL) {
    return(NULL)
   }
    



