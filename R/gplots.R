################ functions to deal with data preparation ########

###### gives the appropriated range of the plot  ############

gplots.range<- function(x,...) UseMethod(".range")

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

.range.multinom<-function(model,depName=NULL,predictions=NULL,rawData=NULL) {
  
  return(c(0,1))
  
}

###### gives the raw date for the plot  ############

gplots.rawData<- function(x,...) UseMethod(".rawData")

.rawData.default<-function(model,depName,groupName,linesName=NULL,plotsName=NULL) {
  data=mf.getModelData(model)
  data$y<-data[[jmvcore::toB64(depName)]]
  data$x<-data[[jmvcore::toB64(groupName)]]
  
  if (is.null(linesName) || !is.factor(data[[jmvcore::toB64(linesName)]]))
    data$z<-1
  else
    data$z<-data[,jmvcore::toB64(linesName)]
  
  if (is.null(plotsName) || !is.factor(data[[jmvcore::toB64(plotsName)]]))
    data$w<-1
  else
    data$w<-data[,jmvcore::toB64(plotsName)]
  
  return(data[,c("y","x","z","w")])
  
}  

.rawData.glm<-function(model,depName,groupName,linesName=NULL,plotsName=NULL) {
  
  
  if (model$family[1] %in% c("binomial")) {
    data=mf.getModelData(model)
    data$y<-ifelse(data[[jmvcore::toB64(depName)]]==levels(data[[jmvcore::toB64(depName)]])[1],1,0)
    data$x<-data[[jmvcore::toB64(groupName)]]
    if (is.null(linesName) || !is.factor(data[[jmvcore::toB64(linesName)]]))
      data$z<-1
    else
      data$z<-data[,jmvcore::toB64(linesName)]
    
    if (is.null(plotsName) || !is.factor(data[[jmvcore::toB64(plotsName)]]))
      data$w<-1
    else
      data$w<-data[,jmvcore::toB64(plotsName)]
    
    return(data[,c("y","x","z","w")])
  }
  
  
  return(.rawData.default(model,depName,groupName,linesName,plotsName))
  
} 

.rawData.multinom<-function(model,depName,groupName,linesName=NULL,plotsName) {
  return(NULL)
}


##### This function prepares the plot predicted data #########

gplots.preparePlotData<- function(x,...) UseMethod(".preparePlotData")

.preparePlotData.default=function(model,
                                     groupName,
                                     linesName=NULL,
                                     plotsName=NULL,
                                     bars="none",
                                     ciwidth=95,
                                     conditioning=NULL) {
  
  selected<-c(groupName,linesName,plotsName)  
  selected64<-jmvcore::toB64(selected)
  preds<-unlist(jmvcore::toB64(c(linesName,plotsName)))
  nsel<-length(selected)
  varnames<-c("group","lines","plots")[1:nsel]
  data<-mf.getModelData(model)
  cond<-conditioning$clone()
  if (groupName %in% cond$vars)
    cond$updateValues(groupName,pretty(c(min(data[[jmvcore::toB64(groupName)]]),max(data[[jmvcore::toB64(groupName)]])),25))
  
  pdata<-try({
    pred.means(model,selected64,cond)
  })
  
  
  if (jmvcore::isError(pdata)) {
    mark(paste("problems with emmeans in plot data",jmvcore::extractErrorMessage(pdata)))
    jmvcore::reject("Plot estimated values cannot be computed. Refine the model or the covariates conditioning (if any)", code='error')
  }
  names(pdata)<-c(varnames,c("fit","SE","df","lwr","upr"))  
  mark(head(pdata))
  if (is.factor(data[[jmvcore::toB64(groupName)]])) 
    pdata$group<-factor(pdata$group)

  if (bars=="se") {
    pdata$lwr<-pdata$fit-pdata$SE
    pdata$upr<-pdata$fit+pdata$SE
  }
  if (bars=="none") {
    pdata$lwr<-NULL
    pdata$upr<-NULL
  } else
    if ("glm" %in% class(model) && "binomial" %in% family(model)["family"]) {
      pdata$lwr<-ifelse(pdata$lwr<0,0,pdata$lwr)
      pdata$lwr<-ifelse(pdata$lwr>1,1,pdata$lwr)
      pdata$upr<-ifelse(pdata$upr<0,0,pdata$upr)
      pdata$upr<-ifelse(pdata$upr>1,1,pdata$upr)
    }
  return(pdata)
}

.preparePlotData.multinom=function(model,
                                      groupName,
                                      linesName=NULL,
                                      plotsName=NULL,
                                      bars="none",
                                      ciwidth=95,
                                      conditioning=NULL) {
  
  depName<-jmvcore::fromB64(names(attr(terms(model),"dataClass"))[1])
  selected<-list(depName,groupName,linesName,plotsName)
  varnames<-c("lines","group","plots2","plots")[1:length(unlist(selected))]
  
  selected<-unlist(selected)
  selected64<-jmvcore::toB64(selected)
  nsel<-length(selected)
  data<-mf.getModelData(model)  
  cond<-conditioning$clone()
  if (groupName %in% cond$vars)
    cond$updateValues(groupName,pretty(c(min(data[[jmvcore::toB64(groupName)]]),max(data[[jmvcore::toB64(groupName)]])),25))
  
  pdata<-try({
    pred.means(model,selected64,cond)
  })
  names(pdata)<-c(varnames,c("fit","SE","df","lwr","upr"))  
  
  if (jmvcore::isError(pdata)) {
    mark(paste("problems with emmeans in plot data",jmvcore::extractErrorMessage(pdata)))
    jmvcore::reject("Plot estimated values cannot be computed. Refine the model or the covariates conditioning (if any)", code='error')
  }
  if (is.factor(data[[jmvcore::toB64(groupName)]])) 
    pdata$group<-factor(pdata$group)
  
  if (bars=="se") {
    pdata$lwr<-pdata$fit-pdata$SE
    pdata$upr<-pdata$fit+pdata$SE
  }
  if (bars=="none") {
    pdata$lwr<-NULL
    pdata$upr<-NULL
  } else  {
    pdata$lwr<-ifelse(pdata$lwr<0,0,pdata$lwr)
    pdata$lwr<-ifelse(pdata$lwr>1,1,pdata$lwr)
    pdata$upr<-ifelse(pdata$upr<0,0,pdata$upr)
    pdata$upr<-ifelse(pdata$upr>1,1,pdata$upr)
  }
  return(pdata)
}


########### those functions deal with plots results ###########

gplots.initPlots=function(obj,data,cov_condition) {
  isAxis <- ! is.null(obj$options$plotHAxis)
  isMulti <- (! is.null(obj$options$plotSepPlots) & ! is.null(obj$options$plotSepLines)) 

  obj$results$get('descPlot')$setVisible( ! isMulti && isAxis)
  obj$results$get('descPlots')$setVisible(isMulti && isAxis)
  
  if (isMulti) {
    sepPlotsName <- obj$options$plotSepPlots
    sepPlotsVar <- data[[jmvcore::toB64(sepPlotsName)]]
    if(is.factor(sepPlotsVar))
      sepPlotsLevels <- levels(sepPlotsVar)
    else 
      sepPlotsLevels<-cov_condition$labels(sepPlotsName)

    array <- obj$results$descPlots
    for (level in sepPlotsLevels) {
      title<-paste(sepPlotsName,"=",level)
      array$addItem(title)
    }
  }
  
}

#### "image" should be from jomovi image object
#### this function assumes that the y is called "mean", the x is "group" and the
#### tracing factor "lines" (in any)
#### errorType is an option for plotting error bars or CI. It can either  "" (empty) which
#### means no bars or CI or any other string. The string will be used as
#### the legend name and the function assumes that the data contains "lwr" and "upr" varianbles
#### "theme" is passed from jmv plot function

gplots.twoWaysPlot<-function(image,theme,depName,groupName,linesName,errorType="none",title=NULL) {
  if (errorType != 'none') {
    dodge <- ggplot2::position_dodge(0.2)
    clabel<-paste(linesName, paste0("(",toupper(errorType),")"),sep="\n")
  }
  else {
    errorType<-""
    clabel<-linesName
    dodge <- ggplot2::position_dodge(0)
  }
  
  gdata<-image$state$data
  gdata$lines<-factor(gdata$lines)

  p <- ggplot2::ggplot()
  
  if (!is.null(image$state$raw)) {
    rawData=image$state$raw
    p <- p + geom_point(data=rawData,aes(x=x, y=y),show.legend=FALSE, alpha=.5,shape=16, size=2)
  }

  

  p <- p+ geom_line(data=gdata,aes(x=group, y=fit, group=lines,colour=lines),size=1.2, position=dodge) 
  p <- p + labs(x=groupName, y=depName,colour=clabel) 
  p <- p + scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) 
  
  
  if (is.factor(image$state$data$group)) {
    
    if (!is.null(image$state$randomData)) {
      data<-image$state$randomData
      data<-aggregate(data$y,list(data$cluster,data$group),mean)
      names(data)<-c("cluster","group","y")
      p <- p + geom_point(data=data,aes(x=group, y=y,group=cluster),color="gray64",show.legend = F,shape=12, alpha=.5,  size=.5)
      p <- p+geom_line(data=data,aes(x=group,y=y,group = cluster),color="gray64",size=.3, alpha=.3,show.legend = F) 
    }
    
    p <- p + geom_point(data=gdata,aes(x=group, y=fit, group=lines,colour=lines),shape=16, size=4, position=dodge)
    if (errorType != '')
      p <- p + geom_errorbar(data=gdata,aes(x=group, ymin=lwr, ymax=upr, width=.1, group=lines,color=lines), size=1.2, position=dodge)
    
    
  } else {
    thinker<-0
    if (!is.null(image$state$randomData)) {
      data<-image$state$randomData
      mark("we are smoothing")
      thinker<-.3
      p<-p+stat_smooth(geom="line",data=data,aes(y=y,x=group,group=cluster),size=.2,colour="gray64", alpha=.8,method = "auto",fullrange = TRUE,se=FALSE,show.legend=F) 
    }
    if (errorType != '')
      p <- p + geom_ribbon(data=gdata,aes(x=group, ymin=lwr, ymax=upr,group=lines,colour=lines,fill = lines),linetype = 0,show.legend=F, alpha=.2)          

  }
  
  if (!is.null(title))
    p<-p+ ggtitle(title)
  ## here we put mean in the middle between mean +offset and -offset    
  levs<-levels(gdata$lines)
  if (sum(grep("Mean",levs,fixed=T))==6) {
    newbreaks<-levs[c(2,1,3)]
    theme[[3]]$breaks<-newbreaks    
  }
  p<-p+theme
  p   
}

gplots.oneWayPlot<-function(image,theme,depName,groupName,errorType="none") {
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
    labs(x=groupName, y=depName,colour=clabel) +
    scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) 
  
  if (!is.null(image$state$raw)) {
    rawData=image$state$raw
    p <- p + geom_point(data=rawData,aes(x=x, y=y), colour="cadetblue3", shape=16,alpha=.5, size=2)
  }
  
  gdata<-image$state$data
  if (is.factor(image$state$data$group)) {
    if (!is.null(image$state$randomData)) {
      data<-image$state$randomData
      data<-aggregate(data$y,list(data$cluster,data$group),mean)
      names(data)<-c("cluster","group","y")
      mark("we are breaking by cluster")
      p <- p + geom_point(data=data,aes(x=group, y=y,group=cluster),show.legend = F,shape=12, alpha=.5,  size=.5)
      p <- p+geom_line(data=data,aes(x=group,y=y,group = cluster),size=.3, alpha=.3,show.legend = F) 
    }
    
    if (errorType != '') {
      p <- p + geom_errorbar(data=gdata,aes(x=group, ymin=lwr, ymax=upr), width=.1,size=.8,show.legend = F)
    }           
    p <- p+geom_line(data=gdata,aes(x=group,y=fit,group = 1),size=2,show.legend = F) 
    p <- p + geom_point(data=gdata,aes(x=group, y=fit), shape=21, fill="white",  size=4,show.legend=F)
    
    
  }  else { 
    if (!is.null(image$state$randomData)) {
      data<-image$state$randomData
      p<-p+stat_smooth(geom="line",data=data,aes(y=y,x=group,group=cluster),size=.2,method = "auto", alpha=.5,fullrange = TRUE,se=FALSE,show.legend=F) 
    }
    
    if (errorType != '')
      p <- p + geom_ribbon(aes(x=group, ymin=lwr, ymax=upr),show.legend=F, alpha=.2)
    
    p <- p+geom_line(aes(x=group,y=fit),size=1.5,show.legend = F) 
    
    
    
  }
  
  p+theme
}



gplots.linesMultiPlot<-function(image,ggtheme,depName,groupName,linesName=NULL,plotsName=NULL,errorType="none",title=NULL) {
  
  data<-image$state$data
  depLabs<-paste("Prob.",depName,"categories")
  vars<-c(depName,groupName,linesName,plotsName)
  if (!is.null(linesName)) {
    plots<-list()
    aplot<-gplots.twoWaysPlot(image,ggtheme,depLabs,groupName,depName,errorType=errorType,title = title)
    labs<-function(x) {
     x$name<-paste0(linesName,":")
     return(x)
    }
    plots<-aplot+ggplot2::facet_grid(plots2 ~ .,labeller = labs )
    return(plots)
  } else
    return(gplots.twoWaysPlot(image,ggtheme,depLabs,groupName,depName,errorType=errorType)+ggtheme)
}


