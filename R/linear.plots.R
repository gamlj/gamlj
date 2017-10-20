#### "image" should be from jomovi image object
#### this function assumes that the y is called "mean", the x is "group" and the
#### tracing factor "lines" (in any)
#### errorType is an option for plotting error bars or CI. It can either  "" (empty) which
#### means no bars or CI or any other string. The string will be used as
#### the legend name and the function assumes that the data contains "lower" and "upper" varianbles
#### "theme" is passed from jmv plot function

.twoWaysPlot<-function(image,theme,depName,groupName,linesName,errorType="none") {

  if (errorType != 'none') {
    dodge <- ggplot2::position_dodge(0.2)
  }
  else {
    errorType<-""
    dodge <- ggplot2::position_dodge(0)
  }
  
  
    p <- ggplot2::ggplot(data=image$state$data, aes(x=group, y=mean, group=factor(lines), colour=lines)) +
       geom_line(size=.8, position=dodge) +
       labs(x=groupName, y=depName, colour=paste(linesName, errorType,sep="\n")) +
       scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) 

    if (is.factor(image$state$data$group)) {
      p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)
      if (errorType != '')
          p <- p + geom_errorbar(aes(x=group, ymin=lower, ymax=upper, width=.1, group=lines), size=.8, position=dodge)
   } else {
    if (errorType != '')
     p <- p + geom_ribbon(aes(x=group, ymin=lower, ymax=upper,group=lines,colour=lines,fill = lines),linetype = 0,show.legend=F, alpha=.2)          
   }
 p   
}

.oneWayPlot<-function(image,theme,depName,groupName,errorType="none") {
  
  if (errorType != 'none') {
    dodge <- ggplot2::position_dodge(0.2)
  }
  else {
    errorType<-""
    dodge <- ggplot2::position_dodge(0)
  }
  
  
   p <- ggplot2::ggplot(data=image$state$data) +
        labs(x=groupName, y=depName, colour=paste("", errorType)) +
        scale_colour_manual(name=paste("", errorType), values=c(colour=theme$color[1]), labels='') +
        scale_y_continuous(limits=c(min(image$state$range), max(image$state$range))) 


  if (is.factor(image$state$data$group)) {
      p <- p + geom_point(aes(x=group, y=mean, colour='colour'), shape=21, fill=theme$fill[1], size=3)
      p <- p+geom_line(aes(x=group,y=mean,group = 1)) 
      if (errorType != '')
           p <- p + geom_errorbar(aes(x=group, ymin=lower, ymax=upper, colour='colour', width=.1), size=.8)
  }  else { 
      p <- p+geom_line(aes(x=group,y=mean)) 
      if (errorType != '')
      p <- p + geom_ribbon(aes(x=group, ymin=lower, ymax=upper),show.legend=F, alpha=.3)
  
  }
   p
}

##### This function prepare the plots #########

.preparePlotData<-function(model,groupName,linesName=NULL,plotsName=NULL) {
    selected<-c(groupName,linesName,plotsName)  
    vars<-all.vars(terms(model))[-1]
    data<-mf.getModelData(model)
    ll<-list()
    for (v in vars) {
        if (is.factor(data[,v])) 
            ll[v]<-list(levels(data[,v]))
        else {
             if (v %in% selected) {
                if (v==groupName)
                    ll[v]<-list(c(-2,-1,0,1,2)*sd(data[,v])+mean(data[,v]))
                else
                    ll[v]<-list(c(-1,0,1)*sd(data[,v])+mean(data[,v]))
             } else 
                   ll[v]<-list(0)
        }
    }
     dm<-expand.grid(ll)
     for (v in vars) {
         if (is.factor(data[,v]))
             dm[,v]<-factor(dm[,v])
     }
     mm<-mf.predict(model,dm)
     dm<-as.data.frame(cbind(mm,dm))
     if (length(selected)==1) {
         by<-list(dm[,selected])
     } else 
          by<-dm[,selected]

      lnames<-c("group","lines","plots")
      dm<-aggregate(dm[,"mm"],by,mean)
      names(dm)<-c(lnames[1:length(selected)],c("mean"))
      if (!is.null(dm$plots) & !is.factor(data[,plotsName])) {
          dm$plots<-factor(dm$plots)
          levels(dm$plots)<-c("-SD","Mean","+SD")
      }

      if (!is.null(dm$lines) & !is.factor(data[,linesName])) {
          dm$lines<-factor(dm$lines)
          levels(dm$lines)<-c("-SD","Mean","+SD")
      }
      dm
}
