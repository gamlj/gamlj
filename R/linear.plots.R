#### "image" should be from jomovi image object
#### this function assumes that the y is called "mean", the x is "group" and the
#### tracing factor "lines" (in any)
#### errorType is an option for plotting error bars or CI. It can either  "" (empty) which
#### means no bars or CI or any other string. The string will be used as
#### the legend name and the function assumes that the data contains "lwr" and "upr" varianbles
#### "theme" is passed from jmv plot function

.twoWaysPlot<-function(image,theme,depName,groupName,linesName,errorType="none") {

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

    if (is.factor(image$state$data$group)) {
      p <- p + geom_point(shape=21, fill='white', size=3, position=dodge)
      if (errorType != '')
          p <- p + geom_errorbar(aes(x=group, ymin=lwr, ymax=upr, width=.1, group=lines), size=.8, position=dodge)
   } else {
    if (errorType != '')
     p <- p + geom_ribbon(aes(x=group, ymin=lwr, ymax=upr,group=lines,colour=lines,fill = lines),linetype = 0,show.legend=F, alpha=.2)          
   }
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


  if (is.factor(image$state$data$group)) {
      p <- p + geom_point(aes(x=group, y=fit, colour='colour'), shape=21, fill=theme$fill[1], size=3)
      p <- p+geom_line(aes(x=group,y=fit,group = 1)) 
      if (errorType != '')
           p <- p + geom_errorbar(aes(x=group, ymin=lwr, ymax=upr, colour='colour', width=.1), size=.8)
  }  else { 
      p <- p+geom_line(aes(x=group,y=fit)) 
      if (errorType != '')
      p <- p + geom_ribbon(aes(x=group, ymin=lwr, ymax=upr),show.legend=T, alpha=.3)
  
  }
   p
}

##### This function prepare the plots #########


lp.preparePlotData=function(model,groupName,linesName=NULL,plotsName=NULL,bars="none") {

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
#                    ll[v]<-list(seq(-2,2,by = .1)*sd(data[,v])+mean(data[,v]))
                     ll[v]<-list(seq(min(data[,v]),max(data[,v]),length.out=100))
                   else
                       ll[v]<-list(c(-1,0,1)*sd(data[,v])+mean(data[,v]))
               }
               else 
                   ll[v]<-list(0)
          }
     }
     eg<-expand.grid(ll)
     for (v in names(model$contrasts)) {
          eg[,v]<-factor(eg[,v])
     }
     mm<-mf.predict(model,eg,bars)
     dm<-as.data.frame(cbind(mm,eg))
     names(dm)<-c(names(mm),names(eg))
     if (length(selected)==1) 
          by<-list(dm[,selected])
     else 
          by<-dm[,selected]
     
     what<-"fit"
     if (bars!="none")
        what<-c("fit","lwr","upr")
     dm<-aggregate(dm[,what],by,mean)
     lnames<-c("group","lines","plots")
     names(dm)<-c(lnames[1:length(selected)],what)
          
     if (!is.null(dm$plots) & !is.factor(data[,plotsName])) {
           dm$plots<-factor(dm$plots)
       levels(dm$plots)<-c("-SD","Mean","+SD")
       }
  
      if (!is.null(dm$lines) & !is.factor(data[,linesName])) {
            dm$lines<-factor(dm$lines)
            levels(dm$lines)<-c("-SD","Mean","+SD")
        }
        dm
   } # end of .preparePlotData()
