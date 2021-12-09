Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Scaffold,
  public=list(
      options=NULL,
      dispatcher=NULL,
      plotData=list(),
      rawData=list(),
      randomData=list(),
      scatterRange=NULL,
      scatterDodge=NULL,
      scatterClabel=NULL,
      scatterY=NULL,
      scatterX=NULL,
      scatterXscale=FALSE,
      scatterZ=NULL,
      scatterCluster=NULL,
      scatterModerators=NULL,
      scatterBars=FALSE,
      scatterRaw=FALSE,
      scatterType=NULL,
      initialize=function(options,operator,results) {
            self$options<-options
            private$.results<-results
            private$.operator<-operator
            private$.datamatic<-operator$datamatic
            self$scatterRaw<-self$options$plotRaw
            if ("plotScale" %in% names(self$options))
                    self$scatterType<-self$options$plotScale
            else 
                    self$scatterType<-"response"
            self$dispatcher<-Dispatch$new()
      },

      initPlots=function() {
           private$.initMainPlot()

      },
      preparePlots=function(image, ggtheme, theme, ...) {
        ## here are the plots that require some preparations. Other types of plots 
        ## are not prepared because they can be handled directly by the plot call
        ## each function skips if the plot is not required

        private$.prepareMainPlot()
        private$.prepareClusterBoxplot()
        private$.prepareClusterResPred()
        
        private$.prepareRandHist()
        
      },
      scatterPlot=function(image) {
        
        ## debug: return this to see what error is in the plotter code ### 
        if (!is.something(self$plotData)) {
           pp<-ggplot2::ggplot(data.frame(1:3))+ggplot2::ggtitle(image$key)
           return(pp)
        }
        
        ## collect the data 
        data<-self$plotData[[image$key]]
        
        ### prepare aestetics for one or two way scatterplot
        if (is.null(self$scatterZ)) {
                     names(data)[1]<-"x"
                    .aestetics<-ggplot2::aes_string(x = "x", y = "estimate",group=1)
                    .aesbar<-ggplot2::aes_string(x = "x", ymin = "lower", ymax = "upper")
                    
                    
        } else {
                     names(data)[1:2]<-c("x","z")
                     if (isFALSE(self$options$plotLinesTypes)) {
                    .aestetics<-ggplot2::aes_string(x = "x", y = "estimate",   group = "z", colour = "z")
                    .aesbar<-ggplot2::aes_string(x = "x", ymin = "lower", ymax = "upper", group = "z",color = "z" ,fill="z")
                     } else {
                       
                       .aestetics<-ggplot2::aes_string(x = "x", y = "estimate",   group = "z", linetype = "z")
                       .aesbar<-ggplot2::aes_string(x = "x", ymin = "lower", ymax = "upper", group = "z",linetype = "z" ,fill="z")
                       
                     }
        }
        
        ## initializa plot 
        p <- ggplot2::ggplot()
        
        # give a scale to the Y axis
        if (is.something(self$scatterRange))
              p <- p + ggplot2::scale_y_continuous(limits = self$scatterRange)
        

        #### plot the actual data if required 
        
        if (self$scatterRaw) {
          
          rawdata<-self$rawData[[image$key]]
          y<-self$scatterY$name64
          x<-self$scatterX$name64
          z<-self$scatterZ$name64
          
          .aesraw<-ggplot2::aes_string(x = x, y = y)
          
          if (!is.null(self$scatterZ))
              if (self$scatterZ$type=="factor")
               .aesraw<-ggplot2::aes_string(x = x, y = y, color=z)


          p <- p +  ggplot2::geom_point(data = rawdata,
                                        .aesraw,
                                        show.legend = FALSE, alpha = 0.5, shape = 16)
        }
        ##### END OF RAW DATA #############
        if (is.something(self$randomData)) {
          
          randomData<-self$randomData[[image$key]]
          if ("z" %in% names(randomData)) {
                      .aesrandom<-ggplot2::aes_string(x = "x", y = "y", group="cluster", colour="z")
                       p <- p + ggplot2::geom_line(data = randomData, 
                                                  .aesrandom, 
                                                  size = 0.4, 
                                                  alpha = .50)
          }
          else { 
                      .aesrandom<-ggplot2::aes_string(x = "x", y = "y", group="cluster")
                      p <- p + ggplot2::geom_line(data = randomData, 
                                                  .aesrandom,
                                                  color="gray74",
                                                  size = 0.4, 
                                                  alpha = .80)
                      
          }

        }
        
        
        
        ######### fix the bars ##########        
        if (self$scatterBars) {
          if (self$scatterX$type=="factor")
            p <- p + ggplot2::geom_errorbar(data = data, .aesbar, size = .9, width=.3, position = self$scatterDodge)
          else
            p <- p + ggplot2::geom_ribbon(data = data, .aesbar, linetype = 0, show.legend = F, alpha = 0.2)
        }
        #########  ##########        
        
        
        
        ### plot the lines 
        p <- p + ggplot2::geom_line(data = data, 
                                    .aestetics,
                                     size = 1.2, 
                                     position=self$scatterDodge)


        ### plot the points for factors
        if (self$scatterX$type=="factor")
              p <- p +  ggplot2::geom_point(data = data,
                                            .aestetics,
                                            shape = 21, size = 4, fill="white",
                                            position = self$scatterDodge)
        
        if (isFALSE(self$options$plotLinesTypes))
           p <- p + ggplot2::labs(x = self$scatterX$name, y = self$scatterY$name, colour = self$scatterClabel)
        else
          p <- p + ggplot2::labs(x = self$scatterX$name, y = self$scatterY$name, linetype = self$scatterClabel)
        
        # if (self$scatterXscale) {
        #      
        #   o<-ggplot2::ggplot_build(p)
        #   values<-o$layout$panel_params[[1]]$x$breaks
        #   newlabs<-private$.rescale(self$scatterX,values)
        #   p<-p+ggplot2::scale_x_continuous(labels= newlabs)
        # }
           
        
        return(p)        
      },
      
      qqplot=function(theme,ggtheme)  {
        
              if (!self$option("qqplot"))
                         return()
        
              if (!is.something(private$.operator$model))
                         return()

              residuals <- as.numeric(scale(stats::residuals(private$.operator$model)))
              df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
              plot<-ggplot2::ggplot(data=df, aes(y=y, x=x)) +
                          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
                          xlab("Theoretical Quantiles") +
                          ylab("Standardized Residuals") 
      
               plot+ggtheme
      },
      
      normplot=function(theme,ggtheme)  {
        
        if (!self$option("normPlot"))
          return()
        
        if (!is.something(private$.operator$model))
          return()
        
        fill <- theme$fill[2]
        color <- theme$color[1]
        data <- as.data.frame(stats::residuals(private$.operator$model))
        names(data) <- "x"
        # library(ggplot2)
        plot <- ggplot2::ggplot(data = data, aes_string(x = "x")) + labs(x = "Residuals", y = "density")
        
        plot <- plot + ggplot2::geom_histogram(aes_string(y = "..density.."), position = "identity", stat = "bin", color = color, fill = fill)
        plot <- plot + ggplot2::stat_function(fun = stats::dnorm, args = list(mean = mean(data$x), sd = stats::sd(data$x)))
        
        themeSpec <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        plot <- plot + ggtheme + themeSpec
        
        return(plot)

      },
      residPlot=function(theme,ggtheme)  {
        
        if (!self$option("residPlot"))
          return()
        
        if (!is.something(private$.operator$model))
          return()
        
            fill <- theme$fill[2]
            color <- theme$color[1]
            data <- as.data.frame(stats::residuals(private$.operator$model))
            names(data) <- "res"
            data$pred <- stats::predict(private$.operator$model)
      
              # library(ggplot2)
            plot <- ggplot(data = data, aes_string(x = "pred", y = "res")) + labs(x = "Predicted", y = "Residuals")
      
            plot <- plot + geom_point(shape = 21, color = color, fill = fill)
            plot <- plot + ggtheme
            return(plot)
      },
      
      clusterBoxplot=function(image,ggtheme,theme)  {
        
        ########## working here ##########

        if (!self$option("clusterBoxplot"))
          return()
        

        if (!is.something(private$.operator$model) )
          return(FALSE)
        
        cluster<-image$state$cluster

        fmodel<-lme4::fortify.merMod(private$.operator$model)
        plot<-ggplot(fmodel, aes_string(cluster,".resid")) + geom_boxplot() + coord_flip()
        plot<-plot+xlab(fromb64(cluster))+ylab("Residuals")
        plot<-plot+ ggtheme 

        return(plot)
        
      },
      clusterResPred=function(image,ggtheme,theme)  {
        
        ########## working here ##########
        
        if (!self$option("clusterResPred"))
          return()
        
        
        if (!is.something(private$.operator$model) )
          return(FALSE)
        
        cluster<-image$state$cluster
        data<-lme4::fortify.merMod(private$.operator$model)
        plot <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = ".fitted", y = ".resid",color=cluster)) 
        plot <- plot + ggplot2::labs(x = "Predicted", y = "Residuals", color=fromb64(cluster))
        
        plot <- plot + ggplot2::geom_point(shape = 21)
        plot <- plot + ggtheme
        
        return(plot)
        
      },
      
        randHist=function(image,ggtheme,theme)  {
  
  
              if (!self$option("randHist"))
                  return()
  
  
              if (!is.something(private$.operator$model) )
                  return(FALSE)
 
              label<-image$state$label
              data<-image$state$data
              fill <- theme$fill[2]
              color <- theme$color[1]
              alpha <- 0.4
              plot <- ggplot(data=data, aes(x=x)) +
                            labs(x="Coefficients", y='density')
          
              plot <- plot + geom_histogram(aes(y=..density..), position="identity",
                                        stat="bin", color=color, fill=fill)
              plot <- plot + stat_function(fun = dnorm, args = list(mean = mean(data$x), sd = sd(data$x)))  
          
              themeSpec <- theme(axis.text.y=element_blank(),
                             axis.ticks.y=element_blank())
              plot <- plot + ggtheme + themeSpec
          
          
          return(plot)
  
        }

  ), # end of public
  private = list(
    .datamatic=FALSE,
    .results=NULL,
    .operator=NULL,
    .initMainPlot=function() {
      
            if (!is.something(self$options$plotHAxis)) 
                   return()
            
            ginfo("PLOTTER: init main plot")
      
            resultsgroup<-private$.results$get("mainPlots")
            y<-self$options$dep
            x<-self$options$plotHAxis
            z<-self$options$plotSepLines
            moderators<-self$options$plotSepPlots
            
            if (self$options$modelSelection=="multinomial") {
                     moderators < c(z,moderators)
                     z   <- self$options$dep
                     self$scatterType="response"
            }
            
            if (self$options$modelSelection=="ordinal" & self$scatterType!="mean.class") {
                     moderators < c(z,moderators)
                     z   <- self$options$dep
              
            }
            
            
            dims<-unlist(lapply(moderators, function(mod) private$.datamatic$variables[[tob64(mod)]]$nlevels))

            if (!is.something(dims))
                dims<-1
            
             n<-prod(dims)
             for (i in seq_len(n)) {
                resultsgroup$addItem(key=i)
             }
             
             ### some options here 
             self$scatterX<-private$.datamatic$variables[[tob64(x)]]
             self$scatterY<-private$.datamatic$variables[[tob64(y)]]
             self$scatterModerators<-moderators
             
             if (is.something(z))
                     self$scatterZ<-private$.datamatic$variables[[tob64(z)]]
             

             if (self$options$plotError != "none") {
               self$scatterBars<-TRUE
               self$scatterDodge <- ggplot2::position_dodge(0.2)
               self$scatterClabel <- paste(self$scatterZ$name, paste0("(", toupper(self$options$plotError), ")"), sep = "\n")
             } else {
               self$scatterDodge<-ggplot2::position_dodge(0)
               self$scatterClabel <- self$scatterZ$name
             }
             
             
             
             

    },
    .prepareMainPlot=function() {
      
      

      if (!is.something(self$options$plotHAxis)) 
        return()

      ginfo("PLOTTER: prepare main plot")
      resultsgroup<-private$.results$get("mainPlots")

      ### stop is filled from previous run ###
      test<-any(unlist(sapply(resultsgroup$items, function(i) !i$isNotFilled())))
      if (test)
           return()
      
      
      moderators<-self$scatterModerators


      ### compute the expected values to be plotted ###
      data<-private$.estimate(self$scatterX$name,unlist(c(self$scatterZ$name,moderators)))

      #### compute the levels combinations
      #### first, gets all levels of factors and covs. Then create the combinations and select the rows of the
      #### emmeans estimates needed for it. It selects the rows using the levels found in datamatic
      ### for the raw data, it selects only if the moderator is a factor, whereas all data for the 
      ### continuous are retained
      #### TODO: this is abstruse, try changing it
      
      dims<-sapply(moderators, function(mod) private$.datamatic$variables[[tob64(mod)]]$levels_labels,simplify = FALSE)
      rawData<-mf.getModelData(private$.operator$model)
      for (var in names(rawData)) {
        if (is.factor(rawData[[var]]))
           levels(rawData[[var]])<-gsub(LEVEL_SYMBOL,"",levels(rawData[[var]]),fixed = T)
      }
      
      ### here we deal with plotting random effects, if needed
      randomData<-NULL
      if (self$option("plotRandomEffects")) {
        
        newdata<-rawData
        mvars<-names(newdata)
        self$scatterCluster<-private$.datamatic$variables[[private$.operator$clusters[[1]]]]
        tozero<-setdiff(mvars,c(self$scatterX$name64,self$scatterY$name64,self$scatterCluster$name64))
        toaggregate<-list()
        for(v in tozero)
          if (!is.factor(newdata[,v])) {
            center<-mean(newdata[,v])
            newdata[,v]<-center
          } else {
            d<-dim(contrasts(newdata[,v]))
            contrasts(newdata[,v])<-matrix(0,d[1],d[2])
          }

        y<-stats::predict(private$.operator$model,type="response",newdata=newdata,allow.new.levels=TRUE)
          # end of zeroing 
        randomData<-as.data.frame(cbind(y,rawData))
        self$dispatcher$warnings<-list(topic="plot",message=paste("Random effects are plotted across",self$scatterCluster$name))

      }
      ### end of random ###

      ### deal with Y ####
        dep64  <- self$scatterY$name64

        ### give a range to the y-axis, if needed
        if (self$options$plotDvScale)
            self$scatterRange<-c(self$scatterY$descriptive$min,self$scatterY$descriptive$max)

      if (self$option("modelSelection","multinomial")) {
        self$scatterRaw<-FALSE
      }
      
      if (self$scatterType=="link") {
        self$scatterRaw<-FALSE
        self$scatterRange<-NULL
      }
      
      
      
      ### we need to be sure that the dependent variable is a continuous variable to plot the raw data ##
      
        if (self$scatterY$type=="factor") {
           levels(rawData[[dep64]])<-0:(self$scatterY$nlevels-1)
           rawData[[dep64]]<-as.numeric(as.character(rawData[[dep64]]))
        }
        if (self$option("modelSelection","ordinal")) {
          rawData[[dep64]]<-rawData[[dep64]]+1
          if (self$option("plotScale","mean.class"))
                    self$scatterRange<-c(1,self$scatterY$nlevels)
        }

      #### deal with rescaling
      if (self$scatterXscale) {
        data[[self$scatterX$name64]]<-private$.rescale(self$scatterX,data[[self$scatterX$name64]])
        if (is.something(rawData))
          rawData[[self$scatterX$name64]]<-private$.rescale(self$scatterX,rawData[[self$scatterX$name64]])
        if (is.something(randomData))
          randomData[[self$scatterX$name64]]<-private$.rescale(self$scatterX,randomData[[self$scatterX$name64]])
        
      }
      
      
      
      if (is.something(dims))  {
        
             grid<-expand.grid(dims,stringsAsFactors = FALSE)
            .names<-names(grid)
            .names64<-tob64(.names)
             selectable<-intersect(.names,self$options$factors)
             selgrid<-as.data.frame(grid[,selectable])
            .sel64<-tob64(selectable)

             for (i in 1:nrow(grid)) {
                label<-paste(.names,grid[i,],sep="=",collapse = " , ")
                aplot<-resultsgroup$get(key=i)
                aplot$setTitle(label)
                sel<-paste(paste0("data$",.names64,sep=""),paste0('"',grid[i,],'"'),sep="==",collapse = " & ")
                localdata<-data[eval(parse(text=sel)),]
                self$plotData[[i]]<-localdata
                
                if (self$scatterRaw) {
                       if (length(selectable)>0) {
                            sel<-paste(paste0("data$",.sel64,sep=""),paste0('"',selgrid[i,],'"'),sep="==",collapse = " & ")
                            raw<-rawData[eval(parse(text=sel)),]
                       } else
                            raw<-rawData
                    
                      self$rawData[[i]]<-raw
                }
                
                if (!is.null(randomData)) {
                       if (length(selectable)>0) {
                              sel<-paste(paste0("randomData$",.sel64,sep=""),paste0('"',selgrid[i,],'"'),sep="==",collapse = " & ")
                              rdata<-randomData[eval(parse(text=sel)),]
                       } else 
                              rdata<-randomData
                         
                        if (is.something(self$scatterZ) && self$scatterZ$type=="factor" && self$scatterZ$isBetween) {
                                   selectorlist<-list(rdata[[self$scatterCluster$name64]], rdata[[self$scatterX$name64]], rdata[[self$scatterZ$name64]])
                                   .rnames<-c("cluster","x","z","y")
                        }
                        else {
                                   selectorlist<-list(rdata[[self$scatterCluster$name64]], rdata[[self$scatterX$name64]])
                                   .rnames<-c("cluster","x","y")
                                   
                        }

                        rdata<- stats::aggregate(rdata$y, selectorlist, mean)
                        names(rdata)<-.rnames
                        self$randomData[[i]]<-rdata
                }
             }

            }  else {
             aplot<-resultsgroup$get(key=resultsgroup$itemKeys[[1]])
             aplot$setTitle(jmvcore::stringifyTerm(c(self$scatterX$name,self$scatterZ$name)))
             self$plotData[[1]]<-data
             if (self$scatterRaw) 
                  self$rawData[[1]]<-rawData
             
             if (!is.null(randomData)) {
               
               rdata<-randomData
               if (is.something(self$scatterZ) && self$scatterZ$type=="factor" && self$scatterZ$isBetween) {
                   selectorlist<-list(rdata[[self$scatterCluster$name64]], rdata[[self$scatterX$name64]], rdata[[self$scatterZ$name64]])
                  .rnames<-c("cluster","x","z","y")
               }
               else {
                  selectorlist<-list(rdata[[self$scatterCluster$name64]], rdata[[self$scatterX$name64]])
                 .rnames<-c("cluster","x","y")
                 
               }
               rdata<- stats::aggregate(rdata$y, selectorlist, mean)
               names(rdata)<-.rnames
               self$randomData[[1]]<-rdata
               
             }
      }

    },
    .prepareClusterBoxplot=function() {
      
      if (!self$option("clusterBoxplot"))
        return()
      
      ### we get the clusters from the model because the model may contain less cluster variables than selected
      clusters<-names(private$.operator$model@cnms)
      
      resultsgroup<-private$.results$assumptions$clusterBoxplot

      for (cluster in clusters) {
        title<-paste("Clustering variable:", fromb64(cluster))
        id<-cluster
        resultsgroup$addItem(id)
        resultsgroup$get(key=id)$setTitle(title)
        resultsgroup$get(key=id)$setState(list(cluster=cluster,label=fromb64(cluster)))
        
      }
    },
    .prepareClusterResPred=function() {
      
      if (!self$option("clusterResPred"))
        return()
      
      ### we get the clusters from the model because the model may contain less cluster variables than selected
      clusters<-names(private$.operator$model@cnms)
      
      resultsgroup<-private$.results$assumptions$clusterResPred
      
      for (cluster in clusters) {
        title<-paste("Clustering variable:", fromb64(cluster))
        id<-cluster
        resultsgroup$addItem(id)
        resultsgroup$get(key=id)$setTitle(title)
        resultsgroup$get(key=id)$setState(list(cluster=cluster,label=fromb64(cluster)))
        
      }
    },
    
    .prepareRandHist=function() {
      
      if (!self$option("randHist"))
        return()
      
  
      if (!is.something(private$.operator$model))
            return()
      
      res<-lme4::ranef(private$.operator$model)
      clusters64<-names(res)
      
      resultsgroup<-private$.results$assumptions$randHist
      
      for (cluster in clusters64) {
        clusterres<-res[[cluster]]
        vars<-names(clusterres)
        for (v in vars) {
          data<-data.frame(clusterres[,v])
          names(data)<-"x"
          label<-fromb64(v,self$vars)
          title<-paste("Coefficient",label," random across",fromb64(cluster))
          id<-paste0(v,cluster)
          resultsgroup$addItem(id)
          resultsgroup$get(key=id)$setTitle(title)
          resultsgroup$get(key=id)$setState(list(data=data,label=label))
        }
        
      }
      
    },
    
    .estimate=function(x,term) {
      
      x64<-tob64(x)
      term64<-tob64(term)
      conditions<-list()
      labels<-list()
      for (.term in term64) {
        var<-private$.datamatic$variables[[.term]]
        if( var$type=="numeric") {
          conditions[[.term]]<-var$levels 
          labels[[.term]]<-var$levels_labels 
        }
      }
      xobj<-private$.datamatic$variables[[x64]]
      
      if (xobj$type=="numeric") {
           conditions[[x64]]<-pretty(c(xobj$descriptive$min,xobj$descriptive$max),n=30)
           if (self$option("plotOriginalScale")) {
                 self$scatterXscale<-TRUE
                 self$dispatcher$warnings<-list(topic="plot",message="Note: The X-axis is in the X-variable original scale")
           }
      }
      allterm64<-c(x64,term64)

      mode <- NULL

      
      if (self$option("modelSelection","ordinal")) {
            if (self$option("plotScale","mean.class"))
                  mode<-"mean.class"
            else  {
                  mode<-"prob"
                  self$scatterRaw<-FALSE
            }
      }

      ### now we get the estimated means #######

      em_opts<-list(
        private$.operator$model,
        specs=allterm64,
        at=conditions,
        type=self$scatterType,
        mode=mode,
        nesting = NULL,
        options  = list(level = private$.operator$ciwidth)
      )
      
      if (self$option("dfmethod"))
             em_opts[["lmer.df"]]<-self$options$dfmethod

      results<-try_hard(do.call(emmeans::emmeans,em_opts))
      self$dispatcher$warnings<-list("topic"="plot",message=results$warning)
      self$dispatcher$errors<-list("topic"="plot",message=results$error)
      referenceGrid<-results$obj
      tableData<-as.data.frame(referenceGrid)
      ### rename the columns ####
      names(tableData)<-c(allterm64,"estimate","se","df","lower","upper")
      if (self$options$plotError=="se") {
           tableData$lower<-tableData$estimate-tableData$se
           tableData$upper<-tableData$estimate+tableData$se
      }
      if (self$options$plotError=="none") {
        tableData$lower<-NULL
        tableData$upper<-NULL
      }
      
      if (xobj$type=="factor") {
             tableData[[xobj$name64]]<-factor(tableData[[xobj$name64]])
             levels(tableData[[xobj$name64]])<-private$.datamatic$variables[[xobj$name64]]$levels_labels
      }
        
      for (term in term64) {
              tableData[[term]]<-factor(tableData[[term]])
              levels(tableData[[term]])<-private$.datamatic$variables[[term]]$levels_labels
      }
      tableData
      
    },
    .rescale=function(varobj,values) {
      
      if (varobj$scaling=="clusterbasedcentered")
          self$dispatcher$warnings<-list(topic="plot",message="Rescaling cluster-wise centered variables may be misleading. Use `Covariates Scaling=None` is the original scale is necessary.")
        
#      len <- sapply(values,function(x)   nchar(as.character(x))-nchar(as.character(trunc(x)))-1)
#      len <- max(min(len,na.rm = T),0)
      if (varobj$scaling=="centered")
          values<-values+varobj$original_descriptive$mean
      if (varobj$scaling=="standardized") 
        values<-varobj$original_descriptive$sd*values+varobj$original_descriptive$mean
      if (varobj$scaling=="log") 
        values<-exp(values)
      
      values
      
    }
    

  ) # end of private
) # end of class
    