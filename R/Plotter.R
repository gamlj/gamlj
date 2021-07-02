Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
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
      initialize=function(operator,results) {
            super$initialize(options=operator$options,vars=operator$vars)
            private$.results<-results
            private$.operator<-operator
            private$.datamatic<-operator$datamatic
            self$scatterRaw<-self$options$plotRaw
      },

      initPlots=function() {
        
           private$.initMainPlot()

      },
      preparePlots=function(image, ggtheme, theme, ...) {
        
        private$.prepareMainPlot()
        
      },
      scatterPlot=function(image) {

        ## collect the data 
        data<-self$plotData[[image$key]]
        
        ### prepare aestetics for one or two way scatterplot
        if (is.null(self$scatterZ)) {
                     names(data)[1]<-"x"
                    .aestetics<-ggplot2::aes_string(x = "x", y = "estimate",group=1)
                    .aesbar<-ggplot2::aes_string(x = "x", ymin = "lower", ymax = "upper")
                    
                    
        } else {
                     names(data)[1:2]<-c("x","z")
                    .aestetics<-ggplot2::aes_string(x = "x", y = "estimate",   group = "z", colour = "z")
                    .aesbar<-ggplot2::aes_string(x = "x", ymin = "lower", ymax = "upper", group = "z",color = "z" ,fill="z")
        }
        
        ## initializa plot 
        p <- ggplot2::ggplot()
        
        # give a scale to the Y axis
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
        
        if (!is.null(self$randomData)) {
          
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
        
        
        p <- p + ggplot2::labs(x = self$scatterX$name, y = self$scatterY$name, colour = self$scatterClabel)
        
        if (self$scatterXscale) {
             
          o<-ggplot2::ggplot_build(p)
          values<-o$layout$panel_params[[1]]$x$breaks
          newlabs<-private$.rescale(self$scatterX,values)
          p<-p+ggplot2::scale_x_continuous(labels= newlabs)
        }
           
        
        return(p)        
      }
      
  ), # end of public
  private = list(
    .datamatic=FALSE,
    .results=NULL,
    .operator=NULL,
    .initMainPlot=function() {
      
            if (!is.something(self$options$plotHAxis)) 
                   return()
            

            resultsgroup<-private$.results$get("mainPlots")
            y<-self$options$dep
            x<-self$options$plotHAxis
            z<-self$options$plotSepLines
            moderators<-self$options$plotSepPlots
            
            if (self$options$modelSelection=="multinomial") {
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
               self$scatterClabel <- paste(x, paste0("(", toupper(self$options$plotError), ")"), sep = "\n")
             } else {
               self$scatterDodge<-ggplot2::position_dodge(0)
               self$scatterClabel <- self$scatterZ$name
             }
             
             
             
             

    },
    .prepareMainPlot=function() {
      

      if (!is.something(self$options$plotHAxis)) 
        return()


      resultsgroup<-private$.results$get("mainPlots")
      
      moderators<-self$scatterModerators

      
      ### give a range to the y-axis, if needed
      if (self$options$plotDvScale)
               self$scatterRange<-c(self$scatterY$descriptive$min,self$scatterY$descriptive$max)
      
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
      
      ### here we deal with plotting random effects, if needed
      randomData<-NULL
      if (self$option("plotRandomEffects")) {
        
        newdata<-rawData
        mvars<-names(newdata)
        self$scatterCluster<-private$.datamatic$variables[[tob64(self$options$cluster[[1]])]]
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
        self$warnings<-list(topic="plot",message=paste("Random effects are plotted across",self$scatterCluster$name))

      }

      ### we need to be sure that the dependent variable is a continuous variable to plot the raw data ##
        dep64  <- tob64(self$options$dep)
        depobj <- private$.datamatic$variables[[dep64]]
       
        if (depobj$type=="factor") {
           levels(rawData[[dep64]])<-0:(depobj$nlevels-1)
           rawData[[dep64]]<-as.numeric(as.character(rawData[[dep64]]))
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
                         
                        if (is.something(self$scatterZ) && self$scatterZ$type=="factor") {
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
               if (is.something(self$scatterZ) && self$scatterZ$type=="factor") {
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
           if (self$option("plotOriginalScale"))
                 self$scatterXscale<-TRUE
      }
      allterm64<-c(x64,term64)

      type="response"
      if (self$option("plotLp")) {
          type<-"link"
          self$scatterRaw<-FALSE
          self$scatterRange<-NULL
      }
      

      ### now we get the estimated means #######
      
      results<-try_hard(emmeans::emmeans(private$.operator$model,
                                         specs=allterm64,
                                         at=conditions,
                                         type=type,
                                         nesting = NULL,
                                         options  = list(level = private$.operator$ciwidth)))

      self$warnings<-list("topic"="plot",message=results$warning)
      self$errors<-list("topic"="plot",message=results$error)
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

      for (term in term64) {
              tableData[[term]]<-factor(tableData[[term]])
              levels(tableData[[term]])<-private$.datamatic$variables[[term]]$levels_labels
      }
      tableData
      
    },
    .rescale=function(varobj,values) {
      
      
      len <- sapply(values,function(x)   nchar(as.character(x))-nchar(as.character(trunc(x)))-1)
      len <- max(min(len,na.rm = T),0)
      if (varobj$scaling=="centered")
          values<-values+varobj$original_descriptive$mean
      if (varobj$scaling=="standardized") 
        values<-varobj$original_descriptive$sd*values+varobj$original_descriptive$mean
      if (varobj$scaling=="standardized") 
        values<-varobj$original_descriptive$sd*values+varobj$original_descriptive$mean
      if (varobj$scaling=="log") 
        values<-exp(values)
      
      values<-round(values,digits = len)

      values
      
    }
    

  ) # end of private
) # end of class
    