Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
      plotData=list(),
      rawData=list(),
      scatterRange=NULL,
      scatterDodge=NULL,
      scatterClabel=NULL,
      scatterY=NULL,
      scatterX=NULL,
      scatterZ=NULL,
      scatterBars=FALSE,
      initialize=function(operator,results) {
            super$initialize(options=operator$options,vars=operator$vars)
            private$.results<-results
            private$.operator<-operator
            private$.datamatic<-operator$datamatic
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
        
        if (self$options$plotRaw) {
          
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
            dims<-1
             
            moderators<-self$options$plotSepPlots
            dims<-unlist(lapply(moderators, function(mod) private$.datamatic$variables[[tob64(mod)]]$nlevels))
            
             n<-prod(dims)
             for (i in seq_len(n)) {
                resultsgroup$addItem(key=i)
             }
             
             ### some options here 
             self$scatterX<-private$.datamatic$variables[[tob64(x)]]
             self$scatterY<-private$.datamatic$variables[[tob64(y)]]
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
      
      moderators<-self$options$plotSepPlots

      ### compute the expected values to be plotted ###
      data<-private$.estimate(self$scatterX$name,unlist(c(self$scatterZ$name,moderators)))
      ### give a range to the y-axis, if needed
      if (self$options$plotDvScale)
           self$scatterRange<-private$.datamatic$variables[[self$scatterY$name64]]$descriptive
      
      #### compute the levels combinations
      #### first, gets all levels of factors and covs. Then creates the combinations and select the rows of the
      #### emmeans estimates needed for it. It selects the rows using the levels found in datamatic
      ### for the raw data, it selects only if the moderator is a factor, whereas all data for the 
      ### continuous are retained
      #### TODO: this is abstruse, try changing it
      
      dims<-sapply(moderators, function(mod) private$.datamatic$variables[[tob64(mod)]]$levels_labels,simplify = FALSE)
      rawData<-mf.getModelData(private$.operator$model)
      
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
                
                if (self$options$plotRaw) {
                    if (length(selectable)>0) {
                        sel<-paste(paste0("data$",.sel64,sep=""),paste0('"',selgrid[i,],'"'),sep="==",collapse = " & ")
                        raw<-rawData[eval(parse(text=sel)),]
                    } else
                        raw<-rawData
                    
                    self$rawData[[i]]<-rawData
                }
              }

      }  else {
             aplot<-resultsgroup$get(key=resultsgroup$itemKeys[[1]])
             aplot$setTitle(jmvcore::stringifyTerm(c(self$scatterX$name,self$scatterZ$name)))
             self$plotData[[1]]<-data
             if (self$options$plotRaw) self$rawData[[1]]<-rawData

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
        conditions[[x64]]<-pretty(xobj$descriptive,n=10)
      }
      
      allterm64<-c(x64,term64)

      ### now we get the estimated means #######
      results<-try_hard(emmeans::emmeans(private$.operator$model,specs=allterm64,
                                      at=conditions,nesting = NULL,
                                      options  = list(level = private$.operator$ciwidth)))

      self$warnings<-list("topic"="plot",message=results$warning)
      self$warnings<-list("topic"="plot",message=results$error)
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
      
    }
    

  ) # end of private
) # end of class
    