Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Scaffold,
  public=list(
      options=NULL,
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
      initialize=function(jmvobj,operator) {
        
            super$initialize(jmvobj)
            private$.results<-jmvobj$results
            private$.operator<-operator
            private$.datamatic<-operator$datamatic
            self$scatterRaw<-self$options$plot_raw
            if (self$option("plot_scale"))
                    self$scatterType<-self$options$plot_scale
            else 
                    self$scatterType<-"response"
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
        private$.prepareClusterResPredGrid()
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
                    .aestetics<-ggplot2::aes(x = x, y = estimate,group=1)
                    .aesbar<-ggplot2::aes(x = x, ymin = lower, ymax = upper)
        } else {
                     names(data)[1:2]<-c("x","z")
                     if (isFALSE(self$options$plot_black)) {
                         .aestetics<-ggplot2::aes(x = x, y = estimate,   group = z, colour = z)
                         .aesbar<-ggplot2::aes(x = x, ymin = lower, ymax = upper, group = z,color = z ,fill=z)
                     } else {
                         .aestetics<-ggplot2::aes(x = x, y = estimate,   group = z, linetype = z)
                         .aesbar<-ggplot2::aes(x = x, ymin = lower, ymax = upper, group = z,linetype = z )
                     }
        }
        
        ## initialize plot 
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
                      .aesrandom<-ggplot2::aes(x = x, y = y, group=cluster, colour=z)
                       p <- p + ggplot2::geom_line(data = randomData, 
                                                  .aesrandom, 
                                                  linewidth = 0.4, 
                                                  alpha = .50)
          }
          else { 
                      .aesrandom<-ggplot2::aes(x = x, y = y, group=cluster)
                      p <- p + ggplot2::geom_line(data = randomData, 
                                                  .aesrandom,
                                                  color="gray74",
                                                  linewidth = 0.4, 
                                                  alpha = .80)
                      if (attr(randomData,"xbetween"))
                                  p <- p + ggplot2::geom_point(data = randomData, 
                                                  .aesrandom,
                                                  color="gray4",
                                                  linewidth = 0.4, 
                                                  alpha = .80)
                      
          }

        }
        
        ######### fix the bars ##########        
        if (self$scatterBars) {
          if (self$scatterX$type=="factor")
            p <- p + ggplot2::geom_errorbar(data = data, .aesbar, linewidth = .9, width=.3, position = self$scatterDodge)
          else
            p <- p + ggplot2::geom_ribbon(data = data, .aesbar, linetype = 0, show.legend = F, alpha = 0.2)
        }
        #########  ##########        
        
        
        
        ### plot the lines 
        p <- p + ggplot2::geom_line(data = data, 
                                    .aestetics,
                                     linewidth = 1.2, 
                                     position=self$scatterDodge)


        ### plot the points for factors
        if (self$scatterX$type=="factor")
              p <- p +  ggplot2::geom_point(data = data,
                                            .aestetics,
                                            shape = 21, size = 4, fill="white",
                                            position = self$scatterDodge,show.legend = FALSE)
        
        if (isFALSE(self$options$plot_black))
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
        
              if (!self$option("qq_plot"))
                         return()
        
              if (!is.something(private$.operator$model))
                         return()

              residuals <- as.numeric(scale(stats::residuals(private$.operator$model)))
              df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
              plot<-ggplot2::ggplot(data=df, ggplot2::aes(y=y, x=x)) +
                ggplot2::geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
                ggplot2::geom_point(ggplot2::aes(x=x,y=y), linewidth=2, colour=theme$color[1]) +
                ggplot2::xlab("Theoretical Quantiles") +
                ggplot2::ylab("Standardized Residuals") 
      
               plot+ggtheme
      },
      
      normplot=function(theme,ggtheme)  {
        
        if (!self$option("norm_plot"))
          return()
        
        if (!is.something(private$.operator$model))
          return()
        
        fill <- theme$fill[2]
        color <- theme$color[1]
        data <- as.data.frame(stats::residuals(private$.operator$model))
        names(data) <- "x"
        # library(ggplot2)
        plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
          ggplot2::labs(x = "Residuals", y = "density")
        
#        plot <- plot + ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), position = "identity", stat = "bin", color = color, fill = fill)
        plot <- plot + ggplot2::geom_histogram(ggplot2::aes_string(y = "..density.."), position = "identity", stat = "bin", color = color, fill = fill)
        plot <- plot + ggplot2::stat_function(fun = stats::dnorm, args = list(mean = mean(data$x), sd = stats::sd(data$x)))
        
        themeSpec <- ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
        plot <- plot + ggtheme + themeSpec
        
        return(plot)

      },
      residPlot=function(theme,ggtheme)  {

#        if (!self$option("resid_plot") && !self$option("cluster_respred") )
#          return()
        
        if (!self$option("resid_plot"))
          return()        
        

        if (!is.something(private$.operator$model))
          return()
        
        
            fill <- theme$fill[2]
            color <- theme$color[1]
            data <- as.data.frame(stats::residuals(private$.operator$model))
            names(data) <- "res"
            data$pred <- stats::predict(private$.operator$model)

              # library(ggplot2)
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = pred, y = res)) + 
                       ggplot2::labs(x = "Predicted", y = "Residuals")
      
            plot <- plot + ggplot2::geom_point(shape = 21, color = color, fill = fill)
            plot <- plot + ggtheme
            return(plot)
      },
      
      clusterBoxplot=function(image,ggtheme,theme)  {
        
        ########## working here ##########

        if (!self$option("cluster_boxplot"))
          return()
        

        if (!is.something(private$.operator$model) )
          return(FALSE)
        
        cluster<-image$state$cluster

        fmodel<-lme4::fortify.merMod(private$.operator$model)
        
        if (inherits(private$.operator$model,"lme"))
          fmodel$.resid<-stats::resid(private$.operator$model,type="normalized")

        fmodel$cluster<-fmodel[[cluster]]
        plot<-ggplot2::ggplot(fmodel, ggplot2::aes(cluster,.resid)) +
              ggplot2::geom_boxplot() + ggplot2::coord_flip()
        plot<-plot+ggplot2::xlab(fromb64(cluster))+ggplot2::ylab("Residuals")
        plot<-plot+ ggtheme 

        return(plot)
        
      },
      clusterResPred=function(image,ggtheme,theme)  {
        
        ########## working here ##########
        
        if (!self$option("cluster_respred"))
          return()
        
        
        if (!is.something(private$.operator$model) )
          return(FALSE)
        
        cluster<-image$state$cluster
        data<-lme4::fortify.merMod(private$.operator$model)
        if (inherits(private$.operator$model,"lme"))
          data$.resid<-stats::resid(private$.operator$model,type="normalized")
        
        data$cluster<-data[[cluster]]
        plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .fitted, y = .resid,color=cluster)) 
        plot <- plot + ggplot2::labs(x = "Predicted", y = "Residuals", color=fromb64(cluster))
        plot <- plot + ggplot2::geom_point(shape = 21)
        plot <- plot + ggtheme
        plot <- plot + ggplot2::theme(legend.position="bottom")
        return(plot)
        
      },
      clusterResPredGrid=function(image,ggtheme,theme)  {
  
      
          if (!self$option("cluster_respred"))
             return()
  
  
          if (!is.something(private$.operator$model) )
            return(FALSE)
  
          cluster<-image$state$cluster
          data<-lme4::fortify.merMod(private$.operator$model)
          if (inherits(private$.operator$model,"lme"))
                   data$.resid<-stats::resid(private$.operator$model,type="normalized")
  
          data$cluster<-data[[cluster]]
          plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .fitted, y = .resid)) 
          plot <- plot + ggplot2::labs(x = "Predicted", y = "Residuals")
          plot <- plot + ggplot2::geom_point(shape = 21)
          plot <- plot + ggplot2::geom_hline(yintercept = 0, colour = "gray")
          plot <- plot + ggtheme 
          plot <- plot + ggplot2::theme(legend.position="bottom")
          plot <- plot + ggplot2::facet_wrap(cluster)
          return(plot)
       },

        randHist=function(image,ggtheme,theme)  {
  
  
              if (!self$option("rand_hist"))
                  return()
  
  
              if (!is.something(private$.operator$model) )
                  return(FALSE)
 
              label<-image$state$label
              data<-image$state$data
              fill <- theme$fill[2]
              color <- theme$color[1]
              alpha <- 0.4
              plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=x)) +
                           ggplot2::labs(x="Coefficients", y='density')
          
              plot <- plot + ggplot2::geom_histogram(ggplot2::aes(y=..density..), position="identity",
                                        stat="bin", color=color, fill=fill)
              plot <- plot + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(data$x), sd = sd(data$x)))  
          
              themeSpec <- ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                             axis.ticks.y=ggplot2::element_blank())
              plot <- plot + ggtheme + themeSpec
          
          
          return(plot)
  
        }

  ), # end of public
  private = list(
    .datamatic=FALSE,
    .results=NULL,
    .operator=NULL,
    .initMainPlot=function() {
      
            if (!is.something(self$options$plot_x)) 
                   return()
            
            jinfo("PLOTTER: init main plot")
      
            resultsgroup<-private$.results$get("mainPlots")
            y<-self$options$dep
            x<-self$options$plot_x
            z<-self$options$plot_z
            moderators<-self$options$plot_by
            if (self$options$model_type=="multinomial") {
                     moderators       <- c(z,moderators)
                     z                <- self$options$dep
                     self$scatterType <- "response"
            }

            if (self$options$model_type=="ordinal" & self$scatterType!="mean.class") {
                     moderators <- c(z,moderators)
                     z          <- self$options$dep
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
             

             if (self$options$plot_around != "none") {
               self$scatterBars<-TRUE
               self$scatterDodge <- ggplot2::position_dodge(0.2)
               self$scatterClabel <- paste(self$scatterZ$name, paste0("(", toupper(self$options$plot_around), ")"), sep = "\n")
             } else {
               self$scatterDodge<-ggplot2::position_dodge(0)
               self$scatterClabel <- self$scatterZ$name
             }

    },
    .prepareMainPlot=function() {

      

      if (!is.something(self$options$plot_x)) 
        return()

      jinfo("PLOTTER: prepare main plot")
      
      private$.results$plotnotes$setContent("")
      
      resultsgroup<-private$.results$get("mainPlots")

      ### stop is filled from previous run ###
      test<-any(unlist(sapply(resultsgroup$items, function(i) !i$isNotFilled())))
      if (test)
           return()
      
      moderators<-self$scatterModerators
      ### compute the expected values to be plotted ###
      data<-private$.estimate(self$scatterX$name,unlist(c(self$scatterZ$name,moderators)))
      rawData<-mf.data(private$.operator$model)
      
      ### here we deal with plotting random effects, if needed
      randomData<-NULL

      if (self$option("plot_re") && 
          !self$option("model_type","ordinal") &&
          !self$option("model_type","multinomial"))     {
        self$scatterCluster<-private$.datamatic$variables[[tob64(private$.operator$formulaobj$clusters[[1]])]]
        if (self$option("plot_re_method","average")) {
            formula<-private$.operator$formulaobj$keep(self$scatterX$name)
            .model<-mf.update(private$.operator$model,formula=formula)
        } else
            .model<-private$.operator$model
        y<-stats::predict(.model,type=self$scatterType)
        randomData<-as.data.frame(cbind(y,rawData))
        if (self$scatterX$type=="factor")
             levels(randomData[[self$scatterX$name64]])<-self$scatterX$levels_labels
                    
        self$warning<-list(topic="plotnotes",message=paste("Random effects are plotted across",self$scatterCluster$name))
        # prepare a test for between variables to plot dots for random effects

        test<-tapply(as.numeric(rawData[[self$scatterX$name64]]),rawData[[self$scatterCluster$name64]],sd)
        test<-sum(sapply(test,function(x) as.numeric(is.na(x) || x==0)))
        nc<-self$scatterCluster$nlevels
        xbetween<-FALSE
        if ((test/nc)>.30) xbetween<-TRUE
      }
      ### end of random ###
      
      ## deal with raw data, if needed

      if (self$option("plot_raw")) {
            for (var in intersect(names(rawData),names(private$.datamatic$variables))) {
                  varobj<-private$.datamatic$variables[[var]]
                  if (varobj$type=="factor")
                  levels(rawData[[var]])<-varobj$levels_labels
            }
      }
      ## end of raw data
    

      ### deal with Y ####
        dep64  <- self$scatterY$name64

        ### give a range to the y-axis, if needed
        if (self$options$plot_yscale)
            self$scatterRange<-c(self$scatterY$descriptive$min,self$scatterY$descriptive$max)

      if (self$option("model_type","multinomial")) {
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
        if (self$option("model_type","ordinal")) {
          rawData[[dep64]]<-rawData[[dep64]]+1
          if (self$option("plot_scale","mean.class"))
                    self$scatterRange<-c(1,self$scatterY$nlevels)
        }
      if (self$option("model_type",c("logistic","multinomial"))) {
          self$scatterRange<-c(0,1)
      }

      
      #### deal with rescaling
      if (self$scatterXscale) {
        
        if (self$scatterX$covs_scale=="clusterbasedcentered")
          self$warning<-list(topic="plotnotes",
                                         message="Rescaling cluster-wise centered variables may be misleading. Use `Covariates Scaling=None` if the original scale is necessary.")

        if (self$scatterX$covs_scale=="clusterbasedstandardized")
          self$warning<-list(topic="plotnotes",
                                         message="Rescaling cluster-wise standardized variables may be misleading. Use `Covariates Scaling=None` if the original scale is necessary.")
        
        data[[self$scatterX$name64]]<-private$.rescale(self$scatterX,data[[self$scatterX$name64]])
        
        if (is.something(rawData))
            rawData[[self$scatterX$name64]]<-private$.rescale(self$scatterX,rawData[[self$scatterX$name64]])
        if (is.something(randomData))
          randomData[[self$scatterX$name64]]<-private$.rescale(self$scatterX,randomData[[self$scatterX$name64]])
      }
      
      #### compute the levels combinations
      #### first, gets all levels of factors and covs. Then create the combinations and select the rows of the
      #### emmeans estimates needed for it. It selects the rows using the levels found in datamatic
      ### for the raw data, it selects only if the moderator is a factor, whereas all data for the 
      ### continuous are retained
      #### TODO: this is abstruse, try changing it

      dims<-sapply(moderators, function(mod) private$.datamatic$variables[[tob64(mod)]]$levels_labels,simplify = FALSE)

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
                            sel<-paste(paste0("rawData$",.sel64,sep=""),paste0('"',selgrid[i,],'"'),sep="==",collapse = " & ")
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
                         
                       selectorlist<-list(rdata[[self$scatterCluster$name64]], rdata[[self$scatterX$name64]])
                       .rnames<-c("cluster","x","y")
                        rdata<- stats::aggregate(rdata$y, selectorlist, mean)
                        names(rdata)<-.rnames
                        attr(rdata,"xbetween")<-xbetween
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
               selectorlist<-list(rdata[[self$scatterCluster$name64]], rdata[[self$scatterX$name64]])
              .rnames<-c("cluster","x","y")
               rdata<- stats::aggregate(rdata$y, selectorlist, mean)
               names(rdata)<-.rnames
               attr(rdata,"xbetween")<-xbetween
               self$randomData[[1]]<-rdata
               
             }
      }

    },
    .prepareClusterBoxplot=function() {
      
      if (!self$option("cluster_boxplot"))
        return()
      
      ### we get the clusters from the model because the model may contain less cluster variables than selected
      if (inherits(private$.operator$model,"lme"))
        clusters<-names(private$.operator$model$groups)
      else
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
      
      if (!self$option("cluster_respred"))
        return()
      
      ### we get the clusters from the model because the model may contain less cluster variables than selected
      if (inherits(private$.operator$model,"lme"))
        clusters<-names(private$.operator$model$groups)
      else
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
    .prepareClusterResPredGrid=function() {
      
      if (!self$option("cluster_respred_grid"))
        return()
      
      ### we get the clusters from the model because the model may contain less cluster variables than selected
      if (inherits(private$.operator$model,"lme"))
        clusters<-names(private$.operator$model$groups)
      else
        clusters<-names(private$.operator$model@cnms)
      
      resultsgroup<-private$.results$assumptions$clusterResPredGrid
      
      for (cluster in clusters) {
        title<-paste("Clustering variable:", fromb64(cluster))
        id<-cluster
        resultsgroup$addItem(id)
        resultsgroup$get(key=id)$setTitle(title)
        resultsgroup$get(key=id)$setState(list(cluster=cluster,label=fromb64(cluster)))
        
      }
    },
    
    .prepareRandHist=function() {
      
      if (!self$option("rand_hist"))
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
           if (self$option("plot_xoriginal")) {
                 self$scatterXscale<-TRUE
                 self$warning<-list(topic="plotnotes",message="The X-axis is in the X-variable original scale")
           }
      }
      allterm64<-c(x64,term64)

      mode <- NULL

      
      if (self$option("model_type","ordinal")) {
            if (self$option("plot_scale","mean.class"))
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
        options  = list(level = private$.operator$ciwidth),
        data =  insight::get_data(private$.operator$model, source="frame")

      )

      ### mmblogit model data are not recognized by emmeans. We need to pass them explicetely      
      if (self$option("model_type","multinomial") & self$option(".caller","glmer"))
           em_opts[["data"]]<-private$.operator$model$data

      if (self$option("df_method"))
             em_opts[["lmer.df"]]<-self$options$df_method

      results<-try_hard(do.call(emmeans::emmeans,em_opts))
      self$warning<-list("topic"="plotnotes",message=results$warning)
      self$error<-list("topic"="plotnotes",message=results$error)
      referenceGrid<-results$obj
      tableData<-as.data.frame(referenceGrid)
      ### rename the columns ####
      names(tableData)<-c(allterm64,"estimate","se","df","lower","upper")

      if (self$options$plot_around=="se") {
           tableData$lower<-tableData$estimate-tableData$se
           tableData$upper<-tableData$estimate+tableData$se
      }
      if (self$options$plot_around=="none") {
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


#      len <- sapply(values,function(x)   nchar(as.character(x))-nchar(as.character(trunc(x)))-1)
#      len <- max(min(len,na.rm = T),0)
      if (varobj$covs_scale=="centered")
          values<-values+varobj$original_descriptive$mean
      if (varobj$covs_scale=="standardized") 
        values<-varobj$original_descriptive$sd*values+varobj$original_descriptive$mean
      if (varobj$covs_scale=="log") 
        values<-exp(values)
      
      values
      
    }
    

  ) # end of private
) # end of class
    
