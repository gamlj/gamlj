gamljGlmClass <- R6::R6Class(
  "gamljGlmClass",
  inherit = gamljGlmBase,
  private=list(
    .model=NA,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .init=function() {
      ginfo("init")
      
      class(private$.results) <- c('gamlj', class(private$.results))
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhorse class
      
      data_machine<-Datamatic$new(self$options,self$data)
      estimate_machine<-Estimate$new(self$options,data_machine)
      plotter_machine<-Plotter$new(estimate_machine,self$results)

      ### info table ###
      j.init_table(self$results$info,estimate_machine$tab_info) 

      ### r2 table does not need initializing ###

      ### anova table ###
      j.init_table(self$results$main$anova,estimate_machine$tab_anova) 

      ### estimates table ###
      j.init_table(self$results$main$coefficients,estimate_machine$tab_coefficients, ci=T,ciwidth=self$options$ciWidth)

      if (!is.something(self$options$factors))
        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)
      
      ### intercept initialized in yalm ###

      ### effectsizes table ###
      j.init_table(self$results$main$effectsizes,estimate_machine$tab_effectsizes,ci=T,ciwidth=self$options$ciWidth) 
      
      
      ### estimate marginal means
      
      if (is.something(self$options$emmeans)) {
  
              self$results$emmeans$setVisible(TRUE)
              for (i in seq_along(self$options$emmeans)) {
                    term<-self$options$emmeans[[i]]
                    spaceby=NULL
                    if (length(term)>1) spaceby<-1
                    
                    aTable <- self$results$emmeans$addItem(key = jmvcore::stringifyTerm(term))
                    j.expand_table(aTable,term)
                    j.init_table(aTable,estimate_machine$tab_emmeans[[i]], ci=T,ciwidth=self$options$ciWidth,spaceby = spaceby)
              } 
      
      }
      
      ### simple effects ####
      if (is.something(estimate_machine$tab_simpleAnova)) {

         j.expand_table(self$results$simpleEffects$anova,self$options$simpleModerators) 
         title<-paste("Omnibus test for variable",self$options$simpleVariable)
         j.init_table(self$results$simpleEffects$anova,estimate_machine$tab_simpleAnova,title=title)
         title<-paste("Parameter estimates for variable",self$options$simpleVariable)
         j.expand_table(self$results$simpleEffects$coefficients,self$options$simpleModerators,superTitle = "Moderators") 
         j.init_table(self$results$simpleEffects$coefficients,estimate_machine$tab_simpleCoefficients, ci=T,ciwidth=self$options$ciWidth,title=title)

      }
      
      ### simple interactions ######
      
      if (is.something(estimate_machine$tab_simpleInteractionCoefficients)) {
  
        self$results$simpleInteractions$setVisible(TRUE)
        ### moderators should be reverted in order so they make sense
        terms<-c(self$options$simpleVariable,self$options$simpleModerators)
        for (i in seq_along(estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$addItem(key = i)
          aTable<-aGroup$interactionCoefficients
          term<-setdiff(terms,estimate_machine$tab_simpleInteractionCoefficients[[i]])
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionCoefficients[[i]],superTitle="Moderator")
          title<-paste("Parameter Estimates for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=T,ciwidth=self$options$ciWidth,title=title)

          aTable<-aGroup$interactionAnova
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionAnova[[i]],superTitle="Moderator")
          title<-paste("ANOVA test for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=F,title=title)
          
          title<-paste("Simple interaction:",  jmvcore::stringifyTerm(term))
          aGroup$setTitle(title)
          
        } 
        
        
      }
        
      ### posthoc ####
      
      if (is.something(self$options$posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
            term<-self$options$posthoc[[i]]
            aTable<-self$results$posthoc$get(key = term)
            aTable$setTitle(paste0("Post Hoc Comparisons - ", jmvcore::stringifyTerm(term)))
            j.expand_table(aTable,names(estimate_machine$tab_posthoc[[i]]),superTitle="Comparison",names64=FALSE)
            j.init_table(aTable,estimate_machine$tab_posthoc[[i]],ci=T,ciwidth = self$options$ciWidth)
        } 
        
      }

      if (is.something(estimate_machine$tab_contrastcodes)) {

        for (i in seq_along(self$options$factors)) {
          term<-self$options$factors[[i]]
          aTable<-self$results$main$contrastCodeTables$get(key = term)
          eTable<-estimate_machine$tab_contrastcodes[[i]]
          levels_pos<-seq_len(length(names(eTable))-2)
          j.expand_table(aTable,names(eTable)[levels_pos],append=T,type="number",names64=FALSE) 
          names(eTable)[levels_pos]<-make.names(names(eTable)[levels_pos])
          j.init_table(aTable,eTable)
        }
      }
      plotter_machine$initPlots()
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      private$.plotter_machine<-plotter_machine
    },
    .run=function() {
      ginfo("run")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      
      ### info table ###
      j.fill_table(self$results$info,private$.estimate_machine$tab_info) 
      
      j.add_warnings(self$results$info,private$.data_machine,"data")
      j.add_warnings(self$results$info,private$.estimate_machine,"info")
      
      ## R2 table ###
      j.fill_table(self$results$main$r2,private$.estimate_machine$tab_r2)
      j.add_warnings(self$results$main$r2,private$.estimate_machine,"tab_r2")
      
      ## Intercept table ###
      j.fill_table(self$results$main$intercept,private$.estimate_machine$tab_intercept)
      j.add_warnings(self$results$main$intercept,private$.estimate_machine,"tab_intercept")
      
      ## anova table ###
      j.fill_table(self$results$main$anova,private$.estimate_machine$tab_anova)
      j.add_warnings(self$results$main$anova,private$.estimate_machine,"tab_anova")

      
      ## effect sizes table ###
      j.fill_table(self$results$main$effectsizes,private$.estimate_machine$tab_effectsizes)
      j.add_warnings(self$results$main$effectsizes,private$.estimate_machine,"tab_effectsizes")
      
      ### parameter estimates
      j.fill_table(self$results$main$coefficients,private$.estimate_machine$tab_coefficients)
      j.add_warnings(self$results$main$coefficients,private$.estimate_machine,"tab_coefficients")

      
      ### simple effects
      j.fill_table(self$results$simpleEffects$anova,private$.estimate_machine$tab_simpleAnova)
      j.add_warnings(self$results$simpleEffects$anova,private$.estimate_machine,"tab_simpleAnova")
      
      j.fill_table(self$results$simpleEffects$coefficients,private$.estimate_machine$tab_simpleCoefficients)
      j.add_warnings(self$results$simpleEffects$coefficients,private$.estimate_machine,"tab_simpleCoefficients")
      
      ### simple interactions
      if (is.something(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
        
        for (i in seq_along(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$get(key = i)
          aTable<-aGroup$interactionCoefficients
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionCoefficients[[i]],append = T)
          aTable<-aGroup$interactionAnova
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionAnova[[i]],append = T)
          
 
        } 
      }
      
      
      ### post hoc
      
      if (is.something(private$.estimate_machine$tab_posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          j.fill_table(aTable,private$.estimate_machine$tab_posthoc[[i]])
        } 
      }
      
    ###  emmeans 
      
      if (is.something(private$.estimate_machine$tab_emmeans)) {
        
        for (i in seq_along(self$options$emmeans)) {
          term<-self$options$emmeans[[i]]
          aTable<-self$results$emmeans$get(key = jmvcore::stringifyTerm(term))
          j.fill_table(aTable,private$.estimate_machine$tab_emmeans[[i]])
        } 
      }
      
      private$.plotter_machine$preparePlots()
      j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")
      
      #save model preds and resids            
      private$.estimate_machine$savePredRes(self$results) 
      

      mark("end")
      return()
          
        
    },


.mainPlot=function(image, ggtheme, theme, ...) {
  
  plot<-private$.plotter_machine$scatterPlot(image)
  plot<-plot + ggtheme
  
  return(plot)
  
},



.qqPlot=function(image, ggtheme, theme, ...) {
  dep <- self$options$dep
  factors <- self$options$factors
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  
  data <- model$model
  residuals <- as.numeric(scale(residuals(model)))
  df <- as.data.frame(qqnorm(residuals, plot.it=FALSE))
  plot<-ggplot2::ggplot(data=df, aes(y=y, x=x)) +
          geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
          geom_point(aes(x=x,y=y), size=2, colour=theme$color[1]) +
          xlab("Theoretical Quantiles") +
          ylab("Standardized Residuals") +ggtheme
  
  return(plot)
},
.normPlot=function(image, ggtheme, theme, ...) {
  
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  plot<-gplots.normPlot(model,ggtheme,theme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {
  
  model<-private$.model      
  if (is.null(model) )
    return(FALSE)
  plot<-gplots.residPlot(model,ggtheme,theme)
  return(plot)
},

.formula=function() {
  jmvcore:::composeFormula(self$options$dep, self$options$modelTerms)
},


.sourcifyOption = function(option) {

  name <- option$name
  value <- option$value

  if (!is.something(value))
    return('')

  if (option$name %in% c('factors', 'dep', 'covs', 'modelTerms'))
    return('')
  
  if (name =='scaling') {
    vec<-sourcifyList(option,"centered")
    return(vec)
  }
  if (name =='contrasts') {
    vec<-sourcifyList(option,"simple")
    return(vec)
  }
  if (name == 'postHoc') {
    if (length(value) == 0)
      return('')
  }
  
  super$.sourcifyOption(option)
}
)
)


