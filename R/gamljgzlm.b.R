gamljGzlmClass <- R6::R6Class(
  "gamljGzlmClass",
  inherit = gamljGzlmBase,
  private=list(
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    
    .init=function() {
      
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
      
      ### additional indices table ###
      j.init_table(self$results$main$fit,estimate_machine$tab_fit)
      
      
      ### anova table ###
      j.init_table(self$results$main$anova,estimate_machine$tab_anova) 

      
      ### relative risk table ###
      j.init_table(self$results$main$relativerisk,estimate_machine$tab_relativerisk,ci=TRUE,ciwidth=self$options$ciWidth) 
      if (!is.something(self$options$factors))
        self$results$main$relativerisk$getColumn('label')$setVisible(FALSE)
      
            
      ### estimates table ###
      j.init_table(self$results$main$coefficients,estimate_machine$tab_coefficients, 
                   ci=T,ciformat="{}% C.I.", ciwidth=self$options$ciWidth)
      j.init_table(self$results$main$coefficients,FALSE, 
                   ci=T,ciroot="expb.",ciformat="{}% Exp(B) C.I.",ciwidth=self$options$ciWidth)
      
            
      if (!is.something(self$options$factors))
        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)
      

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
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionCoefficients[[i]],superTitle="Moderator",startAt=2)
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
          j.expand_table(aTable,
                         names(estimate_machine$tab_posthoc[[i]]),
                         superTitle="Comparison",
                         names64=FALSE,
                         startAt=2)
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
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      
      if (is.something(private$.estimate_machine$errors)) 
            stop(paste(private$.estimate_machine$errors,collapse = " ; "))
      
      
      
      ## info table and posssible main problems to report
      j.fill_table(self$results$info,private$.estimate_machine$tab_info) 
      
      j.add_warnings(self$results$info,private$.data_machine,"data")
      j.add_warnings(self$results$info,private$.estimate_machine,"info")
      
      ## R2 table ###
      j.fill_table(self$results$main$r2,private$.estimate_machine$tab_r2)
      j.add_warnings(self$results$main$r2,private$.estimate_machine,"tab_r2")
      
      ## additional fit indices table ###
      
      j.fill_table(self$results$main$fit,private$.estimate_machine$tab_fit)
      j.add_warnings(self$results$main$fit,private$.estimate_machine,"tab_fit")
      
      ## anova table ###
      j.fill_table(self$results$main$anova,private$.estimate_machine$tab_anova)
      j.add_warnings(self$results$main$anova,private$.estimate_machine,"tab_anova")

      ## relative risk table ###
      j.fill_table(self$results$main$relativerisk,private$.estimate_machine$tab_relativerisk)
      j.add_warnings(self$results$main$relativerisk,private$.estimate_machine,"tab_relativerisk")
      
      

      ### parameter estimates
      j.fill_table(self$results$main$coefficients,private$.estimate_machine$tab_coefficients)
      j.add_warnings(self$results$main$coefficients,private$.estimate_machine,"tab_coefficients")
      
      
      ### post hoc
      
      if (is.something(private$.estimate_machine$tab_posthoc)) {
        
        for (i in seq_along(self$options$posthoc)) {
          term<-self$options$posthoc[[i]]
          aTable<-self$results$posthoc$get(key = term)
          j.fill_table(aTable,private$.estimate_machine$tab_posthoc[[i]])
        } 
      }
      
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
      
      ###  emmeans 
      
      if (is.something(private$.estimate_machine$tab_emmeans)) {
        
        for (i in seq_along(self$options$emmeans)) {
          term<-self$options$emmeans[[i]]
          aTable<-self$results$emmeans$get(key = jmvcore::stringifyTerm(term))
          j.fill_table(aTable,private$.estimate_machine$tab_emmeans[[i]])
          j.add_warnings(aTable,private$.estimate_machine,"tab_emmeans")
        } 
      }
      
      private$.plotter_machine$preparePlots()
      j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")
      
      #save model preds and resids            
      private$.estimate_machine$savePredRes(self$results) 
      
      private$.plotter_machine$preparePlots()
      j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")

    },

    .mainPlot=function(image, ggtheme, theme, ...) {
      
      if (is.something(private$.estimate_machine$errors))
          return()
      
      plot<-private$.plotter_machine$scatterPlot(image)
      plot<-plot + ggtheme
      
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


