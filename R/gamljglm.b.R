gamljGlmClass <- R6::R6Class(
  "gamljGlmClass",
  inherit = gamljGlmBase,
  private=list(
    .model=NA,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .smartTabs=list(),
    .init=function() {
      ginfo("### Module phase: init ###")
      
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
      aSmartTab<-SmartTable$new(self$results$info,estimate_machine)
      private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)

      ### r2 table does not need initializing ###

      ## R2 table ###
      aSmartTab<-SmartTable$new(self$results$main$r2,estimate_machine)
      private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)
      

      ### anova table ###

      aSmartTab<-SmartTable$new(self$results$main$anova,estimate_machine)
      private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)
      

      ### estimates table ###
      aSmartTab<-SmartTable$new(self$results$main$coefficients,estimate_machine)
      aSmartTab$ci(c("est"),self$options$ciWidth)
      private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)
      
      ### constrast code tables
      aSmartArray<-SmartArray$new(self$results$main$contrastCodeTables,estimate_machine)
      aSmartArray$expandable<-TRUE
      private$.smartTabs<-append_list(private$.smartTabs,aSmartArray)

      ### intercept initialized in yalm ###

      ### intercept more info table ###
      
      aSmartTab<-SmartTable$new(self$results$main$intercept,estimate_machine)
      private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)
      
      ### effectsizes table ###

      aSmartTab<-SmartTable$new(self$results$main$effectsizes,estimate_machine)
      aSmartTab$ci(c("es"),self$options$ciWidth)
      private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)

      ## post hoc #####
      
      aSmartArray<-SmartArray$new(self$results$posthoc,estimate_machine)
      aSmartArray$expandable<-TRUE
      private$.smartTabs<-append_list(private$.smartTabs,aSmartArray)
      
      # if (is.something(self$options$posthoc)) {
      #   
      #   for (i in seq_along(self$options$posthoc)) {
      #     term<-self$options$posthoc[[i]]
      #     aTable<-self$results$posthoc$get(key = term)
      #     aTable$setTitle(paste0("Post Hoc Comparisons - ", jmvcore::stringifyTerm(term)))
      #     j.expand_table(aTable,names(estimate_machine$tab_posthoc[[i]]),superTitle="Comparison",names64=FALSE)
      #     j.init_table(aTable,estimate_machine$tab_posthoc[[i]],ci=T,ciwidth = self$options$ciWidth)
      #   } 
      #   
      # }
      
      
            
      ### init all ####

 
      for (tab in private$.smartTabs)
            tab$initTable()
      


#      if (!is.something(self$options$factors))
#        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)
      
      ### intercept initialized in yalm ###


      
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
          aTable<-aGroup$coefficients
          term<-setdiff(terms,estimate_machine$tab_simpleInteractionCoefficients[[i]])
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionCoefficients[[i]],superTitle="Moderator")
          title<-paste("Parameter Estimates for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=T,ciwidth=self$options$ciWidth,title=title)

          aTable<-aGroup$anova
          j.expand_table(aTable,estimate_machine$tab_simpleInteractionAnova[[i]],superTitle="Moderator")
          title<-paste("ANOVA test for simple interaction",  jmvcore::stringifyTerm(term))
          j.init_table(aTable,FALSE, ci=F,title=title)
          
          title<-paste("Simple interaction:",  jmvcore::stringifyTerm(term))
          aGroup$setTitle(title)
          
        } 
        
        
      }
        
      ### posthoc ####
      


      #### normality assuption test ####
      if (is.something(estimate_machine$tab_normtest))
        j.init_table(self$results$assumptions$normTest,estimate_machine$tab_normtest)
      
      
      
      
      plotter_machine$initPlots()
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      private$.plotter_machine<-plotter_machine
    },
    .run=function() {
      ginfo("### Module phase: run ###")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      
      ### info table ###
      for (smarttab in private$.smartTabs)
           smarttab$fillTable()


    
      
      
      ### simple interactions
      if (is.something(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
        
        for (i in seq_along(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
          aGroup <- self$results$simpleInteractions$get(key = i)
          aTable<-aGroup$coefficients
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionCoefficients[[i]],append = T)
          aTable<-aGroup$anova
          j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionAnova[[i]],append = T)

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
      
      ###### levene's test #######
      if (self$options$homoTest) {
        j.fill_table(self$results$assumptions$homoTest,private$.estimate_machine$tab_levene)
        j.add_warnings(self$results$assumptions$homoTest,private$.estimate_machine,"tab_levene")
        
      }
      
      ###### normality test #######
      
      if (self$options$normTest) {
        j.fill_table(self$results$assumptions$normTest,private$.estimate_machine$tab_normtest)
        j.add_warnings(self$results$assumptions$homoTest,private$.estimate_machine,"tab_normtest")
        
      }
      
      
      
      private$.plotter_machine$preparePlots()
      j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")
      
      #save model preds and resids            
      private$.estimate_machine$savePredRes(self$results) 
      

      ginfo("### Module phase: end ###")
      return()
          
        
    },


.mainPlot=function(image, ggtheme, theme, ...) {
  
  plot<-private$.plotter_machine$scatterPlot(image)
  plot<-plot + ggtheme
  
  return(plot)
  
},



.qqPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$qqplot(theme,ggtheme)
  return(plot)
  
},
.normPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$normplot(theme,ggtheme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$residPlot(theme,ggtheme)
  
  return(plot)
},

.formula=function() {
  
  private$.estimate_machine$formula
  
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


