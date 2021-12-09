gamljGlmClass <- R6::R6Class(
  "gamljGlmClass",
  inherit = gamljGlmBase,
  private=list(
    .dispatcher=NULL,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .time=NULL,
    .smartObjs=list(),
    .init=function() {
      ginfo("MODULE:  #### phase init ####")
      class(private$.results) <- c('gamlj', class(private$.results))
      private$.time<-Sys.time()
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhorse class
      dispatcher<-Dispatch$new()
      data_machine<-Datamatic$new(self$options,dispatcher,self$data)
      estimate_machine<-Estimate$new(self$options,dispatcher,data_machine)
#      plotter_machine<-Plotter$new(estimate_machine,self$results)

      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)


      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      

      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,estimate_machine)
      aSmartObj$spaceAt<-1
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### estimates table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,estimate_machine)
      aSmartObj$ci(c("est"),self$options$ciWidth)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,estimate_machine)
      aSmartObj$expandable<-TRUE
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)

      ### intercept more info table ###
      
      aSmartObj<-SmartTable$new(self$results$main$intercept,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### effectsizes table ###

      aSmartObj<-SmartTable$new(self$results$main$effectsizes,estimate_machine)
      aSmartObj$ci(c("est"),self$options$ciWidth)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)

      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,estimate_machine)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFromBegining<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci(c("est"),self$options$ciWidth)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      

      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,estimate_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFromBegining<-TRUE
      aSmartObj$combineBelow="new"
      aSmartObj$spaceBy="new"
      aSmartObj$ci(c("est"),self$options$ciWidth)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### simple effects
      ##### anova
      aSmartObj<-SmartTable$new(self$results$simpleEffects$anova,estimate_machine)
      aSmartObj$activated<-(is.something(self$options$simpleVariable) & is.something(self$options$simpleModerators))
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFromBegining<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simpleVariable
      aSmartObj$combineBelow<-1:(length(self$options$simpleModerators)-1)
      aSmartObj$spaceBy<-(length(self$options$simpleModerators)-1)
      
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ##### coefficients
      aSmartObj<-SmartTable$new(self$results$simpleEffects$coefficients,estimate_machine)
      aSmartObj$activated<-(is.something(self$options$simpleVariable) & is.something(self$options$simpleModerators))
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFromBegining<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simpleVariable
      aSmartObj$ci(c("est"),self$options$ciWidth)
      aSmartObj$combineBelow<-1:(length(self$options$simpleModerators)-1)
      aSmartObj$spaceBy<-(length(self$options$simpleModerators)-1)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### simple interaction
      aSmartObj<-SmartArray$new(self$results$simpleInteractions,estimate_machine)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$ci(c("est"),self$options$ciWidth)
      aSmartObj$combineBelow<-"new"
      aSmartObj$spaceBy<-"new"
      
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### init all ####

      
 
      for (tab in private$.smartObjs) {
            tab$initTable()
      }
      


#      if (!is.something(self$options$factors))
#        self$results$main$coefficients$getColumn('label')$setVisible(FALSE)
      
      ### intercept initialized in yalm ###


      
      
      ### simple effects ####
         # j.expand_table(self$results$simpleEffects$anova,self$options$simpleModerators) 
         # title<-paste("Omnibus test for variable",self$options$simpleVariable)
         # j.init_table(self$results$simpleEffects$anova,estimate_machine$tab_simpleAnova,title=title)
         # title<-paste("Parameter estimates for variable",self$options$simpleVariable)
         # j.expand_table(self$results$simpleEffects$coefficients,self$options$simpleModerators,superTitle = "Moderators") 
         # j.init_table(self$results$simpleEffects$coefficients,estimate_machine$tab_simpleCoefficients, ci=T,ciwidth=self$options$ciWidth,title=title)
         # 

      ### simple interactions ######
      # 
      # if (is.something(estimate_machine$tab_simpleInteractionCoefficients)) {
      # 
      #   self$results$simpleInteractions$setVisible(TRUE)
      #   ### moderators should be reverted in order so they make sense
      #   terms<-c(self$options$simpleVariable,self$options$simpleModerators)
      #   for (i in seq_along(estimate_machine$tab_simpleInteractionCoefficients)) {
      #     aGroup <- self$results$simpleInteractions$addItem(key = i)
      #     aTable<-aGroup$coefficients
      #     term<-setdiff(terms,estimate_machine$tab_simpleInteractionCoefficients[[i]])
      #     j.expand_table(aTable,estimate_machine$tab_simpleInteractionCoefficients[[i]],superTitle="Moderator")
      #     title<-paste("Parameter Estimates for simple interaction",  jmvcore::stringifyTerm(term))
      #     j.init_table(aTable,FALSE, ci=F,ciwidth=self$options$ciWidth,title=title)
      # 
      #     aTable<-aGroup$anova
      #     j.expand_table(aTable,estimate_machine$tab_simpleInteractionAnova[[i]],superTitle="Moderator")
      #     title<-paste("ANOVA test for simple interaction",  jmvcore::stringifyTerm(term))
      #     j.init_table(aTable,FALSE, ci=F,title=title)
      #     
      #     title<-paste("Simple interaction:",  jmvcore::stringifyTerm(term))
      #     aGroup$setTitle(title)
      #     
      #   } 
      #   
      #   
      # }
      #   
      # ### posthoc ####
      # 
      # 
      # 
      # #### normality assuption test ####
      # if (is.something(estimate_machine$tab_normtest))
      #   j.init_table(self$results$assumptions$normTest,estimate_machine$tab_normtest)
      # 
      # 
      
      
      #plotter_machine$initPlots()
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      #private$.plotter_machine<-plotter_machine
    },
    .run=function() {
      ginfo("MODULE:  #### phase run ####")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)

      ### info table ###
      for (smarttab in private$.smartObjs)
           smarttab$runTable()

      for (smarttab in private$.smartObjs)
        smarttab$setNotes(private$.estimate_machine$dispatcher)
      
      ### info table ###
      mark(private$.estimate_machine$dispatcher$warnings)
          


      
      # ### simple interactions
      # if (is.something(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
      #   
      #   for (i in seq_along(private$.estimate_machine$tab_simpleInteractionCoefficients)) {
      #     aGroup <- self$results$simpleInteractions$get(key = i)
      #     aTable<-aGroup$coefficients
      #     j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionCoefficients[[i]],append = T)
      #     aTable<-aGroup$anova
      #     j.fill_table(aTable,private$.estimate_machine$tab_simpleInteractionAnova[[i]],append = T)
      # 
      #   } 
      # }
      # 
      

    ###  emmeans 
      
      # if (is.something(private$.estimate_machine$tab_emmeans)) {
      #   
      #   for (i in seq_along(self$options$emmeans)) {
      #     term<-self$options$emmeans[[i]]
      #     aTable<-self$results$emmeans$get(key = jmvcore::stringifyTerm(term))
      #     j.fill_table(aTable,private$.estimate_machine$tab_emmeans[[i]])
      #   } 
      # }
      # 
      # ###### levene's test #######
      # if (self$options$homoTest) {
      #   j.fill_table(self$results$assumptions$homoTest,private$.estimate_machine$tab_levene)
      #   j.add_warnings(self$results$assumptions$homoTest,private$.estimate_machine,"tab_levene")
      #   
      # }
      # 
      # ###### normality test #######
      # 
      # if (self$options$normTest) {
      #   j.fill_table(self$results$assumptions$normTest,private$.estimate_machine$tab_normtest)
      #   j.add_warnings(self$results$assumptions$homoTest,private$.estimate_machine,"tab_normtest")
      #   
      # }
      # 
      # 
      # 
      # private$.plotter_machine$preparePlots()
      # j.add_warnings(self$results$plotnotes,private$.plotter_machine,"plot")
      # 
      # #save model preds and resids            
      # private$.estimate_machine$savePredRes(self$results) 
      # 

      ginfo("MODULE:  #### phase end ####")
      now<-Sys.time()
      ginfo("TIME:",now-private$.time," secs")
      
      return()
          
        
    },


.mainPlot=function(image, ggtheme, theme, ...) {
  
#  plot<-private$.plotter_machine$scatterPlot(image)
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


