gamljGzlmClass <- R6::R6Class(
  "gamljGzlmClass",
  inherit = gamljGzlmBase,
  private=list(
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
      
      
      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      
      
      

      
      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### some models do not have adjusted r2 ###
      if (self$options$modelSelection %in% c("multinomial","ordinal", "custom"))
             self$results$main$r2$getColumn("ar2")$setVisible(FALSE)
      
      
      ### additional fit table ###
      aSmartObj<-SmartTable$new(self$results$main$fit,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### estimates table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,estimate_machine)
      aSmartObj$ci("est",self$options$ciWidth)
      aSmartObj$ci("expb",self$options$ciWidth)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,estimate_machine)
      aSmartObj$expandable<-TRUE
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      
      ### relativerisk tables
      aSmartObj<-SmartTable$new(self$results$main$relativerisk,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      
      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,estimate_machine)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFromBegining<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("est",self$options$ciWidth)
      aSmartObj$setColumnTitle("estimate",estimate_machine$infomatic$comparison)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      

      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,estimate_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFromBegining<-TRUE
      aSmartObj$combineBelow="new"
      aSmartObj$spaceBy="new"
      aSmartObj$ci("est",self$options$ciWidth)
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
      aSmartObj$ci("est",self$options$ciWidth)
      aSmartObj$combineBelow<-1:(length(self$options$simpleModerators)-1)
      aSmartObj$spaceBy<-(length(self$options$simpleModerators)-1)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### simple interaction
      aSmartObj<-SmartArray$new(self$results$simpleInteractions,estimate_machine)
      aSmartObj$activated<-(self$options$simpleInteractions & is.something(self$options$simpleVariable) & length(self$options$simpleModerators)>1)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$ci("est",self$options$ciWidth)
      aSmartObj$combineBelow<-"new"
      aSmartObj$spaceBy<-"new"
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### init all ####
      for (tab in private$.smartObjs) {
        tab$initTable()
      }
      
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      
    },
    .run=function() {
      
      ginfo("MODULE:  #### phase run ####")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      
      ### run tables ###
      for (smarttab in private$.smartObjs)
        smarttab$runTable()
      
      ### retrive additional warnings and errors
      for (smarttab in private$.smartObjs)
        smarttab$setNotes(private$.estimate_machine$dispatcher)
      
      
      private$.checkpoint()
      

      

    },

    .mainPlot=function(image, ggtheme, theme, ...) {
      
      if (is.something(private$.estimate_machine$errors))
          return()
      if (!is.something(image$key))
        return()
      
      plot<-private$.plotter_machine$scatterPlot(image)
      plot<-plot + ggtheme
      
      return(plot)
      
    },
    
    


.formula=function() {
  jmvcore:::composeFormula(self$options$dep, self$options$modelTerms)
},
.sourcifyOption = function(option) {
  
  skip<-c("modelTerms","factors","covs","dep")
  defaults<-c(scaling="centered",contrasts="simple")
  
  if (option$name %in% skip)
    return('')
  sourcifyOption(option,defaults)
  
}

  )


)


