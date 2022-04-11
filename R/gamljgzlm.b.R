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
      
      ginfo(paste("MODULE:",self$options$.caller,self$options$modeltype,"  #### phase init  ####"))
      class(private$.results) <- c('gamlj', class(private$.results))
      
      private$.time<-Sys.time()

      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhorse class
      dispatcher<-Dispatch$new(self$results)
      data_machine<-Datamatic$new(self$options,dispatcher,self$data)
      estimate_machine<-Estimate$new(self$options,dispatcher,data_machine)
      
      
      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      
      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### some models do not have adjusted r2 ###
      if (self$options$modeltype %in% c("multinomial","ordinal", "custom"))
             self$results$main$r2$getColumn("ar2")$setVisible(FALSE)
      
      
      ### additional fit table ###
      aSmartObj<-SmartTable$new(self$results$main$fit,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### estimates table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,estimate_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("expb",width=self$options$ci_width,label="Exp(B)")
      aSmartObj$spaceBy<-"response"
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,estimate_machine)
      aSmartObj$expandable<-TRUE
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      
      ### relativerisk tables
      aSmartObj<-SmartTable$new(self$results$main$relativerisk,estimate_machine)
      aSmartObj$ci("est",width=self$options$ci_width)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### paralleltest tables
      aSmartObj<-SmartTable$new(self$results$main$paralleltest,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)

      ### vcov table ###
      
      aSmartObj<-SmartTable$new(self$results$main$vcov,estimate_machine)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFrom<-2
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,estimate_machine)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow<-"response"
      aSmartObj$expandFrom<-2
      aSmartObj$setColumnTitle("estimate",estimate_machine$infomatic$comparison)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      

      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,estimate_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFrom<-2
      aSmartObj$combineBelow="new!"
      aSmartObj$spaceBy="new!"
      aSmartObj$ci("est",self$options$ci_width)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### simple effects
      ##### anova
      aSmartObj<-SmartTable$new(self$results$simpleEffects$anova,estimate_machine)
      aSmartObj$activated<-(is.something(self$options$simple_effects) & is.something(self$options$simple_moderators))
      aSmartObj$expandable<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_effects
      aSmartObj$combineBelow<-1:(length(self$options$simple_moderators)-1)
      aSmartObj$spaceBy<-(length(self$options$simple_moderators)-1)
      
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ##### coefficients
      aSmartObj<-SmartTable$new(self$results$simpleEffects$coefficients,estimate_machine)
      aSmartObj$activated<-(is.something(self$options$simple_effects) & is.something(self$options$simple_moderators))
      aSmartObj$expandable<-TRUE
      aSmartObj$expandFrom<-2
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_effects
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("expb",width=self$options$ci_width,format="Exp(B) {}% Confidence Intervals")
      aSmartObj$combineBelow<-1:(length(self$options$simple_moderators)-1)
      aSmartObj$spaceBy<-(length(self$options$simple_moderators)-1)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### simple interaction
      aSmartObj<-SmartArray$new(self$results$simpleInteractions,estimate_machine)
      aSmartObj$activated<-(self$options$simple_interactions & is.something(self$options$simple_effects) & length(self$options$simple_moderators)>1)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow<-"new!"
      aSmartObj$spaceBy<-"new!"
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### init all ####
      for (tab in private$.smartObjs) {
        tab$initTable()
      }
      
      
      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      
      now<-Sys.time()
      ginfo("INIT TIME:",now-private$.time," secs")
      
    },
    .run=function() {
      
      runnow<-Sys.time()
      
      ginfo("MODULE:  #### phase run ####")
      
      if (self$options$donotrun) return()
        
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      
      data<-private$.data_machine$cleandata(self$data)
      private$.estimate_machine$estimate(data)
      
      ### run tables ###
      for (smarttab in private$.smartObjs)
        smarttab$runTable()
      

      private$.checkpoint()
      
      ginfo("MODULE:  #### phase end ####")
      
      ginfo("RUN TIME:",Sys.time()-runnow," secs")
      
      ginfo("TIME:",Sys.time()-private$.time," secs")
      

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
  jmvcore:::composeFormula(self$options$dep, self$options$model_terms)
},
.sourcifyOption = function(option) {
  
  skip<-c("model_terms","factors","covs","dep")
  defaults<-c(scaling="centered",contrasts="simple")
  
  if (option$name %in% skip)
    return('')
  sourcifyOption(option,defaults)
  
}

  )


)


