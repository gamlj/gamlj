gamljMixedClass <- R6::R6Class(
  "gamljMixedClass",
  inherit = gamljMixedBase,
  private=list(
    .dispatcher=NULL,
    .data_machine=NULL,
    .estimate_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .time=NULL,
    .smartObjs=list(),
    .init=function() {
      
      ginfo(paste("MODULE:",self$options$.caller,"  #### phase init  ####"))
      class(private$.results) <- c('gamlj', class(private$.results))
      private$.time<-Sys.time()
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      emmeans::emm_options(lmerTest.limit = 25000)  
      
      ### set up the R6 workhorse class
      dispatcher<-Dispatch$new()
      data_machine<-Datamatic$new(self$options,dispatcher,self$data)
      estimate_machine<-Estimate$new(self$options,dispatcher,data_machine)
     
      
      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)


      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,estimate_machine)
      aSmartObj$spaceBy<-"model"
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      

      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### estimates table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,estimate_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("beta",self$options$ci_width,label=greek_vector[["beta"]])
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,estimate_machine)
      aSmartObj$expandable<-TRUE
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)




      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,estimate_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandable<-TRUE
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
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_effects
      aSmartObj$ci("est",self$options$ci_width)
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
      
      ### assumptions nromtest
      aSmartObj<-SmartTable$new(self$results$assumptions$normtest,estimate_machine)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)

      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,estimate_machine)
      aSmartObj$expandable<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("est",self$options$ci_width)
      private$.smartObjs<-append_list(private$.smartObjs,aSmartObj)
      
      
      ### init all ####

      
 
      for (tab in private$.smartObjs) {
            tab$initTable()
      }
      

      private$.data_machine<-data_machine
      private$.estimate_machine<-estimate_machine
      
      ######## plotting class #######
      plotter_machine<-Plotter$new(self$options,estimate_machine,self$results)
      plotter_machine$initPlots()
      private$.plotter_machine<-plotter_machine
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
      for (smarttab in private$.smartObjs) {
           smarttab$runTable()
           private$.checkpoint()
      }

      for (smarttab in private$.smartObjs) 
        smarttab$setNotes(private$.estimate_machine$dispatcher)
      

      private$.checkpoint()
      
      # #save model preds and resids            
      # private$.estimate_machine$savePredRes(self$results) 
      # 
      private$.plotter_machine$preparePlots()
      
      if ("plot" %in% private$.plotter_machine$dispatcher$warnings_topics) {
          self$results$plotnotes$setContent(paste(private$.plotter_machine$dispatcher$warnings[["plot"]],collapse = "; "))
          self$results$plotnotes$setVisible(TRUE)
      }  
      private$.checkpoint()
      
      ### save the model if we are in R ###
      if (self$options$.interface=="r")
              self$results$.setModel(private$.estimate_machine$model)
      
      ginfo("MODULE:  #### phase end ####")
      now<-Sys.time()
      ginfo("TIME:",now-private$.time," secs")
      
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

  skip<-c("modelTerms","factors","covs","dep")
  defaults<-c(scaling="centered",contrasts="simple")
  
  if (option$name %in% skip)
     return('')
 sourcifyOption(option,defaults)

}
)
)


