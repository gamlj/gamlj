gamljlmClass <- R6::R6Class(
  "gamlj_lm_Class",
  inherit = gamljlmBase,
  private=list(
    .data_machine=NULL,
    .runner_machine=NULL,
    .plotter_machine=NULL,
    .ready=NULL,
    .time=NULL,
    .smartObjs=list(),
    .init=function() {
      
      jinfo(paste("MODULE:",self$options$.caller,"  #### phase init  ####"))
      class(private$.results) <- c('gamlj', class(private$.results))

      private$.time<-Sys.time()
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        if(private$.ready$report)
          self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
        return()
      }
      
      ### set up the R6 workhorse class
      dispatch_message_cleaner(self)
      data_machine            <-  Datamatic$new(self)
      runner_machine          <-  Runner$new(self,data_machine)
      runner_machine$storage  <-  self$results$info      

      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj

     
      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      

      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,runner_machine)
      aSmartObj$spaceAt<-c(1,-2)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### coefficents table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("beta",self$options$ci_width,label=greek_vector[["beta"]])
      if (is.something(self$options$factors))
            aSmartObj$setColumnVisible<-"label"
      
      ladd(private$.smartObjs)<-aSmartObj

      ### custom contrasts 
      aSmartObj<-SmartTable$new(self$results$main$contrasts,runner_machine)
      aSmartObj$activateOnData<-TRUE
      aSmartObj$ci("est",self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj

      ## custom_effectsizes      
      
      aSmartObj<-SmartTable$new(self$results$main$customEffectsizes,runner_machine)
      aSmartObj$activateOnData<-TRUE      
      aSmartObj$ci("est",self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj

      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      ladd(private$.smartObjs)<-aSmartObj

      ### intercept more info table ###
      
      aSmartObj<-SmartTable$new(self$results$main$intercept,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### effectsizes table ###

      aSmartObj<-SmartTable$new(self$results$main$effectsizes,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$spaceBy="effect"
      ladd(private$.smartObjs)<-aSmartObj

      ### vcov table ###
      
      aSmartObj<-SmartTable$new(self$results$main$vcov,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandFrom<-2
      ladd(private$.smartObjs)<-aSmartObj
      
      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("est",self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj
      
      aSmartObj<-SmartArray$new(self$results$posthocEffectSize,runner_machine)
      aSmartObj$activated<-(is.something(self$options$posthoc) & is.something(self$options$posthoc_es))
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Comparison"
      aSmartObj$ci("dm",self$options$ci_width)
      aSmartObj$ci("ds",self$options$ci_width)
      aSmartObj$ci("g",self$options$ci_width)
#      aSmartObj$restNotes(self$options$dci==FALSE)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,runner_machine)
      aSmartObj$activated<-is.something(self$options$emmeans)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$combineBelow="new!"
      aSmartObj$spaceBy="new!"
      aSmartObj$ci("est",self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### simple effects
      ##### anova
      aSmartObj<-SmartTable$new(self$results$simpleEffects$anova,runner_machine)
      aSmartObj$activated<-(is.something(self$options$simple_x) & is.something(self$options$simple_mods))
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_x
      aSmartObj$combineBelow<-1:(length(self$options$simple_mods)-1)
      aSmartObj$spaceBy<-(length(self$options$simple_mods)-1)
      
      ladd(private$.smartObjs)<-aSmartObj
      
      ##### coefficients
      aSmartObj<-SmartTable$new(self$results$simpleEffects$coefficients,runner_machine)
      aSmartObj$activated<-(is.something(self$options$simple_x) & is.something(self$options$simple_mods))
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$key<-self$options$simple_x
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow<-1:(length(self$options$simple_mods)-1)
      aSmartObj$spaceBy<-(length(self$options$simple_mods)-1)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### simple interaction
      aSmartObj<-SmartArray$new(self$results$simpleInteractions,runner_machine)
      aSmartObj$activated<-(self$options$simple_interactions & is.something(self$options$simple_x) & length(self$options$simple_mods)>1)
      aSmartObj$expandOnInit<-TRUE
      aSmartObj$expandSuperTitle<-"Moderator"
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow<-"new!"
      aSmartObj$spaceBy<-"new!"
      ladd(private$.smartObjs)<-aSmartObj
      
      ### assumptions hometest
      aSmartObj<-SmartTable$new(self$results$assumptions$homotest,runner_machine)
      aSmartObj$hideOn<-list(df2=NA)
      ladd(private$.smartObjs)<-aSmartObj
      ### assumptions nromtest
      aSmartObj<-SmartTable$new(self$results$assumptions$normtest,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### assumptions collinearity stats
      aSmartObj<-SmartTable$new(self$results$assumptions$collitest,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### init all ####

      
 
      for (tab in private$.smartObjs) {
            tab$initTable()
            tab$mutenotes<-self$options$mute
      }
      

      private$.data_machine<-data_machine
      private$.runner_machine<-runner_machine
      
      ######## plotting class #######
      plotter_machine<-Plotter$new(self,runner_machine)
      plotter_machine$initPlots()
      private$.plotter_machine<-plotter_machine
      self$results$plotnotes$setContent("")
      
      now<-Sys.time()
      jinfo("INIT TIME:",now-private$.time," secs")
      
    },
    .run=function() {
      jinfo("MODULE:  #### phase run ####")
      
      if (!private$.ready$ready) {
        return()
      }
      runnow<-Sys.time()
      data<-private$.data_machine$cleandata(self$data)
      private$.runner_machine$estimate(data)
      
      ### run tables ###
      
      for (smarttab in private$.smartObjs)
           smarttab$runTable()

      private$.checkpoint()
      
      #save model preds and resids            
      private$.runner_machine$savePredRes(self$results) 
      # 
      private$.plotter_machine$preparePlots()
      # 
      # if ("plot" %in% private$.plotter_machine$warning_topics) {
      #     self$results$plotnotes$setContent(paste(private$.plotter_machine$warning[["plot"]],collapse = "; "))
      #     self$results$plotnotes$setVisible(TRUE)
      # }  
      

     if (self$options$.interface=="R") 
       self$results$.setModel(private$.runner_machine$model)
      
      jinfo("MODULE:  #### phase end ####")
      
      jinfo("RUN TIME:",Sys.time()-runnow," secs")
      
      jinfo("TIME:",Sys.time()-private$.time," secs")

      return()
          
    },


.mainPlot=function(image, ggtheme, theme, ...) {

  plot<-private$.plotter_machine$scatterPlot(image,ggtheme,theme)
  return(plot)
  
},

.jnPlot=function(image, ggtheme, theme, ...) {

  plot<-private$.plotter_machine$jnPlot(image,ggtheme,theme)

  return(plot)
  
},


.qqPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$qqplot(image,theme,ggtheme)
  return(plot)
  
},
.normPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$normplot(image,theme,ggtheme)
  return(plot)
},

.residPlot=function(image, ggtheme, theme, ...) {

  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$residPlot(image,theme,ggtheme)
  
  return(plot)
},

.formula=function() {
  
  if (!is.something(private$.runner_machine$formulaobj))
    return()
  
  private$.runner_machine$formulaobj$formula()
  
},


.sourcifyOption = function(option) {


  defaults<-c(covs_scale="centered",contrasts="simple")
  
  if (option$name=="nested_terms") {
    if (self$options$comparison)
            if (!is.something(option$value)) {
              if (self$options$nested_intercept)
                return("nested_terms= ~1")
              else
                return("nested_terms= ~0")
            }
         
  }

  if (option$name %in% NO_R_OPTS)
     return('')

 sourcifyOption(option,defaults)

}
)
)


