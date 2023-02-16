gamljmixedClass <- R6::R6Class(
  "gamljmixedClass",
  inherit = gamljmixedBase,
  private=list(
    .dispatcher=NULL,
    .data_machine=NULL,
    .runner_machine=NULL,
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
      
      emmeans::emm_options(lmerTest.limit = 20000)  
      ### set up the R6 workhorse class
      dispatcher<-Dispatch$new(self$results)
      data_machine<-Datamatic$new(self$options,dispatcher,self$data)
      runner_machine<-Runner$new(self$options,dispatcher,data_machine)
      
      ### info table ###
      aSmartObj<-SmartTable$new(self$results$info,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj


      ## R2 table ###
      aSmartObj<-SmartTable$new(self$results$main$r2,runner_machine)
      aSmartObj$spaceBy<-"model"
      ladd(private$.smartObjs)<-aSmartObj
      

      ### anova table ###
      aSmartObj<-SmartTable$new(self$results$main$anova,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      ### estimates table ###
      aSmartObj<-SmartTable$new(self$results$main$coefficients,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$ci("beta",self$options$ci_width,label=greek_vector[["beta"]])
      if (is.something(self$options$factors))
        aSmartObj$setColumnVisible<-"label"
      ladd(private$.smartObjs)<-aSmartObj
      

      
      ### contrasts code tables
      aSmartObj<-SmartArray$new(self$results$main$contrastCodeTables,runner_machine)
      aSmartObj$expandOnInit<-TRUE
      ladd(private$.smartObjs)<-aSmartObj

      ### random variances table
      aSmartObj<-SmartTable$new(self$results$main$random,runner_machine)
      aSmartObj$ci("var",self$options$ci_width)
      ladd(private$.smartObjs)<-aSmartObj

      ### random variances table
      aSmartObj<-SmartTable$new(self$results$main$randomcov,runner_machine)
      aSmartObj$activateOnData<-TRUE
      ladd(private$.smartObjs)<-aSmartObj

      ### random variances lrt table
      aSmartObj<-SmartTable$new(self$results$main$ranova,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj
      
      
      ### estimate marginal means
      
      aSmartObj<-SmartArray$new(self$results$emmeans,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$activated          <- is.something(self$options$emmeans)
      aSmartObj$expandOnInit       <- TRUE
      aSmartObj$combineBelow       <- "new!"
      aSmartObj$spaceBy            <- "new!"
      aSmartObj$hideOn             <- list(df=Inf)
      
      ladd(private$.smartObjs)<-aSmartObj
      
      ### simple effects
      ##### anova
      aSmartObj<-SmartTable$new(self$results$simpleEffects$anova,runner_machine)
      aSmartObj$activated         <- (is.something(self$options$simple_x) & is.something(self$options$simple_mods))
      aSmartObj$expandOnInit      <- TRUE
      aSmartObj$expandSuperTitle  <- "Moderator"
      aSmartObj$key               <- self$options$simple_x
      aSmartObj$combineBelow      <- 1:(length(self$options$simple_mods)-1)
      aSmartObj$spaceBy           <- (length(self$options$simple_mods)-1)
      aSmartObj$hideOn            <- list(df2=Inf)
      ladd(private$.smartObjs)<-aSmartObj
      
      ##### coefficients
      aSmartObj<-SmartTable$new(self$results$simpleEffects$coefficients,runner_machine)
      aSmartObj$activated<-(is.something(self$options$simple_x) & is.something(self$options$simple_mods))
      aSmartObj$expandOnInit      <- TRUE
      aSmartObj$expandSuperTitle  <- "Moderator"
      aSmartObj$key               <- self$options$simple_x
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$combineBelow      <-1:(length(self$options$simple_mods)-1)
      aSmartObj$spaceBy           <-(length(self$options$simple_mods)-1)
      aSmartObj$hideOn            <- list(df=Inf)
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
      
      ### assumptions nromtest
      aSmartObj<-SmartTable$new(self$results$assumptions$normtest,runner_machine)
      ladd(private$.smartObjs)<-aSmartObj

      ## post hoc #####
      
      aSmartObj<-SmartArray$new(self$results$posthoc,runner_machine)
      aSmartObj$ci("est",self$options$ci_width)
      aSmartObj$expandOnInit       <-TRUE
      aSmartObj$expandSuperTitle   <-"Comparison"
      aSmartObj$hideOn             <- list(df=Inf)
      
      ladd(private$.smartObjs)<-aSmartObj
      
      
      ### init all ####

      
 
      for (tab in private$.smartObjs) {
            tab$initTable()
      }
      

      private$.data_machine<-data_machine
      private$.runner_machine<-runner_machine
      
      ######## plotting class #######
      plotter_machine<-Plotter$new(self$options,runner_machine,self$results)
      plotter_machine$initPlots()
      private$.plotter_machine<-plotter_machine
      self$results$plotnotes$setContent("")
      
    },
    .run=function() {
      ginfo("MODULE:",self$options$.caller,"  #### phase run ####")
      
      private$.ready<-readiness(self$options)
      if (!private$.ready$ready) {
        return()
      }
      data<-private$.data_machine$cleandata(self$data)
      private$.runner_machine$estimate(data)

      ### run tables ###
      for (smarttab in private$.smartObjs) {
           smarttab$runTable()
      }


      private$.checkpoint()
      
       #save model preds and resids            
       private$.runner_machine$savePredRes(self$results) 
       # plotting if necessary
       private$.plotter_machine$preparePlots()
      

      private$.checkpoint()
      
      ### save the model if we are in R ###
      if (self$options$.interface=="R")
              self$results$.setModel(private$.runner_machine$model)
      
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

.clusterBoxplot=function(image, ggtheme, theme, ...) {
  
  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$clusterBoxplot(image,ggtheme,theme)
  return(plot)
},
.clusterResPred=function(image, ggtheme, theme, ...) {
  
  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$clusterResPred(image,ggtheme,theme)
  
  return(plot)
},

.randHist=function(image, ggtheme, theme, ...) {
  
  if (!private$.ready$ready) 
    return()
  
  plot<-private$.plotter_machine$randHist(image,ggtheme,theme)

  return(plot)
},


.formula=function() {
  
  private$.runner_machine$formula
  
},

.sourcifyOption = function(option) {


  if (option$name=="nested_re") 
          return('')

  if (option$name=="nested_terms") 
      return('')
  
  if (option$name=="comparison") 
        if (self$options$comparison) {
            form<-stats::as.formula(fromb64(private$.runner_machine$nested_formula64))  
            return(paste0("nested_model = ",deparse(form)))
        }


  defaults<-c(covs_scale="centered",contrasts="simple",scale_missing="complete")
  if (option$name %in% NO_R_OPTS)
     return('')
  
  sourcifyOption(option,defaults)

}

)
)


