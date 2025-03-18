Saver <- R6::R6Class(
  "Saver",
  cloneable=FALSE,
  class=FALSE,
  inherit = Scaffold,
  public=list(
      initialize=function(jmvobj,runner, plotter) {
        
            super$initialize(jmvobj)
            private$.runner<-runner
            private$.plotter<-plotter
        
      },
      run = function() {
  
             private$.savePredRes()      
             private$.export()
          
      }
      
  ), ## end of public
  private=list(
    .runner=NULL,
    .plotter=NULL,
    .savePredRes=function() {
                            
                            results <- self$analysis$results
                            
                            if (self$options$predicted && results$predicted$isNotFilled()) {
                                jinfo("Saving predicted")
                                pdf     <- predicted(private$.runner$model,self)
                                results$predicted$set(1:ncol(pdf),
                                                      names(pdf),
                                                      rep("Predicted",ncol(pdf)),
                                                      rep("continuous",ncol(pdf)))
                                results$predicted$setValues(pdf)
                            }
      
                            if (self$options$residuals && results$residuals$isNotFilled()) {
                                jinfo("Saving residuals")
                                p<-stats::residuals(private$.runner$model,type="response")
                              # we need the rownames in case there are missing in the datasheet
                              pdf <- data.frame(residuals=p, row.names=rownames(insight::get_data(self$model, source="frame")))
                              results$residuals$setValues(pdf)
                            }
     },
    .export = function() {
      
           if (!self$option("export"))
               return()

         ##### estimated marginal means ##########
      
               if (self$option("export_emm")) {
               
                  emm<-procedure.emmeans(private$.runner)
                  if (is.something(emm)) {
                  
                    for (i in seq_along(emm)) {
                          jmvReadWrite:::jmvOpn(dtaFrm = data.frame(emm[[i]]), dtaTtl =  paste0("emmean",i))
                    }
                 } else {
                   self$warning<-list(topic="savenotes",
                                    message=paste("Estimated marginal means were not requested. File cannot be exported."),
                                    head="warning")
               }
               }
       ######### plot data ##############
      
               if (self$option("export_plot")) {
              
                  plotgroup  <-  self$analysis$results$get("mainPlots")
                  
                  if (is.something(plotgroup$items)) {
                    
                    for (i in seq_along(plotgroup$items)) {
                      plot            <-  plotgroup$items[[i]]
                      plotdata        <-  data.frame(plot$state$plotData)  
                      names(plotdata) <-  fromb64(names(plotdata))
                      jmvReadWrite:::jmvOpn(dtaFrm = plotdata, dtaTtl =  paste0("plotdata",i))
                      if ("randomData" %in% names(plot$state)) {
                        rdata            <-   plot$state$randomData
                        names(rdata)[2]  <-   names(plotdata)[1]
                        jmvReadWrite:::jmvOpn(dtaFrm =rdata , dtaTtl =  paste0("plotrandom",i))
                      }
                    }
                 } else {
                 self$warning<-list(topic="savenotes",
                                    message=paste("No plot was requested. File cannot be exported."),
                                    head="warning")
                }
               }
              ####### random effects ########
      
              if (self$option("export_re")) {
               
                  model      <-  private$.runner$model
                  re         <-  gRanef(model,self)
                  names(re) <-  fromb64(names(re))
                  
                  for (i in seq_along(re)) {
                           goodname <- make.names(names(re)[i])
                           data<-data.frame(re[[i]])
                           names(data)<-fromb64(names(data))
                           data[[goodname]]<-rownames(data)
                           jmvReadWrite:::jmvOpn(dtaFrm = data, dtaTtl =  paste0("ranef",i))
              }
               
            }
          }
  ) # end of private
) # end of class
