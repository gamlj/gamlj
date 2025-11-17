Saver <- R6::R6Class(
    "Saver",
    cloneable = FALSE,
    class = FALSE,
    inherit = Scaffold,
    public = list(
        initialize = function(jmvobj, runner, plotter) {
            super$initialize(jmvobj)
            private$.runner <- runner
            private$.plotter <- plotter
        },
        run = function() {
            private$.savePredRes()
            private$.export()
        }
    ), ## end of public
    private = list(
        .runner = NULL,
        .plotter = NULL,
        .savePredRes = function() {
            results <- self$analysis$results

            if (self$options$predicted && results$predicted$isNotFilled()) {
                jinfo("Saving predicted")
                pdf <- predicted(private$.runner$model, self)
                results$predicted$set(
                    1:ncol(pdf),
                    names(pdf),
                    rep("Predicted", ncol(pdf)),
                    rep("continuous", ncol(pdf))
                )
                results$predicted$setValues(pdf)
            }

            if (self$options$residuals && results$residuals$isNotFilled()) {
                jinfo("Saving residuals")
                p <- stats::residuals(private$.runner$model, type = "response")
                # we need the rownames in case there are missing in the datasheet
                pdf <- data.frame(residuals = p, row.names = rownames(insight::get_data(self$model, source = "frame")))
                results$residuals$setValues(pdf)
            }
        },
        .export = function() {
            if (!self$option("export")) {
                return()
            }
            ### define the performer function based on jamovi version
            option<-self$options$option("export")
         
            if (is.null(option$perform)) {
              ## old style
              .saverfun <- function(data,title,msg) {
                jinfo("SAVER: saving old style")
                jmvReadWrite:::jmvOpn(dtaFrm = data, dtaTtl = title)
              }
              .failfun <- function(msg) {
                self$warning=list(topic="modelnotes",message=msg,head="warning")
              }
            } else {
              # new style
              .saverfun <- function(data,title,msg) {
                  jinfo("SAVER: saving new style")
                  option$perform(function(action) {
                    list(
                      data = data,
                      title = title)
                  })
              }
              .failfun <- function(msg) {
                option$perform(function(action) stop(msg))
              }      
              
            } ### end
            


            ##### estimated marginal means ##########

            if (self$option("export_emm")) {
                emm <- procedure.emmeans(private$.runner)
                if (is.something(emm)) {
                    message = paste("Estimated marginal means cannot be exported")
                    for (i in seq_along(emm)) {
                      .saverfun(data.frame(emm[[i]]), paste0("emmean", i),message)
                    }
                } else {
                  message = paste("Estimated marginal means were not requested")
                   .failfun(message)
                }
            }
            ######### plot data ##############

            if (self$option("export_plot")) {
                plotarray <- self$analysis$results$get("mainPlots")

                if (is.something(plotarray$items)) {
                    for (i in seq_along(plotarray$items)) {
                      for(j in seq_along(plotarray$items[[i]]$items)) {
                        plot <- plotarray$items[[i]]$items[[j]]
                        plotdata <- data.frame(plot$state$plotData)
                        names(plotdata) <- fromb64(names(plotdata))
                        .saverfun(plotdata, paste0("plotdata", i,"_",j))
                        if ("randomData" %in% names(plot$state)) {
                            rdata <- plot$state$randomData
                            names(rdata)[2] <- names(plotdata)[1]
                            .saverfun(rdata, paste0("plotrandom", i,"_",j))
                        }
                      }
                    }
                } else {
                    message = paste("No plot was requested")
                    .failfun(message)
                }
              }
            
            ####### random effects ########

            if (self$option("export_re")) {
                model <- private$.runner$model
                re <- gRanef(model, self)
                names(re) <- fromb64(names(re))
                for (i in seq_along(re)) {
                    goodname <- make.names(names(re)[i])
                    data <- data.frame(re[[i]])
                    names(data) <- fromb64(names(data))
                    data[[goodname]] <- rownames(data)
                    .saverfun(data, paste0("ranef", i))
                }
            }
        }
    ) # end of private
) # end of class
