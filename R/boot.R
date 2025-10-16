#### here we deal with bootstrap. The code relies on parameters:bootstrap_model, but it
#### overrides it for simple models because parameters:bootstrap_model does not work for
#### intercept-only models


gboot <- function(x, obj) UseMethod(".gboot")


### if we do not have a specific function, we set up parameters::bootstrap_model() and run it

.gboot.default<-function(model, obj) {
  
  if (!obj$formulaobj$hasTerms) {
     stop("Bootstrap method for intercept-only models is not available for models of class:", class(model)[1])
  }
  opts<-.gboot_options(obj)
  return(do.call(parameters::bootstrap_model,opts))
  
  
}

.gboot.lm<-function(model, obj) {
  
  opts<-.gboot_options(obj)
  
  if (obj$hasTerms)
    return(do.call(parameters::bootstrap_model,opts))
  else {
    fun=function(model,data,indices) {
      .data <- data[indices, ,drop=FALSE]
      .model <- suppressMessages(stats::update(model, data = .data))
      stats::coef(.model)
    }
    
    results <- boot::boot(data = model$model, statistic = fun, 
                          R = opts$iterations, sim = "ordinary", parallel = opts$parallel, 
                          ncpus = opts$n_cpus, model = model)
  ## this make the output compatible with parameters:: and emmeans:: functions
    out <- as.data.frame(results$t)
    out <- out[stats::complete.cases(out),, drop=FALSE ]
    names(out) <- insight::get_parameters(model, verbose = FALSE)$Parameter
    class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", 
                         class(out)))
    attr(out, "original_model") <- model
    
    return(out) 
    }
}

.gboot.glm<-function(model, obj) .gboot.lm(model, obj) 


.gboot_options<-function(obj) {
  
  opts_list <- list(model = obj$model, iterations = obj$options$boot_r)
  jinfo(paste("GBOOT: Boot repetitions: ", opts_list$iterations))
  
  ### check if we can go in paraller ###
  test <- try_hard(find.package("parallel"))
  if (isFALSE(test$error)) {
    if (Sys.info()["sysname"] != "Windows") {
      opts_list[["n_cpus"]] <- parallel::detectCores()
      opts_list[["parallel"]] <- "multicore"
    } else {
      opts_list[["parallel"]] <- "no"
    }
  }
  
  opts_list
  
}


  
