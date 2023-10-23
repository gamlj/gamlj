
readiness <- function(options) {
  
  result <- list(reason = NULL, ready = TRUE, report = FALSE)

  if(isTRUE(options$donotrun)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "Do not run option activated"
    return(result)
  } 
  
if(!is.something(options$dep)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- "Please select the dependent variable"
    return(result)
} 

  if (utils::hasName(options,"cluster"))
    if (!is.something(options$cluster))  
    {
      result$ready <- FALSE
      result$report <- TRUE
      result$reason <- "Please select a cluster variable"
      return(result)
    } 
  
  if (utils::hasName(options,"re")) {
     
       if (any(sapply(options$re,function(x) length(x)==0)))  
       {
         result$ready <- FALSE
         result$report <- TRUE
         result$reason <- "Please define the random coefficients"
         return(result)
       } 
  }
  return(result)
}
