
readiness <- function(options) {
  
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


if(!is.something(options$dep)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Please select the dependent variable")
    return(result)
} 

  if (hasName(options,"cluster"))
    if (!is.something(options$cluster))  
    {
      result$ready <- FALSE
      result$report <- TRUE
      result$reason <- glue::glue("Please select a cluster variable")
      return(result)
    } 
  
  if (hasName(options,"randomTerms")) {
     
       if (any(sapply(options$randomTerms,function(x) length(x)==0)))  
       {
         result$ready <- FALSE
         result$report <- TRUE
         result$reason <- glue::glue("Please define the random coefficients")
         return(result)
       } 
  }
  return(result)
}