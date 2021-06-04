
readiness <- function(options) {
  
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


if(!is.something(options$dep)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Please define the dependent variable")
    return(result)
} 
  
  if(!is.something(options$factors) &  !is.something(options$covs)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Please define an independent variables")
    return(result)
  } 
  
  
  return(result)
}
