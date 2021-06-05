
readiness <- function(options) {
  
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


if(!is.something(options$dep)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Please define the dependent variable")
    return(result)
} 
  

  
  return(result)
}
