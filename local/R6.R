library(R6)
library(storr)
library(pryr)
source("R/jScafFunctions.R")
options<-list(a=1,b=1:10,c=sample(letters,20))
first<-Dispatch$new(options)
second<-Scaffold$new(first)
second$options<-first$options
second$option("c")

system.time({
for (i in seq_len(90000))  
     second$getoption("c")

})

system.time({

  for (i in seq_len(90000))  
      second$dispatcher$options$c
  
})

system.time({
  
  for (i in seq_len(90000))  
    second$options$c
  
})
