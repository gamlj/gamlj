install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
library(jmvtools)

instdir<-"/home/marcello/LocalForge/gamlj/build/R"
install.packages("nloptr",lib = instdir)
install.packages("pbkrtest",lib = instdir)
#jmvtools::create('galmjglm')
install.packages("Hmisc",lib = instdir)
install.packages("lme4",type="source",lib = instdir)

install.packages("lmerTest",type="source",lib = instdir)
install.packages("viridisLite",lib = instdir)
install.packages("viridis",lib = instdir)
install.packages("minqa",lib = instdir)
install.packages("car",lib = instdir)

jmvtools::check("/home/marcello/LocalForge/jamovi/")
install(debug = F,home = "/home/marcello/LocalForge/jamovi/")

#jpath<-"/home/marcello/LocalForge/jamovi/lib/R/library"    
#install.packages("stats",lib = jpath)
