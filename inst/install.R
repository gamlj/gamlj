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
=======
 

install(debug = F)
VarCorr
lme4::VarCorr()    
>>>>>>> 2e975584bc37e58ef6260ab2e43dbd0d616d7c9a
