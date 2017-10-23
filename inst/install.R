library(jmvtools)

instdir<-"/home/marcello/LocalForge/gamlj/build/R"
ff<-find.package("nloptr",lib.loc = instdir,quiet = T)
if (length(ff)==0)
  install.packages("nloptr",lib = instdir)
ff<-find.package("pbkrtest",lib.loc = instdir,quiet = T)
if (length(ff)==0)
  install.packages("pbkrtest",lib = instdir)
#jmvtools::create('galmjglm')
install.packages("lmerTest",lib = instdir)
install.packages("Hmisc",lib = instdir)
 

install(debug = F)
VarCorr
lme4::VarCorr()    
