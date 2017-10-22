library(jmvtools)

instdir<-"/home/marcello/LocalForge/gamlj/build/R"
install.packages("nloptr",lib = instdir)
install.packages("pbkrtest",lib = instdir)
#jmvtools::create('galmjglm')
install.packages("lmerTest",lib = instdir)
install.packages("Hmisc",lib = instdir)
install.packages("viridisLite",lib = instdir)
install.packages("viridis",lib = instdir)

jmvtools::check("/home/marcello/Downloads/jamovi/")


install(debug = F,home = "/home/marcello/Downloads/jamovi/")

    
