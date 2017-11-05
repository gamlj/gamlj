install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
library(jmvtools)

instdir<-"/home/marcello/LocalForge/gamlj/build/R"
install.packages("nloptr",lib = instdir)
install.packages("pbkrtest",lib = instdir)
#jmvtools::create('galmjglm')
install.packages("Formula",lib = instdir)
install.packages("Hmisc",lib = instdir)
install.packages("quantreg",lib = instdir)
install.packages("car",lib = instdir)
install.packages("lme4",lib = instdir)
install.packages("lmerTest",type="source",lib = instdir)
install.packages("latticeExtra",lib = instdir)
install.packages("htmlwidgets",lib = instdir)
install.packages("data.table",lib = instdir)
install.packages("nnet",lib = instdir)

install.packages("htmltools",lib = instdir)
install.packages("htmlTable",lib = instdir)

install.packages("viridisLite",lib = instdir)
install.packages("viridis",lib = instdir)

deps<-c("Formula","car","multcomp", "lmerTest", "gridExtra","lsmeans","Hmisc")
install.packages(deps, lib = instdir,dependencies = c("Depends", "Imports", "LinkingTo"),
                 configure.vars=c("--no-data","--no-docs"))
#devtools::install_github(repo = "cran/nnet",subdir = instdir)

#jmvtools::check("/home/marcello/LocalForge/jamovi/")
#install(debug = F,home = "/home/marcello/LocalForge/jamovi/")


#jpath<-"/home/marcello/LocalForge/jamovi/lib/R/library/compiler/"    
#install(home = jpath)
jmvtools::check()
install()

  #install.packages("stats",lib = jpath)
R.version

