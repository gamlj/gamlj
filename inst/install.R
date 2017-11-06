#install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
library(jmvtools)

instdir<-"build/R/"
install.packages("viridisLite",lib = instdir)
install.packages("viridis",lib = instdir)

#devtools::install_github(repo = "cran/nnet",subdir = instdir)

#jmvtools::check("/home/marcello/LocalForge/jamovi/")
#install(debug = F,home = "/home/marcello/LocalForge/jamovi/")


#jpath<-"/home/marcello/LocalForge/jamovi/lib/R/library/compiler/"    
#install(home = jpath)
jmvtools::check()
install()

  #install.packages("stats",lib = jpath)
R.version

