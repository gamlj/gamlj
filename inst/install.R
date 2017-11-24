#install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
library(jmvtools)

instdir<-"build/R/"
install.packages("viridisLite",lib = instdir)
install.packages("viridis",lib = instdir)
library(devtools)
#jmvtools::check("/home/marcello/LocalForge/jamovi/")
#install(debug = F,home = "/home/marcello/LocalForge/jamovi/")
withr::with_libpaths(new = instdir, install_github("rvlenth/emmeans"))

#jpath<-"/home/marcello/LocalForge/jamovi/lib/R/library/compiler/"    
#install(home = jpath)
#jmvtools::check()
jmvtools::install()
    
  