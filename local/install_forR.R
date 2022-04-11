Sys.getenv()
localdir="/home/marcello/Skinner/Forge/jamovi/site-library"

install.packages('jmvtools', localdir,repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))


what="Matrix"
install.packages(what,localdir,repos='http://cran.rstudio.com/')

what="RcppEigen"
install.packages(what,localdir,repos='http://cran.rstudio.com/')

install.packages("/home/marcello/Skinner/Forge/jamovi/openxlsx.tar.bz2",localdir,repos = NULL, type="source")

what="devtools"
install.packages(what,localdir,repos='http://cran.rstudio.com/')
what="numDeriv"
install.packages(what,localdir,repos='http://cran.rstudio.com/')

what="ggplot2"
install.packages(what,localdir,repos='http://cran.rstudio.com/')

what="Rcpp"
install.packages(what,localdir,repos='http://cran.rstudio.com/')

library(devtools)

with_libpaths(localdir, 
              devtools::install("/home/marcello/Skinner/Stat/lmerTestR/",local=FALSE,dependencies = "Depends"))

library(RcppEigen)

