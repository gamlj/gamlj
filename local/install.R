install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
devtools::install_github('jamovi/jmvcore')

#install.packages('jmvcore')
version

#devtools::install_github("gamlj/gamlj")
source("local/functions.R")
library(jmvcore)

#home<-"C:\\Program Files/jamovi 2.2.5.0/"
jmvtools::version()
jmvtools::check()
home<-"flatpak"
jmvtools::check(home=home)

installme("gamlj")
  
