<<<<<<< HEAD

library(yaml)
check.version<-function() {
desc<-yaml.load_file("DESCRIPTION")
versio<-desc$Version
j000<-yaml.load_file("jamovi/0000.yaml")
j000$version
if (j000$version!=desc$Version) {
    cat("The version does not correspond in the jamovi e description file")
    cat(paste("jamovi:",j000$version))
    cat(paste("DESCRIPTION:",j000$version))
    return(FALSE)   
}
desc$Version
}

=======
library(yaml)
#rmarkdown::render_site(input = "./docssource/")
getVersion<-function() {
  desc<-yaml.load_file("DESCRIPTION")  
  j000<-yaml.load_file("jamovi/0000.yaml")
  if (desc$Version!=j000$version) {
    cat("Version mismatch\n")
    cat(paste("jamovi:",j000$version,"  "))
    cat(paste("R:",desc$Version))
    return(FALSE)    
  }
  cat(paste("preparing version",desc$Version))
  desc$Version    
}

version<-getVersion()

cmdz<-paste0("gitversion ",version)
system(cmdz)
>>>>>>> b32813d866de113bc7509de6e9c030199af88090
