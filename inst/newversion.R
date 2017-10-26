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
