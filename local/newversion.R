
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
version
setwd("~/Skinner/Forge/jamovi/gamlj")
here<-getwd()
forge<-"/home/marcello/LocalForge/"
target="/home/marcello/LocalForge/gamlj/"
binaries<-"/home/marcello/LocalForge/binaries"
del<-system(paste("rm -r",target))
setwd(forge)
clone<-system("git clone https://github.com/mcfanda/gamlj.git")
setwd(forge)
cp<-system(paste("cp -r ",paste0(here,"/*"),target))
setwd(target)
(cmdz<-paste("gitversion",version))
system(cmdz)
setwd(here)
cp<-system(paste0("cp gamlj.jmo ",binaries,"/gamlj_linux.jmo"))
setwd(binaries)
git<-system(paste("gitroutine",version))
setwd(here)

