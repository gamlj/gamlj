
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

