
fixclasses<-function(){

  fixes<-yaml::yaml.load_file("jamovi/fixclasses.yaml")
  refs<-as.data.frame(do.call("rbind",fixes))
  refs$name<-unlist(refs$name)
  refs$class<-unlist(refs$class)
  files<-list.files("R/",".h.R")
  comms<-unlist(lapply(files, function(f) gsub(".h.R","",f,fixed=T)))
  for (i in seq_along(files)) {
    f<-files[[i]]
    txt<-readLines(paste0("R/",f))
    newtxt<-character()
    name<-comms[[i]]
    .class<-refs[refs$name==name,"class"]
    cat("Fixing",f,"for class",.class,"....")
    .search<-paste0(.class,"Results <- if")
    if (length(grep("classname",txt))>0) {
      cat("not necessary\n")
      next()
      
    }
    for (t in txt) {
      newtxt[[length(newtxt)+1]]<-t
      if (length(grep(.search,t,fixed=T))>0) {
        .classstring<-paste0("       classname=c(\"gamlj\",\"",.class,"\"),")
        newtxt[[length(newtxt)+1]]<-.classstring
        cat("done\n")
      }
    }
    writeLines(newtxt, con=paste0("R/",f))
  }
  
  
}


installme<-function(what,home="flatpak") {
  library(what,character.only=TRUE)
  s<-sessionInfo()
  pkg<-s$otherPkgs[[what]]
  pv<-pkg$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  cat("yaml version:",zv,"\n")
  cat("pack version:",pv,"\n")
  cat("git version:",gv,"\n")
  
  if (all(c(pv,zv)==gv))
    jmvtools::install(home = home)
  else
    warning("versions mismatch")
  
}
