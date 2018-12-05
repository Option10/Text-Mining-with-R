loadPackage <- function(package1, ...){   
  
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages())){
      cat("Loading package ",package,"\n")
      do.call('library', list(package))
    }
    # if package is not installed locally, download, then load
    else {
      cat("Installing package ",package,"\n")
      install.packages(package)
      do.call("library", list(package))
    }
  } 
}