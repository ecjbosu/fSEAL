#==============================================================================
# Name   : setSource
# Author : Joe W. Byers
# Date   : 01/08/2014
# Version: 1.0001
# Aim    : script to load the source files required by GIST.
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
# setSource('/METMAT/Rfiles/sources','+core');

setSource <- function (srcPath=getwd(), srcDirs ){

#set sourced directories
#put in a system cache database  

if (missing(srcDirs)) {
  srcDirs <- list.files(srcPath, pattern = '^[+]', all.files=F, full.names=F, 
  include.dirs=T);
  }
  
for (i in 1:length(srcDirs)) {
  sapply(list.files(file.path(srcPath, srcDirs[i]), pattern='^[^\\+]', all.files = F, 
    full.names = T, include.dirs=F), source, echo=F);
}


}                                             