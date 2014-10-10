#==============================================================================
# Name   : links::setDriverString
# Author : Joe W. Byers
# Date   : 12/12/2013
# Version: 1.0001
# Aim    : sets the driver string based on teh driver
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#

setDriverString<-function(drv, host, schema, port) {
    if(grepl('mysql',drv)) return(paste('jdbc:mysql://', host, ':', port, '/', schema, sep=''));
    if(grepl('microsoft',drv)) return(paste('jdbc:sqlserver://', host, ':', port, ';DatabaseName=', schema, sep=''));
    if(grepl('oracle',drv)) return(paste('"jdbc:oracle:thin:@', host, ':', port, '/', schema, sep=''));
  }
