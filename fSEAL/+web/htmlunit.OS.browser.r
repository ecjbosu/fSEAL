#==============================================================================
# Name   : htmlunit.OS.browser
# Author : Joe W. Byers
# Date   : 11/13/2013
# Version: 1.0001
# Aim    : return htmlunit browser version based on OS
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#

htmlunit.OS.browser <- function() {
attach( javaImport(packages = "com.gargoylesoftware.htmlunit"),
  pos= 2, name = 'com.gargoylesoftware.htmlunit');

#default
ret<- BrowserVersion$CHROME;
if (.Platform$OS.type == 'windows')
  ret<-BrowserVersion$INTERNET_EXPLORER_10;
  #BrowserVersion$FIREFOX_17
return(ret)

}