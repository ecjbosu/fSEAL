#==============================================================================
# Name   : xml_mail.parse
# Author : Joe W. Byers
# Date   : 11/11/2014
# Version: 1.0001
# Aim    : xml mail file parse script
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#parse a xml file of mailling lists of the following form.
#<?xml version="1.0" standalone="yes"?>
#<MailLIST>
#<CONTACTGROUP Group="Default">
#  <MEMBER>
#    <NAME>Joe Byers</NAME>
#    <EMAIL>ecjbosu@aol.com</EMAIL>
#  </MEMBER>
#</CONTACTGROUP>
#</MailLIST>
#

xml_mail.parse <- function(xmlfile, mailgroup) {
library(XML)

#check formals
if (missing(xmlfile) | class(xmlfile)!='character')
    stop('xml::xml_mail.parse : xmlfile must be specified and character mode');
if (missing(mailgroup) | class(mailgroup)!='character')
    stop('xml::xml_mail.parse : mailgroup must be specified and character mode');

    #get mailing list
    doc <- xmlParse(xmlfile,  useInternalNodes = TRUE) #, asText=T)
    doc <- xmlRoot(doc)
    to  <- getNodeSet(doc,paste("//CONTACTGROUP[@Group='",mailgroup,"']",sep=''))
    to  <- to[[1]];
    to  <- xmlToDataFrame(xmlChildren(to))
    #to  = paste(to$EMAIL,collapse=';')
    to  <- to$EMAIL;

    detach('package:XML');
    
    return(to);
}