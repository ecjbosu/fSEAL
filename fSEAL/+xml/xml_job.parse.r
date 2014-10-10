#==============================================================================
# Name   : xml_job.parse
# Author : Joe W. Byers
# Date   : 11/14/2013
# Version: 1.0001
# Aim    : xml job meta data file parse script
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#parse a xml file of data of the following form. All tags will be returned as a 
#named list that contains the meta data for the job.
#<?xml version="1.0" standalone="yes"?>
#<Job  job=libor_scrape>
#<urls>http://www.global-rates.com/interest-rates/libor/american-dollar/american-dollar.aspx</urls>
#<dbTable>
#  <raw>rates_raw</raw>
#  <data>rates</data>
#</dbTable>
#<host>db.server.com</host>
#<port>3306</port>
#<schema>my_schema</schema>
#<user>my_user</user>
#<pswd>mypassword</pswd>
#<driverClass>com.mysql.jdbc.Driver</driverClass>
#<xmlfile>xmlfile.xml</xmlfile>
#<mailgroup>Default</mailgroup>
#<smtpserver>smtp.server.com</smtpserver>
#</Job>
##

xml_job.parse <- function(xmlfile, xpathfilter=NULL) {
require(XML)
#require(hash)

setMode <- function(x) {
  out1 = list();
  for (i in 1:xmlSize(x)) {
    if (xmlSize(x[[i]])==1){
      out1[xmlName(x[[i]])]=xmlValue(x[[i]])
      if (identical(names(xmlAttrs(x[[i]])),'mode')){
        if (identical(as.vector(xmlAttrs(x[[i]])),'list')){
          out1[xmlName(x[[i]])]=list(read.csv(text=xmlValue(x[[i]]), sep=',', header=F));
        }
        mode(out1[xmlName(x[[i]])])=xmlAttrs(x[[i]])}
      }  
      else {
        out1[[i]]=setMode(x[[i]])
        names(out1)[i]=xmlName(x[[i]])     
      }
    }
  return(out1);    
}
        
# xpath filter is if multiple job meta data is stored in one xml file.
#check formals
out = NULL;

if (missing(xmlfile) | class(xmlfile)!='character')
    stop('xml::xml_job.parse : xmlfile must be specified and character mode');
if (!missing(xpathfilter) )
  if (class(xpathfilter)!='character' & !is.null(xpathfilter))
    stop('xml::xml_job.parse : xpathfilter character mode');

    #get mailing list
    t1 <- xmlTreeParse(xmlfile,  useInternalNodes = TRUE) #, asText=T)
    t1 <- xmlRoot(t1)
    if (!is.null(xpathfilter)) {
      t1  <- getNodeSet(t1,paste('//Job[@Job=',xpathfilter,']',sep=''))
      t1  <- t1[[1]];
    }
    t1  <- xmlChildren(t1);
    #parse values and set the mode for each parameter
    out         <- setMode(t1);
    
#    detach('package:XML');
#    detach('package:hash');
    rm('setMode');
    
    return(out);
}

