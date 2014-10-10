
#==============================================================================
#' Name   : links::ODBC.Pull
#' Author : Joe W. Byers
#' Date   : 08/25/2014
#' Version: 1.0001
#' Aim    : pull data
#' Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#links.ODBC.Pull ODBC method to connect and retrieve a data table with a 
#data.frame.  

ODBC.Pull <- function(sqlstr, DSN, dbInfoFlag=F) {
  #
  #returnFilter:
  # for Delete: paste(" where MarkDate ='", format(MarkDate, '%Y-%m-%d'), 
  #             "';", sep= '');
  
  #parameter check
  #data
  if (missing(sqlstr) | class(sqlstr)!='character')
    stop('links::ODBC : sql string must be a character string');
  # these are to be changed to the environment calls to the xml meta data
  if (missing(DSN))
    stop('links::ODBC : DSN must be specified');
  
 #set required libraries
 require(RODBC);
 
  data <- NULL;
   
  #set the connection
  conn <- odbcConnect(DSN, case='nochange');
  #do the download
  data = sqlQuery(conn, sqlstr);
         
  odbcClose(conn);
  gc(verbose = FALSE);
 
  detach('package:RODBC');
   
  return(data);
    
} #end method: links.JDBC
    