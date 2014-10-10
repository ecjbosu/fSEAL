
#==============================================================================
# Name   : links::JDBC.Push
# Author : Joe W. Byers
# Date   : 11/01/2013
# Version: 1.0001
# Aim    : rJava htmlunit scrape of libor rates
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#links.JDBC.Push JDBC method to connect and update a data table with a 
#data.frame.  Rerun deletion of table is allowed if a filter is applied

ODBC.Push <- function(queryAsString, DSN, data, dbTable, indexVector) {
  if (missing(data) | class(data)!='data.frame')
    stop('links::ODBC.Push : data must be a date.frame');
  if (missing(dbTable) | class(dbTable)!='character')
    stop('links::ODBC.Push : dbTable must be a character variable');

  require(RODBC);
  
  conn <- odbcConnect(DSN, case='nochange');
  checkResults <- sqlQuery(conn, queryAsString);
  if (nrow(checkResults)==0){
    res <- sqlSave(conn, data, tablename=dbTable, rownames = FALSE, append = TRUE);
  } else if (nrow(checkResults)>0){
    res <- sqlUpdate(conn, data, tablename=dbTable, index=indexVector);
  }
  odbcClose(conn);
  gc(verbose = FALSE);
  
  detach('package:RODBC');

  return(res);
    
} #end method: links.ODBC.Push
    