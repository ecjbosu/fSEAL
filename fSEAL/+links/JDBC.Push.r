
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

JDBC.Push <- function(data, dbTable, host, port=3306, schema, user, pswd, 
  driverClass, rerun = F, rerunFilter) {
  #
  #returnFilter:
  # for Delete: paste(" where MarkDate ='", format(MarkDate, '%Y-%m-%d'), 
  #             "';", sep= '');
  #parameter check
  #data
  if (missing(data) | class(data)!='data.frame')
    stop('links::JDBC : data must be a date.frame');
  if (missing(dbTable) | class(dbTable)!='character')
    stop('links::JDBC : dbTable must be a character variable');
    
  # these are to be changed to the environment calls to the xml meta data
  if (missing(host))
    stop('links::JDBC : host must be specified');
  if (missing(port)) port = 3306;
  if (missing(schema))
    stop('links::JDBC : schema must be specified');
  if (missing(user))
    stop('links::JDBC : user must be specified');
  if (missing(pswd))
    stop('links::JDBC : pswd must be specified');
  if (missing(driverClass))
    stop('links::JDBC : driverClass must be specified');
  if (class(driverClass) != 'list')
    driverClass = list(driverClass);

  if (rerun == T & (missing(rerunFilter) | trim(rerunFilter)=='' |
    nchar(trim(rerunFilter))==0))
    stop('links::JDBC : a rerunFilter must be specified if rerun flag = T');

  #set required libraries
  library(RJDBC)

  res <- NULL;
   
  #set the connection
  drv <- JDBC(driverClass,.jclassPath(),"`");
  connstr = setDriverString(driverClass, host, schema, port);
  conn <- dbConnect(drv, connstr,user,pswd);
  #if(grepl('microsoft',driverClass[[1]])) conn$setCatalog(schema); 
  
  cat('load data to ', connstr , 'into table ', dbTable, '.\n');
  
  #do the upload
    #make sure columns are in correct order
    fields <- dbListFields(conn, dbTable)
    idx = names(data)%in%fields;
    data = data[,idx];
    reordered.names <- names(data)[match(fields, names(data))];
    #remove NA columns in DB, these are autoincrement and nullable.
    reordered.names <- reordered.names[!is.na(reordered.names)];
 
    data <- data[ ,reordered.names]

    if (rerun){
      sqlstr = paste('delete from ', dbTable, rerunFilter, sep='');
      res <- dbSendUpdate(conn, sqlstr);

    }

    #insert try catch to handle data exists and not a rerun.  Generate email 
    #already completed, try rerun with needed.
    res<-dbWriteTable(conn, dbTable, data, overwrite=F, append=T, row.names=F);

    detach('package:RJDBC');
    detach('package:DBI');

    return(res);
    
} #end method: links.JDBC
    