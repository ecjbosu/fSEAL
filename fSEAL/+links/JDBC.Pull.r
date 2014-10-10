
#==============================================================================
# Name   : links::JDBC.Pull
# Author : Joe W. Byers
# Date   : 12/02/2013
# Version: 1.0001
# Aim    : pull data
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#links.JDBC.Pull JDBC method to connect and retrieve a data table with a 
#data.frame.  

JDBC.Pull <- function(sqlstr, host, port=3306, schema, user, pswd, 
  driverClass, dbInfoFlag=F) {
  #
  #returnFilter:
  # for Delete: paste(" where MarkDate ='", format(MarkDate, '%Y-%m-%d'), 
  #             "';", sep= '');
  
  setDriverString<-function(drv, host, schema, port) {
    if(grepl('mysql',drv)) return(paste('jdbc:mysql://', host, ':', port, '/', schema, sep=''));
    if(grepl('microsoft',drv)) return(paste('jdbc:sqlserver://', host, ':', port, ';DatabaseName=', schema, sep=''));
    if(grepl('oracle',drv)) return(paste('"jdbc:oracle:thin:@', host, ':', port, '/', schema, sep=''));
  }  
  #parameter check
  #data
  if (missing(sqlstr) | class(sqlstr)!='character')
    stop('links::JDBC : sql string must be a character string');
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
  
 #set required libraries
 library(RJDBC)

  res <- NULL;
   
  #set the connection
  drv <- JDBC(driverClass,.jclassPath(),"`");
  connstr = setDriverString(driverClass[[1]], host, schema, port);
  #connstr = paste(setDriverString(driverClass[[1]]), host, ifelse(port=='','',':'), 
  #  port,'/',schema, sep='');
  conn <- dbConnect(drv, connstr,user,pswd);
  #if(grepl('microsoft',driverClass[[1]])) conn$setCatalog(schema);
  #do the download

    #insert try catch to handle data exists and not a rerun.  Generate email 
  #already completed, try rerun with needed.
  res = dbSendQuery(conn, sqlstr);
         
  data = fetch(res, n=-1);
  
  if (dbInfoFlag){
    #set the date field class for each column
    cols <- dbColumnInfo(res);
    #dates
    idx <- cols$field.type=='date';
    if (any(idx)) data[,idx]<-lapply(data[,idx], as.Date);
    #date times
    idx <- cols$field.type=='datetime';
    if (any(idx)) data[,idx]<-lapply(data[,idx], as.POSIXct);
  }

  detach('package:RJDBC');
  detach('package:DBI');


  
  return(data);
    
} #end method: links.JDBC
    