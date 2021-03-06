#==============================================================================
# Name   : tableScrapeJob.mail
# Author : Joe W. Byers
# Date   : 11/14/2013
# Version: 1.0001
# Aim    : email wrapper for tableScrapeJob job
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#email wrapper for tableScrapeJobjob

tableScrapeJob.mail <- function(from, to, subject, msgBody, caption, dataTable='',
    smtpserver, mimemessagetype = 'html', signature='') {

ret<-NULL;
  
ret <- lapply(list.files(file.path(getwd(),'xml'), all.files = F, 
    full.names = T, include.dirs=F), source);
    
ret<-NULL;

#===============================================================================
#inputs/constants
  if (missing(from) | class(from)!='character') 
      stop('tableScrapeJob::mail : from is required and must be character');
  if (missing(to) | class(to)!='character') 
      stop('tableScrapeJob::mail : to is required and must be character');
  if (missing(subject) | class(subject)!='character') 
      stop('tableScrapeJob::mail : subject is required and must be character');
  if (missing(msgBody) | class(msgBody)!='character') 
      stop('tableScrapeJob::mail : msgBody is required and must be character');
  if (missing(smtpserver)) smtpserver  = 'mail.financialseal.com'; 
    # put in xml and load
  if (missing(caption)) caption = '';
  if (missing(dataTable) | class(dataTable)=='character')
    if (missing(dataTable) | nchar(dataTable)==0)
      warning('tableScrapeJob::mail : dataTable has no data!!!');
      
#==============================================================================

  ht <- hwriter::hwrite( rbind(hwriter::hwrite(as.matrix(caption), bgcolor='red', 
      style=list('text-align:center;color:white')),
      hwriter::hwrite(dataTable, row.names=F, heading=3, bgcolor='white',   
      table.style='text-align:right', row.bgcolor='#0066FF', 
      row.style=list('text-align:center;color:white')
     )),row.style=list('text-align:center;color:white'));
  bdy1 <- paste(msgBody, '<div>', ht,'</div>', '<div>', signature,'</div>', collapse='');
  
  ret = jsendMail(from=from, to=to, subject=subject, msgBody=bdy1, 
      smtpServer=smtpserver, mimemessagetype = 'html');

return(ret);

}
                      
