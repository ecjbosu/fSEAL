#==============================================================================
# Name   : libor_scrape.mail
# Author : Joe W. Byers
# Date   : 11/01/2013
# Version: 1.0001
# Aim    : email wrapper for libor_scrape job
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#email wrapper for libor_scrape job

libor_scrape.mail <- function(from, to, subject, msgBody, caption, dataTable='',
    smtpserver, mimemessagetype = 'html') {
  
  ret <- lapply(list.files(file.path(getwd(),'xml'), all.files = F, 
    full.names = T, include.dirs=F), source);
  ret = NULL;
#===============================================================================
#inputs/constants
  if (missing(from) | class(from)!='character') 
      stop('libor_scrap::mail : from is required and must be character');
  if (missing(to) | class(to)!='character') 
      stop('libor_scrap::mail : to is required and must be character');
  if (missing(subject) | class(subject)!='character') 
      stop('libor_scrap::mail : subject is required and must be character');
  if (missing(msgBody) | class(msgBody)!='character') 
      stop('libor_scrap::mail : msgBody is required and must be character');
  if (missing(smtpserver)) smtpserver  = 'mail.martinmlp.com'; 
    # put in xml and load
  if (missing(caption)) caption = '';
  if (missing(dataTable) | class(dataTable)=='character')
    if (missing(dataTable) | nchar(dataTable)==0)
      warning('libor_scrap::mail : dataTable has no data!!!');
      
#==============================================================================

  ht <- hwriter::hwrite( rbind(hwriter::hwrite(as.matrix(caption), bgcolor='red', 
      style=list('text-align:center;color:white')),
      hwriter::hwrite(dataTable, row.names=F, heading=3, bgcolor='white',   
      table.style='text-align:right', row.bgcolor='#0066FF', 
      row.style=list('text-align:center;color:white')
     )),row.style=list('text-align:center;color:white'));
  bdy1 <- paste(msgBody, '<div>', ht,'</div>',collapse='');
  
  ret = jsendMail(from=from, to=to, subject=subject, msgBody=bdy1, 
      smtpServer=smtpserver, mimemessagetype = 'html');

return(ret);

}
                      