#==============================================================================
# Name   : jobs::tableArchiver
# Author : Joe W. Byers
# Date   : 11/15/2013
# Version: 1.0001
# Aim    : table arhive method
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
# Archive data.frame to a file.  Select excel, csv, open document.
tableArchiver <- function(data, fileName, fileType=NULL) {
#FileName includes full path with extension.  If fileType empty will check
#extenion.  If extension missing, fileType will be appended.  If extension and
#file type conflicted, file type will take precendence.
# xlsm is not supported in XLConnect required by xlsx.  The apache POI 
# components have not been implemented or Apache POI does not support it.

library(xlsxjars);
library(xlsx);
if (.Platform$OS.type != 'windows') library(ROpenOffice)


if (missing(data) | class(data)!='data.frame')
      stop('jobs::tableArchiver : data is required and must be data.frame');

if (missing(fileName) | class(fileName)!='character')
      stop('jobs::tableArchiver : fileName is required and must be character');

#move this to a meta data and load
validtypes = c('csv', 'odt', 'dat', 'xls', 'xlsx', 'xlsm');

ext1 = strsplit(fileName,'\\.');
ext1 = ext1[[1]][length(ext1[[1]])];

#get extension if fileType empty
if (is.null(fileType)) {
  if (ext1 %in% validtypes) {
    fileType = tolower(ext1);
    ext = ext1; }
    else {
        warning('jobs::tableArchiver : fileType is empty and no valid extension on filename, defaulting to csv');
        fileName = paste(fileName,'csv',sep='.');
        ext = fileType = 'csv';
    }
}  else {
  #check if ext valid
    fileName = sub(ext1, fileType, fileName);
    ext = ext1
}
#check if ext valid
if (!(ext %in% validtypes))
      stop('jobs::tableArchiver : fileType is invalid');
      
#check if fileName had extension that conflicts with fileType
if (fileType != ext) {
      warning('jobs::tableArchiver : fileType differs from fileName.  File type takes precedence');
}

#write csv file
if (tolower(ext) %in% c('csv','dat'))
  write.csv(data,fileName, row.names=F);

#write odt file
if (tolower(ext) %in% c('odt'))
  if (.Platform$OS.type != 'windows'){
    write.csv(data,fileName, row.names=F);
    } else {
      warning('jobs::tableArchiver : fileType is open document format, OS is windows, switching to Excel');
  }


#write excel file
if (tolower(ext) %in% c('xls', 'xlsx', 'xlsm')) {
  if (tolower(ext) == 'xlsm') {
    fileName = sub(ext, 'xlsx', fileName);
    warning('jobs::tableArchiver : xlsm fileType is not supported, switching to xlsx');  
  }
  write.xlsx2(data,fileName, row.names=F);
}

  detach("package:xlsx");
  detach("package:xlsxjars");
  
} #end jobs::tableArchiver
