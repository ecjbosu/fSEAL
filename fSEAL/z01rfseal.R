#' ==============================================================================
#' Name   : cynosure link class
#' Author : Joe W. Byers
#' Date   : 10/08/2014
#' Version: 1.0001
#' Aim    : provide the ability to easily extract and deliver data 
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================
#' link   The link class that wraps up JDBC, ODBC, and other methods for data 
#' extraction and delivery.  

link <- setRefClass("link"
                     ###
                     #' link is the a linking class.
                     #' Fields:
                     #'   
                     #' Example:  
                     #'   lnk=new('link')
                     #'   lnk$getDBmart('curvesDB','curveDefs')
                     ###
                     ,contains = NULL
                     ,methods = list(
                      initialize = function() {
                        # initialize the object
                      },
                      getDBmart = function(dbmart,keymaster, ext) {
                        # read/set the dbmartdata 
                        if (missing(ext)) ext <- 'xlsm';
                        tryCatch( {
                          dbMart <- file.path(rootDBPath, workEnv, dbSubPath
                                              , paste(dbmart, ext, sep='.'));
                          out    <- xlsx::read.xlsx(file=dbMart, sheetName=keymaster);
                          return(out);
                        },
                        error = function(cond) 
                        {stop('fseal::links::getDBmart : dbmart file or tab does not exist'); 
                        }
                        )
                      },
                      setRiskFactors = function() {
                          #' setRiskFactors: read/set the Factors
                          tryCatch( {
                              out <- link()$getDBmart('curvesDB','curveDefs');
                              #reduce to internal curves only
                              out <- out[!is.na(out$CurveID)&!is.na(out$BASISParentID),];
                              #get mapping for ice
                              t1  <- link()$getDBmart('IceDB','ICEMAP');
                              t1  <- t1[!is.na(t1$MET_HUB),];
                              #get commodity definitions for base curve
                              t2 <- link()$getDBmart('curvesDB', 'commodityDefs')
                              t2 <- t2[!is.na(t2$BaseSeriesID),];
                              t2 <- out[out$SeriesID==t2$BaseSeriesID,];
                              #make sure base curves in mappings
                              for (i in 1:nrow(t2)) {
                                t1[nrow(t1)+1,] <- data.frame(t2$CodeName[i], NA, NA);
                              }
                              #merge factors 
                              out <- merge(out, t1, by='CodeName');
                              #keep basecurve for all commodities in risk factors.
                              return(out);
                            },
                          error = function(cond) {
                            stop('fseal::links::setRiskFactors : error setting risk factors check dbmarts'); 
                            }
                          )
                      },
                      writeXLObj = function(data, filenm, row.names=F){
                        ###
                        #' writeXLObj will write multiple data.frames or other class to an XL spreadsheet.
                        #' Example:  
                        #'   link()$writeXLObj(data,'C:/temp/test.xlsx')
                        #'   
                        ###
                        #
                        #parse inputs
                        if (missing(data))
                          stop('fseal::link::writeXLObj : data in a data.frame format required');
                        if (missing(filenm))
                          stop('fseal::link::writeXLObj : a file identifier is required');
                        
                        #load xlsc package
                        require(xlsx);
                        
                        rNs <- F;
                        if(hasArg(row.names)) rNs <- row.names;
                        
                        #initialize loop end point.  1 for singleton, >1 for lists.
                        len <- 1;
                        
                        #check for correct objects: first if a list of data.frames
                        if (is.null(dim(data))) {
                          #check object elements class are data.frames
                          len <- length(data);
                          if (!all(as.logical(lapply(data, is.data.frame))))
                            stop('fseal::link::writeXLObj : data.frame objects are required1');
                        } else {
                          #singleton
                          if (!is.data.frame(data))
                            stop('fseal::links::writeXLObj : data.frame objects are required');
                        }
                        #check for valid extension
                        #move this to a validextension method call.
                        if (!tools::file_ext(filenm)%in%c('xlsx','xlsm','xls'))
                          stop('fseal::links::writeXLObj : only valid Excel file types allowed (xls,xlsx,xlsm)');
                        
                        cat("fseal::links::writeXLObj : Create and Populate the workbook\n");
                        wb<-createWorkbook();
                        if (len>1) {
                          for (i in 1:len){
                            #cat(names(data)[i],'\n')
                            sheet <- createSheet(wb, sheetName=names(data)[i]);
                            #cat(names(data)[i],' add frame\n')
                            addDataFrame(data[[i]], sheet, startRow=1, startCol=1, row.names=rNs);
                          }
                        } else {
                          sheet <- createSheet(wb);
                          #cat(names(data)[i],' add frame\n')
                          addDataFrame(data, sheet, startRow=1, startCol=1, row.names=rNs);
                        }
                        cat("fseal::links::writeXLObj : This will overwrite an existing file\n");
                        cat(filenm, '\n')
                        saveWorkbook(wb, filenm);
                       
                        detach("package:xlsx");
                      },
                      archive = function(data, markDate, path, name, type, datedFlag=T) {
                        #Archive portfolio Object
                        if (identical(class(data),"list")) {
                          if(!all(unlist(lapply(lapply(data,FUN=class),"data.frame",FUN=identical))))
                            stop('fseal::links::archive : data must be a data.frame');
                         } else {
                           if (!identical(class(data),"data.frame"))
                             stop('fseal::links::archive : data must be a data.frame');
                         }
                        
                        if (missing(markDate)) markDate <- as.Date(Sys.Date);
                          
                        tryCatch({
                          #add environment to spath
                          path = file.path(rootDBPath, workEnv, 'Applications', path);
                          
                          cat("Write to external spreadsheet\n")
                          if (datedFlag) {
                            link()$writeXLObj(data, 
                                              file.path(path, 
                                                        paste(name,'-', format(markDate,'%Y-%m-%d.'), 
                                                              type, sep='')))
                          }else{
                            link()$writeXLObj(data, 
                                              file.path(path, paste(name, '.'), type, sep=''));
                          }
                          return(T)
                        },
                        error = function(cond) {
                          print(cond);
                          stop('fseal::link::archive : Archive failed'); 
                        }
                        )
                      }
                    )
)
