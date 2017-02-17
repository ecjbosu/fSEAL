#' ==============================================================================
#' Name   : fseal rePorts class
#' Author : Joe W. Byers
#' Date   : 10/05/2014
#' Version: 1.0001
#' Aim    : fseal rePorts class
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================
#' rePorts   rePorts class
rePorts <- setRefClass("rePorts"
                         ###
                         #' rePorts reporting class.
                         #' Fields:
                         #'   
                         #' Example:  
                         ###
                          ,fields = list( 
                            markDate     = "ANY",
                            limits       = "data.frame",
                            levels       = "data.frame",
                            netPosition  = "ANY",
                            calendarSprd = "ANY",
                            basisSprd    = "ANY",
                            openPosition = "ANY",
                            MVaR         = "ANY",
                            histVols     = "ANY"
                          )
                         ,contains = "fseal"
                         ,methods = list(
                           initialize = function(markDate = Sys.Date(), 
                                                 limits   = data.frame(),
                                                 levels   = data.frame(),
                                                 netPosition  = list(),
                                                 calendarSprd = list(),
                                                 basisSprd    = list(),
                                                 openPosition = list(),
                                                 histVols     = list()
                           )
                                                  {
                             # initialize the object
                             markDate <<- markDate; 
                             limits   <<- limits;
                             callSuper();
#                              initFields(levels     = data.frame());
                           },
                           setOpenPosition = function(obj,limits) {
                             #' generate credit open position
                             #' 
                             if (missing(obj))
                               stop('fseal::rePorts::setOpenPosition : portfolio object object required ');
                             # set useLims true
                             useLims <- T;
                             if (missing(limits)) useLims <- F;
                             posname <- "exposureInMMBTU";
                             rfname  <- "pricingMonth";
                             insname <- "dealTypeName";
                             id      <- 10002;
                             
                             idx     <- obj$positions$Portfolio==obj$levels$Description[obj$levels$SeriesID==id];  
                           
                             openPosition <<- data.frame(Portfolio="FSEAL Credit Facility", 
                                                        NetOpenPosition=sum(obj$positions[idx, posname]));
                             
                             # limits and utlization
                             openPosition$Limit <<- limits$ObservationDefault;
                             openPosition$Utilization <<- abs(openPosition$NetOpenPosition)/openPosition$Limit;
                           },                           
                           setNetPosition = function(obj,limits) {
                             #' generate net position
                             #' 
                             if (missing(obj))
                               stop('fseal::rePorts::setOpenPosition : portfolio object object required ');
                             # set useLims true
                             useLims <- T;
                             if (missing(limits)) useLims <- F;
                             posname <- "exposureInMMBTU";
                             rfname  <- "pricingMonth";
                             insname <- "dealTypeName";
                             
                             # hard code the sort for now
                             srt <- c(1,2,6,7,3,4,5)
                             netPosition <<- lapply(obj$levelCodes, 
                                                        FUN=function(x) aggregate(obj$positions[posname], 
                                                                                  by=obj$positions[c(x)], FUN=sum));
                             netPosition <<- lapply(netPosition, FUN=function(x) {names(x)[1]<-'Portfolio'; return(x)});
                             netPosition <<- do.call(rbind,netPosition);
                             
                             #rename and sort
                             idx <- unlist(lapply(netPosition$Portfolio, FUN=function(x) 
                               which(obj$levels$FullName%in%x)))
                             netPosition$Portfolio <<- obj$levels$Description[idx];
                             
                             netPosition <<- netPosition[srt,];
                             names(netPosition)[2] <<- "NetPosition"
                             # limits and utlization
                             idx <- unlist(lapply(netPosition$Portfolio, FUN=function(x) 
                               which(limits$Description%in%x)));
                             netPosition$Limit <<- limits$ObservationDefault[idx];
                             netPosition$Utilization <<- abs(netPosition$NetPosition)/netPosition$Limit;
                             
                           },
                           setCalendarSprd = function(obj,limits) {
                             #' generate calendar spread position
                             #' 
                             if (missing(obj))
                               stop('fseal::rePorts::scalendarSprd : portfolio object object required ');
                             # set useLims true
                             useLims <- T;
                             if (missing(limits)) useLims <- F;
                             posname <- "exposureInMMBTU";
                             rfname  <- "pricingMonth";
                             insname <- "dealTypeName";
                             # hard code the sort for now
                             srt <- c(3,1,5,7,6,2,4)
                            
                             calendarSprd <<- lapply(obj$levelCodes, 
                                                   FUN=function(x) aggregate(obj$positions[posname], 
                                                                             by=obj$positions[c(x, rfname)], 
                                                                             FUN=sum));
                             calendarSprd <<- lapply(calendarSprd, FUN=function(x) {names(x)[1]<-'Portfolio'; return(x)});
                             calendarSprd <<- do.call(rbind,calendarSprd);
                             longs        <- aggregate(calendarSprd[calendarSprd[posname]>=0,posname], 
                                                       by=list(calendarSprd[calendarSprd[posname]>=0,'Portfolio']), 
                                                       FUN=sum);
                             names(longs) <- c('Portfolio','Longs');
                             shorts <- aggregate(calendarSprd[calendarSprd[posname]<0,posname], 
                                                    by=list(calendarSprd[calendarSprd[posname]<0,'Portfolio']), 
                                                    FUN=sum);
                             names(shorts) <- c('Portfolio','Shorts');
                             calendarSprd <<- merge(longs,shorts, all=T);
                             calendarSprd$Calendar_Spread <<- apply(
                               cbind(abs(calendarSprd$Longs), abs(calendarSprd$Shorts)),1, FUN=min);
                             calendarSprd <<- calendarSprd[,c(-2,-3)];
                           
                             #rename and sort
                             idx <- unlist(lapply(calendarSprd$Portfolio, FUN=function(x) 
                               which(obj$levels$FullName%in%x)))
                             calendarSprd$Portfolio <<- obj$levels$Description[idx];
                             
                             calendarSprd <<- calendarSprd[srt,];
                             names(calendarSprd)[2] <<- "Calendar_Spread"
                             # limits and utlization
                             idx <- unlist(lapply(calendarSprd$Portfolio, FUN=function(x) 
                               which(limits$Description%in%x)));
                             calendarSprd$Limit <<- limits$ObservationDefault[idx];
                             calendarSprd$Utilization <<- abs(calendarSprd$Calendar_Spread)/calendarSprd$Limit;
                           },
                           setBasisSprd = function(obj,limits, roots="Henry Hub") {
                             #' generate calendar spread position
                             #' roots is the MET trading hub names, remove if want to calc the true spread
                             if (missing(obj))
                               stop('fseal::rePorts::scalendarSprd : portfolio object object required ');
                             # set useLims true
                             useLims <- T;
                             if (missing(limits)) useLims <- F;
                             posname <- "exposureInMMBTU";
                             rfname  <- "exposureHub";
                             insname <- "dealTypeName";
                             # hard code the sort for now
                             srt <- c(3,1,5,7,6,2,4)
                             
                             basisSprd <<- lapply(obj$levelCodes, 
                                                    FUN=function(x) aggregate(obj$positions[posname], 
                                                                              by=obj$positions[c(x, rfname)], 
                                                                              FUN=sum));
                             basisSprd <<- lapply(basisSprd, FUN=function(x) {names(x)[1]<-'Portfolio'; return(x)});
                             basisSprd <<- do.call(rbind,basisSprd);
                             basisSprd <<- basisSprd[basisSprd$exposureHub!=roots,];
                             
                             longs        <- aggregate(basisSprd[basisSprd[posname]>=0,posname], 
                                                       by=list(basisSprd[basisSprd[posname]>=0,'Portfolio']), 
                                                       FUN=sum);
                             names(longs) <- c('Portfolio','Longs');
                             shorts <- aggregate(basisSprd[basisSprd[posname]<0,posname], 
                                                 by=list(basisSprd[basisSprd[posname]<0,'Portfolio']), 
                                                 FUN=sum);
                             names(shorts) <- c('Portfolio','Shorts');
                             basisSprd <<- merge(longs,shorts, all=T);
                             
                             #min -max to get around not having hub in the calculation
                             basisSprd$Locational_Spread  <<- -1*apply(
                               cbind(-abs(basisSprd$Longs), -abs(basisSprd$Shorts)),1, FUN=min);
                             basisSprd <<- basisSprd[,c(-2,-3)];
                             #rename and sort
                             idx <- unlist(lapply(basisSprd$Portfolio, FUN=function(x) 
                              which(obj$levels$FullName%in%x)))
                             basisSprd$Portfolio <<- obj$levels$Description[idx];
                             
                             basisSprd <<- basisSprd[srt,];
                             # limits and utIlization
                             idx <- unlist(lapply(basisSprd$Portfolio, FUN=function(x) 
                               which(limits$Description%in%x)));
                             basisSprd$Limit <<- limits$ObservationDefault[idx];
                             basisSprd$Utilization <<- abs(basisSprd$Locational_Spread)/basisSprd$Limit;
                           },
                           setHistVol = function(obj) {
                             #' generate credit open position
                             #' 
                             if (missing(obj))
                               stop('fseal::rePorts::setHistVol : VaR object object required ');
                             
                             #t1 <- do.call(rbind, strsplit(rownames(obj$COV),'.',fixed=T));
                             voldaysperyr <- 252;
                             
                             histVols <<- data.frame(TradeDate=markDate, CONTRACT=obj$axis[,2], 
                                                   Month=lubridate::month(obj$axis[,3]),
                                                   Year=lubridate::year(obj$axis[,3]), Day=1, Prompt=obj$axis[,4], 
                                                   ContractDate= obj$axis[,3], 
                                                   Settle=sqrt(diag(obj$COV)*voldaysperyr))
                             
                           },
                           upload = function(data, obj, creds, rerun=F, rerunsql) {
                             #' set levels for the VaR calculation
                             #' 
                             if (missing(data) | missing(obj) | missing(creds))
                               stop('fseal::rePorts::upload : data and parameters are required object required ');
                             if (rerun & missing(rerunsql))
                                stop('fseal::rePorts::upload : on rerun sql required for updates');
                             if(!rerun) rerunsql="";
                             
                             res<-JDBC.Push(data=data, dbTable=obj$dbTable, host=obj$host, port=obj$port, 
                                            schema=obj$schema, user=creds$items$User, 
                                            pswd=creds$base64decode(creds$items$Password), 
                                            driverClass=obj$driverClass, rerun = rerun, 
                                            rerunFilter=rerunsql);
                             return(res);
                           }, 
                           loadReports = function(obj, creds, rerun=F) {
                             #' load exposures summaries
                             #' 
                             if (missing(obj) | missing(creds))
                               stop('fseal::rePorts::loadReports : data and parameters are required object required ');
                             if (!identical(length(netPosition), 0L)) {
                               data <- cbind(MarkDate=markDate, Description='Net Position', 
                                             netPosition[,c(-3,-4)], RiskFactor=NA_character_);
                               names(data)[c(3,4)] <- c("Level", "Value");
                             }
                             if (!identical(length(calendarSprd), 0L)) {
                               t1 <- cbind(MarkDate=markDate, Description='Calendar Spread', 
                                             calendarSprd[,c(-3,-4)], RiskFactor=NA_character_);
                               names(t1)[c(3,4)] <- c("Level", "Value");
                               data <- rbind(data,t1);
                             }
                             if (!identical(length(basisSprd), 0L)) {
                               t1 <- cbind(MarkDate=markDate, Description='Locational Spread', 
                                           basisSprd[,c(-3,-4)], RiskFactor=NA_character_);
                               names(t1)[c(3,4)] <- c("Level", "Value");
                               data <- rbind(data,t1);
                             }
                             if (!identical(length(openPosition), 0L)) {
                               t1 <- cbind(MarkDate=markDate, Description='Net Open Position', 
                                           openPosition[,c(-3,-4)], RiskFactor=NA_character_);
                               names(t1)[c(3,4)] <- c("Level", "Value");
                               data <- rbind(data,t1);
                             }
                                 
                             dss <- unique(data$Description);
                             rerunsql <- "";
                             if(rerun) rerunsql <- paste(" where MarkDate =", format(markDate, "'%Y-%m-%d'"), 
                                                  " AND Description IN ('",  paste(dss, collapse="','"), "') ",
                                                  sep = '');
                             res <- upload(data, obj, creds, rerun=rerun, rerunsql)
                             return(res);
                           },                           
                           loadHistVols = function(obj, creds, rerun=F) {
                             #' load historical volitilties from the VaR calculation
                             #' 
                             if (missing(obj) | missing(creds))
                               stop('fseal::rePorts::loadHistVols : data and parameters are required object required ');
                             if (identical(length(histVols), 0L)) 
                               stop('fseal::rePorts::loadHistVols : VaR historical volitilites must be extracted from object ');

                             rerunsql <- "";
                             if(rerun) rerunsql <- paste(" where TradeDate =", format(markDate, "'%Y-%m-%d'"), 
                                                          " ", sep = '');
                             if (!is.null(obj$hVolTable)) obj$dbTable <- obj$hVolTable;

                             res <- upload(data=histVols, obj, creds, rerun=rerun, rerunsql);
                
                            return(res);

                           },
                           setMVaR = function(obj,limits) {
                             #' generate VaR report
                             #' 
                             if (missing(obj))
                               stop('fseal::rePorts::setMVaR : VaR object object required ');
                             # set useLims true
                             useLims <- T;
                             if (missing(limits)) useLims <- F;
                             
                             MVaR <<- obj$results$MVaR[,-2];
                             names(MVaR)[1] <<- "Portfolio";
                             
                             # limits and utlization
                             idx <- unlist(lapply(MVaR$Portfolio, FUN=function(x) 
                               which(limits$FullName%in%x)));
                             MVaR$Limit <<- NA_real_;
                             MVaR$Limit <<- limits$ObservationDefault[idx];
                             MVaR$Utilization <<- abs(MVaR$Value)/MVaR$Limit;
                             MVaR$Portfolio <<- limits$Desc[idx]
                           }                           
                         )
)
