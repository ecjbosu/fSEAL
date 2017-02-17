#' ==============================================================================
#' Name   : fseal portFolio class
#' Author : Joe W. Byers
#' Date   : 10/05/2014
#' Version: 1.0001
#' Aim    : fseal portFolio class
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================
#' portFolio   portFolio class
portFolio <- setRefClass("portFolio"
                         ###
                         #' portFolio class for handling trades.
                         #' Fields:
                         #'   
                         #' Example:  
                         #'   portFolio$setSettled(tradetypeList, settleDates, markDate)
                         #'   returns logical vector of active trades in portfolio
                         ###
                         ,fields = list( 
                           markDate        = "ANY",
                           baseReportLevel = "numeric",
                           levelCodes      = "ANY",
                           positions       =  "data.frame",
                           levels          = "data.frame",
                           exposures       = "list",
                           riskExposures   = "list",
                           sql             = "character"
                         )
                         ,contains = "fseal"
                         ,methods = list(
                           initialize = function(markDate = Sys.Date(), 
                                                 baseReportLevel = 0,
                                                 positions = data.frame()) {
                             # initialize the object
                             markDate <<- markDate; 
                             baseReportLevel <<- baseReportLevel;
                             positions <<- positions;
                             callSuper();
                             initFields(exposures     = list(),
                                        riskExposures = list(),
                                        levelCodes    = list(),
                                        levels        = data.frame(),
                                        sql           = NA_character_
                             );
                           },
                           setLevelCodes = function() {
                             #' set unique levels for the portfolio calculation
                             #' 
                             if (identical(length(levels),0L))
                               stop('fseal::portFolio::setLevelCodes : reporting levels object required ');
                             
                             levelCodes <<- unique(levels$LevelCode);
                           },
                           setLevels = function() {
                             #' set levels for the portfolio calculation
                             #' 
                             if (identical(length(positions),0L))
                               stop('fseal::portFolio::setLevels : position object levels required ');
                             
                             levels <<- buildReportLevels(baseReportLevel, unique(positions$strategy));
                           },
                           rollBOMMonth = function(field, markDate) {
                             #' roll cash to prompt
                             #' use if not running cash month VaR
                             if (missing(field))
                             if (missing(markDate))
                               #set to first of next month.self
                               stop('fseal::portFolio::rollBOMMonth : a markDate is required');
                             
                             if (!is.data.frame(positions))
                               stop('fseal::portFolio::rollBOMMonth : data.frame objects are required');
                             
                             yr <- lubridate::year(markDate);
                             
                             # -3 business days for nymex NG expirations.
                             currentExp      <- bizdays::add.bizdays(lubridate::ceiling_date(markDate,'month'), -3, 
                                                       bizdays::Calendar(name="FSEAL", 
                                                         start.date=lubridate::ceiling_date(markDate,'month'), 
                                                         end.date=lubridate::ceiling_date(markDate,'month'),
                                                         holidays=as.Date(timeDate::holidayNYSE(yr)@Data)
                                                         , weekdays = c("saturday", "sunday")))
                             
                             mm <- lubridate::month(markDate);
                             
                             #find the trade in the cash month
                             idx <- lubridate::month(positions[,field])==mm & lubridate::year(positions[,field])==yr
                             lubridate::ceiling_date(positions[idx, field],unit='month')
                             
                             tryCatch( {
                               idx <- positions[,field]<currentExp;
                               positions[idx,field] <<- update(positions[idx,field], 
                                                                 month=lubridate::month(currentExp)+1);
                               
                             },
                             error = function(cond) 
                             {stop('fseal::portFolio::rollBOMMonth : Date roll failed'); 
                             }
                             )
                           },
                           setSettled = function(tradetypeList=NULL, settleDates, markDate) {
                             #' check if trade is settled
                             #' 
                             if (missing(settleDates) || missing(markDate))
                               stop('fseal::portFolio::setSettled : a markDate is required');
                             
                             # load the instruments
                             tradetypes <- link()$getDBmart('riskDB','instruments');
                             #filter only the tail Levels
                             idx        <- tradetypes$LevelCode=='Tail';
                             tradetypes <- tradetypes[idx, ];
                             
                             tradetypeflag <- T;
                             
                             if (is.null(tradetypeList)) tradetypeflag <- F;
                             
                             out = logical(length(positions[,settleDates])); #init to false
                             
                             yr <- lubridate::year(markDate);
                             mm <- lubridate::month(markDate);
                             
                             #create calendar
                             cal  <- bizdays::Calendar(name="FSEAL", start.date=markDate, end.date=markDate+30*365,
                                                       holidays=as.Date(timeDate::holidayNYSE(seq(yr,yr+30))@Data)
                                                       , weekdays = c("saturday", "sunday"));
                             
                             if (tradetypeflag) {
                               
                               for (i in  1:dim(tradetypes)[1]) {
                                 
                                 #find all that match this tradetype
                                 idx = positions[,tradetypeList]==tradetypes$Description[i];
                                 
                                 #find the trade in the cash month
                                 #do month check in leui of product code
                                 idx1 <- lubridate::month(positions[,settleDates])==mm & 
                                   lubridate::year(positions[,settleDates])==yr;
                                 
                                 # filter off PNL run date against expiration.  If fut settle today, no
                                 # exposure tomorrow, swing are done for the next 2 PNL run date
                                 out[idx&idx1]     = positions[idx&idx1,settleDates] >= 
                                   bizdays::add.bizdays(markDate, 1 * tradetypes$SettleLead[i], cal);
                                 out[idx&!idx1]     = positions[idx&!idx1,settleDates] >= 
                                   bizdays::add.bizdays(markDate, 1 , cal);
                               }
                             } else {
                               
                               #set other settles like run date must be less than expirations or
                               #only include unexpired trades
                               out <- settleDates < markDate;
                               
                             }
                             positions <<-  positions[out, ]
                             return(1);
                           },
                           setExpoReportLevels = function() {
                             #' setExpoReportLevels set the reporting levels
                             #'    
                             if (dim(positions)[1]==0)
                               stop('fseal::portFolio::setExpoReportLevels : exposures required');
                             
                             dbmart  <- link()$getDBmart('riskDB', 'Portfolios');
                             
                             ulevels <- unique(dbmart[, c('LevelCode', 'LevelID')]);
                             
                             positions$SeriesID <<- 
                               dbmart$SeriesID[unlist(lapply(positions$strategy, 
                                                             FUN=function(x) match(x, dbmart$Description)))]
                             
                             #create new columns
                             for (i in 0:baseReportLevel) {
                               t1 <- ulevels$LevelCode[ulevels$LevelID == i];
                               t2 <- dbmart[unlist(lapply(positions$SeriesID, 
                                                          FUN=function(x) match(x, dbmart$SeriesID))),];
                               positions[,t1] <<- levelrerecur(i, t2, dbmart);
                               
                             }
                             return(1);
                           }, 
                           setInstrumentLevels = function() {
                             #' setExpoReportLevels set the reporting levels
                             #'    
                             if (dim(positions)[1]==0)
                               stop('fseal::portFolio::setInsrumentLevels : exposures required');
                             
                             dbmart  <- link()$getDBmart('riskDB', 'instruments');
                             
                             ulevels <- unique(dbmart[, c('LevelCode', 'LevelID')]);
                             
                             positions$InstrumentSeriesID <<- 
                               dbmart$SeriesID[unlist(lapply(positions$dealTypeName, 
                                                             FUN=function(x) match(x, dbmart$Description)))]
                             
                             positions$instrumentType <<- 
                               dbmart$Type[unlist(lapply(positions$dealTypeName, 
                                                         FUN=function(x) match(x, dbmart$Description)))]
                             
                             #create new columns
                             for (i in 0:(max(ulevels$LevelID)-1)) {
                               t1 <- ulevels$LevelCode[ulevels$LevelID == i];
                               t2 <- dbmart[unlist(lapply(positions$InstrumentSeriesID, 
                                                          FUN=function(x) match(x, dbmart$SeriesID))),];
                               positions[,paste('Instrument_',t1,sprintf('%02d',i),sep="")] <<- 
                                 levelrerecur(i, t2, dbmart);
                               
                             }
                             return(1);
                           }, 
                           setRiskFactors = function(hub, tenorcol, curveDB) {
                             #' setRiskFactors initialize risk factor fields
                             #'    
                             if (missing(hub) | missing(tenorcol) | missing(curveDB))
                               stop('fseal::portFolio::setRiskFactors : all parameters are required');
                             
                             positions$riskFactor <<- NULL;
                             positions$riskFactor <<- unlist(lapply(positions[,hub], 
                                FUN=function(x) curveDB$CodeName[!is.na(curveDB$HUB)][x==curveDB$HUB[!is.na(curveDB$MET_HUB)]]))
                             positions$riskFactor <<- paste(positions$riskFactor, positions[,tenorcol], sep=".")
                             return(1);
                           },
                           setExposures = function() {
                             #' set report vectors for the Exposure calculation
                             #'  By levels, instruments, risk factors.
                             if (identical(length(positions),0L))
                              stop('fseal::portFolio::setExposures : positions object required ');
                             if (identical(length(levels),0L))
                              stop('fseal::portFolio::setExposures : reporting levels must be defined ');
                             posname <- "exposureInMMBTU";
                             rfname  <- "riskFactor";
                             insname <- "dealTypeName";
                            
                             exposures[["levels"]] <<-lapply(levelCodes, 
                                                          FUN=function(x) aggregate(positions[posname], 
                                                                                    by=positions[c(x,rfname)], FUN=sum))
                             names(exposures$levels) <<- levelCodes;
                            
                           },
                           setRiskExposures = function(setInstruments=T, setRiskFactors=T) {
                             #' set report vectors for the Exposure calculation
                             #'  By levels, instruments, risk factors.
                             if (identical(length(positions),0L))
                               stop('fseal::portFolio::setExposures : positions object required ');
                             if (identical(length(levels),0L))
                               stop('fseal::portFolio::setExposures : reporting levels must be defined ');
                             posname <- "exposureInMMBTU";
                             rfname  <- "riskFactor";
                             insname <- "dealTypeName";
                             
                             riskExposures[["levels"]] <<-lapply(levelCodes, 
                                                             FUN=function(x) aggregate(positions[posname], 
                                                                                       by=positions[c(x,rfname)], FUN=sum))
                             names(riskExposures$levels) <<- levelCodes;
                             
                             if (setInstruments) riskExposures$riskFactor  <<- aggregate(positions[posname], 
                                                                                     by=positions[rfname], FUN=sum);
                             if (setRiskFactors) riskExposures$instruments <<- aggregate(positions[posname], 
                                                                                     by=positions[c(insname, rfname)], FUN=sum);
                           },
                           setSQL = function(dtwindow) {
                             #' set levels for the VaR calculation
                             #' 
                             if (missing(dtwindow))
                               stop('fseal::portFolio::setSQL : date window is required ');
                             
                             sql <<- paste(" SELECT * FROM bsvarexposuresdatadump WHERE asOfDate = ", 
                                           format(dtwindow$startdt, "'%Y-%m-%d'"), " AND exposureStatus = 'Open' ", 
                                           " ORDER BY asOfDate desc, dealID, flowStartDate, flowEndDate,", 
                                           " exposureHub, exposureRiskLabel, pricingStartDate, pricingEndDate");
                           },
                           buildReportLevels = function (din=0, books) {
                             #' BUILDREPORTLEVELS get the reporting levels
                             #'   
                             
                             lowlevel  <- link()$getDBmart('riskDB', 'Portfolios');
                             
                             out       <- lowlevel[lowlevel$LevelID<=din,];
                             
                             baselevel <- lowlevel[lowlevel$LevelID==max(lowlevel$LevelID),];
                             baselevel <- baselevel[baselevel$Description%in%books, ];
                             lowlevel  <- rbind(out,baselevel);
                             
                             out$Books <- NULL;
                             
                             #load the reporting levels link lists
                             for (i in 1:dim(out)[1]) {
                               
                               sortlist <- lowlevel[lowlevel$SeriesID!=out$SeriesID[i] & 
                                                      lowlevel$LevelID>out$LevelID[i],];
                               out$Books[i] <- list(levelrecur(sortlist, out$SeriesID[i]));
                               
                             }
                             
                             return(out);
                             
                           },
                           levelrerecur = function(rID, din, levels) {
                             
                             out <-  vector(length=nrow(din), mode="character");
                             
                             for (i in 1:nrow(din)) {

                               if (levels$LevelID[levels$SeriesID == din$Parent[i]] != rID) {
                                 
                                 idx <- levels[levels$LevelID <= levels$LevelID[levels$SeriesID == din$Parent[i]], ];
                                 t1  <- levelrerecur(rID, levels[levels$SeriesID == din$Parent[i], ], idx);
                                 
                               } else {
                                 
                                 t1 = levels$FullName[levels$SeriesID == din$Parent[i]];
                                 
                               }
                               out[i] = t1;
                             }
                             
                             return(out);
                             
                           },
                           levelrecur = function(dlist, ID) {
                             idx <- dlist$Parent%in%ID;
                             loc <- which(idx);
                             out <- NULL;
                             
                             if (any(idx)) { 
                               for (j in 1:length(loc)) {
                                 out = c(out, levelrecur(dlist, dlist$SeriesID[loc[j]]))
                               }  
                             } else { 
                               
                               out = ID;
                               
                             }
                             return(out)
                           },
                           dbGet = function(host=parms$host, port=parms$port, 
                                            schema=parms$schema, user=dbcred$items$User, 
                                            pwd=dbcred$base64decode(dbcred$items$Password), 
                                            drvClass=parms$driverClass, dbInfoFlag=T) {
                             if (is.na(sql))
                               stop('fseal::portFolio::dbGet :  query string required');
                             if (missing(host) | missing(port) | missing(schema) | missing(user) |
                                   missing(pwd) | missing(drvClass))
                               stop('fseal::portFolio::dbGet : all parameters are required');
                             
                             out <- JDBC.Pull(sql, host, port, schema, user, pwd, drvClass, dbInfoFlag);
                             
                             if (!exists('out')) stop('Exposures did not download');
                             
                             #set contractmonth, this is a hard coded column should be in metadata.
                             out$ContractMonth <- out[,'pricingMonth'];
                             
                             positions <<- out;
                           }
                         )
)
