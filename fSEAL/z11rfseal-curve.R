#' ==============================================================================
#' Name   : fseal curve class
#' Author : Joe W. Byers
#' Date   : 08/13/2014
#' Version: 1.0001
#' Aim    : fseal curve class
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================
#' curve   curve class
curve <- setRefClass("curve"
                         ###
                         #' curve class for handling curve objects.
                         #' Fields:
                         #'   
                         #' Example:  
                         #'   portFolio$setSettled(tradetypeList, settleDates, markDate)
                         #'   returns logical vector of active trades in portfolio
                         ###
                         ,fields = list( 
                           markDate   = "ANY",
                           maxTenor   = "numeric",
                           windowHist = "numeric",
                           data       = "data.frame",
                           tenorMap   = "data.frame",
                           dataXTS    = "ANY",
                           priceObj   = "ANY",
                           baseObj    = "ANY",
                           basisObj   = "ANY",
                           bomObj     = "ANY",
                           physIndObj = "ANY",
                           sql        = "character"
                         )
                         ,contains = list("fseal")
                         ,methods  = list(
                           initialize = function(markDate   = Sys.Date(), 
                                                 maxTenor   = NA_integer_,
                                                 windowHist = NA_integer_,
                                                 data       = data.frame()) {
                             # initialize the object
                              initFields(dataXTS    = list(),
                                         priceObj   = list(),
                                         baseObj    = list(),
                                         basisObj   = list(),
                                         bomObj     = list(),
                                         physIndObj = list(),
                                         sql        = NA_character_);
                             markDate   <<- markDate; 
                             maxTenor   <<- maxTenor;
                             windowHist <<- windowHist;
                             data       <<- data;
                             callSuper();
                           },
                           order = function(colorder) {
                             #' order the curve object
                             #'    
                            if (missing(colorder))
                               stop('fseal::curves::order :  order vector of column names or positions required');
                             
                            if (identical(class(colorder),"numeric"))
                             switch(class(colorder),
                                    numeric={
                                      data<<-data[do.call(order,(data[colorder])),];
                                    },
                                    character={   
                                      #dd[do.call(order, dd[as.character(lk[, 1])]), ]
                                      data<<-data[do.call(order,(data[names(data)[names(data)%in%colorder]])),];
                                    },
                                    stop('fseal::curves::order :  order vector invalide')
                                    )
                             return(1);
                           },
                           setPrompt = function(startdt, contractcolumn) {
                             #' setPrompt the curve object
                             #'    
                             #set the prompt using monthly sequence trick from markDate to ContractDate
                             if (missing(startdt))
                               stop('fseal::curves::setPrompt :  start date required');
                             if (missing(contractcolumn))
                               stop('fseal::curves::setPrompt :  column field of contract tenors is required');
                             
                             data$Prompt <<- mapply(startdt, y=data[,contractcolumn], 
                                                 FUN = function(x,y) length(seq(x,y, by="months"))); 
                             return(1);
                           },  
                           setInstrument = function(contractcolumn) {
                             #' setPrompt the curve object
                             #'    
                             if (missing(contractcolumn))
                               stop('fseal::curves::setIntrument :  column field of contract types is required');
                             if (dim(data)[1]==0)
                               stop('fseal::curves::setPrompt :  data must be populated');
                             if (class(data$Prompt)=="NULL")
                               stop('fseal::curves::trimToTenor :  Prompts must be set');

                             data$Instrument <<- paste(data[,contractcolumn],sprintf('%02d',data$Prompt), sep='.'); 
                             
                             return(1);
                           },  
                           setBaseObj = function(commDB, curveDB) {
                             #' setPrompt the curve object
                             #'    
                             if (missing(commDB) | missing(curveDB))
                               stop('fseal::curves::setBaseObj :  all parameters are required');
                             
                             idx      <- commDB$BaseSeriesID==curveDB$SeriesID;
                             baseObj  <<- dataXTS[[curveDB$CodeName[idx]]];
                             
                             return(1);
                           },
                           setBasisObj = function(curveDB) {
                             #' setBasisObj the curve object
                             #'    
                             if (missing(curveDB))
                               stop('fseal::curves::setBasisObj :  all parameters are required');
                             
                             idx      <- curveDB$CURVE_TYPE=='BASIS';
                             basisObj <<- dataXTS[which(!is.na(match(names(dataXTS),curveDB$CodeName[idx])))];
                             
                             return(1);
                           },   
                           setPriceObj = function(basisFlag=T) {
                             #' setPriceObj the curve object
                             #'    
                             
                             if(basisFlag) {
                               priceObj <<- lapply(basisObj, FUN = function(x) x + baseObj[,1:min(dim(baseObj)[2],dim(x)[2])]);
                             } else {
                               priceObj <<- baseObj;
                             }
                             #set a parameter attributes for object
                             if (identical(class(priceObj),'list')) {
                               t1 <- lapply(priceObj, FUN=function(x) 
                               {dimSpecs <- data.frame(promptMonth=as.numeric(names(x)),
                                                       contractMonth=tenorMap[as.numeric(names(x)),1]);
                               })
                               
                               commP <- data.frame(mapply(x=priceObj,y=names(priceObj), 
                                                          FUN=function(x,y) 
                                                            paste(y, sprintf('%02d', as.numeric(names(x))), sep=".")),
                                                   check.names=F);
                               
                               priceObj <<-lapply(names(priceObj),
                                                  FUN=function(x) 
                                                    {xtsAttributes(priceObj[[x]])<<-
                                                         list(dimSpecs=cbind(t1[[x]], commPrompt=commP[,x]));
                                                         return(priceObj[[x]])});
                               
                               names(priceObj) <<- names(t1);  #lost the list names in the last lapply
                             } else {
                               t1 <- data.frame(promptMonth=as.numeric(names(priceObj)),
                                                       contractMonth=tenorMap[as.numeric(names(priceObj)),1]);
                               
                               commP <- data.frame(commPrompt=paste(unique(data$CONTRACT), 
                                                         sprintf('%02d', as.numeric(names(priceObj))), sep="."),
                                                   check.names=F);
                               
                               xtsAttributes(priceObj) <<- list(dimSpecs=cbind(t1, commPrompt=commP));
                               
                               #names(priceObj) <<- names(t1);  #lost the list names in the last lapply
                               
                             }
                             return(1);
                           },
                           trimToTenor = function() {
                             #' setPrompt the curve object
                             #'    
                             if (is.na(maxTenor))
                               stop('fseal::curves::trimToTenort :  maxTenor is required');
                             if (class(data$Prompt)=="NULL")
                               stop('fseal::curves::trimToTenort :  Prompts must be set');
                             
                             data <<- data[data$Prompt<=maxTenor,]; 
                             
                             return(1);
                           },       
                           setTenorMap = function() {
                             #' getTenorMap of  the curve object
                             #'    
                              
                             tenorMap <<- data.frame(ContractDate=unique(data$ContractDate),
                                               Prompt=unique(data$Prompt));
                             return(1);
                           },  
                           dbGet = function(host=parms$host, port=parms$port, 
                                            schema=parms$schema, user=dbcred$items$User, 
                                            pwd=dbcred$base64decode(dbcred$items$Password), 
                                            drvClass=parms$driverClass, dbInfoFlag=T) {
                             if (is.na(sql))
                               stop('fseal::curves::dbGet :  query string required');
                             if (missing(host) | missing(port) | missing(schema) | missing(user) |
                                   missing(pwd) | missing(drvClass))
                               stop('fseal::curves::dbGet : all parameters are required');
                             
                             out <- JDBC.Pull(sql, host, port, schema, user, pwd, drvClass, dbInfoFlag);
                             
                             if (!exists('out')) stop('Curves did not download');
                             
                             data <<- out;
                             return(1);
                             
                            },  
                            setXTSPrices = function(valueVar='Settle', dateVar='TradeDate', curveID='CONTRACT', 
                                                    tenorID='Prompt', na.flag=F) {
                              #' setXTSPrices for the curve object
                              #'    
                              #' convert data property from data.frame to xts object
                              if (missing(valueVar) | missing(dateVar) | missing(curveID) | missing(tenorID)) 
                                stop('fseal::curves::setXTSPrices :  all parameters required');
                              
                              crvs <- unique(data[,curveID]);
                              
                              dataXTS <<- lapply(crvs
                                              , FUN=function(x) reshape(data[data[, curveID]==x,c(dateVar, tenorID, valueVar)], 
                                                                        idvar=dateVar, timevar=tenorID, direction="wide"));
                              dataXTS <<- lapply(dataXTS, FUN=function(x) xts(x[,names(x)!=dateVar], order.by=x[,dateVar]));
                              
                              #reset names make sure sorted correctly
                              names(dataXTS) <<- crvs;
                              dataXTS        <<- lapply(dataXTS,FUN=function(x) {
                                names(x)<-sub(paste(valueVar,"",sep="."),"",names(x)); return(x)}
                                );
                              #dataXTS <<- out;
                              if (na.flag) na.fix(down=T, fromlast=T);
                              
                              return(1);
                            },    
                              na.fix = function(down=T, fromlast=T) {
                              #' na.fix for the curve object
                              #'    
                              #' see xts for replacing nan's.
                              out <- dataXTS;
                              if(down)     dataXTS <<- lapply(dataXTS, na.locf);
                              if(fromlast) dataXTS <<- lapply(dataXTS, 
                                                     FUN= function(x) na.locf(x, na.rm=F, fromLast = T));
                              
                              #dataXTS <<- out;
                              return(1);
                            },
                            setSQL = function(tickers, comm, dtwindow, lenfun=" LEN") {
                              #' set levels for the VaR calculation
                              #' 
                              if (missing(tickers) | missing(comm) | missing(dtwindow))
                                stop('fseal::VaRCalc::setSQL : a ticker, commodity, and date window are required ');
                              
                              sql <<- paste(" SELECT TradeDate,ContractDate, ", 
                                            " CodeName AS CONTRACT, Settle ", 
                                            " FROM CommodityPrices.Futures ", 
                                             " WHERE forDate BETWEEN ", format(dtwindow$enddt, "'%Y-%m-%d'")," AND ",  
                                            format(dtwindow$startdt, "'%Y-%m-%d'"), "  ", 
					    " tickers t ", 
                                            " WHERE t.Ticker IN ( ", tickers," ) ", 
                                            " AND t.Commodity = ", comm, " ", 
                                            " AND f.CodeName IN ( t.ticker, t.Electronic, t.OpenOutcry) ",
                                            " ORDER BY TradeDate,CONTRACT,ContractDate "
                                            , sep="");
                            },
                            archive = function(path, name, type, datedFlag=T) {
                              #Archive curvces Object
                               tryCatch({
                                #add environment to spath
                                t1 <- lapply(priceObj,data.frame);
                                t1 <- lapply(names(priceObj), 
                                               FUN = function(x) { names(t1[[x]]) <- 
                                                                     xtsAttributes(priceObj[[x]])$dimSpec$contractMonth;
                                                                   return(t1[[x]])});
                                names(t1) <- names(priceObj);
                                
                                link()$archive(t1, markDate, path=path, name=name, type=type, datedFlag=datedFlag);
                                
                                remove(t1);
                                
                                return(T)
                               },
                               error = function(cond) {
                                 print(cond);
                                 stop('fseal::curve::archive : Archive failed'); 
                                }
                               )
                            }
                         )
)
