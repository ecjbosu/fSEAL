#' ==============================================================================
#' Name   : fseal VaR Wrapper class
#' Author : Joe W. Byers
#' Date   : 08/13/2014
#' Version: 1.0001
#' Aim    : VaR wrapper class to calculate MVaR and Component VaR
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================
#' curve   curve class
VaRCalc <- setRefClass("VaRCalc"
                     ###
                     #' VaR calculator class.
                     #' Fields:
                     #'   
                     #' Example:  
                     ###
                     ,fields = list( 
                       markDate   = "ANY",
                       alpha      = "numeric",
                       axis       = "data.frame",
                       prices     = "ANY",
                       current    = "ANY",
                       returns    = "ANY",
                       COV        = "ANY",
                       levelCodes = "list",
                       weights    = "list",
                       results    = "list"
                     )
                     ,contains = list("fseal")
                     ,methods = list(
                       initialize = function(markDate   = Sys.Date(), alpha=.95) {
                         # initialize the object
                         initFields(current    = list(),
                                    prices     = list(),
                                    returns     = list(),
                                    COV        = list(),
                                    results    = list(),
                                    axis       = data.frame());
                         markDate   <<- markDate; 
                         alpha      <<- alpha;
                         callSuper();
                       },
                       setPrices = function(obj) {
                         #' set Prices for the VaR calculation
                         #' 
                         if (missing(obj))
                           stop('fseal::VaRCalc::setPrices : curve object required to set prices');
                         
                         prices        <<- do.call(cbind,obj$priceObj);
                         names(prices) <<- unlist(lapply(obj$priceObj, 
                                                 FUN=function(x) xtsAttributes(x)$dimSpecs[,"commPrompt"]));
                         
                         current       <<- prices[which(max(index(prices))==index(prices)),];
                         prices        <<- prices[which(max(index(prices))!=index(prices)),];
                         
                         xtsAttributes(current) <<- NULL;
                         xtsAttributes(prices)  <<- NULL;
                       },
                       setAxis = function(obj) {
                         #' set Axis for the VaR calculation
                         #' 
                         if (missing(obj))
                           stop('fseal::VaRCalc::setAxis : curve object required to set axes');
                         
                         axis <<- data.frame(
                           commPrompt=unlist(lapply(obj$priceObj, 
                                           FUN=function(x) xtsAttributes(x)$dimSpecs[,"commPrompt"])),
                           commodity=gsub('\\.[0-9]*', '', 
                                          unlist(lapply(obj$priceObj, 
                                           FUN=function(x) xtsAttributes(x)$dimSpecs[,"commPrompt"]))),
                           contractMonth=as.Date(unlist(lapply(obj$priceObj, 
                                           FUN=function(x) as.character(xtsAttributes(x)$dimSpecs[,"contractMonth"])))),
                           promptMonth=unlist(lapply(obj$priceObj, 
                                           FUN=function(x) xtsAttributes(x)$dimSpecs[,"promptMonth"]))
                           );
                       },
                       setLevelCodes = function(obj) {
                         #' set levels for the VaR calculation
                         #' 
                         if (missing(obj))
                           stop('fseal::VaRCalc::setLevelCodes : reporting levels object required ');
                         
                         levelCodes <<- unique(obj$LevelCode);
                       },
                       setReturns = function() {
                         #' set returns for the VaR calculation
                         #' 
                         if (is.null(prices))
                           stop('fseal::VaRCalc::setReturns : price object required ');
                         
                         returns <<- na.omit(Return.calculate(prices, method='compound')); 
                         #na.omit(diff(log(varObj$prices)));
                       },
                       setCOV = function() {
                         #' set covariance for the VaR calculation
                         #' 
                         if (is.null(returns))
                           stop('fseal::VaRCalc::setCOV : returns object required ');
                         
                         COV <<- stats::cov(returns,use='pairwise.complete.obs');
                       },                       
                       setWeights = function(obj, setInstruments=T, setRiskFactors=T) {
                         #' set weights vectors for the VaR calculation
                         #'  By levels, instruments, risk factors.
                         if (missing(obj))
                           stop('fseal::VaRCalc::setWeights : positions object required ');
                         if (is.null(levels))
                           stop('fseal::VaRCalc::setWeights : reporting levels must be defined ');
                         posname <- "exposureInMMBTU";
                         rfname  <- "riskFactor";
                         insname <- "dealTypeName";
                         
                         weights[["levels"]] <<-lapply(levelCodes, 
                                                  FUN=function(x) aggregate(obj$positions[posname], 
                                                                 by=obj$positions[c(x,rfname)], FUN=sum))
                         names(weights$levels) <<- levelCodes;
                         
                         if (setInstruments) weights$riskFactor  <<- aggregate(obj$positions[posname], 
                                                                by=obj$positions[rfname], FUN=sum);
                         if (setRiskFactors) weights$instruments <<- aggregate(obj$positions[posname], 
                                                                 by=obj$positions[c(insname, rfname)], FUN=sum);
                       },
                       calculate = function(obj, muFlag=T, varTypes=c('MVaR','Component','Marginal')) {
                         
                         if (missing(obj) | is.null(obj))
                           stop('fseal::VaRCalc::calculate : positions object required ');
                         
                         varTypes <- match.arg(varTypes, c('MVaR','Component','Marginal'), several.ok=T);
                         
                         results$levels <<- list();
                         vdim <- sum(unlist(lapply(lapply(obj$riskExposures$levels, 
                                                          FUN= function(x) unique(x[,names(x)[1]])), length)));
                         results$MVaR <<- data.frame(Level= rep(NA_character_,vdim), 
                                                           Description= rep(NA_character_,vdim), Value= rep(NA_real_,vdim));
                         results$ComponentVaR <<- list();
                         cvardf <- data.frame(RiskFactor=NULL, ComponentVaR=NULL, Component_PCT=NULL);
                         marvardf <- data.frame(RiskFactor=NULL, MarginalVaR=NULL, Marginal_PCT=NULL);
                         
                         k <- 1;
                         
                         #do lookups on commDB when expanding to other commodities and to riskfactor names..
                         rftemp         <- paste(axis$commodity, axis$contractMonth, sep=".");
                         for (i in 1:length(obj$levelCodes)) {
                           cat("Risk metrics for : ", obj$levelCodes[i], "\n");
                           lcats <- unique(obj$riskExposures$levels[[i]][,obj$levelCodes[i]]);
                           results$levels[[i]] <<- list();
                           
                           results$ComponentVaR[[i]] <<- cvardf;
                           results$MarginalVaR[[i]]  <<- marvardf;
                           
                           for (j in 1:length(lcats)) {
                             cat("Risk metrics for : ", lcats[j], "\n");
                             #build weights vector, must be zero even if no risk factor exposure in portfolio.  The alternative is to collapse
                             #varObj parameters to the held risk factors.
                             wts        <- matrix(data=0, nrow=length(axis$commPrompt));
                             if (muFlag) mu <- matrix(data=0, nrow=length(axis$commPrompt));
                             idx            <- unlist(
                               lapply(obj$riskExposures$levels[[i]]$riskFactor[obj$riskExposures$levels[[i]][,obj$levelCodes[i]]==
                                      lcats[j]], FUN=function(x) which(x==rftemp)));
                             wts[idx]   <- obj$riskExposures$levels[[i]]$exposureInMMBTU[obj$riskExposures$levels[[i]][,obj$levelCodes[i]]==lcats[j]];
                             results$levels[[i]]$weights[[j]] <<- matrix(data=NA_real_, nrow=length(axis$commPrompt));; 
                             
                             results$levels[[i]]$weights[[j]][idx] <<- wts[idx]; #set to NA if no exposure exists
                             #final weightings is price times risk factor exposure.
                             #TODO: add UOM to calculation
                             wts <- wts * t(current);
                             t1  <- VaR(NULL, p=alpha, method="gaussian", portfolio_method="component",
                                     mu=mu, sigma=COV, weights=wts);
                             
                             results$MVaR[k,] <<- data.frame(Level=lcats[j], Description="MVaR", Value=t1$VaR);
                             #ComponentVaR, note using weights index to eliminated NA's that are zero for VaR calc.
                             if(any(varTypes%in%c('Component'))) results$ComponentVaR[[i]] <<- rbind(results$ComponentVaR[[i]], 
                                                                       data.frame(RiskFactor=rftemp[idx], Level=lcats[j],
                                                                                  ComponentVaR=t1$contribution[idx], 
                                                                                  Component_PCT=t1$pct_contrib_VaR[idx]));    
                             #     t1             <- VaR(varObj$returns, p=.95, method="gaussian", 
                             #                           portfolio_method="single",
                             #                           mu=mu, sigma=varObj$COV, m3=mu, m4=mu,
                             #                           weights=weights/as.numeric(varObj$current)/sum(weights/as.numeric(varObj$current)));
                             #     varObj$results$MarginalVaR[[i]] <- rbind(varObj$results$MarginalVaR[[i]], 
                             #                                               data.frame(RiskFactor=rftemp[idx],
                             #                                                          MarginalVaR=t1$contribution[idx], 
                             #                                                          MarginalVaR_PCT=t1$pct_contrib_VaR[idx]));    
                             k <- k + 1;
                           }
                         }
                         
                       },
                       upload = function(obj, creds, includeCompVaR=F, rerun=F) {
                         #' set levels for the VaR calculation
                         #' 
                         if (missing(obj) | missing(creds))
                           stop('fseal::VaRCalc::upload : parameters are required object required ');
                         
                         res <- cbind(MarkDate=markDate, results$MVaR, RiskFactor=NA_character_);
                         if (includeCompVaR) {
                           t2 <- cbind(MarkDate=markDate, Description="ComponentVaR", 
                                       do.call(rbind,results$ComponentVaR)[,-4]);
                           names(t2)[5] <- "Value";
                           t3 <- cbind(MarkDate=markDate, Description="Component_PCT", 
                                       do.call(rbind,results$ComponentVaR)[,-3]);
                           names(t3)[5] <- "Value";
                           
                           res <- rbind(res,t2,t3);
                         }
                         dss <- unique(res$Description);
                         res<-JDBC.Push(data=res, dbTable=obj$dbTable, host=obj$host, port=obj$port, 
                                        schema=obj$schema, user=creds$items$User, 
                                        pswd=creds$base64decode(creds$items$Password), 
                                        driverClass=obj$driverClass, rerun = rerun, 
                         rerunFilter=paste(" where MarkDate =", format(markDate, "'%Y-%m-%d'"), 
                                            " AND Description IN ('",  paste(dss, collapse="','"), "') ",
                                            sep = ''));
#                                        rerunFilter=paste(" where MarkDate =", format(markDate, "'%Y-%m-%d'"),
#                                                          " ", sep = ''));
                         return(res);
                       }
                       
                     )
)
                         