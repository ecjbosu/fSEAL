#FutrackUpload
#Created by: J Byers
#Modified by: J Byers
#Date: 8/26/2008 6/282012
#
#comments:
# convert raw nymex file to futrack upload format
#Header
#PRODUCT SYMBOL,CONTRACT MONTH,CONTRACT YEAR,CONTRACT DAY,PUT/CALL,STRIKE,
# CONTRACT,PRODUCT DESCRIPTION,OPEN,HIGH,HIGH AB INDICATOR,LOW,LOW AB INDICATOR,
# LAST,LAST AB INDICATOR,SETTLE,PT CHG,EST. VOL,PRIOR SETTLE,PRIOR VOL,
# PRIOR INT,TRADEDATE

# Variable Definitions/User Inputs
cmePush <- function(cutdate, dbfile, dbcols, con, csvloc, coltypes, rerun=F) {

#dbnames = c('PRODUCT.SYMBOL', 'CONTRACT.MONTH', 'CONTRACT.YEAR', 
#  'CONTRACT.DAY', 'CONTRACT', 'PRODUCT.DESCRIPTION',
#  'OPEN', 'HIGH', 'HIGH.AB.INDICATOR',  
#  'LOW', 'LOW.AB.INDICATOR', 'LAST',
#  'LAST.AB.INDICATOR', 'SETTLE', 'PT.CHG',   
#  'EST.VOL', 'PRIOR.SETTLE', 'PRIOR.VOL',
# 'PRIOR.INT', 'TRADEDATE');
# 
#dbcols = c('CONTRACT','Month', 'Year', 'Day', 'ContractQuoteName', 'ContractDescription',
#  'Open_', 'HIGH', 'HIGHABIND', 'LOW', 'LOWABIND', 'LAST', 'LASTABIND', 'Settle','PT_CHG',
#  'Volume','PriorSettle', 'PriorVol', 'PriorInt','TradeDate');#, 'Prompt', 'ContractDate');
#
#fix dbcols and names if needed

#if (identical(class(dbnames), 'list'))  dbnames <- unlist(dbnames);
if (identical(class(dbcols), 'list'))  dbcols <- unlist(dbcols);
if (identical(class(coltypes), 'list'))  coltypes <- unlist(coltypes);

# get the files listing
  rfile<-list.files(csvloc);
# filter out the non innf files
  rfile <- rfile[substring(rfile, nchar(rfile)-2, nchar(rfile))%in%c('csv')];

  #set the instrument type
  instrument <- substring(tolower(dbfile), 1, nchar(dbfile)-1);
  
  idx <- as.numeric(regexpr(instrument, rfile));
  rfile <- rfile[as.logical(idx>0)] ;
  
# read the title and parse the date
  idx1 <- as.numeric(regexpr('[:.:]', rfile));
  file.date<-as.Date(substring(rfile,idx1+1,idx1+10));
  cat('File dates checked:', 
    paste(file.date=format(file.date), cutdate, sep=':'), sep='\n')
  
  rfile <- rfile[file.date == as.Date(cutdate)];
  len <-length(rfile);
  file.date <- file.date[file.date == as.Date(cutdate)];
  
  res <- 0;
  dts <- unique(file.date);
  
  for (i in 1:length(dts)) {
    if (rerun)  {
      sqlstr = paste('delete from `', dbfile, "` where TradeDate = '", 
	format(dts[i],'%Y-%m-%d'), "'", sep='');
      res <- dbSendUpdate(con, sqlstr);
    } 
  }
  
  for (i in 1:len) {
     
    # get only the non weekends data files

    cat('Files for dates uploaded:', 
      paste(file.date=format(file.date[i]), cutdate, rfile[i], sep=':'), sep='\n')

    Tmps<-read.csv(paste(csvloc,"/",rfile[i],sep=''),header=T,stringsAsFactors=F);
       
    Tmps$TRADEDATE<-as.Date(Tmps$TRADEDATE,'%m/%d/%Y');

    #filter out days not the same as the file date.  Primarily to handle 
    #weekends and holidays not handled in the job autoexecution
    idx <- Tmps$TRADEDATE == file.date[i];
    Tmps <- Tmps[idx,];
           
    names(Tmps) <- dbcols;
    Tmps <- cbind(Tmps, ContractDate=
      as.Date(paste(Tmps$Year,Tmps$Month,ifelse(is.na(Tmps$Day),'1',Tmps$Day)),'%Y%m%d'));
    a=as.data.frame(cbind(A=rep(1,dim(Tmps)[1]),C=Tmps$CONTRACT));
    idx <- is.na(Tmps$Day);
    Tmps$Day[idx]=1;
      
    Tmps <- cbind(Tmps,Prompt=unsplit(c(lapply(split(a,a$C),function(x) cumsum(x$A))),a$C));
    
    if (identical(dbfile,'Futures')) { 
      Tmps <- cbind(Tmps[,1:5],Tmps[,7:14], Tmps[,16:22]);
    } else {
      if (identical(dbfile,'Options')) {
         Tmps<- cbind(Tmps[,1:3],Tmps[,5:7],Tmps[,9:16], Tmps[,18:22]);
         # again because of column name issue getting rid of Lasts
         Tmps<- cbind(Tmps[,1:11],Tmps[,14:19]);
      } else {
        cat('links:cmePush: Instrument file is not Futures or Options \n');
        res <- 1;
        return(res);
      }
    }  
       
    #make sure numbers are numbers
    idnx <- dbcols%in%names(Tmps)
    idx <- coltypes[idnx] == 'numeric';
    #Tmps[,idx] <- as.numeric(Tmps[,names(Tmps)[idx]]); 
    Tmps[,idx] <- sapply(Tmps[,names(Tmps)[idx]], as.numeric)        
    # as.numeric(Tmps[,names(Tmps)[idx][2]                           

    td <- dim(Tmps)[1];
    res <- 0;
    if (td > 0) {
      fields <- dbListFields(con,dbfile);
      reordered.names <- names(Tmps)[match(fields, names(Tmps))];
      Tmps <- Tmps[ ,reordered.names];
      
      overwrite=F;
            
      print(Tmps[1:10,])
      res<-dbWriteTable(con, dbfile, Tmps, overwrite=overwrite, append=T, row.names=F);
    }
  } 
  return(res);
}    
# end of script
      
