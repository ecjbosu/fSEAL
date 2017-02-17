#ICE EOD Upload
#Created by: J Byers
#Modified by: J Byers
#Date: 03/05/20162
#
#comments:
# convert raw nymex file to futrack upload format
#Header


# Variable Definitions/User Inputs
icePush <- function(cutdate, dbfile, con, csvloc, ForO = T, rerun=F) {             


# get the files listing
  rfile<-list.files(csvloc);
# filter out the non text delimited files not dat files
  rfile <- rfile[substring(rfile, nchar(rfile)-2, nchar(rfile))%in%c('dat')];
  
  #set the instrument type
  
  instrument = 'option'
  
  idx <- grepl(instrument, rfile);
  
  if (ForO) {rfile <- rfile[!idx];} else 
    { rfile <- rfile[idx];};
  
# read the title and parse the date
  idx1 <- as.numeric(regexpr('[:.:]', rfile));
  file.date<-as.Date(sort(substring(rfile,idx1-10,idx1-1)),'%Y_%m_%d');
  rdate <- as.Date(substring(rfile,idx1-10,idx1-1),'%Y_%m_%d');
  cat('File dates checked:', 
    paste(file.date=format(file.date), cutdate, sep=':'), sep='\n')
  
  file.date <- file.date[file.date == as.Date(cutdate)];
  rfile     <- rfile[file.date == rdate];
  len1       <- length(rfile);
   
  cat('File to load:', 
    paste(rfile, sep=':'), sep='\n')
  cat('File # to load:', 
    paste(len1, sep=':'), sep='\n')
  
  res <- 0;

  dts <- unique(file.date);
  len <- length(dts);
  if (len!=0 & len1!=0) {
    for (i in 1:len) {
      if (rerun)  {
        sqlstr = paste('delete from ', dbfile, " where Trade_Date = '",
          format(dts[i],'%Y-%m-%d'), "'", sep='');
        res <- dbSendUpdate(con, sqlstr);
      }
    }
    
    #reset to number of files since multiple files
    len <- length(rfile);
    
    if(res) res <- 0;
    
    for (i in 1:len) {
                                                                 
      # read over the files
  
      cat('Files for dates uploaded:', 
        paste(file.date=format(file.date[i]), cutdate, rfile[i], sep=':'), sep='\n')
  
      Tmps<-read.delim(paste(csvloc,"/",rfile[i],sep=''),sep='|',header=T,stringsAsFactors=F);
      
      dformat <- '%m/%d/%Y';   
      if (nchar(strsplit(Tmps$TRADE.DATE,'/')[[1]][3])!=4) {
        dformat <- '%m/%d/%y' };
      Tmps$TRADE.DATE      <-as.Date(Tmps$TRADE.DATE,dformat);
      if (nchar(strsplit(Tmps$STRIP,'/')[[1]][3])!=4) {
        dformat <- '%m/%d/%y' };
      Tmps$STRIP           <-as.Date(Tmps$STRIP,dformat);
      
      test <- any(names(Tmps)%in%'EXPRIRATION.DATE')
      if (test) { names(Tmps)[names(Tmps)%in%'EXPRIRATION.DATE'] <- 'EXPIRATION.DATE'}
      
       if (nchar(strsplit(as.character(Tmps$EXPIRATION.DATE),'/')[[1]][3])!=4) {
        dformat <- '%m/%d/%y' };
      Tmps$EXPIRATION.DATE <-as.Date(Tmps$EXPIRATION.DATE,dformat);
      
      if ( is.logical(Tmps$CONTRACT.TYPE )) {
        Tmps$CONTRACT.TYPE   <-as.character(substr(Tmps$CONTRACT.TYPE,1,1))};
      
      #filter out days not the same as the file date.  Primarily to handle 
      #weekends and holidays not handled in the job autoexecution
      idx <- Tmps$TRADE.DATE == file.date[i];
      Tmps <- Tmps[idx,];
      
      # fix missing product_id
      test <- any(names(Tmps)%in%'PRODUCT.ID')|any(names(Tmps)%in%'PRODUCT_ID');
      
      if (test==F){
        cat('Fixing PRODUCT_ID for ', paste(cutdate, sep=':'), '\n');
        Tmps <- data.frame(Tmps,PRODUCT.ID=NA);
      }
      
      if (ForO==F) {
        # fix missing option vol and delta
        test <- any(names(Tmps)%in%'OPTION.VOLATILITY')|any(names(Tmps)%in%'OPTION_VOLATILITY');
        
        if (test==F){
          cat('Fixing OPTION.VOLATILITY for ', paste(cutdate, sep=':'), '\n');
          Tmps <- data.frame(Tmps,OPTION.VOLATILITY=NA);
        }
        
        test <- any(names(Tmps)%in%'DELTA.FACTOR')|any(names(Tmps)%in%'DELTA_FACTOR');
        
        if (test==F){
          cat('Fixing DELTA.FACTOR for ', paste(cutdate, sep=':'), '\n');
          Tmps <- data.frame(Tmps,DELTA.FACTOR=NA);
        }
      }      
       
      #drop futures in options file
      if (!ForO) {idx <- !Tmps$CONTRACT.TYPE%in%c('F','D','M')
                Tmps <- Tmps[idx,]};
      
      #reset column names with dash instead of dot for db table columns
      names(Tmps) <- gsub('\\.','_',names(Tmps));
         
      td <- dim(Tmps)[1];
      if (td > 0) {
        
        overwrite <- F;
        
        print(Tmps[1:10,]);
        
        res<-dbWriteTable(con, dbfile, Tmps, overwrite=overwrite, append=T, row.names=F);
  
        if (res) res<-0;
      }
    } 
  }
  return(res);
}    
# end of script
      
