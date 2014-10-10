r#' ==============================================================================
#' Name   : fseal class
#' Author : Joe W. Byers
#' Date   : 10/09/2014
#' Version: 1.0001
#' Aim    : fseal core class
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================
#' GIST   The parent class of all classes

fseal <- setRefClass("fseal"
###
#' fseal is the a core class.
#' Fields:
#'   Tag is a short description for objects
#'   Label is a long description for objects
#'   Timestamp is the datetime stamp when the object was created
###
  ,fields = list( 
       Tag       = "character",
       Label     = "character",
       Timestamp =  "POSIXt")
 #,contains = list("list")
  ,methods = list(
   initialize = function() {
           initFields(Tag       = NA_character_,
                      Label     = NA_character_,
                      Timestamp = Sys.time())
          },
   initializeOutput = function(copyFlag=F){
    #' initializeOutput initializes a gist object as a reference
    #' or a copy based on copyflag.  Default is reference.
    #' Example: out <- initializeOutput(obj, F);
     if (length(copyFlag)>1)                            
       stop('fseal : Invalid copyFlag must be a scalar.');

     copyFlag <- as.logical(copyFlag);
     
     out <- .self;
     if (copyFlag) out <- .self$copy();
     return(out)
          
   })  
  #     ,package = 'fseal'
  );

fsealHelpers <- setRefClass("fsealHelpers"
                    ###
                    #' fsealHelpers contain miscellaneous helper functions class.
                    #' Fields:
                    #'   
                    #' Example:  
                    #'   fsealHelpers$dateWindow(markDate, window, bizDayFlag)
                    ###
                    ,contains = "fseal"
                    ,methods = list(
                      initialize = function() {
                        # initialize the object
                      },
                      dateWindow = function(markDate=as.Date(Sys.Date()), windows=10, bizDayFlag=T) {
                        # find first and last dates in a Date window.  
                        # bizDayFlag adjusts for buziness days.
                        # require: lubridate and timeDate.
                        tryCatch( {
                          dts <- timeDate::timeSequence(markDate-(windows+45), markDate);
                          idx <- timeDate::isBizday(dts, holidays=timeDate::holidayNYSE(lubridate::year(markDate)));
                          dts <- dts[idx];
                          dts <- tail(dts, n=windows+1);
                          return(data.frame(startdt=markDate, enddt=as.Date(head(dts, n=1)@Data)));
                        },
                        error = function(cond) 
                        {
                          print(cond);
                          stop('fseal::fsealHelpers::dateWindow : error in setting start and end date'); 
                        }
                        )
                      }
                    )
)
