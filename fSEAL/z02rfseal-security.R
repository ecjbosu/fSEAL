#' ==============================================================================
#' Name   : rsecurity class
#' Author : Joe W. Byers
#' Date   : 10/06/2014
#' Version: 1.0001
#' Aim    : facility handling of database security.
#' Mail   : <<<ecjbosu@aol.com>>>
#' ==============================================================================

rsecurity <- setRefClass("rsecurity"
                     ###
                     #' rsecurity security class.
                     #' Fields:
                     #'   filename is a full file path and name
                     #'   tree is the xml doc tree
                     #'   listitems is the xml parsed structure
                     #' example
                     #' x <- rsecurity$new()  
                     #' x$setItems()
                     #' str <- 'FSEAL'
                     #' rec <- x$getDB(str)
                     #' x$base64decode(rec$Password)
                     ###
                     ,fields = list( 
                       filename  = "ANY",
                       tree      = "ANY",
                       items =  "data.frame")
                     #,contains = ""
                     ,methods = list(
                       initialize = function(filename) {
                       # initialize the object
                         initFields(filename  = NA_character_,
                                    tree      = NA_character_,
                                    items     = data.frame())
                         
                         if (!missing(filename)) {
                           filename <<- filename;
                         } else {
                           filename <<- file.path(rootDBPath, workEnv, 
                                                  dbSubPath,"xml",'database.xml');
                         }
                                                  
                       },
                       setItems = function() {
                       # read/set the xml from the .self$filename
                         if (nchar(.self$filename)==0) 
                           stop('fseal::rsecturity : a xml file must be specified');
                         tryCatch( {
                           items <<- XML::xmlToDataFrame(.self$filename);},
                           error = function(cond) 
                            {stop('fseal::rsecturity:setItems : a xml file must exist on the specifited path'); 
                            }
                         )
                       },
                       getDB = function(str) {
                       # get the require items from list   
                         if (missing(str))
                           stop('fseal::rsecturity:getDB : db specifier is required');
                         if (length(str)>1)
                           stop('fseal::rsecturity:getDB : only one db specifier is required');
                         out = .self$items[.self$items$Name==str,];
                         return(out);
                       },
                       base64encode = function (str) {
                       #default sun encrytion
                         encoder <- .jnew('sun.misc.BASE64Encoder');
                         out<- encoder$encode(.jarray(.jnew('java.lang.String', str)$getBytes()));
                         return(out);
                       },
                       base64decode = function (str) {
                       #default sun decrytion
                         decoder <- .jnew('sun.misc.BASE64Decoder');
                         out <- rawToChar(decoder$decodeBuffer(.jnew('java.lang.String', str)));
                         return(out);
                       }
                     )   
                     #     ,package = 'rsecurity'
);

