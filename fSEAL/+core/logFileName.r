#'==============================================================================
#' Name   : logFileName
#' Author : Joe W. Byers
#' Date   : 10/07/2014
#' Version: 1.0001
#' Aim    : generate a logFileName from the script name and time stamp
#' Mail   : <<<ecjbosu@aol.com>>>
#'==============================================================================
#' Used in conjuction with sink, log4r, and other packages.
#'
#' reference:
#' <<<http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script>>>
#' Example:
#' logname <- logFileName();
#' cat('Log file name is ', logname, '\n');

logFileName <- function(initial.options=commandArgs(trailingOnly = FALSE, asValue=F), TimeStamp=T) {
#create a log file name from the script name and timestamp if required

# script designamte with the --file= argument
  file.arg.name <- "--file=";
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  if (identical(script.name, character(0))) {
  # script designamte with the -f argument
    file.arg.name <- "-f";
    script.name <- initial.options[grep(file.arg.name, initial.options)+1];
  }

  script.basename <- sub("\\.r", "\\.", tolower(basename(script.name)));
  out <- paste(script.basename,
    format(Sys.time(),'%Y-%m-%d.%H.%M.%S'), '.log', sep="");

  cat('log file name= ', paste(out), '\n');
  return(out);
}

