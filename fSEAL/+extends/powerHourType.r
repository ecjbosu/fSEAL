#===============================================================================
# Name   : powerHourType
# Author : Joe W. Byers
# Date   : 1/25/2014
# Version: 1.0001
# Aim    : return on / off peak flag for hour
# Mail   : <<<ecjbosu@aol.com>>>
#===============================================================================
# 
# pjm peak <<<http://www.pjm.com/Glossary.aspx>>>
# ercot peak <<<http://www.ercot.com/glossary/o>>>
# naesb <<<www.naesb.org/pdf/r04013_attach4.doc>>>
# powerHourType <- function(in, region=c('Eastern', 'ERCOT', 'Western'), endHourFlag = T)
#   #
#   if(!c('package:lubridate')%in%search()) library(lubridate);
# 
#   hourFlagAdj <- 0; #0 for endHourFlag True, -1 for False
#   if (endHourFlag == F) hourFlagAdj <- -1;
# 
#   region <- tolower(match.arg(region));
#   ret <- 0;
#   if (region == 'eastern')