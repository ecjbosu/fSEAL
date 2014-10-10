#==============================================================================
# Name   : .GBSStrike
# Author : Joe W. Byers
# Date   : 12/02/2013
# Version: 1.0001
# Aim    : .GBSStrike extension of fOPtions
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#calculate the strike delta of an  option


.GBSStrike <- function(TypeFlag, S, X, Time, r, b, sigma) {
  library(fOptions)
    result = .ProbITM(TypeFlag, S, X, Time, r, b, sigma)
    if (TypeFlag == "c")
        result =  -exp((b - r) * Time) * result
    if (TypeFlag == "p")
        result =  exp((b - r) * Time) * result
    result
}
