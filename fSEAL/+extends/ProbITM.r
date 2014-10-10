#==============================================================================
# Name   : .ProbITM
# Author : Joe W. Byers
# Date   : 12/02/2013
# Version: 1.0001
# Aim    : .ProbITM extension of fOPtions
# Mail   : <<<ecjbosu@aol.com>>>
#==============================================================================
#calculate the probability of being ITM of an  option


.ProbITM <- function(TypeFlag, S, X, Time, r, b, sigma) {
    d1 = (log(S / X) + (b + sigma ^ 2 / 2) * Time) / (sigma * sqrt(Time))
    d2 = d1 - sigma * sqrt(Time)
    if (TypeFlag == "c")
        result =  CND(d2)
    if (TypeFlag == "p")
        result =  CND(-d2)
    result
}
