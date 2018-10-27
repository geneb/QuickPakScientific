' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION BETA (Z, W)
DECLARE FUNCTION GAMMA (X)

FUNCTION BETA (Z, W) STATIC

    ' Beta function

    ' Input
   
    '  Z, W = function arguments

    ' Output
   
    '  BETA = function value

    ' NOTE: requires function GAMMA.BAS
    
    BETA = (GAMMA(Z) * GAMMA(W)) / GAMMA(Z + W)

END FUNCTION

