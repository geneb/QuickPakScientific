' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION SINH (X)

FUNCTION SINH (X) STATIC

    ' Hyperbolic sine function

    SINH = .5# * (EXP(X) - EXP(-X))

END FUNCTION

