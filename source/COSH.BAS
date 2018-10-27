' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION COSH (X)

FUNCTION COSH (X) STATIC

    ' Hyperbolic cosine function

    COSH = .5# * (EXP(X) + EXP(-X))

END FUNCTION

