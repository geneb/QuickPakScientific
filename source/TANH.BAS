' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION TANH (X)

FUNCTION TANH (X) STATIC

    ' Hyperbolic tangent function

    TANH = (1# - EXP(-2# * X)) / (1# + EXP(-2# * X))

END FUNCTION

