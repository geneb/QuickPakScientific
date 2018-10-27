' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ASINH (X)

FUNCTION ASINH (X) STATIC

    ' Inverse hyperbolic sine function

    ASINH = LOG(X + SQR(X * X + 1#))

END FUNCTION

