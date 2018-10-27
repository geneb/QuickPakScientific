' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ATANH (X)

FUNCTION ATANH (X) STATIC

    ' Inverse hyperbolic tangent function

    IF (ABS(X) >= 1#) THEN
       ATANH = SGN(X) * 1D+308
    ELSE
       ATANH = .5# * LOG((1# + X) / (1# - X))
    END IF

END FUNCTION

