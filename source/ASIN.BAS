' Copyright (c) 1988, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ASIN (X)

FUNCTION ASIN (X) STATIC

    ' Inverse sine function

    ' Input

    '  X = function argument

    ' Output

    '  ASIN = inverse sine of X ( radians )

    PIDIV2 = 1.570796326794897#

    IF (ABS(X) >= 1#) THEN
       ASIN = SGN(X) * PIDIV2
    ELSE
       ASIN = ATN(X / SQR(1# - X ^ 2))
    END IF

END FUNCTION

