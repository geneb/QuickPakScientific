' Copyright (c) 1988, 1990 Crescent Software
' Released to the Public Domain on 10/26/18.
DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ACOS (X)

FUNCTION ACOS (X) STATIC

    ' Inverse cosine function

    ' Input

    '  X = function argument

    ' Output

    '  ACOS = inverse cosine of X ( radians )

    PIDIV2 = 1.570796326794897#

    IF (ABS(X) >= 1#) THEN
       A = SGN(X) * PIDIV2
    ELSE
       A = ATN(X / SQR(1# - X ^ 2))
    END IF

    ACOS = PIDIV2 - A

END FUNCTION


