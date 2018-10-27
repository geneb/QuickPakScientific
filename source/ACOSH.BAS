' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ACOSH (X)

FUNCTION ACOSH (X) STATIC

    ' Inverse hyperbolic cosine function

    IF (ABS(X) < 1#) THEN
       ACOSH = 0#
    ELSE
       ACOSH = LOG(X + SQR(X * X - 1#))
    END IF

END FUNCTION

