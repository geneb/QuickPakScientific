' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ERF(X)

FUNCTION ERF (X) STATIC

    ' Error function

    ' Input
   
    '  X = function argument

    ' Output
   
    '  ERF = function value

    TOL = .00000001#
    SQRTPI = SQR(3.141592653589793#)

    IF (X = 0#) THEN
       ERF = 0#
    ELSEIF (X > 4#) THEN
       ERF = 1#
    ELSE
       X2 = X * X
       SUM = X
       TERM = X
       I = 0
       DO
          I = I + 1
          SUM1 = SUM
          TERM = 2# * TERM * X2 / (1# + 2# * I)
          SUM = TERM + SUM1
       LOOP UNTIL (TERM < TOL * SUM)
       ERF = 2# * SUM * EXP(-X2) / SQRTPI
    END IF

END FUNCTION

