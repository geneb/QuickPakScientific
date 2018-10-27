' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION GAMMA(X)

FUNCTION GAMMA (X)

    ' Recursive Gamma function

    ' Input
   
    '  X = function argument

    ' Output
   
    '  GAMMA = function value

    PI = 3.141592653589793#

    IF (X = 0#) THEN
       GAMMA = 0#
    ELSEIF (X > 0#) THEN
       Y = X + 2#
       GAM = SQR(2# * PI / Y) * EXP(Y * LOG(Y) + (1# - 1# / (30# * Y * Y)) / (12# * Y) - Y)
       GAMMA = GAM / (X * (X + 1#))
    ELSEIF (X < 0#) THEN
       J = 0
       Y = X
       DO
          J = J + 1
          Y = Y + 1#
       LOOP UNTIL (Y > 0#)

       GAM = GAMMA(Y)
      
       FOR I = 0 TO J - 1
           GAM = GAM / (X + I)
           GAMMA = GAM
       NEXT I
    END IF

END FUNCTION

