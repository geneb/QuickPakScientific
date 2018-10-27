' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (K, P(), Y) STATIC

    ' System of non-linear equations subroutine

    '  Y1 = 3*X1 - COS(X2*X3) - .5 = 0
  
    '  Y2 = X1^2 - 81*(X2 + .1)^2 + SIN(X3) + 1.06 = 0
  
    '  Y3 = EXP(-X1*X2) + 20*X3 + (10*PI - 3 ) / 3 = 0

    ' Input

    '  K   = index of X data array
    '  P() = array of X data values

    ' Output

    '  Y = function value at X(K)
    
    PI = 4# * ATN(1#)

    X1 = P(1)
    X2 = P(2)
    X3 = P(3)

    SELECT CASE K
    CASE 1
       Y = 3# * X1 - COS(X2 * X3) - .5#
    CASE 2
       Y = X1 * X1 - 81# * (X2 + .1#) ^ 2 + SIN(X3) + 1.06#
    CASE 3
       Y = EXP(-X1 * X2) + 20# * X3 + (10# * PI - 3#) / 3#
    END SELECT

END SUB

