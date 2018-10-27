' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X(), FX) STATIC

    ' Non-linear function subroutine

    '   FX1 = 3*X1 - COS(X2*X3) - .5 = 0
   
    '   FX2 = X1^2 - 81*(X2 + .1)^2 + SIN(X3) + 1.06 = 0
   
    '   FX3 = EXP(-X1*X2) + 20*X3 + (10*PI - 3 ) / 3 = 0

    ' Input

    '  X() = function argument vector

    ' Output

    '  FX = function value evaluated at X
    '  FX = FX1^2 + FX2^2 + FX3^2 + ... + FXN^2

    PI = 4# * ATN(1#)

    FX1 = -(3# * X(1) - COS(X(2) * X(3)) - .5#)
    FX2 = -(X(1) ^ 2 - 81# * (X(2) + .1#) ^ 2 + SIN(X(3)) + 1.06#)
    FX3 = -(EXP(-X(1) * X(2)) + 20# * X(3) + (10# * PI - 3#) / 3#)

    FX = FX1 ^ 2 + FX2 ^ 2 + FX3 ^ 2

END SUB

