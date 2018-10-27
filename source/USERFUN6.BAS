' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FX) STATIC

    ' Objective function subroutine

    ' F(X) = X^3 + 5 / SQR(X + 2)

    ' Input

    '  X = function argument

    ' Output

    '  FX = value of objective function at X

    FX = (X ^ 3 + 5#) / SQR(X + 2#)

END SUB

