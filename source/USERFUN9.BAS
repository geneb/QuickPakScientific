' Copyright 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FX, FPX) STATIC

    ' User-defined function subroutine

    ' F(X) = X - EXP(1/X)

    ' Input

    '  X = X value

    ' Output

    '  FX  = function value at X
    '  FPX = function derivative at X

    FX = X - EXP(1# / X)

    FPX = 1# + EXP(X) / X ^ 2

END SUB

