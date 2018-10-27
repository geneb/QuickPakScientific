' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FX) STATIC

    ' User-defined function subroutine

    ' f(x) = x^2 * (x^2 - 2) * sin(x)

    ' Input

    '  X = function argument

    ' Output

    '  FX = function value at X

    FX = (X * X) * (X * X - 2#) * SIN(X)

END SUB

