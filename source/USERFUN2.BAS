' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FX) STATIC

    ' User-defined function subroutine

    ' f(x) = sqrt(sin(x) + 2.5 exp(x))

    ' Input

    '  X = function argument

    ' Output

    '  FX = function value at X

    FX = SQR(SIN(X) + 2.5# * EXP(X))

END SUB

