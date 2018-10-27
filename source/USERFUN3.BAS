' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FVAL) STATIC

    ' User-defined function subroutine

    ' F(X) = X * e^X - 10

    ' Input

    '  X = function argument

    ' Output

    '  FVAL = function value at X

    FVAL = X * EXP(X) - 10#

END SUB

