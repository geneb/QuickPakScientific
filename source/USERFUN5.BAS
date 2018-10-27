' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FX) STATIC

    ' User-defined function subroutine

    ' F(X) = X^4 - 12*X^3 + 15*X^2 + 56*X - 60

    ' Input

    '  X = function argument

    ' Output

    '  FX = negative value of function

    FX = X ^ 4 - 12# * X ^ 3 + 15# * X ^ 2 + 56# * X - 60#

    FX = -FX

END SUB

