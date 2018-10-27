' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X, FVAL) STATIC

    ' User function subroutine

    ' f(x) = e^(-x^2)

    ' Input

    '  X = function argument

    ' Output

    '  FVAL = function value = f(X)

    FVAL = EXP(-X * X)
    
END SUB

