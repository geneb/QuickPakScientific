' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (K, X(), P(), Y) STATIC

    ' Non-linear fitting function subroutine

    ' Input

    '  K   = index of X data array
    '  X() = array of X data points
    '  P() = array of fitting parameters

    ' Output

    '  Y = function value at X(K)

    B1 = P(1)
    B2 = P(2)
    B3 = P(3)

    Y = B1 / (1# + B2 * EXP(X(K) * B3))

END SUB

