' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

FUNCTION ROUND (X, N) STATIC

    ' Round number function

    ' Input

    '  X = real, floating point number
    '  N = number of decimal places

    ' Output

    '  ROUND = X rounded to N decimal places

    A = 10# ^ N

    ROUND = INT(X / A + .5#) * A

END FUNCTION

