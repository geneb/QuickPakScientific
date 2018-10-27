' Copyright 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X(), FX()) STATIC
   
    ' User-defined system of non-linear equations subroutine

    '   FX1 = X1/X2 - 5 * LN(0.4*X2/X3) + 4.45977 

    '   FX2 = X2 + X1 - 1

    '   FX3 = X3 + 0.5 * X1 - 0.4
   
    ' Input

    '  X() = function argument vector (1 column by 3 rows)

    ' Output

    '  FX() = array of function values evaluated at X
    '         (1 column by 3 rows)

    FX(1) = X(1) / X(2) - 5# * LOG(.4# * X(2) / X(3)) + 4.45977#
   
    FX(2) = X(2) + X(1) - 1#
   
    FX(3) = X(3) + .5# * X(1) - .4#

END SUB

