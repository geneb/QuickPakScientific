' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X(), FX(), DFDX()) STATIC

    ' Objective function and partial derivatives subroutine

    ' f1(X1,X2,X3,X4) = - X1^2 - X2^2 - X3^2 + X4       = 0
    ' f2(X1,X2,X3,X4) =   X1^2 + X2^2 + X3^2 + X4^2 - 1 = 0
    ' f3(X1,X2,X3,X4) =   X1   -  X2                    = 0
    ' f4(X1,X2,X3,X4) =           X2  -  X3             = 0
   
    ' Input

    '  X = function argument vector

    ' Output

    '  FX   = vector of negative function values evaluated at X
    '  DFDX = function gradient evaluated at X

    FX(1) = -(-X(1) ^ 2 - X(2) ^ 2 - X(3) ^ 2 + X(4))
    FX(2) = -(X(1) ^ 2 + X(2) ^ 2 + X(3) ^ 2 + X(4) ^ 2 - 1#)
    FX(3) = -(X(1) - X(2))
    FX(4) = -(X(2) - X(3))

    ' Jacobian

    DFDX(1, 1) = -2# * X(1)
    DFDX(1, 2) = -2# * X(2)
    DFDX(1, 3) = -2# * X(3)
    DFDX(1, 4) = 1#
    DFDX(2, 1) = 2# * X(1)
    DFDX(2, 2) = 2# * X(2)
    DFDX(2, 3) = 2# * X(3)
    DFDX(2, 4) = 2# * X(4)
    DFDX(3, 1) = 1#
    DFDX(3, 2) = -1#
    DFDX(3, 3) = 0#
    DFDX(3, 4) = 0#
    DFDX(4, 1) = 0#
    DFDX(4, 2) = 1#
    DFDX(4, 3) = -1#
    DFDX(4, 4) = 0#

END SUB

