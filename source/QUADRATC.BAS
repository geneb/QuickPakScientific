' Copyright (c) 1988, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB QUADRATIC (C1, C2, C3, X1, X2, NROOT) STATIC

    ' Solution of the quadratic equation subroutine

    ' C1 * X^2 + C2 * X + C3 = 0

    ' Input

    '  C1, C2, C3 = coefficients of the quadratic equation

    ' Output

    '  NROOT  = number of real roots
    '  X1,X2  = real roots of the quadratic equation

    NROOT = 0

    X1 = 0#
    X2 = 0#

    A = C2 * C2 - 4# * C1 * C3

    IF (A > 0#) THEN
       Q = -.5# * (C2 + SGN(C2) * SQR(A))
       X1 = Q / C1
       X2 = C3 / Q
       NROOT = 2
    END IF

END SUB

