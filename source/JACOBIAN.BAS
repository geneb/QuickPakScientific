' Copyright 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB JACOBIAN (X(), DY()) STATIC
   
    ' Analytic Jacobian of a user-defined system
    ' of non-linear equations subroutine
   
    ' Input

    '  X() = function argument vector (1 column by 3 rows)

    ' Output

    '  DY() = Jacobian of non-linear system evaluated at X
    '         (3 rows by 3 columns)

    '  where DY(I, J) = d[f(xi)]/d[xj]

    '  DY(1, 1) = d[f(x1)]/d[x1], DY(1, 2) = d[f(x1)]/d[x2]
    '  DY(1, 3) = d[f(x1)]/d[x3], etc.

    DY(1, 1) = 1# / X(2)
    DY(1, 2) = -X(1) / X(2) ^ 2 - 5# / X(2)
    DY(1, 3) = 5# / X(3)

    DY(2, 1) = 1#
    DY(2, 2) = 1#
    DY(2, 3) = 0#

    DY(3, 1) = .5#
    DY(3, 2) = 0#
    DY(3, 3) = 1#

END SUB

