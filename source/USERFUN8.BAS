' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X(), F) STATIC

    ' User-defined function subroutine

    ' F(x) = 100(x2 - x1^2)^2 + (1 - x1)^2 + 90(x4 - x3^2)^2
    '        + (1 - x3^2)^2 + 10.1((x2 - 1)^2 + (x4 - 1)^2)
    '        + 19.9(x2 - 1)(x4 - 1)

    ' Input

    '  X() = function argument vector

    ' Output

    '  F = scalar value of objective function at X()

    A = X(2) - X(1) * X(1)
    B = X(4) - X(3) * X(3)

    F = 100# * A * A + (1# - X(1)) ^ 2 + 90# * B * B + (1# - X(3)) ^ 2
    F = F + 10.1# * ((X(2) - 1#) ^ 2 + (X(4) - 1#) ^ 2)
    F = F + 19.8# * (X(2) - 1#) * (X(4) - 1#)

END SUB

