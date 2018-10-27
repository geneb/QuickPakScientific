' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB USERFUNC (X(), F, GRADIENT()) STATIC

    ' User-defined function and gradient subroutine

    ' F(x) = 100(x2 - x1^2)^2 + (1 - x1)^2 + 90(x4 - x3^2)^2
    '        + (1 - x3^2)^2 + 10.1((x2 - 1)^2 + (x4 - 1)^2)
    '        + 19.9(x2 - 1)(x4 - 1)

    ' Input

    '  X() = function argument vector

    ' Output

    '  F          = scalar value of objective function at X()
    '  GRADIENT() = function gradient vector evaluated at X()

    A = X(2) - X(1) * X(1)
    B = X(4) - X(3) * X(3)

    ' calculate function value

    F = 100# * A * A + (1# - X(1)) ^ 2 + 90# * B * B + (1# - X(3)) ^ 2
    F = F + 10.1# * ((X(2) - 1#) ^ 2 + (X(4) - 1#) ^ 2)
    F = F + 19.8# * (X(2) - 1#) * (X(4) - 1#)

    ' calculate elements of gradient

    GRADIENT(1) = -2# * (200# * X(1) * A + 1# - X(1))
    GRADIENT(2) = 2# * (100# * A + 10.1# * (X(2) - 1#) + 9.9# * (X(4) - 1#))
    GRADIENT(3) = -2# * (180# * X(3) * B + 1# - X(3))
    GRADIENT(4) = 2# * (90# * B + 10.1# * (X(4) - 1#) + 9.9# * (X(2) - 1#))

END SUB

