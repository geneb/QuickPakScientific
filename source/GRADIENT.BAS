' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB GRADIENT (X(), G()) STATIC
   
    ' Analytic gradient of a user-defined 
    ' non-linear equation subroutine
   
    ' F(x) = 100(x2 - x1^2)^2 + (1 - x1)^2 + 90(x4 - x3^2)^2
    '        + (1 - x3^2)^2 + 10.1((x2 - 1)^2 + (x4 - 1)^2)
    '        + 19.9(x2 - 1)(x4 - 1)

    ' Input

    '  X() = function argument vector

    ' Output

    '  G() = gradient of non-linear equation evaluated at X
   
    A = X(2) - X(1) * X(1)
    B = X(4) - X(3) * X(3)

    G(1) = -2# * (200# * X(1) * A + 1# - X(1))
    G(2) = 2# * (100# * A + 10.1# * (X(2) - 1#) + 9.9# * (X(4) - 1#))
    G(3) = -2# * (180# * X(3) * B + 1# - X(3))
    G(4) = 2# * (90# * B + 10.1# * (X(4) - 1#) + 9.9# * (X(2) - 1#))

END SUB
