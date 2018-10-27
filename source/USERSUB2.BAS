' Copyright (c) 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB FAXY (X, Y, F) STATIC

    ' z = fa(x,y) subroutine

    F = SQR(X ^ 2 + Y ^ 2)

END SUB

SUB FBXY (X, Y, F) STATIC

    ' z = fb(x,y) subroutine

    F = 2#

END SUB

SUB FCX (X, F) STATIC

    ' y = fc(x) subroutine

    F = -SQR(4# - X ^ 2)

END SUB

SUB FDX (X, F) STATIC

    ' y = fd(x) subroutine

    F = SQR(4# - X ^ 2)

END SUB

SUB FXYZ (X, Y, Z, F) STATIC

    ' f(x,y,z) subroutine

    F = Z * SQR(X ^ 2 + Y ^ 2)

END SUB

