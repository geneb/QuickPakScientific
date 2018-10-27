' Copyright (c) 1989 Crescent Software

DEFINT H-I
DEFDBL A-H, O-Z

SUB FCX (X, F) STATIC

    ' y = fc(x) subroutine

    F = X ^ 3

END SUB

SUB FDX (X, F) STATIC

    ' y = fd(x) subroutine

    F = X ^ 2

END SUB

SUB FXY (X, Y, F) STATIC

    ' f(x,y) subroutine

    F = EXP(Y / X)

END SUB

