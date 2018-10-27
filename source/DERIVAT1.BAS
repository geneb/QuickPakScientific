' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB DERIVATIVE (T, Y(), YDOT()) STATIC

    ' System of differential equations subroutine

    '   Y1' = Y2 - Y3 + T
    '   Y2' = 3T^2
    '   Y3' = Y2 + e^(-T)

    ' Input

    '  T   = integration time
    '  Y() = vector of integration variables ( 3 rows )

    ' Output

    '  YDOT() = vector of differential equations ( 3 rows )

    YDOT(1) = Y(2) - Y(3) + T

    YDOT(2) = 3# * T ^ 2

    YDOT(3) = Y(2) + EXP(-T)

END SUB

