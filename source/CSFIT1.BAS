' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CSFIT1 (N, X(), Y(), S(), INDEX(), X, SX) STATIC

    ' Cublic spline interpolation subroutine

    ' Input

    '  N       = number of X and Y data points 
    '  X()     = array of X data points (N rows by 1 column)
    '  Y()     = array of Y data points (N rows by 1 column)
    '  S()     = array of cubic spline coefficients
    '            (N rows by 1 column)
    '  INDEX() = array of indices (N rows by 1 column)
    '  X       = X data value to fit

    ' Output

    '  SX = cubic spline interpolated value for X

    FOR I = 2 TO N
        II = INDEX(I)
        IF (X <= X(II)) THEN EXIT FOR
    NEXT I

    L = I - 1
    IL = INDEX(L)
    ILP1 = INDEX(L + 1)

    A = X(ILP1) - X
    B = X - X(IL)

    HL = X(ILP1) - X(IL)

    SX = A * S(L) * (A * A / HL - HL) / 6# + B * S(L + 1) * (B * B / HL - HL) / 6# + (A * Y(IL) + B * Y(ILP1)) / HL

END SUB
