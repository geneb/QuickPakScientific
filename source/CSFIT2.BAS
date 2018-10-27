' Copyright (c) 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CSFIT2 (N, X(), Y(), YP1, YPN, X, Y) STATIC

    ' Clamped cubic spline interpolation subroutine

    ' Input

    '  N   = number of X and Y data points
    '  X() = vector of X data points ( N rows )
    '  Y() = vector of Y data points ( N rows )
    '  YP1 = derivative at data point 1
    '  YPN = derivative at data point N
    '  X   = X data point to fit

    ' Output

    '  Y = interpolated Y data point

    ' NOTE: X(1) < X(2) < X(3) < ... < X(N)

    DIM U(N), YPP(N)

    YPP(1) = -.5#
    U(1) = (3# / (X(2) - X(1))) * ((Y(2) - Y(1)) / (X(2) - X(1)) - YP1)

    FOR I = 2 TO N - 1
        SIG = (X(I) - X(I - 1)) / (X(I + 1) - X(I - 1))
        P = SIG * YPP(I - 1) + 2#
        YPP(I) = (SIG - 1#) / P
        U(I) = (6# * ((Y(I + 1) - Y(I)) / (X(I + 1) - X(I)) - (Y(I) - Y(I - 1)) / (X(I) - X(I - 1))) / (X(I + 1) - X(I - 1)) - SIG * U(I - 1)) / P
    NEXT I

    QN = .5#
    UN = (3# / (X(N) - X(N - 1))) * (YPN - (Y(N) - Y(N - 1)) / (X(N) - X(N - 1)))

    YPP(N) = (UN - QN * U(N - 1)) / (QN * YPP(N - 1) + 1#)

    FOR K = N - 1 TO 1 STEP -1
        YPP(K) = YPP(K) * YPP(K + 1) + U(K)
    NEXT K

    ' interpolate

    KLO = 1
    KHI = N

    WHILE (KHI - KLO > 1)
       K = (KHI + KLO) / 2#
       IF (X(K) > X) THEN
          KHI = K
       ELSE
          KLO = K
       END IF
    WEND

    H = X(KHI) - X(KLO)

    A = (X(KHI) - X) / H
    B = (X - X(KLO)) / H

    Y = A * Y(KLO) + B * Y(KHI) + ((A ^ 3 - A) * YPP(KLO) + (B ^ 3 - B) * YPP(KHI)) * (H ^ 2) / 6#

    ERASE U, YPP

END SUB

