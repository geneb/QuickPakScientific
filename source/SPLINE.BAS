' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB SPLINE (N, X(), Y(), SUM) STATIC

    ' Cubic spline integration of tabulated data subroutine

    ' Input

    '  N    = number of X and Y data points ( N >= 2 )
    '  X()  = vector of X data ( N rows )
    '  Y()  = vector of Y data ( N rows )

    ' Output

    '  SUM = integral from X(1) to X(N)

    DIM S(N), G(N - 1), WORK(N - 1)

    FOR I = 2 TO (N - 1)
        XI = X(I)
        XIM1 = X(I - 1)
        XIP1 = X(I + 1)
        YI = Y(I)
        YIM1 = Y(I - 1)
        YIP1 = Y(I + 1)
        X = XI - XIM1
        H = XIP1 - XIM1
        WORK(I) = .5# * X / H
        T = ((YIP1 - YI) / (XIP1 - XI) - (YI - YIM1) / X) / H
        S(I) = 2# * T
        G(I) = 3# * T
    NEXT I

    S(1) = 0#
    S(N) = 0#

    W = 8# - 4# * SQR(3#)

    DO
       U = 0#

       FOR I = 2 TO (N - 1)
           T = W * (-S(I) - WORK(I) * S(I - 1) - (.5# - WORK(I)) * S(I + 1) + G(I))
           H = ABS(T)
           IF (H > U) THEN U = H
           S(I) = S(I) + T
       NEXT I
    LOOP UNTIL (U < .00000001#)

    SUM = 0#

    FOR I = 1 TO (N - 1)
        H = X(I + 1) - X(I)
        SUM = SUM + .5# * H * (Y(I) + Y(I + 1)) - (1# / 24#) * H ^ 3 * (S(I) + S(I + 1))
    NEXT I

    ERASE S, G, WORK

END SUB

