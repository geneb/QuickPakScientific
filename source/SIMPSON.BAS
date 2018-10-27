' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB SIMPSON (N, X(), Y(), SUM) STATIC

    ' Simpson integration of tabulated data subroutine

    ' Input

    '  N   = number of X and Y data points ( must be odd )
    '  X() = vector of X data points ( N rows )
    '  Y() = vector of Y data points ( N rows )

    ' Output

    '  SUM = integral from X(1) to X(N)

    X2 = X(1)
    F2 = Y(1)

    SUM = 0#

    NM2 = N - 2

    FOR I = 1 TO NM2 STEP 2
        X0 = X2
        F0 = F2

        X1 = X(I + 1)
        F1 = Y(I + 1)

        X2 = X(I + 2)
        F2 = Y(I + 2)

        DEL1 = (F1 - F0) / (X1 - X0)
        DEL2 = (F2 - F1) / (X2 - X1)
        DEL3 = DEL2 - DEL1
        DEL4 = X2 - X0

        DSUM = DEL4 * (F0 + .5# * DEL4 * DEL1)
        DSUM = DSUM + (2# * X2 ^ 2 - X0 * X2 + 3# * X0 * X1 - X0 ^ 2 - 3# * X1 * X2) * DEL3 / 6#

        SUM = SUM + DSUM
    NEXT I

END SUB

