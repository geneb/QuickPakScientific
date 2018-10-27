' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CSCOEF (N, X(), Y(), S(), INDEX()) STATIC

    ' Cublic spline coefficients subroutine

    ' Input

    '  N   = number of X and Y data points 
    '  X() = array of X data points (N rows by 1 column)
    '  Y() = array of Y data points (N rows by 1 column)

    ' Output

    '  S()     = array of cubic spline coefficients
    '            (N rows by 1 column)
    '  INDEX() = array of indices (N rows by 1 column)

    DIM RHO(N), TAU(N)

    NM1 = N - 1

    FOR I = 1 TO N
        INDEX(I) = I
    NEXT I

    ' ascending order data sort

    FOR I = 1 TO NM1
        IP1 = I + 1
        FOR J = IP1 TO N
            II = INDEX(I)
            IJ = INDEX(J)
            IF (X(II) > X(IJ)) THEN
               ITEMP = INDEX(I)
               INDEX(I) = INDEX(J)
               INDEX(J) = ITEMP
            END IF
        NEXT J
    NEXT I

    NM2 = N - 2

    RHO(2) = 0#
    TAU(2) = 0#

    FOR I = 2 TO NM1
        IIM1 = INDEX(I - 1)
        II = INDEX(I)
        IIP1 = INDEX(I + 1)
        HIM1 = X(II) - X(IIM1)
        HI = X(IIP1) - X(II)
        TEMP = (HIM1 / HI) * (RHO(I) + 2#) + 2#
        RHO(I + 1) = -1# / TEMP
        D = 6# * ((Y(IIP1) - Y(II)) / HI - (Y(II) - Y(IIM1)) / HIM1) / HI
        TAU(I + 1) = (D - HIM1 * TAU(I) / HI) / TEMP
    NEXT I

    S(1) = 0#
    S(N) = 0#

    ' compute cubic spline coefficients

    FOR I = 1 TO NM2
        IB = N - I
        S(IB) = RHO(IB + 1) * S(IB + 1) + TAU(IB + 1)
    NEXT I

    ERASE RHO, TAU

END SUB
