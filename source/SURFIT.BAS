' Copyright 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB SURFIT (NDEG, NDATA, XDATA(), YDATA(), ZDATA(), COEF()) STATIC

    ' Surface fit subroutine

    ' Input

    '  NDEG    = degree of surface fit (NDEG <= 10)
    '  NDATA   = number of X, Y and Z data points
    '  XDATA() = vector of X data values ( NDATA rows )
    '  YDATA() = vector of Y data values ( NDATA rows )
    '  ZDATA() = vector of Z data values ( NDATA rows )

    ' Output

    '  COEF() = array of surface fit coefficients ( 11 rows by 11 columns )

    NDEG1 = NDEG + 1
    NDEG2 = NDEG + 2
    NROW = NDEG1 * NDEG2 / 2
    NCOL = NROW + 1
    NALF = 2 * NDEG
    NALF1 = NALF + 1
    NALF2 = NALF + 2

    DIM XA(NDEG1 * NDEG2 / 2)
    DIM ZADD(NDEG1, NDEG1)
    DIM HSMG(2 * NDEG1, 2 * NDEG1)
    DIM XMATRIX(NDEG1 * NDEG2 / 2, NDEG1 * NDEG2 / 2 + 1)

    FOR I = 1 TO NALF1
        FOR J = 1 TO NALF2 - I
            HSMG(I, J) = 0#
        NEXT J
    NEXT I

    FOR I = 1 TO NDEG1
        FOR J = 1 TO NDEG2 - I
            ZADD(I, J) = 0#
        NEXT J
    NEXT I

    Z1 = 0#

    FOR I = 1 TO NDATA
        Z1 = Z1 + ZDATA(I)
    NEXT I

    IF (NDEG <> 0) THEN
       FOR I = 1 TO NDATA
           A = 1#
           XA(1) = 1#
           X = XDATA(I)
           Y = YDATA(I)
           Z = ZDATA(I)
           FOR J = 2 TO NALF1
               A = A * X
               XA(J) = A
               IF (J <= NDEG1) THEN ZADD(1, J) = ZADD(1, J) + A * Z
               HSMG(1, J) = HSMG(1, J) + A
           NEXT J
           FOR L = 2 TO NALF1
               FOR M = 1 TO NALF2 - L
                   XA(M) = XA(M) * Y
                   HSMG(L, M) = HSMG(L, M) + XA(M)
               NEXT M
               IF (L <= NDEG1) THEN
                  FOR N = 1 TO NDEG2 - L
                      ZADD(L, N) = ZADD(L, N) + XA(N) * Z
                  NEXT N
               END IF
           NEXT L
       NEXT I
    END IF

    HSMG(1, 1) = NDATA
    ZADD(1, 1) = Z1

    M = 0

    FOR I = 1 TO NDEG1
        FOR J = 1 TO NDEG2 - I
            N = 0
            M = M + 1
            XMATRIX(M, NCOL) = ZADD(I, J)
            FOR K = I TO NDEG + I
                FOR L = J TO NDEG + I + J - K
                    N = N + 1
                    XMATRIX(M, N) = HSMG(K, L)
                NEXT L
            NEXT K
        NEXT J
    NEXT I

    IF (NDEG = 0) THEN
       XA(1) = XMATRIX(1, 2) / XMATRIX(1, 1)
       GOTO EXITSUB
    END IF

    NR1 = NROW - 1

    FOR K = 1 TO NR1
        NB = 0
        AMAX = XMATRIX(K, K)
        I1 = K + 1
        FOR KI = I1 TO NROW
            IF (ABS(XMATRIX(KI, K)) > ABS(AMAX)) THEN
               AMAX = XMATRIX(KI, K)
               NB = KI
            END IF
        NEXT KI

        IF (NB <> 0) THEN
           FOR NL = K TO NCOL
               SAVE = XMATRIX(NB, NL)
               XMATRIX(NB, NL) = XMATRIX(K, NL)
               XMATRIX(K, NL) = SAVE
           NEXT NL
        END IF

        CONST1 = XMATRIX(K, K)

        FOR I = I1 TO NROW
            XA(I) = -XMATRIX(I, K) / CONST1
        NEXT I

        FOR J = I1 TO NCOL
            CONST1 = XMATRIX(K, J)
            FOR I = I1 TO NROW
                XMATRIX(I, J) = XMATRIX(I, J) + CONST1 * XA(I)
            NEXT I
        NEXT J
  
    NEXT K

    XA(NROW) = XMATRIX(NROW, NCOL) / XMATRIX(NROW, NROW)

    FOR I = 1 TO NR1
        K = NROW - I
        SUM = 0#
        CONST1 = XMATRIX(K, K)
        FOR J = K + 1 TO I + K
            SUM = SUM + XMATRIX(K, J) * XA(J)
        NEXT J
        XA(K) = (XMATRIX(K, NCOL) - SUM) / CONST1
    NEXT I

    ' load COEF array

    K = 0

    FOR I = 1 TO NDEG1
        FOR J = 1 TO NDEG2 - I
            K = K + 1
            COEF(I, J) = XA(K)
        NEXT J
    NEXT I

EXITSUB:

    ERASE XA, ZADD, HSMG, XMATRIX

END SUB

