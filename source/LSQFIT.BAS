' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB LSQFIT (NPTS, X(), Y(), MDEG, SDEV, COEF()) STATIC

    ' Least squares curve fit subroutine

    ' Y = COEF(1) + COEF(2)*X + COEF(3)*X^2 + .... + COEF(N)*X^(N-1)

    ' Input

    '  NPTS = number of X and Y data points
    '  X()  = array of X data ( NPTS columns )
    '  Y()  = array of Y data ( NPTS columns )
    '  MDEG = degree of curve fit

    ' Output

    '  SDEV   = standard deviation of fit
    '  COEF() = array of curve fit coefficients ( NPTS columns )

    MP1 = MDEG + 1

    DIM A(MP1), B(MP1), D(NPTS), E(NPTS), F(MP1), V(NPTS)

    D1 = SQR(NPTS)
    W = D1

    FOR I = 1 TO NPTS
        E(I) = 1# / W
    NEXT I

    F1 = D1
    A1 = 0#

    FOR I = 1 TO NPTS
        A1 = A1 + X(I) * E(I) ^ 2
    NEXT I

    C1 = 0#

    FOR I = 1 TO NPTS
        C1 = C1 + Y(I) * E(I)
    NEXT I

    B(1) = 1# / F1
    F(1) = B(1) * C1

    FOR I = 1 TO NPTS
        V(I) = V(I) + C1 * E(I)
    NEXT I

    FOR MDEG = 2 TO MP1
        F2 = F1
        A2 = A1
        F1 = 0#

        FOR I = 1 TO NPTS
            B1 = E(I)
            E(I) = (X(I) - A2) * E(I) - F2 * D(I)
            D(I) = B1
            F1 = F1 + E(I) ^ 2
        NEXT I

        F1 = SQR(F1)

        FOR I = 1 TO NPTS
            E(I) = E(I) / F1
        NEXT I

        A1 = 0#

        FOR I = 1 TO NPTS
            A1 = A1 + X(I) * E(I) ^ 2
        NEXT I

        C1 = 0#

        FOR I = 1 TO NPTS
            C1 = C1 + E(I) * Y(I)
        NEXT I

        FOR I = 0 TO MDEG
            LDEG = MDEG - I
            B2 = B(LDEG)
            IF (LDEG > 1) THEN
               D1 = B(LDEG - 1)
            ELSE
               D1 = 0#
            END IF
            D1 = D1 - A2 * B(LDEG) - F2 * A(LDEG)
            B(LDEG) = D1 / F1
            A(LDEG) = B2
        NEXT

        FOR I = 1 TO NPTS
            V(I) = V(I) + C1 * E(I)
        NEXT I

        FOR I = 1 TO MP1
            F(I) = F(I) + C1 * B(I)
            COEF(I) = F(I)
        NEXT I

        V = 0#

        FOR I = 1 TO NPTS
            V = V + (V(I) - Y(I)) ^ 2
        NEXT I

        V = SQR(V / (NPTS - LDEG - 1))
        LDEG = MDEG

    NEXT MDEG
   
    MDEG = MDEG - 1                      'Reset to incoming value + 1

    SDEV = V

    ERASE A, B, D, E, F, V

END SUB

