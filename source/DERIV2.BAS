' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB DERIV2 (NPTS, NDEG, X(), Y(), XVAL, YVAL) STATIC

    ' Differentiation of tabular data subroutine

    ' Input

    '  NPTS = number of X and Y data points (NPTS >= 4 )
    '  NDEG = degree of interpolation
    '         ( 1 <= NDEG <= minimum ( NPTS-2, 9 )
    '  X()  = vector of X data points ( NPTS rows )
    '  Y()  = vector of Y data points ( NPTS rows )
    '  XVAL = X argument ( X(1) <= X <= X(NPTS-NDEG )

    ' Output

    '  YVAL = value of derivative at XVAL

    DIM XL(9), XM(9, 9)

    I = 0

    ' compute data index

    DO
      I = I + 1
    LOOP UNTIL (XVAL < X(I))

    I = I - 1

    FOR J = 0 TO NDEG
        XL(J) = 0#
        FOR K = 0 TO NDEG
            XM(J, K) = 1#
        NEXT K
    NEXT J

    YVAL = 0#

    FOR K = 0 TO NDEG
        FOR J = 0 TO NDEG
            IF (J <> K) THEN
               FOR L = 0 TO NDEG
                   IF (L <> K) THEN
                      IF (L = J) THEN
                         XM(L, K) = XM(L, K) / (X(I + K) - X(I + J))
                      ELSE
                         XM(L, K) = XM(L, K) * (XVAL - X(J + I)) / (X(I + K) - X(I + J))
                      END IF
                   END IF
               NEXT L
            END IF
        NEXT J

        FOR L = 0 TO NDEG
            IF (L <> K) THEN XL(K) = XL(K) + XM(L, K)
        NEXT L

        YVAL = YVAL + XL(K) * Y(I + K)
    NEXT K

    ERASE XL, XM

END SUB

