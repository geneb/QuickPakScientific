' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB INTERP2 (NX, NY, X(), Y(), Z(), XVAL, YVAL, ZVAL) STATIC

    ' Bilinear interpolation subroutine

    ' z = f(x,y)

    ' Input

    '  NX   = number of X data points ( NX >= 2 )
    '  NY   = number of Y data points ( NY >= 2 )      
    '  X()  = vector of X data ( NX rows )
    '  Y()  = vector of Y data ( NY rows )
    '  Z()  = array of Z data ( NX rows by NY columns )
    '  XVAL = X argument
    '  YVAL = Y argument

    ' Output

    '  ZVAL = Z function value at XVAL, YVAL

    ' NOTE: the data must be ordered as follows:

    '  X(1) < X(2) < X(3) < .... < X(N)
    '  Y(1) < Y(2) < Y(3) < .... < Y(N)

    ' compute X index and interpolation factor

    FOR I = 1 TO NX
        IF (XVAL <= X(I)) THEN
           IF (I = 1) THEN
              XINFAC = 0#
              INDEXX = 1
           ELSE
              J = I - 1
              XINFAC = (XVAL - X(J)) / (X(I) - X(J))
              INDEXX = J
           END IF
           EXIT FOR
        END IF
    NEXT I

    ' compute Y index and interpolation factor

    FOR I = 1 TO NY
        IF (YVAL <= Y(I)) THEN
           IF (I = 1) THEN
              YINFAC = 0#
              INDEXY = 1
           ELSE
              J = I - 1
              YINFAC = (YVAL - Y(J)) / (Y(I) - Y(J))
              INDEXY = J
           END IF
           EXIT FOR
        END IF
    NEXT I

    G1 = Z(INDEXX, INDEXY) + XINFAC * (Z(INDEXX + 1, INDEXY) - Z(INDEXX, INDEXY))

    G2 = Z(INDEXX, INDEXY + 1) + XINFAC * (Z(INDEXX + 1, INDEXY + 1) - Z(INDEXX, INDEXY + 1))

    ZVAL = G1 + YINFAC * (G2 - G1)

END SUB

