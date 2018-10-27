' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB INTERP1 (N, X(), Y(), XVAL, FVAL) STATIC

    ' Linear interpolation subroutine

    ' y = f(x)

    ' Input

    '  N    = number of X and Y data points ( N >= 2 )
    '  X()  = vector of X data ( N rows )
    '  Y()  = vector of Y data ( N rows )
    '  XVAL = X argument

    ' Output

    '  FVAL = interpolated function value at XVAL

    ' compute index and interpolation factor

    FOR I = 1 TO N
        IF (XVAL <= X(I)) THEN
           IF (I = 1) THEN
              XINFAC = 0#
              INDEX = 1
           ELSE
              J = I - 1
              XINFAC = (XVAL - X(J)) / (X(I) - X(J))
              INDEX = J
           END IF
           EXIT FOR
        END IF
    NEXT I

    Y1 = Y(INDEX)
    Y2 = Y(INDEX + 1)

    FVAL = Y1 + XINFAC * (Y2 - Y1)

END SUB

