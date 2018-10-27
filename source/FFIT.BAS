' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB FFIT (N, X(), Y(), ITYPE, A, B) STATIC

    ' Function curve fit subroutine

    ' Input

    '  N   = number of data points (N >= 3)
    '  X() = vector of X data points ( N rows )
    '  Y() = vector of Y data points ( N rows )
    '  ITYPE = type of curve fit
    '      1 = linear       Y = A + B * X
    '      2 = logarithmic  Y = A + B * LOG(X)
    '      3 = exponential  Y = A * EXP(B * X)

    ' Output

    '  A, B = coefficients of curve fit

    X1 = Y1 = Z = X2 = Y2 = 0#

    SELECT CASE ITYPE
    CASE 1
         ' Linear
         FOR I = 1 TO N
             X1 = X1 + X(I)
             Y1 = Y1 + Y(I)
             X2 = X2 + X(I) * X(I)
             Y2 = Y2 + Y(I) * Y(I)
             Z = Z + X(I) * Y(I)
         NEXT I
        
         X1 = X1 / N
         Y1 = Y1 / N
         B = (Z - N * X1 * Y1) / (X2 - N * X1 * X1)
         A = Y1 - B * X1
    CASE 2
         ' Logarithmic
         FOR I = 1 TO N
             XLOGX = LOG(X(I))
             X1 = X1 + XLOGX
             Y1 = Y1 + Y(I)
             X2 = X2 + XLOGX * XLOGX
             Y2 = Y2 + Y(I) * Y(I)
             Z = Z + XLOGX * Y(I)
         NEXT I

         X1 = X1 / N
         Y1 = Y1 / N

         B = (Z - N * X1 * Y1) / (X2 - N * X1 * X1)
         A = Y1 - B * X1
    CASE 3
         ' Exponential
         FOR I = 1 TO N
             XLOGY = LOG(Y(I))
             X1 = X1 + X(I)
             X2 = X2 + X(I) * X(I)
             Y1 = Y1 + XLOGY
             Y2 = Y2 + XLOGY * XLOGY
             Z = Z + X(I) * XLOGY
         NEXT I

         X1 = X1 / N
         Y1 = Y1 / N

         B = (Z - N * X1 * Y1) / (X2 - N * X1 * X1)
         A = EXP(Y1 - B * X1)
     END SELECT

END SUB

