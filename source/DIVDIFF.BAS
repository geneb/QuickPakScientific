' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (T, Y, YP, FVAL)
DECLARE SUB LIEBMANN (AIV, BIV, ALPHA, BETA, NDIG, N, ITER, IFLAG, T(), Y())

SUB DIVDIFF (N, AIV, ALPHA, BIV, BETA, TR(), YR()) STATIC

    ' Divided-difference TPBVP subroutine
    
    ' Solves the second-order boundary value problem
    '  y'' = f(t, y, y') subject to the boundary
    '  conditions y(a) = alpha and y(b) = beta

    ' Input

    '  N     = number of intervals
    '  AIV   = initial time
    '  ALPHA = initial boundary value
    '  BIV   = final time
    '  BETA  = final boundary value

    ' Output

    '  TR() = vector of time values ( N rows by 1 column )
    '  YR() = array of solution values ( N rows by 1 column )

    ' Note: requires subroutine LIEBMANN

    DIM T(2 * N), Y1(2 * N), Y2(2 * N)

    NDIG = 8

    CALL LIEBMANN(AIV, BIV, ALPHA, BETA, NDIG, N, ITER, IFLAG1, T(), Y1())

    N2 = 2 * N

    CALL LIEBMANN(AIV, BIV, ALPHA, BETA, NDIG, N2, ITER, IFLAG2, T(), Y2())

    ' compute results

    FOR J = 0 TO N
        J2 = 2 * J
        IF (IFLAG1 = 1 AND IFLAG2 = 1) THEN
           ' Richardson's formula
           TR(J) = T(J2)
           YR(J) = (4# * Y2(J2) - Y1(J)) / 3#
        END IF
    NEXT J

    ERASE T, Y1, Y2

END SUB

SUB LIEBMANN (AIV, BIV, ALPHA, BETA, NDIG, N, ITER, IFLAG, T(), Y()) STATIC
   
    ' Divided difference support subroutine

    H = (BIV - AIV) / N

    TWOH = 2# * H
    HH = H * H

    SLOPE = (BETA - ALPHA) / (BIV - AIV)

    FOR J = 0 TO N
        T(J) = AIV + J * H
        Y(J) = ALPHA + SLOPE * J * H
    NEXT J

    TOL = 10# ^ (-NDIG)

    MAXITER = 50 * N * NDIG

    ITER = 0

    DO
       IFLAG = 1
     
       FOR J = 1 TO N - 1
           YPJ = (Y(J + 1) - Y(J - 1)) / TWOH
           CALL USERFUNC(T(J), Y(J), YPJ, FVAL)
           XNEWYJ = (Y(J - 1) + Y(J + 1) - HH * FVAL) / 2#
           TMP = .01#
           IF (ABS(XNEWYJ) > TMP) THEN TMP = ABS(XNEWXJ)
           IF (ABS(XNEWYJ - Y(J)) > TOL * TMP) THEN IFLAG = 0
           Y(J) = XNEWYJ
       NEXT J

       ITER = ITER + 1
    LOOP UNTIL (IFLAG = 1 OR ITER = MAXITER)

END SUB

SUB USERFUNC (T, Y, YP, FVAL) STATIC

    ' User-defined differential equation subroutine

    ' Input

    '  T  = current time
    '  Y  = current Y value
    '  YP = current Y' value

    ' Output

    '  FVAL = function evaluated at T, Y, YP

    FVAL = YP * (1# / T + 2# * YP / Y)

END SUB

