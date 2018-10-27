' Copyright (c) 1988, 1990, 1994 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB CUBIC (B1, B2, B3, B4, X1, X2, X3, NROOT)
DECLARE SUB QUADRATIC (C1, C2, C3, X1, X2, NROOT)

SUB QUARTIC (A1, A2, A3, A4, A5, Q1, Q2, Q3, Q4, NROOT) STATIC

    ' Solution of the quartic equation subroutine

    ' A1 * X^4 + A2 * X^3 + C * X^2 + D * X + E = 0

    ' Input

    '  A1, A2, A3, A4, A5 = coefficients of the quartic equation

    ' Output

    '  NROOT          = number of real roots
    '  Q1, Q2, Q3, Q4 = real roots of the quartic equation

    ' NOTE: requires SUB CUBIC and SUB QUADRATIC

    NROOT = 0

    Q1 = 0#
    Q2 = 0#
    Q3 = 0#
    Q4 = 0#

    BP = A2 / A1
    CP = A3 / A1
    DP = A4 / A1
    EP = A5 / A1
   
    H = -BP / 4#

    H2 = H * H
    H3 = H2 * H
    H4 = H3 * H

    P = 6# * H2 + 3# * BP * H + CP
    Q = 4# * H3 + 3# * BP * H2 + 2# * CP * H + DP
    R = H4 + BP * H3 + CP * H2 + DP * H + EP

    TMP1 = 1#
    TMP2 = 2# * P
    TMP3 = P * P - 4# * R
    TMP4 = -Q * Q

    CALL CUBIC(TMP1, TMP2, TMP3, TMP4, T1, T2, T3, NCUBIC)

    SELECT CASE NCUBIC
    CASE 0
         EXIT SUB
    CASE 1
         RP = T1
    CASE 2
         RP = T1
         IF (T2 > RP) THEN RP = T2
    CASE 3
         RP = T1
         IF (T2 > RP) THEN RP = T2
         IF (T3 > RP) THEN RP = T3
    END SELECT

    SQRP = SQR(RP)

    XI = (P + RP - Q / SQRP) / 2#
    BETA = (P + RP + Q / SQRP) / 2#

    CALL QUADRATIC(1#, SQRP, XI, Y1, Y2, NQUAD1)

    CALL QUADRATIC(1#, -SQRP, BETA, Y3, Y4, NQUAD2)

    IF (NQUAD1 + NQUAD2 = 0) THEN EXIT SUB

    IF (NQUAD1 + NQUAD2 = 4) THEN
       Q1 = Y1 + H
       Q2 = Y2 + H
       Q3 = Y3 + H
       Q4 = Y4 + H
       NROOT = 4
       EXIT SUB
    ELSEIF (NQUAD1 = 0) THEN
       Q1 = Y3 + H
       Q2 = Y4 + H
       NROOT = 2
       EXIT SUB
    ELSE
       Q1 = Y1 + H
       Q2 = Y2 + H
       NROOT = 2
    END IF

END SUB

