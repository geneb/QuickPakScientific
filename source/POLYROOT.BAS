' Copyright 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB POLYROOT (COEF(), NPOLY, ROOTR(), ROOTI(), NFLG) STATIC

    ' Real and complex roots of a polynomial subroutine

    ' F(X) = COEF(NPOLY+1) * X ^ NPOLY + COEF(NPOLY) * X ^ (NPOLY-1)
    '        + COEF(NPOLY-1) * X ^ (NPOLY-2) + ... + COEF(1)

    ' Input

    '  COEF() = array of polynomial coefficents
    '  NPOLY  = order of the polynomial
    '           ( 1 <= order <= 36 )

    ' Output

    '  ROOTR() = array of real parts of roots
    '  ROOTI() = array of imaginary parts of roots
    '  NFLG    = error flag
    '            ( NFLG = 0 ==> no error )
    '            ( NFLG = 1 ==> iterations > 500 )

    DIM CTMP(NPOLY + 1)

    EPS = .000000000001#

    NFLG = 0
    IFLG = 0
   
    N = NPOLY
    N1 = N
    N2 = 1
    N3 = N + 1
    NP1 = N + 1

    FOR L = 1 TO NP1
        CTMP(NP1 - L + 1) = COEF(L)
    NEXT L

    DO
       XO = .00500101#
       YO = .01000101#

       IPASS = 0

POLY1:
       X = XO
       XO = -10# * YO
       YO = -10# * X
       X = XO
       Y = YO
       IPASS = IPASS + 1
       NITER = 0

POLY2:
       UX = 0#
       UY = 0#
       V = 0#
       YT = 0#
       XT = 1#
       U = CTMP(N + 1)

       IF (U = 0#) THEN
          X = 0#
          N1 = N1 - 1
          N3 = N3 - 1
          Y = 0#
          SS = 0#
          ALPHA = X
          N = N - 1
          GOTO POLY5
       END IF

       FOR I = 1 TO N
           TMP = CTMP(N - I + 1)
           XT1 = X * XT - Y * YT
           YT1 = X * YT + Y * XT
           U = U + TMP * XT1
           V = V + TMP * YT1
           UX = UX + I * XT * TMP
           UY = UY - I * YT * TMP
           XT = XT1
           YT = YT1
       NEXT I

       IF (ABS(UX) < 9.999999999999999D-21) THEN UX = 0#

       IF (ABS(UY) < 9.999999999999999D-21) THEN UY = 0#

       SS = UX ^ 2 + UY ^ 2

       IF (SS = 0#) THEN
          IF (IFLG <> 0) THEN
             X = XSAVED
             Y = YSAVED
             GOTO POLY4
          ELSE
             GOTO POLY1
          END IF
       END IF

       DX = (V * UY - U * UX) / SS
       X = X + DX
       DY = -(U * UY + V * UX) / SS
       Y = Y + DY

       IF ((ABS(DY) + ABS(DX)) < EPS) THEN GOTO POLY3

       NITER = NITER + 1

       IF (NITER < 500) THEN GOTO POLY2

       IF (IFLG <> 0) THEN GOTO POLY3

       IF (IPASS >= 5) THEN
          NFLG = 1
          EXIT DO
       ELSE
          GOTO POLY1
       END IF

POLY3:
       FOR L = 1 TO N3
           TMP = COEF(NP1 - L + 1)
           COEF(NP1 - L + 1) = CTMP(L)
           CTMP(L) = TMP
       NEXT L

       ITEMP = N
       N = N1
       N1 = ITEMP

       IF (IFLG <> 0) THEN
          GOTO POLY4
       ELSE
          IFLG = 1
          XSAVED = X
          YSAVED = Y
          NITER = 0
          GOTO POLY2
       END IF

POLY4:
       IFLG = 0

       IF (ABS(Y) >= EPS * ABS(X)) THEN
          ALPHA = X + X
          SS = X ^ 2 + Y ^ 2
          N = N - 2
       ELSE
          Y = 0#
          SS = 0#
          ALPHA = X
          N = N - 1
       END IF

POLY5:
       CTMP(2) = CTMP(2) + ALPHA * CTMP(1)

       FOR L = 2 TO N
           CTMP(L + 1) = CTMP(L + 1) + ALPHA * CTMP(L) - SS * CTMP(L - 1)
       NEXT L

       DO
          ROOTI(N2) = Y
          ROOTR(N2) = X

          N2 = N2 + 1

          IF (SS <> 0#) THEN
             Y = -Y
             SS = 0#
          ELSE
             EXIT DO
          END IF
       LOOP

       IF (N <= 0) THEN EXIT DO
    LOOP

    ERASE CTMP

END SUB

