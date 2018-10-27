' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB RKF4 (N, T, TN, Y(), EPS, SCAMIN, SCAMAX, IPRT)
DECLARE SUB SECANT (NSIG, MAXIT, FRACT, XPREV, X, Y, AIV, BIV, ALPHA, BETA, IEXIT)
DECLARE SUB USERFUNC (T, Y(), F())
DECLARE SUB XKMATRIX (J, N, T, Y(), H, XK())
DECLARE SUB ERROREST (N, T, Y(), H, ERREST, XK())
DECLARE SUB NEXTRKF (N, Y(), XK())
DECLARE SUB EOFX (X, E, AIV, BIV, ALPHA, BETA, IPRT)
DECLARE SUB KEYCHECK ()

SUB EOFX (X, E, AIV, BIV, ALPHA, BETA, IPRT) STATIC

    ' E(X) = BETA - Y[X](B) subroutine

    Y(1) = ALPHA
    Y(2) = X

    N = 2
    TOL = .000001#

    X1 = AIV
    X2 = BIV

    SCALEMIN = .1#
    SCALEMAX = 4#

    CALL RKF4(N, X1, X2, Y(), TOL, SCALEMIN, SCALEMAX, IPRT)

    E = BETA - Y(1)

END SUB

SUB ERROREST (N, T, Y(), H, ERREST, XK()) STATIC

    ' RKF4 error estimate subroutine

    FOR J = 1 TO 6
        CALL XKMATRIX(J, N, T, Y(), H, XK())
    NEXT J

    ERREST = 0#

    FOR I = 1 TO N
        XERR = XK(I, 1) / 360# - 128# * XK(I, 3) / 4275# - 2197# * XK(I, 4) / 75240# + XK(I, 5) / 50# + 2# * XK(I, 6) / 55#
        IF (ABS(XERR) > ERREST) THEN ERREST = ABS(XERR)
    NEXT I

END SUB

SUB NEXTRKF (N, Y(), XK()) STATIC

    ' RKF4 support subroutine

    FOR I = 1 TO N
        Y(I) = Y(I) + 25# * XK(I, 1) / 216# + 1408# * XK(I, 3) / 2565# + 2197# * XK(I, 4) / 4104# - XK(I, 5) / 5#
    NEXT I

END SUB

SUB RKF4 (N, T, TF, Y(), EPS, SCALEMIN, SCALEMAX, IPRT) STATIC

    ' Fourth-order Runge-Kutta-Fehlberg integration subroutine

    DIM XK(9, 6)

    RMAX = EPS / ABS(TF - T)

    H = SGN(TF - T) * RMAX ^ .25#

    HMIN = ABS(H) * .0000001#

    DO
       IF (ABS(H) >= ABS(TF - T)) THEN H = TF - T

       CALL ERROREST(N, T, Y(), H, ETMP, XK())

       RATIO = ETMP / ABS(H)
       
       IF (RATIO < RMAX) THEN
          IF (IPRT = 1) THEN
             ' print current values
             PRINT TAB(5); T; TAB(30); Y(1); TAB(55); Y(2)
          END IF
          T = T + H
          CALL NEXTRKF(N, Y(), XK())
          IF (IPRT = 1 AND T = TF) THEN
             ' print final values
             PRINT TAB(5); T; TAB(30); Y(1); TAB(55); Y(2)
          END IF
       END IF

       IF (T <> TF) THEN
          IF (RATIO = 0#) THEN
             SCALE = SCALEMAX
          ELSE
             SCALE = (.5# * RMAX / RATIO) ^ .25#
             AMAX1 = SCALE
             IF (SCALEMIN > AMAX1) THEN AMAX1 = SCALEMIN
             IF (SCALEMAX < AMAX1) THEN SCALE = SCALEMAX
          END IF
          H = SCALE * H
          IF (ABS(H) < HMIN) THEN EXIT DO
       END IF

       IF (T = TF) THEN EXIT DO
    LOOP

    ERASE XK

END SUB

SUB SECANT (NSIG, MAXIT, FRACT, XPREV, X, Y, AIV, BIV, ALPHA, BETA, IEXIT) STATIC

    ' Secant root finding subroutine

    CALL EOFX(X, Y, AIV, BIV, ALPHA, BETA, IPRT)

    CALL EOFX(XPREV, YPREV, AIV, BIV, ALPHA, BETA, IPRT)

    DXPREV = X - XPREV

    DXTOL = 10# ^ (-NSIG)

    YTMP = ABS(Y)

    IF (ABS(YPREV) < YTMP) THEN YTMP = ABS(YPREV)

    YTOL = YTMP * 10# ^ (-2 * NSIG)

    IEXIT = 4

    FOR K = 1 TO MAXIT
        IF (Y = YPREV) THEN
           IEXIT = 3
        ELSE
           DX = -Y * DXPREV / (Y - YPREV)
           XMAXDX = FRACT * ABS(X)
           IF (ABS(DX) > XMAXDX) THEN DX = SGN(DX) * XMAXDX
           X = X + DX
           DXPREV = DX
           YPREV = Y
           CALL EOFX(X, Y, AIV, BIV, ALPHA, BETA, IPRT)
           IF (ABS(DX) <= DXTOL * ABS(X)) THEN IEXIT = 1
           IF (ABS(Y) < YTOL) THEN IEXIT = 2
        END IF
        IF (IEXIT <> 4) THEN EXIT SUB
    NEXT K

END SUB

SUB SHOOT (AIV, ALPHA, BIV, BETA) STATIC

    ' Shooting method TPBVP subroutine
   
    ' Solves the second-order boundary value problem
    '  y'' = f(t, y, y') subject to the boundary
    '  conditions y(a) = alpha and y(b) = beta

    ' Input

    '  AIV   = initial time
    '  ALPHA = initial boundary value
    '  BIV   = final time
    '  BETA  = final boundary value

    ' Output

    '  Screen display of solution via subroutine EOFX

    X = .25# * (BETA - ALPHA) / (BIV - AIV)

    XPREV = 1.02# * X

    IF (X = 0#) THEN XPREV = .02#

    NSIG = 6
    MAXIT = 20
    FRACT = .3#

    CALL SECANT(NSIG, MAXIT, FRACT, XPREV, X, FOFX, AIV, BIV, ALPHA, BETA, IEXIT)

    IF (IEXIT <= 2) THEN
       ' display results
       CLS
       LOCATE 3, 1
       PRINT TAB(32); "Program DEMOBVP1"
       PRINT
       PRINT TAB(14); "< Solution of the Two-point Boundary Value Problem >"
       PRINT
       PRINT TAB(13); "Time"; TAB(39); "Y"; TAB(64); "Y'"
       PRINT
       IPRT = 1
       CALL EOFX(X, FOFX, AIV, BIV, ALPHA, BETA, IPRT)
       CALL KEYCHECK
    END IF

END SUB

SUB USERFUNC (T, Y(), F()) STATIC

    ' Evaluate function subroutine

    ' Input

    '  T = current time
    '  Y() = current variable vector

    ' Output

    '  F() = differential equations evaluated at T, Y()

    WOFX = 3#                ' cable weight per unit length (lbs/ft)
    TENSION = 8000#          ' cable tension (lbs)

    F(1) = Y(2)
    F(2) = -WOFX / TENSION

END SUB

SUB XKMATRIX (J, N, T, Y(), H, XK()) STATIC

    ' RKF4 support subroutine

    DIM YJ(9), FJ(9)

    IF (ICOEFF = 0) THEN
       DIM P(6)

       P(1) = 0#
       P(2) = .25#
       P(3) = .375#
       P(4) = 12# / 13#
       P(5) = 1#
       P(6) = .5#
    
       ICOEFF = 1
    END IF

    FOR I = 1 TO N
        SELECT CASE I
        CASE 1
           YJ(I) = Y(I)
        CASE 2
           YJ(I) = Y(I) + XK(I, 1) / 4#
        CASE 3
           YJ(I) = Y(I) + (3# * XK(I, 1) + 9# * XK(I, 2)) / 32#
        CASE 4
           YJ(I) = Y(I) + (1932# * XK(I, 1) - 7200# * XK(I, 2) + 7296# * XK(I, 3)) / 2197#
        CASE 5
           YJ(I) = Y(I) + 439# * XK(I, 1) / 216# - 8# * XK(I, 2) + 3680# * XK(I, 3) / 513# - 845# * XK(I, 4) / 4104#
        CASE 6
           YJ(I) = Y(I) - 8# * XK(I, 1) / 27# + 2# * XK(I, 2) - 3544# * XK(I, 3) / 2565# + 1859# * XK(I, 4) / 4104# - 11# * XK(I, 5) / 40#
        END SELECT
    NEXT I

    TTMP = T + P(J) * H

    CALL USERFUNC(TTMP, YJ(), FJ())

    FOR I = 1 TO N
        XK(I, J) = H * FJ(I)
    NEXT I

    ERASE YJ, FJ

END SUB

