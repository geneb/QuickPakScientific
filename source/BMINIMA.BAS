' Copyright (c) 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB BMINIMA (XI, DX, XMULT, NBRAC, XB1, XB2, IER) STATIC

    ' Minima bracketing subroutine

    ' Input

    '  XI    = initial X search value
    '  DX    = X step size
    '  NOTE: DX > 0 ==> search forward
    '        DX < 0 ==> search backward
    '  XMULT = X step size multiplier
    '  NBRAC = maximum number of bracketing iterations

    ' Output

    '  XB1 = first X bracket value
    '  XB2 = second X bracket value
    '  IER = error flag
    '    0 = no error
    '    1 = iterations > NBRAC
   
    ' NOTE: requires SUB USERFUNC

    IER = 0
    NITER = 0

    XNM2 = XI
    DXSAVED = DX

    CALL USERFUNC(XNM2, FXNM2)

    XNM1 = XNM2 + DXSAVED

    CALL USERFUNC(XNM1, FXNM1)

    N = 2

    DO
       NITER = NITER + 1

       DXSAVED = XMULT * DXSAVED

       XN = XNM1 + DXSAVED

       CALL USERFUNC(XN, FXN)

       IF (FXNM2 > FXNM1) AND (FXNM1 < FXN) OR (NITER > NBRAC) THEN
          IF (DXSAVED > 0#) THEN
             XB1 = XNM2
             XB2 = XN
          ELSE
             XB1 = XN
             XB2 = XNM2
          END IF
          IF (NITER > NBRAC) THEN IER = 1
          EXIT DO
       END IF

       ' save X(N-2) value

       IF ((N / 2) = FIX(N / 2)) THEN XNM2 = XNM1

       XNM1 = XN

       FXNM2 = FXNM1
       FXNM1 = FXN

       N = N + 1
       
    LOOP

END SUB

