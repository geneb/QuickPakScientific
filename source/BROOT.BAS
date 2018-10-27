' Copyright (c) 1989, 1990, 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FVAL)

SUB BROOT (XI, DX, XMULT, DXMAX, NROOT, XB1, XB2, IER) STATIC

    ' Root bracketing subroutine

    ' Input

    '  XI    = initial X search value
    '  DX    = initial X step size ( + forward, - backward)
    '  XMULT = X step size multiplier ( XMULT > 0 )
    '  DXMAX = maximum bracketing rectification interval
    '  NROOT = maximum number of bracketing iterations

    ' Output

    '  XB1 = first X bracket value
    '  XB2 = second X bracket value
    '  IER = error flag
    '    0 = no error
    '    1 = iterations > NROOT

    ' NOTE: requires subroutine USERFUNC.BAS

    IER = 0
    NITER = 0
   
    CALL USERFUNC(XI, F1)

    X2 = XI + DX
    X3 = XI

    DO
       NITER = NITER + 1

       ' increment

       X2 = X2 + XMULT * (X2 - X3)

       ' evaluate

       CALL USERFUNC(X2, F2)
   
       ' rectify, if necessary

       IF (ABS(X2 - X3) > DXMAX) THEN X3 = X2 - DX

    LOOP UNTIL (((F1 * F2) < 0#) OR (NITER > NROOT))

    XB1 = X3
    XB2 = X2

    IF (NITER > NROOT) THEN IER = 1

END SUB

