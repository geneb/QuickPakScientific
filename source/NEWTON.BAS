' Copyright 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX, FPX)

SUB NEWTON (X1, X2, TOL, MAXITER, ROOT, NITER) STATIC

    ' Solve non-linear equation subroutine

    ' Newton's Method

    ' Input

    '  X1      = initial X value of search interval
    '  X2      = final X value of search interval
    '  TOL     = convergence tolerance
    '  MAXITER = maximum number of iterations

    ' Output

    '  ROOT = root of non-linear equation
    '  NITER = number of iterations

    ' NOTE: requires SUB USERFUNC

    CALL USERFUNC(X1, FL, DF)
    CALL USERFUNC(X2, FH, DF)

    IF (FL < 0#) THEN
       XL = X1
       XH = X2
    ELSE
       XH = X1
       XL = X2
       TMP = FL
       FL = FH
       FH = TMP
    END IF

    ROOT = .5# * (X1 + X2)
    DXOLD = ABS(X2 - X1)
    DX = DXOLD

    CALL USERFUNC(ROOT, F, DF)

    NITER = 0

    DO
        NITER = NITER + 1
        IF ((((ROOT - XH) * DF - F) * ((ROOT - XL) * DF - F) >= 0#) OR (ABS(2# * F) > ABS(DXOLD * DF))) THEN
           DXOLD = DX
           DX = .5# * (XH - XL)
           ROOT = XL + DX
           IF (XL = ROOT) THEN EXIT DO
        ELSE
           DXOLD = DX
           DX = F / DF
           TEMP = ROOT
           ROOT = ROOT - DX
           IF (TEMP = ROOT) THEN EXIT DO
        END IF

        IF (ABS(DX) < TOL) OR (NITER > MAXITER) THEN EXIT DO

        CALL USERFUNC(ROOT, F, DF)

        IF (F < 0#) THEN
           XL = ROOT
           FL = F
        ELSE
           XH = ROOT
           FH = F
        END IF
    LOOP

END SUB

