' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X(), FX(), DFDX())
DECLARE SUB LINEAR1 (N, A(), B(), X(), IER)

SUB NLINEAR (N, MAXITER, XTOL, FTOL, NITER, X(), IER) STATIC

    ' Solution of a system of non-linear equations subroutine

    ' Newton's method

    ' Input

    '  N       = number of equations
    '  X()     = initial guess for solution vector
    '  MAXITER = maximum number of iterations
    '  XTOL    = convergence tolerance in X values
    '  FTOL    = convergence tolerance in function values

    ' Output

    '  NITER = number of algorithm iterations
    '  X()   = solution vector
    '  IER   = error flag ( 0 = no error, 1 = singular matrix )

    ' NOTE: requires SUB LINEAR1 and SUB USERFUNC

    DIM DFDX(N, N), FX(N), DX(N)

    FOR K = 1 TO MAXITER
        NITER = K
        CALL USERFUNC(X(), FX(), DFDX())

        FERR = 0#

        FOR I = 1 TO N
            FERR = FERR + ABS(FX(I))
        NEXT I

        ' check function tolerance

        IF (FERR <= FTOL) THEN GOTO EXITSUB

        CALL LINEAR1(N, DFDX(), FX(), DX(), IER)

        IF (IER = 1) THEN EXIT FOR

        XERR = 0#

        FOR I = 1 TO N
            XERR = XERR + ABS(FX(I))
            X(I) = X(I) + DX(I)
        NEXT I

        ' check x tolerance

        IF (XERR <= XTOL) THEN EXIT FOR
      NEXT K

EXITSUB:
      ERASE DFDX, FX, DX

END SUB

