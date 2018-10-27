' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB MINIMA1 (X1, DX, XMULT, XTOL, NITER, XMIN, FMIN) STATIC

    ' Single variable minimization subroutine

    ' Input

    '  X1    = initial guess for solution
    '  DX    = search step size for X
    '  XMULT = step size multiplier for X
    '  XTOL  = convergence criteria

    ' Output

    '  NITER = number of iterations
    '  XMIN  = minmax solution value
    '  FMIN  = minmax function value

    ' NOTE: requires SUB USERFUNC

    DIM X(3), F(3)

    IPASS = 0
    ITERATE = 1

    CALL USERFUNC(X1, F1)

    X2 = X1 + DX

    DO
       CALL USERFUNC(X2, F2)

       IF (F2 - F1) < 0# THEN
          IF (IPASS - 1) <= 0 THEN
             IPASS = IPASS + 1
             DX = -DX
             X2 = X1 + DX
          ELSE
             X(1) = X1
             X(2) = X1 + .5# * DX
             X(3) = X2
             ITERATE = 0
             EXIT DO
          END IF
       ELSE
          X3 = X2 + DX
          EXIT DO
       END IF
    LOOP

    IF ITERATE = 1 THEN
       DO
          CALL USERFUNC(X3, F3)

          DX = XMULT * DX

          X4 = X3 + DX

          CALL USERFUNC(X4, F4)

          IF ((F4 - F3) < 0#) THEN
             X(1) = X3
             X(2) = X3 + .5# * DX
             X(3) = X4
             EXIT DO
          ELSE
             X3 = X4
          END IF
       LOOP
    END IF

    FOR I = 1 TO 3
        CALL USERFUNC(X(I), F(I))
    NEXT I

    NITER = 0

    DO
       NITER = NITER + 1
       A = X(2) - X(3)
       B = X(3) - X(1)
       C = X(1) - X(2)
       D = X(2) ^ 2 - X(3) ^ 2
       E = X(3) ^ 2 - X(1) ^ 2
       F = X(1) ^ 2 - X(2) ^ 2

       XNUM = .5# * (D * F(1) + E * F(2) + F * F(3))
       XDEN = A * F(1) + B * F(2) + C * F(3)
       XSTAR = XNUM / XDEN

       CALL USERFUNC(XSTAR, FSTAR)

       FOR I = 1 TO 3
           IF ABS(X(I) - XSTAR) <= XTOL THEN
              XMIN = X(I)
              CALL USERFUNC(XMIN, FMIN)
              FMIN = -FMIN
              EXIT DO
           END IF
       NEXT I

       FMIN = 1D+30

       FOR I = 1 TO 3
           IF F(I) < FMIN THEN
              FMIN = F(I)
              ISTAR = I
           END IF
       NEXT I

       X(ISTAR) = XSTAR
       F(ISTAR) = FSTAR
    LOOP

    ERASE X, F

END SUB

