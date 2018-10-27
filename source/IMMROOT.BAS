' Copyright 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB IMMROOT (XMIN, XMAX, TOL, IER, MAXIT, NORDER) STATIC

    ' Improved memory method root-finding subroutine

    ' Input

    '  XMIN   = initial estimate of left side of bracketing interval
    '  XMAX   = initial estimate of right side of bracketing interval
    '  TOL    = convergence tolerance
    '  MAXIT  = maximum number of iterations permitted
    '  NORDER = algorithm order ( NORDER >= 2 )
   
    ' Output

    '  XMIN  = estimate of the function root
    '  MAXIT = actual number of iterations performed
    '  IER   = error flag
    '      0 = no error; solution found
    '      1 = XMIN and XMAX do not bracket a root
    '      2 = number of function evaluations >= MAXIT

    ' Note: requires SUB USERFUNC

    DIM A(11), B(11), PSI(11), X(11), Y(11)

    ' machine epsilon

    EPS = 2.220446049250313D-16

    ' initialize number of iterations

    IT = 0

    ' reverse bracket if necessary

    IF (XMIN > XMAX) THEN
       XMID = XMAX
       XMAX = XMIN
       XMIN = XMID
    END IF

    ' function evaluations

    CALL USERFUNC(XMIN, YMIN)
    CALL USERFUNC(XMAX, YMAX)

    ' check for zero at either endpoint

    IF (YMAX = 0#) THEN
       XMIN = XMAX
       GOTO EXITSUB
    ELSEIF (YMIN = 0#) THEN
       GOTO EXITSUB
    END IF

    ' check for proper bracketing interval

    IF (YMAX * YMIN > 0#) THEN
       IER = 1
       GOTO EXITSUB
    END IF

    IT = 2

    ' begin main iteration

    DO
       X(1) = XMIN
       Y(1) = YMIN
       X(2) = XMAX
       Y(2) = YMAX
   
       ERATIO = 0#

       FOR N = 2 TO 10
           IBISEC = 0
           LB = N - (NORDER - 2)
           IF (LB < 2) THEN LB = 2
           A(LB - 1) = X(LB - 1)
       
           FOR K = LB TO N
               IF (LB = 2 AND K < N) THEN
                  ' NULL
               ELSE
                  B(LB - 1) = X(K)
                  FOR I = LB TO K
                      DIV = B(I - 1) - A(I - 1)
                      IF (DIV = 0#) THEN GOTO IMM10
                      B(I) = (Y(K) - Y(I - 1)) / DIV
                  NEXT I
                  A(K) = B(K)
               END IF
           NEXT K

           PSI(N) = A(N)

           FOR I = N TO LB STEP -1
               DIV = PSI(I)
               IF (DIV = 0#) THEN GOTO IMM10
               PSI(I - 1) = A(I - 1) - Y(I - 1) / PSI(I)
           NEXT I

           X(N + 1) = PSI(LB - 1)

           ' check if outside bracketing interval

           IF ((X(N + 1) < XMIN) OR (X(N + 1) > XMAX)) THEN GOTO IMM10

           D1 = ABS(X(N + 1) - X(N))
           D2 = ABS(X(N) - X(N - 1))

           IF (D1 <> 0#) THEN
              TERATIO = D2 / D1
              IF (TERATIO > ERATIO) THEN ERATIO = TERATIO
           END IF

           IF ((N >= 3) AND (ERATIO < 2#)) THEN GOTO IMM10
             
           CALL USERFUNC(X(N + 1), Y(N + 1))
           IT = IT + 1
           GOTO IMM20
IMM10:
           ' switch to bisection method

           X(N + 1) = XMIN + (XMAX - XMIN) / 2#
        
           CALL USERFUNC(X(N + 1), Y(N + 1))

           IT = IT + 1

           IBISEC = 1
IMM20:
           IF (Y(N + 1) = 0#) THEN
              XMIN = X(N + 1)
              IER = 0
              MAXIT = IT
              EXIT DO
           END IF

           IF (Y(N + 1) * YMIN >= 0#) THEN
              XMIN = X(N + 1)
              YMIN = Y(N + 1)
           ELSE
              XMAX = X(N + 1)
              YMAX = Y(N + 1)
           END IF

           ' check for convergence

           DIST = ABS(X(N + 1) - X(N))

           IF ((ABS(X(N + 1) - X(N - 1))) < DIST) THEN
              DIST = ABS(X(N + 1) - X(N - 1))
           END IF

           DELTA = (5# * EPS + TOL) * ABS(X(N + 1))

           IF ((DIST > DELTA) AND (ABS(Y(N + 1)) > TOL)) THEN GOTO IMM30

           XMID = X(N + 1)

           IF (ABS(XMAX - XMIN) < DELTA) THEN
              XMIN = X(N + 1)
              IER = 0
              EXIT DO
           END IF

           IF (XMID = XMAX) THEN DELTA = -DELTA

           IT = IT + 1

           CALL USERFUNC(XMID + DELTA, FTMP)

           IF (FTMP * Y(N + 1) > 0#) GOTO IMM30

           XMIN = X(N + 1)

           IER = 0
           EXIT DO
IMM30:
           IF (IT >= MAXIT) THEN
              IER = 2
              XMIN = X(N + 1)
              EXIT DO
           END IF

           IF (IBISEC = 1 OR N >= 10) THEN EXIT FOR

       NEXT N
    LOOP

EXITSUB:
    ' set iterations required
   
    MAXIT = IT
   
    ' erase working arrays
   
    ERASE A, B, PSI, X, Y

END SUB

