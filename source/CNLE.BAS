' Copyright 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X(), FX())
DECLARE SUB JACOBIAN (X(), DY())
DECLARE SUB LINEAR1 (N, A(), B(), X(), IER)

SUB CNLE (N, X(), ICON(), TOL, MAXIT, IDER, Y(), INFO, NITER) STATIC

    ' Solve system of constrained non-linear equations subroutine

    ' Input

    '  N      = number of equations and unknowns (N >=2)
    '  X()    = input as initial guess and output as the solution
    '           (1 column by N rows)
    '  ICON() = variable constraint type (1 column by 3 rows)
    '       0 = variable is not constrained
    '       1 = variable is "physically" constrained
    '       2 = variable is "absolutely" constrained
    '  TOL    = convergence tolerance
    '  MAXIT  = maximum number of iterations allowed
    '  IDER   = Jacobian calculation flag
    '       1 = analytic Jacobian supplied by user
    '       2 = numerical Jacobian calculated by software

    ' Output

    '  Y()   = array of residuals (1 column by N rows)
    '  INFO  = information indicator
    '      0 = solution found
    '      1 = improper input parameters
    '      2 = initial estimate of solution does not satisfy constraints
    '      3 = number of iterations >= MAXIT
    '      4 = DTHETA has reached a minimal value
    '      5 = iteration is not making progress or is diverging
    '  NITER = actual number of iterations

    ' Note: requires SUB USERFUNC, SUB JACOBIAN and SUB LINEAR1

    REDIM X0(N), Y0(N), XKEEP(N), XJTY(N), DDY(N), XJTJKEP(N, N)
    REDIM DY(N, N), TVEC(N), TARRAY(N, N)

    ' initilization

    EPS = 2.20446049250313D-16
    XINER = 0#
    NITER = 0
    OLDSUM = 1D+100
    INFO = 1

    ' check for proper inputs

    IF ((N < 2) OR (TOL < 0#) OR (MAXIT < 0)) THEN GOTO CNLE4

    IF (TOL < SQR(EPS)) THEN TOL = SQR(EPS)

    FOR I = 1 TO N
        IF ((ICON(I) > 0) AND (X(I) < 0#)) THEN
           INFO = 2
           GOTO CNLE4
        END IF
    NEXT I

    CALL USERFUNC(X(), Y0())

    FOR I = 1 TO N
        XKEEP(I) = X(I)
        X0(I) = X(I)
        XINER = XINER + Y0(I) * Y0(I)
    NEXT I

    THETA = 1#
    ALPHA = .5#
    DTHETA = 1#
    NEWTH = 0
    NS = 1

CNLE1:
    IF (DTHETA / 10# < .09#) THEN
       INFO = 5
       GOTO CNLE4
    END IF
CNLE2:
    IF (NEWTH = 0) THEN
       ' NULL
    ELSE
       THETA = 1#
       ALPHA = .5#
       DTHETA = DTHETA / 10#
       NS = FIX(THETA / DTHETA + .1#)
       NEWTH = 0

       FOR I = 1 TO N
           X(I) = X0(I)
       NEXT I

       IF (DTHETA < .001#) THEN
          INFO = 4
          GOTO CNLE4
       END IF
    END IF

    FOR II = 1 TO NS
        THETA = THETA - DTHETA
        IF (THETA < 0#) THEN THETA = 0#
       
        OLDSUM = 1D+100
        INIT = 0
CNLE3:
        NITER = NITER + 1
        INIT = INIT + 1
       
        CALL USERFUNC(X(), Y())
       
        ERSUM = 0#
       
        FOR I = 1 TO N
            ERSUM = ERSUM + (Y(I) - THETA * Y0(I)) ^ 2
        NEXT I

        IF (ABS(ERSUM - OLDSUM) < .001# * ERSUM) THEN NEWTH = 1

        IF (NEWTH = 1) THEN GOTO CNLE1

        IF ((ERSUM < 10# * OLDSUM) AND (ALPHA < 1#)) THEN ALPHA = 2# * ALPHA

        IF (ERSUM > OLDSUM) THEN ALPHA = ALPHA / 4#

        IF (ERSUM < 1000# * OLDSUM) THEN
           ' NULL
        ELSE
           ALPHA = ALPHA / 4#

           FOR I = 1 TO N
               X(I) = X(I) - T * XJTY(I)
           NEXT I

           CALL USERFUNC(X(), Y())
        END IF

        IF (ALPHA < .001#) THEN NEWTH = 1
          
        IF (NEWTH = 1) THEN GOTO CNLE1

        OLDSUM = ERSUM

        IF ((THETA > 0#) OR (ERSUM > XINER)) THEN
           ' NULL
        ELSE
           XINER = ERSUM

           FOR I = 1 TO N
               XKEEP(I) = X(I)
           NEXT I
        END IF

        IF (NITER > MAXIT) THEN INFO = 3
          
        IF (INFO = 3) THEN GOTO CNLE4

        IF (IDER = 1) THEN
           ' analytic Jacobian
           CALL JACOBIAN(X(), DY())
        ELSE
           ' divided difference Jacobian
           FOR I = 1 TO N
               TMP = ABS(X(I) * SQR(EPS))
               DEL = .0000000001#
               IF (TMP > DEL) THEN DEL = TMP
               X(I) = X(I) + DEL
               CALL USERFUNC(X(), DDY())
               FOR J = 1 TO N
                   DY(J, I) = (DDY(J) - Y(J)) / DEL
               NEXT J

               X(I) = X(I) - DEL
           NEXT I
        END IF

        XNORM = 1#

        FOR I = 1 TO N
            DDY(I) = 1#
            FOR J = 1 TO N
                TMP = ABS(DY(I, J))
                IF (TMP > DDY(I)) THEN DDY(I) = TMP
            NEXT J
        NEXT I

        FOR I = 1 TO N
            Y(I) = -(Y(I) - THETA * Y0(I)) / DDY(I)
            IF (DDY(I) > XNORM) THEN XNORM = DDY(I)
            XJTY(I) = Y(I)
            FOR J = 1 TO N
                DY(I, J) = DY(I, J) / DDY(I)
                XJTJKEP(I, J) = DY(I, J)
            NEXT J
        NEXT I

        FOR I = 1 TO N
            FOR J = 1 TO N
                TARRAY(I, J) = DY(I, J)
            NEXT J
        NEXT I

        CALL LINEAR1(N, TARRAY(), XJTY(), TVEC(), KS)

        FOR I = 1 TO N
            XJTY(I) = TVEC(I)
        NEXT I

        IBIG = 0

        FOR I = 1 TO N
            XINTR = ABS(X(I))
            IF (XINTR < .1#) THEN XINTR = .1#
            IF (ABS(XJTY(I)) > 10# * ALPHA * XINTR) THEN IBIG = 1
        NEXT I

        IF ((KS = 0) AND (IBIG = 0)) THEN
           ' NULL
        ELSE
           ' Levenberg-Marquardt correction vector
           FOR I = 1 TO N
               FOR J = 1 TO N
                   DY(I, J) = XJTJKEP(I, J)
               NEXT J
           NEXT I

           FOR I = 1 TO N
               FOR J = 1 TO N
                   SUM = 0#
                   FOR K = 1 TO N
                       SUM = SUM + DY(K, I) * DY(K, J)
                   NEXT K
                   XJTJKEP(I, J) = SUM
                   IF (I = J) THEN XJTJKEP(I, J) = XNORM / DDY(I) + SUM
               NEXT J
           NEXT I

           FOR I = 1 TO N
               SUM = 0#
               FOR J = 1 TO N
                   SUM = SUM + DY(J, I) * Y(J)
               NEXT J
               XJTY(I) = SUM
           NEXT I
       
           FOR I = 1 TO N
               FOR J = 1 TO N
                   TARRAY(I, J) = XJTJKEP(I, J)
               NEXT J
           NEXT I

           CALL LINEAR1(N, TARRAY(), XJTY(), TVEC(), KS)

           FOR I = 1 TO N
               XJTY(I) = TVEC(I)
           NEXT I
        END IF

        T = 1#

        FOR I = 1 TO N
            XINTR = 10# * ABS(X(I))
            IF (XINTR < 1#) THEN XINTR = 1#
            IF (ABS(XJTY(I)) > 0#) THEN
               TMP = ABS(ALPHA * XINTR / XJTY(I))
               IF (TMP < T) THEN T = TMP
            END IF
            IF ((ICON(I) <> 2) OR (XJTY(I) >= 0#)) THEN
               ' NULL
            ELSE
               TMP = .99# * ABS(X(I) / XJTY(I))
               IF (TMP < T) THEN T = TMP
            END IF
        NEXT I

        CORSUM = 0#

        FOR I = 1 TO N
            X(I) = X(I) + T * XJTY(I)
            IF (ABS(X(I)) >= TOL) THEN
               CORSUM = CORSUM + (XJTY(I) / X(I)) ^ 2
            ELSE
               CORSUM = CORSUM + XJTY(I) ^ 2
            END IF
        NEXT I

        IF (SQR(CORSUM) > TOL) THEN GOTO CNLE3

    NEXT II

    CALL USERFUNC(X(), Y())

    ERSUM = 0#

    FOR I = 1 TO N
        IF ((ICON(I) = 1) AND (X(I) < 0#)) THEN NEWTH = 1
        ERSUM = ERSUM + (Y(I) / DDY(I)) ^ 2
    NEXT I

    IF (NEWTH = 1) THEN GOTO CNLE2

    IF (SQR(ERSUM) > 10# * TOL) THEN NEWTH = 1
      
    IF (NEWTH = 1) THEN GOTO CNLE1

    INFO = 0

CNLE4:

    ERASE X0, Y0, XKEEP, XJTY, DDY, XJTJKEP, DY, TVEC, TARRAY

END SUB

