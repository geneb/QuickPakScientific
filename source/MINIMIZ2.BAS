' Copyright (c) 1988, 1989, 1990, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB NUMGRAD1 (N, X(), FX, GX())

SUB MINIMIZ2 (METHOD, N, EPS, MAXITER, IFLAG, NITER, F, X()) STATIC

    ' Multivariable minimization subroutine

    ' Numerical gradient method

    ' Input

    '  METHOD  = method of solution ( 1 = conjugate gradient,
    '                                 2 = quasi-Newton )
    '  N       = number of variables
    '  EPS     = convergence criteria
    '  MAXITER = maximum number of iterations
    '  IFLAG   = diagnostic flag
    '        0 = converged
    '        1 = maximum number of function evaluations
    '        2 = linear search failure
    '        3 = search vector failure
    '  X()     = initial guess for solution vector

    ' Output

    '  NITER = number of algorithm iterations
    '  F     = final objective function value
    '  X()   = final solution vector

    ' NOTE: requires SUB NUMGRAD1

    REDIM GRADIENT(N)

    IF (METHOD = 1) THEN
       REDIM WORK(5 * N + 2)
    ELSE
       REDIM WORK(N * (N + 7) / 2)
    END IF

    NITER = 0
    IFUN = 0
    IFLAG = 0

    NX = N
    NG = NX + N

    IF (METHOD = 1) THEN
       NRY = NG + N
       NRD = NRY + N
       NCONS = 5 * N
       NCONS1 = NCONS + 1
       NCONS2 = NCONS + 2
    ELSE
       NCONS = 3 * N
    END IF

ILOOP1:

    CALL NUMGRAD1(N, X(), F, GRADIENT())

    NRST = N
    NRSW = 1

    DG1 = 0#
    XSQ = 0#

    FOR I = 1 TO N
        WORK(I) = -GRADIENT(I)
        XSQ = XSQ + X(I) ^ 2
        DG1 = DG1 - GRADIENT(I) ^ 2
    NEXT I

    GSQ = -DG1

    XMAX = 1#
    IF (XSQ > XMAX) THEN XMAX = XSQ

    IF (GSQ <= EPS * EPS * XMAX) THEN GOTO EXITSUB

ILOOP2:

    FMIN = F

    NCALLS = IFUN

    ALPHA = ALPHA * DG / DG1

    IF (NRST = 1 OR METHOD = 2) THEN ALPHA = 1#

    IF (NRSW = 1) THEN ALPHA = 1# / SQR(GSQ)

    AP = 0#
    FP = FMIN
    DP = DG1

    DG = DG1

    NITER = NITER + 1

    STP = 0#

    FOR I = 1 TO N
        STP = STP + WORK(I) * WORK(I)
        NXPI = NX + I
        NGPI = NG + I
        WORK(NXPI) = X(I)
        WORK(NGPI) = GRADIENT(I)
    NEXT I

    STP = SQR(STP)

ILOOP3:

    IF (ALPHA * STP) <= 9.999999999999999D-21 THEN
       IF (NRSW <> 1) THEN
          GOTO ILOOP1
       ELSE
          IFLAG = 2
          GOTO EXITSUB
       END IF
    END IF

    FOR I = 1 TO N
        NXPI = NX + I
        X(I) = WORK(NXPI) + ALPHA * WORK(I)
    NEXT I

    CALL NUMGRAD1(N, X(), F, GRADIENT())

    IFUN = IFUN + 1

    IF (IFUN > MAXITER) THEN
       IFLAG = 1
       GOTO EXITSUB
    END IF

    DAL = 0#

    FOR I = 1 TO N
        DAL = DAL + GRADIENT(I) * WORK(I)
    NEXT I

    IF (F > FMIN AND DAL < 0#) THEN
       ALPHA = ALPHA / 3#
       AP = 0#
       FP = FMIN
       DP = DG
       GOTO ILOOP3
    END IF

    IF (F > (FMIN + .0001# * ALPHA * DG) OR ABS(DAL / DG) > .9#) OR ((IFUN - NCALLS) <= 1 AND ABS(DAL / DG) > EPS AND METHOD = 1) THEN
       GOTO ILOOP4
    ELSE
       GOTO ILOOP7
    END IF

ILOOP4:

    U1 = DP + DAL - 3# * (FP - F) / (AP - ALPHA)
    U2 = U1 * U1 - DP * DAL
    IF (U2 < 0#) THEN U2 = 0#
    U2 = SQR(U2)
    AT = ALPHA - (ALPHA - AP) * (DAL + U2 - U1) / (DAL - DP + 2# * U2)

    IF (DAL / DP) > 0# THEN GOTO ILOOP5

    AMIN = ALPHA
    IF (AP < AMIN) THEN AMIN = AP

    AMAX = ALPHA
    IF (AP > AMAX) THEN AMAX = AP

    IF (AT < (1.01# * AMIN)) OR AT > (.99# * AMAX) THEN AT = .5# * (ALPHA + AP)

    GOTO ILOOP6

ILOOP5:

    AMIN = ALPHA
    IF (AP < AMIN) THEN AMIN = AP

    IF (DAL > 0# AND 0# < AT AND AT < (.99# * AMIN)) THEN GOTO ILOOP6

    AMAX = ALPHA
    IF (AP > AMAX) THEN AMAX = AP

    IF (DAL <= 0# AND AT > (1.01# * AMAX)) THEN GOTO ILOOP6

    IF (DAL <= 0#) THEN
       AT = 2# * AMAX
    ELSE
       AT = .5# * AMIN
    END IF

ILOOP6:

    AP = ALPHA
    FP = F
    DP = DAL
    ALPHA = AT
    GOTO ILOOP3

ILOOP7:

    GSQ = 0#
    XSQ = 0#

    FOR I = 1 TO N
        GSQ = GSQ + GRADIENT(I) ^ 2
        XSQ = XSQ + X(I) ^ 2
    NEXT I

    XMAX = 1#
    IF (XSQ > XMAX) THEN XMAX = XSQ

    IF (GSQ <= EPS ^ 2 * XMAX) THEN GOTO EXITSUB

    FOR I = 1 TO N
        WORK(I) = ALPHA * WORK(I)
    NEXT I

    IF (METHOD = 1) THEN
       RTST = 0#

       FOR I = 1 TO N
           NGPI = NG + I
           RTST = RTST + GRADIENT(I) * WORK(NGPI)
       NEXT I

       IF (ABS(RTST / GSQ) > .2#) THEN NRST = N

       IF (NRST = N) THEN
          WORK(NCONS + 1) = 0#
          WORK(NCONS + 2) = 0#

          FOR I = 1 TO N
              NRDPI = NRD + I
              NRYPI = NRY + I
              NGPI = NG + I
              WORK(NRYPI) = GRADIENT(I) - WORK(NGPI)
              WORK(NRDPI) = WORK(I)
              WORK(NCONS1) = WORK(NCONS1) + WORK(NRYPI) * WORK(NRYPI)
              WORK(NCONS2) = WORK(NCONS2) + WORK(I) * WORK(NRYPI)
          NEXT I
       END IF

       U1 = 0#
       U2 = 0#

       FOR I = 1 TO N
           NRDPI = NRD + I
           NRYPI = NRY + I
           U1 = U1 - WORK(NRDPI) * GRADIENT(I) / WORK(NCONS1)
           U2 = U2 + WORK(NRDPI) * GRADIENT(I) * 2# / WORK(NCONS2) - WORK(NRYPI) * GRADIENT(I) / WORK(NCONS1)
       NEXT I

       U3 = WORK(NCONS2) / WORK(NCONS1)

       FOR I = 1 TO N
           NXPI = NX + I
           NRDPI = NRD + I
           NRYPI = NRY + I
           WORK(NXPI) = -U3 * GRADIENT(I) - U1 * WORK(NRYPI) - U2 * WORK(NRDPI)
       NEXT I

       IF (NRST <> N) THEN
          U1 = 0#
          U2 = 0#
          U3 = 0#
          U4 = 0#

          FOR I = 1 TO N
              NGPI = NG + I
              NRDPI = NRD + I
              NRYPI = NRY + I
              U1 = U1 - (GRADIENT(I) - WORK(NGPI)) * WORK(NRDPI) / WORK(NCONS1)
              U2 = U2 - (GRADIENT(I) - WORK(NGPI)) * WORK(NRYPI) / WORK(NCONS1) + 2# * WORK(NRDPI) * (GRADIENT(I) - WORK(NGPI)) / WORK(NCONS2)
              U3 = U3 + WORK(I) * (GRADIENT(I) - WORK(NGPI))
          NEXT I

          STP = 0#

          FOR I = 1 TO N
              NGPI = NG + I
              NRDPI = NRD + I
              NRYPI = NRY + I
              STP = (WORK(NCONS2) / WORK(NCONS1)) * (GRADIENT(I) - WORK(NGPI)) + U1 * WORK(NRYPI) + U2 * WORK(NRDPI)
              U4 = U4 + STP * (GRADIENT(I) - WORK(NGPI))
              WORK(NGPI) = STP
          NEXT I

          U1 = 0#
          U2 = 0#

          FOR I = 1 TO N
              U1 = U1 - WORK(I) * GRADIENT(I) / U3
              NGPI = NG + I
              U2 = U2 + (1# + U4 / U3) * WORK(I) * GRADIENT(I) / U3 - WORK(NGPI) * GRADIENT(I) / U3
          NEXT I

          FOR I = 1 TO N
              NGPI = NG + I
              NXPI = NX + I
              WORK(NXPI) = WORK(NXPI) - U1 * WORK(NGPI) - U2 * WORK(I)
          NEXT I
       END IF

       DG1 = 0#

       FOR I = 1 TO N
           NXPI = NX + I
           WORK(I) = WORK(NXPI)
           DG1 = DG1 + WORK(I) * GRADIENT(I)
       NEXT I

       IF (DG1 <= 0#) THEN
          IF (NRST = N) THEN NRST = 0
          NRST = NRST + 1
          NRSW = 0
          GOTO ILOOP2
       ELSE
          IFLAG = 3
          GOTO EXITSUB
       END IF

    END IF

    U1 = 0#

    FOR I = 1 TO N
        NGPI = NG + I
        WORK(NGPI) = GRADIENT(I) - WORK(NGPI)
        U1 = U1 + WORK(I) * WORK(NGPI)
    NEXT I

    IF (NRSW = 1) THEN
       U2 = 0#

       FOR I = 1 TO N
           NGPI = NG + I
           U2 = U2 + WORK(NGPI) ^ 2
       NEXT I

       IJ = 1
       U3 = U1 / U2

       FOR I = 1 TO N
           FOR J = I TO N
               NCONS1 = NCONS + IJ
               WORK(NCONS1) = 0#
               IF (I = J) THEN WORK(NCONS1) = U3
               IJ = IJ + 1
           NEXT J
           NXPI = NX + I
           NGPI = NG + I
           WORK(NXPI) = U3 * WORK(NGPI)
       NEXT I

       U2 = U3 * U2
    ELSE
       U2 = 0#

       FOR I = 1 TO N
           U3 = 0#
           IJ = I
           IF (I <> 1) THEN
              II = I - 1
              FOR J = 1 TO II
                  NGPJ = NG + J
                  NCONS1 = NCONS + IJ
                  U3 = U3 + WORK(NCONS1) * WORK(NGPJ)
                  IJ = IJ + N - J
              NEXT J
           END IF
           FOR J = I TO N
               NCONS1 = NCONS + IJ
               NGPJ = NG + J
               U3 = U3 + WORK(NCONS1) * WORK(NGPJ)
               IJ = IJ + 1
           NEXT J
           NGPI = NG + I
           U2 = U2 + U3 * WORK(NGPI)
           NXPI = NX + I
           WORK(NXPI) = U3
       NEXT I
    END IF

    U4 = 1# + U2 / U1

    FOR I = 1 TO N
        NXPI = NX + I
        NGPI = NG + I
        WORK(NGPI) = U4 * WORK(I) - WORK(NXPI)
    NEXT I

    IJ = 1

    FOR I = 1 TO N
        NXPI = NX + I
        U3 = WORK(I) / U1
        U4 = WORK(NXPI) / U1
        FOR J = I TO N
            NCONS1 = NCONS + IJ
            NGPJ = NG + J
            WORK(NCONS1) = WORK(NCONS1) + U3 * WORK(NGPJ) - U4 * WORK(J)
            IJ = IJ + 1
        NEXT J
    NEXT I

    DG1 = 0#

    FOR I = 1 TO N
        U3 = 0#
        IJ = I
        IF (I <> 1) THEN
           II = I - 1
           FOR J = 1 TO II
               NCONS1 = NCONS + IJ
               U3 = U3 - WORK(NCONS1) * GRADIENT(J)
               IJ = IJ + N - J
           NEXT J
        END IF
        FOR J = I TO N
            NCONS1 = NCONS + IJ
            U3 = U3 - WORK(NCONS1) * GRADIENT(J)
            IJ = IJ + 1
        NEXT J
        DG1 = DG1 + U3 * GRADIENT(I)
        WORK(I) = U3
    NEXT I

    IF (DG1 > 0#) THEN GOTO EXITSUB

    NRSW = 0
    GOTO ILOOP2

EXITSUB:
    ERASE GRADIENT, WORK

END SUB

