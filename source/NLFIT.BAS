' Copyright (c) 1992 Crescent Software 

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB PARDIF (N, K, X(), P(), DIF())
DECLARE SUB FIND (N, EMATRIX(), BMATRIX(), D(), XINV(), GMATRIX(), P(), P1(), XMY)
DECLARE SUB SQUARESUM (M, P(), X(), Y(), SQRSUM)
DECLARE SUB EXPLORE (N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), EMATRIX(), CMATRIX(), BMATRIX(), IRESP)
DECLARE SUB DECOMPOSE (N, EMATRIX(), BMATRIX())
DECLARE SUB FILL (N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), IRESP)
DECLARE SUB USERFUNC (K, X(), P(), Y)
DECLARE SUB SCHECK (N, P(), POLD(), F, FOLD, ISIG)

SUB DECOMPOSE (N, EMATRIX(), BMATRIX()) STATIC

    ' Decompose eigenvector matrix subroutine

    EPS = .00000001#
    ITMAX = 10
    IT = 0
    NI = N * (N - 1) / 2
    KI = NI
                             
    DO
       IT = IT + 1
   
       FOR J = 1 TO N - 1
           FOR H = J + 1 TO N
               SCALPROD = 0#
               XJLONG = 0#
               XHLONG = 0#
              
               FOR I = 1 TO N
                   WIJ = BMATRIX(I, J)
                   WIH = BMATRIX(I, H)
                   SCALPROD = SCALPROD + WIJ * WIH
                   XJLONG = XJLONG + WIJ * WIJ
                   XHLONG = XHLONG + WIH * WIH
               NEXT I
              
               XLE = SQR(XHLONG * XJLONG)
              
               IF (XLE > 0#) THEN
                  AC = ABS(SCALPROD / XLE)
               ELSE
                  AC = 0#
               END IF

               SCALPROD = SCALPROD + SCALPROD
               WW = XJLONG - XHLONG
              
               IF (AC > EPS OR WW < 0#) THEN
                  IF (ABS(WW) >= ABS(SCALPROD)) THEN
                     TETA = ABS(SCALPROD / WW)
                     CTETA = 1# / SQR(1# + TETA * TETA)
                     STETA = TETA * CTETA
                  ELSE
                     TETA = ABS(WW / SCALPROD)
                     STETA = 1# / SQR(1# + TETA * TETA)
                     CTETA = STETA * TETA
                  END IF
                 
                  CTETA = SQR((1# + CTETA) / 2#)
                  STETA = STETA / (CTETA + CTETA)
                 
                  IF (WW < 0#) THEN
                     FLIP = CTETA
                     CTETA = STETA
                     STETA = FLIP
                  END IF
                 
                  IF (SCALPROD < 0#) THEN STETA = -STETA
            
                  FOR I = 1 TO N
                      FLIP = BMATRIX(I, J)
                      BMATRIX(I, J) = BMATRIX(I, H) * STETA + FLIP * CTETA
                      BMATRIX(I, H) = BMATRIX(I, H) * CTETA - FLIP * STETA
                  NEXT I
              
                  KI = NI
               ELSE
                  KI = KI - 1
               END IF
           NEXT H
       NEXT J
    LOOP UNTIL ((KI <= 0) OR (IT = ITMAX))

    FOR J = 1 TO N
        EMATRIX(J) = 0#
        FOR I = 1 TO N
            EMATRIX(J) = EMATRIX(J) + BMATRIX(I, J) * BMATRIX(I, J)
        NEXT I
        EMATRIX(J) = SQR(EMATRIX(J))
    NEXT J
    
    FOR J = 1 TO N
        IF (EMATRIX(J) <> 0#) THEN
           FOR I = 1 TO N
               BMATRIX(I, J) = BMATRIX(I, J) / EMATRIX(J)
           NEXT I
        END IF
    NEXT J

END SUB

SUB EXPLORE (N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), EMATRIX(), CMATRIX(), BMATRIX(), IRESP) STATIC

    CALL FILL(N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), IRESP)
   
    IF (IRESP) THEN
       FOR I = 1 TO N
           FOR J = I TO N
               CMATRIX(I, J) = XINFO(I, J) / D(I) / D(J)
               CMATRIX(J, I) = CMATRIX(I, J)
           NEXT J
       NEXT I
    
       FOR I = 1 TO N
           FOR J = 1 TO N
               BMATRIX(I, J) = CMATRIX(I, J)
           NEXT J
       NEXT I
    
       CALL DECOMPOSE(N, EMATRIX(), BMATRIX())
    END IF

END SUB

SUB FILL (N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), IRESP) STATIC

    ' Fill working matrices subroutine

    DIM DIF(M)

    IRESP = 1
      
    FOR I = 1 TO N
        FOR J = I TO N
            XINFO(I, J) = 0#
        NEXT J
        GMATRIX(I) = 0#
    NEXT I
      
    FOR K = 1 TO M
        CALL PARDIF(N, K, X(), P(), DIF())
        CALL USERFUNC(K, X(), P(), Y)
         
        DELTA = Y - Y(K)
          
        FOR I = 1 TO N
            FOR J = I TO N
                XINFO(I, J) = XINFO(I, J) + DIF(I) * DIF(J)
            NEXT J
            GMATRIX(I) = GMATRIX(I) + DIF(I) * DELTA
        NEXT I
    NEXT K
      
    FOR I = 1 TO N
        D(I) = SQR(XINFO(I, I))
        IF (D(I) = 0#) THEN IRESP = 0
        FOR J = 1 TO I - 1
            XINFO(I, J) = XINFO(J, I)
        NEXT J
    NEXT I

    ERASE DIF

END SUB

SUB FIND (N, EMATRIX(), BMATRIX(), D(), XINV(), GMATRIX(), P(), P1(), XMY) STATIC

    FOR I = 1 TO N
        FOR J = I TO N
            A = 0#
            FOR K = 1 TO N
                IF (XMY > 0# OR ((XMY = 0#) AND (EMATRIX(K) > .000001#))) THEN
                   A = A + BMATRIX(I, K) * BMATRIX(J, K) / D(I) / D(J) / (EMATRIX(K) + XMY)
                END IF
            NEXT K
            XINV(I, J) = A
            XINV(J, I) = A
        NEXT J
    NEXT I

    FOR I = 1 TO N
        A = 0#
        FOR J = 1 TO N
            A = A - XINV(I, J) * GMATRIX(J)
        NEXT J
        P1(I) = P(I) + A
    NEXT I

END SUB

SUB NLFIT (N, M, X(), Y(), PI(), PU(), PL(), POPT(), SDEV(), NITER) STATIC

    ' Levenberg-Marquardt non-linear curve-fit subroutine

    ' Input

    '  N    = number of fitting parameters
    '  M    = number of X and Y data points
    '  X()  = array of X data points ( M rows by 1 column )
    '  Y()  = array of Y data points ( M rows by 1 column )
    '  PI() = array of initial guesses for fitting parameters
    '         ( N rows by 1 column )
    '  PU() = array of upper bounds for fitting parameters
    '         ( N rows by 1 column )
    '  PL() = array of lower bounds for fitting parameters
    '         ( N rows by 1 column )

    ' Output

    '  POPT() = array of optimized fitting parameters
    '           ( N rows by 1 column )
    '  SDEV() = array of standard deviations
    '           ( N rows by 1 column )
    '  NITER  = number of algorithm iterations

    DIM P(N), POLD(N), P1(N), EMATRIX(N), GMATRIX(N), D(N)
    DIM BMATRIX(N, N), XINV(N, N), CMATRIX(N, N), XINFO(N, N)

    MAXITER = 20
    EPS = .0001#
    XMYMAX = 1000000#
    XMY = .01#
    NITER = 0
    
    FOR I = 1 TO N
        P(I) = PI(I)
        POLD(I) = P(I)
    NEXT I
    
    CALL SQUARESUM(M, P(), X(), Y(), F)
    
    FOLD = F

    DO
       ISC = 0
       CALL EXPLORE(N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), EMATRIX(), CMATRIX(), BMATRIX(), IRESP)
       NITER = NITER + 1
       IF (IRESP) THEN
          WHILE (XMY <= XMYMAX) AND (ISC = 0)
             CALL FIND(N, EMATRIX(), BMATRIX(), D(), XINV(), GMATRIX(), P(), P1(), XMY)
             INSIDE = 1
             FOR I = 1 TO N
                 IF (P1(I) >= PU(I) OR P1(I) <= PL(I)) THEN INSIDE = 0
             NEXT I
             IF (INSIDE = 1) THEN
                CALL SQUARESUM(M, P1(), X(), Y(), F1)
                IF (F1 < F) THEN
                   ISC = 1
                   FOLD = F
                   F = F1
                   XMY = XMY / 100#
                   FOR I = 1 TO N
                       POLD(I) = P(I)
                       P(I) = P1(I)
                   NEXT I
                END IF
             END IF
             XMY = XMY * 10#
          WEND
       END IF
       CALL SCHECK(N, P(), POLD(), F, FOLD, ISIG)
    LOOP UNTIL ((NITER = MAXITER) OR (ISC = 0) OR (ISIG = 0))

    IF (ISC = 1) THEN CALL EXPLORE(N, M, X(), Y(), P(), GMATRIX(), D(), XINFO(), EMATRIX(), CMATRIX(), BMATRIX(), IRESP)
   
    IF (IRESP = 0) THEN
       ICONVER = 0
    ELSE
       IRD = 0
      
       FOR I = 1 TO N
           IF (EMATRIX(I) < EPS) THEN IRD = 1
       NEXT I
      
       CALL FIND(N, EMATRIX(), BMATRIX(), D(), XINV(), GMATRIX(), P(), P1(), XMY)
      
       IF (M > N) THEN
          FOR I = 1 TO N
              SDEV(I) = SQR(XINV(I, I) * F / (M - N))
          NEXT I

          CALL SCHECK(N, P1(), P(), F, F1, ISIG)

          ICONVER = ISIG
      
       END IF
    END IF

    ' load optimal parameter array

    FOR I = 1 TO N
        POPT(I) = P(I)
    NEXT I

    ERASE P, POLD, P1, EMATRIX, GMATRIX, D, BMATRIX, XINV, CMATRIX, XINFO

END SUB

SUB PARDIF (N, K, X(), P(), DIF()) STATIC

    ' Numerical partial derivatives subroutine

    DIM XINCP(N), XDECP(N)
   
    XINC = .01#

    FOR I = 1 TO N
        XINCP(I) = P(I)
        XDECP(I) = P(I)
    NEXT I
    
    FOR I = 1 TO N
        PS = P(I)
        DEL = XINC * PS
        IF (PS = 0#) THEN DEL = XINC
        XINCP(I) = PS + DEL
        XDECP(I) = PS - DEL
        CALL USERFUNC(K, X(), XINCP(), FINC)
        CALL USERFUNC(K, X(), XDECP(), FDEC)
        DIF(I) = (FINC - FDEC) / 2# / DEL
        XINCP(I) = PS
        XDECP(I) = PS
    NEXT I

    ERASE XINCP, XDECP

END SUB

SUB SCHECK (N, Q1(), Q2(), FQ1, FQ2, ISIG) STATIC

    ' Check for significant change in fit parameter subroutine

    EPS = .00001#
   
    ISIG = 0
    
    FOR I = 1 TO N
        IF (Q1(I) <> 0#) THEN
           IF (ABS(1# - Q2(I) / Q1(I)) > EPS) THEN ISIG = 1
        END IF
    NEXT I
   
    IF (ABS(FQ2 < .0000000000000001#)) THEN
       ISIG = 0
    ELSEIF (ABS(1# - FQ2 / FQ1) > EPS) THEN
       ISIG = 1
    END IF

END SUB

SUB SQUARESUM (M, P(), X(), Y(), SQRSUM) STATIC

    ' Sum of squares subroutine

    SQRSUM = 0#
   
    FOR K = 1 TO M
        CALL USERFUNC(K, X(), P(), Y)
        SQRSUM = SQRSUM + (Y - Y(K)) ^ 2
    NEXT K

END SUB

