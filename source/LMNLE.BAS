' Copyright 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (K, P(), Y)    

SUB LMNLE (N, MAXITER, EPS, PI(), PU(), PL(), POPT(), NITER) STATIC

    ' Solve system of non-linear equations subroutine

    ' Levenberg-Marquardt method

    ' Input
    '  N       = number of variables
    '  MAXITER = maximum number of algorithm iterations
    '  EPS     = convergence criteria
    '  PI()    = array of initial guesses for variables
    '  PU()    = array of upper bounds for variables
    '  PL()    = array of lower bounds for variables

    ' Output
    '  POPT() = array of solution variables
    '  NITER  = number of algorithm iterations

    DIM P(N), POLD(N), P1(N), EMATRIX(N), GMATRIX(N), D(N)
    DIM BMATRIX(N, N), XINV(N, N), CMATRIX(N, N), XINFO(N, N)
    DIM XINCP(N), XDECP(N)

    XMYMAX = 1000000#
    XMY = .001#
    NITER = 0
    
    FOR I = 1 TO N
        P(I) = PI(I)
        POLD(I) = P(I)
    NEXT I
    
    F = 0#
  
    FOR K = 1 TO N
        CALL USERFUNC(K, P(), Y)
        F = F + Y ^ 2
    NEXT K

    FOLD = F

    DO
       ISC = 0
       DIM DIF(N)

       IRESP = 1
    
       FOR I = 1 TO N
           FOR J = I TO N
               XINFO(I, J) = 0#
           NEXT J
           GMATRIX(I) = 0#
       NEXT I
    
       FOR K = 1 TO N
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
               CALL USERFUNC(K, XINCP(), FINC)
               CALL USERFUNC(K, XDECP(), FDEC)
               DIF(I) = (FINC - FDEC) / 2# / DEL
               XINCP(I) = PS
               XDECP(I) = PS
           NEXT I
           
           CALL USERFUNC(K, P(), DELTA)
          
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
   
       END IF

       NITER = NITER + 1
      
       IF (IRESP) THEN
          WHILE (XMY <= XMYMAX) AND (ISC = 0)
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
            
             INSIDE = 1
             FOR I = 1 TO N
                 IF (P1(I) >= PU(I) OR P1(I) <= PL(I)) THEN INSIDE = 0
             NEXT I
             IF (INSIDE = 1) THEN
                F1 = 0#
                FOR K = 1 TO N
                    CALL USERFUNC(K, P1(), Y)
                    F1 = F1 + Y ^ 2
                NEXT K

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
   
       XEPS = .00001#
  
       ISIG = 0
   
       FOR I = 1 TO N
           IF (P(I) <> 0#) THEN
              IF (ABS(1# - POLD(I) / P(I)) > XEPS) THEN ISIG = 1
           END IF
       NEXT I
  
       IF (ABS(FOLD < .0000000000000001#)) THEN
          ISIG = 0
       ELSEIF (ABS(1# - FOLD / F) > XEPS) THEN
          ISIG = 1
       END IF

    LOOP UNTIL ((NITER = MAXITER) OR (ISC = 0) OR (ISIG = 0))

    ' load optimal solution array

    FOR I = 1 TO N
        POPT(I) = P(I)
    NEXT I

    ERASE P, POLD, P1, EMATRIX, GMATRIX, D, BMATRIX, XINV, CMATRIX, XINFO
    ERASE XINCP, XDECP

END SUB

