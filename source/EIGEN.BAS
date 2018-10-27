' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB CORTH (NM, N, LOW, IGH, AR(), AI(), ORTR(), ORTI())
DECLARE SUB COMQR2 (NM, N, LOW, IGH, ORTR(), ORTI(), HR(), HI(), WR(), WI(), ZR(), ZI(), IERR)
DECLARE SUB CBABK2 (NM, N, LOW, IGH, SCALE(), M, ZR(), ZI())
DECLARE SUB CBAL (NM, N, AR(), AI(), LOW, IGH, SCALE())

SUB CBABK2 (NM, N, LOW, IGH, SCALE(), M, ZR(), ZI()) STATIC

    ' Back transformation subroutine

    IF (M = 0) THEN EXIT SUB
   
    IF (IGH <> LOW) THEN
       FOR I = LOW TO IGH
           S = SCALE(I)
           FOR J = 1 TO M
               ZR(I, J) = ZR(I, J) * S
               ZI(I, J) = ZI(I, J) * S
           NEXT J
       NEXT I
    END IF

    FOR II = 1 TO N
        I = II
        IF (I < LOW OR I > IGH) THEN
           IF (I < LOW) THEN I = LOW - II
           K = SCALE(I)
           IF (K <> I) THEN
              FOR J = 1 TO M
                  S = ZR(I, J)
                  ZR(I, J) = ZR(K, J)
                  ZR(K, J) = S
                  S = ZI(I, J)
                  ZI(I, J) = ZI(K, J)
                  ZI(K, J) = S
              NEXT J
           END IF
        END IF
    NEXT II

END SUB

SUB CBAL (NM, N, AR(), AI(), LOW, IGH, SCALE()) STATIC
  
    ' Balance a complex matrix subroutine

    RADIX = 16#
    B2 = RADIX * RADIX
    K = 1
    L = N
    GOTO CBAL2

CBAL1:
    SCALE(M) = J
   
    IF (J <> M) THEN
       FOR I = 1 TO L
           F = AR(I, J)
           AR(I, J) = AR(I, M)
           AR(I, M) = F
           F = AI(I, J)
           AI(I, J) = AI(I, M)
           AI(I, M) = F
       NEXT I
       FOR I = K TO N
           F = AR(J, I)
           AR(J, I) = AR(M, I)
           AR(M, I) = F
           F = AI(J, I)
           AI(J, I) = AI(M, I)
           AI(M, I) = F
       NEXT I
    END IF
   
    IF (IEXC = 2) THEN GOTO CBAL4

    IF (L = 1) THEN
       LOW = K
       IGH = L
       EXIT SUB
    END IF

    L = L - 1

CBAL2:
    FOR JJ = 1 TO L
        J = L + 1 - JJ
        FOR I = 1 TO L
            IF (I <> J) THEN
               IF (AR(J, I) <> 0# OR AI(J, I) <> 0#) THEN GOTO CBAL3
            END IF
        NEXT I
        M = L
        IEXC = 1
        GOTO CBAL1
CBAL3:
    NEXT JJ
    GOTO CBAL5

CBAL4:
   
    K = K + 1

CBAL5:
    FOR J = K TO L
        FOR I = K TO L
            IF (I <> J) THEN
               IF (AR(I, J) <> 0# OR AI(I, J) <> 0#) THEN GOTO CBAL6
            END IF
        NEXT I
        M = K
        IEXC = 2
        GOTO CBAL1
CBAL6:
    NEXT J
     
    FOR I = K TO L
        SCALE(I) = 1#
    NEXT I

    DO
       NOCONV = 0
   
       FOR I = K TO L
           C = 0#
           R = 0#
           FOR J = K TO L
               IF (J <> I) THEN
                  C = C + ABS(AR(J, I)) + ABS(AI(J, I))
                  R = R + ABS(AR(I, J)) + ABS(AI(I, J))
               END IF
           NEXT J
        
           IF (C = 0# OR R = 0#) THEN
              ' NULL
           ELSE
              G = R / RADIX
              F = 1#
              S = C + R
   
              DO
                 IF (C >= G) THEN EXIT DO
                 F = F * RADIX
                 C = C * B2
              LOOP

              G = R * RADIX

              DO
                 IF (C < G) THEN EXIT DO
                 F = F / RADIX
                 C = C / B2
              LOOP

              IF (((C + R) / F) < .95# * S) THEN
                 G = 1# / F
                 SCALE(I) = SCALE(I) * F
                 NOCONV = 1
                 FOR J = K TO N
                     AR(I, J) = AR(I, J) * G
                     AI(I, J) = AI(I, J) * G
                 NEXT J
        
                 FOR J = 1 TO L
                     AR(J, I) = AR(J, I) * F
                     AI(J, I) = AI(J, I) * F
                 NEXT J
               END IF
           END IF
       NEXT I
   
       IF (NOCONV = 0) THEN EXIT DO
    LOOP

    LOW = K
    IGH = L

END SUB

SUB COMQR2 (NM, N, LOW, IGH, ORTR(), ORTI(), HR(), HI(), WR(), WI(), ZR(), ZI(), IERR) STATIC
    
    ' QR method subroutine

    IERR = 0
    EPS = 2.22D-16
   
    FOR I = 1 TO N
        FOR J = 1 TO N
            ZR(I, J) = 0#
            ZI(I, J) = 0#
            IF (I = J) THEN ZR(I, J) = 1#
         NEXT J
    NEXT I
     
    IEND = IGH - LOW - 1
     
    IF (IEND < 0) THEN
       GOTO CQR2
    ELSEIF (IEND = 0) THEN
       GOTO CQR1
    END IF

    FOR II = 1 TO IEND
        I = IGH - II
       
        IF (ORTR(I) = 0# AND ORTI(I) = 0#) THEN
           ' NULL
        ELSEIF (HR(I, I - 1) = 0# AND HI(I, I - 1) = 0#) THEN
           ' NULL
        ELSE
           XNORM = HR(I, I - 1) * ORTR(I) + HI(I, I - 1) * ORTI(I)
           IP1 = I + 1
       
           FOR K = IP1 TO IGH
               ORTR(K) = HR(K, I - 1)
               ORTI(K) = HI(K, I - 1)
           NEXT K
        
           FOR J = I TO IGH
               SR = 0#
               SI = 0#
               FOR K = I TO IGH
                   SR = SR + ORTR(K) * ZR(K, J) + ORTI(K) * ZI(K, J)
                   SI = SI + ORTR(K) * ZI(K, J) - ORTI(K) * ZR(K, J)
               NEXT K
           
               SR = SR / XNORM
               SI = SI / XNORM
           
               FOR K = I TO IGH
                   ZR(K, J) = ZR(K, J) + SR * ORTR(K) - SI * ORTI(K)
                   ZI(K, J) = ZI(K, J) + SR * ORTI(K) + SI * ORTR(K)
               NEXT K
           NEXT J
        END IF
    NEXT II

CQR1:
    L = LOW + 1
     
    FOR I = L TO IGH
        IF (I + 1 < IGH) THEN
           LL = I + 1
        ELSE
           LL = IGH
        END IF
        
        IF (HI(I, I - 1) <> 0#) THEN
           XNORM = SQR(HR(I, I - 1) * HR(I, I - 1) + HI(I, I - 1) * HI(I, I - 1))
           YR = HR(I, I - 1) / XNORM
           YI = HI(I, I - 1) / XNORM
           HR(I, I - 1) = XNORM
           HI(I, I - 1) = 0#
       
           FOR J = I TO N
               SI = YR * HI(I, J) - YI * HR(I, J)
               HR(I, J) = YR * HR(I, J) + YI * HI(I, J)
               HI(I, J) = SI
           NEXT J
        
           FOR J = 1 TO LL
               SI = YR * HI(J, I) + YI * HR(J, I)
               HR(J, I) = YR * HR(J, I) - YI * HI(J, I)
               HI(J, I) = SI
           NEXT J
        
           FOR J = LOW TO IGH
               SI = YR * ZI(J, I) + YI * ZR(J, I)
               ZR(J, I) = YR * ZR(J, I) - YI * ZI(J, I)
               ZI(J, I) = SI
           NEXT J
        END IF
    NEXT I

CQR2:
    FOR I = 1 TO N
        IF (I < LOW OR I > IGH) THEN
           WR(I) = HR(I, I)
           WI(I) = HI(I, I)
        END IF
    NEXT I
     
    IEN = IGH
    TR = 0#
    TI = 0#

CQR3:
    IF (IEN < LOW) THEN
       XNORM = 0#
    
       FOR I = 1 TO N
           FOR J = I TO N
               XNORM = XNORM + ABS(HR(I, J)) + ABS(HI(I, J))
           NEXT J
       NEXT I
    
       IF (N = 1 OR XNORM = 0#) THEN EXIT SUB
    
       FOR NN = 2 TO N
           IEN = N + 2 - NN
           XR = WR(IEN)
           XI = WI(IEN)
           NM1 = IEN - 1
           FOR II = 1 TO NM1
               I = IEN - II
               ZZR = HR(I, IEN)
               ZZI = HI(I, IEN)
          
               IF (I <> NM1) THEN
                  IP1 = I + 1
                  FOR J = IP1 TO NM1
                      ZZR = ZZR + HR(I, J) * HR(J, IEN) - HI(I, J) * HI(J, IEN)
                      ZZI = ZZI + HR(I, J) * HI(J, IEN) + HI(I, J) * HR(J, IEN)
                  NEXT J
               END IF
          
               YR = XR - WR(I)
               YI = XI - WI(I)
          
               IF ((YR = 0#) AND (YI = 0#)) THEN YR = EPS * XNORM
          
               YSQR = YR * YR + YI * YI

               HR(I, IEN) = (ZZR * YR + ZZI * YI) / YSQR
               HI(I, IEN) = (ZZI * YR - ZZR * YI) / YSQR
           NEXT II
       NEXT NN
    
       NM1 = N - 1
    
       FOR I = 1 TO NM1
           IF ((I < LOW) OR (I > IGH)) THEN
              IP1 = I + 1

              FOR J = IP1 TO N
                  ZR(I, J) = HR(I, J)
                  ZI(I, J) = HI(I, J)
              NEXT J
           END IF
       NEXT I
    
       FOR JJ = LOW TO NM1
           J = N + LOW - JJ
           IF (J - 1 < IGH) THEN
              M = J - 1
           ELSE
              M = IGH
           END IF

           FOR I = LOW TO IGH
               ZZR = ZR(I, J)
               ZZI = ZI(I, J)
               FOR K = LOW TO M
                   ZZR = ZZR + ZR(I, K) * HR(K, J) - ZI(I, K) * HI(K, J)
                   ZZI = ZZI + ZR(I, K) * HI(K, J) + ZI(I, K) * HR(K, J)
               NEXT K
               ZR(I, J) = ZZR
               ZI(I, J) = ZZI
           NEXT I
       NEXT JJ
   
       EXIT SUB
    END IF

    ITS = 0
    NM1 = IEN - 1

CQR4:
    FOR LL = LOW TO IEN
        L = IEN + LOW - LL
        IF (L = LOW) THEN EXIT FOR
        TST1 = ABS(HR(L, L - 1))
        TST2 = EPS * (ABS(HR(L - 1, L - 1)) + ABS(HI(L - 1, L - 1)) + ABS(HR(L, L)) + ABS(HI(L, L)))
        
        IF (TST1 <= TST2) THEN EXIT FOR
    NEXT LL

    IF (L = IEN) THEN
       HR(IEN, IEN) = HR(IEN, IEN) + TR
       WR(IEN) = HR(IEN, IEN)
       HI(IEN, IEN) = HI(IEN, IEN) + TI
       WI(IEN) = HI(IEN, IEN)
       IEN = NM1
       GOTO CQR3
    END IF

    IF (ITS = 30) THEN
       IERR = IEN
       EXIT SUB
    END IF

    IF (ITS = 10 OR ITS = 20) THEN
       SR = ABS(HR(IEN, NM1)) + ABS(HR(NM1, IEN - 2))
       SI = 0#
    ELSE
       SR = HR(IEN, IEN)
       SI = HI(IEN, IEN)
       XR = HR(NM1, IEN) * HR(IEN, NM1)
       XI = HI(NM1, IEN) * HR(IEN, NM1)
       IF (XR = 0# AND XI = 0#) THEN
          ' NULL
       ELSE
          YR = (HR(NM1, NM1) - SR) / 2#
          YI = (HI(NM1, NM1) - SI) / 2#
   
          AR = YR * YR - YI * YI + XR
          AI = 2# * YR * YI + XI

          ASQRT = SQR(AR * AR + AI * AI)
          ZZR = SQR((ASQRT + AR) / 2#)
          ZZI = SQR((ASQRT - AR) / 2#)
          IF (AI < 0#) THEN ZZI = -ZZI

          IF (YR * ZZR + YI * ZZI < 0#) THEN
             ZZR = -ZZR
             ZZI = -ZZI
          END IF

          YSQR = (YR + ZZR) * (YR + ZZR) + (YI + ZZI) * (YI + ZZI)

          XR = (XR * (YR + ZZR) + XI * (YI + ZZI)) / YSQR
          XI = (XI * (YR + ZZR) - XR * (YI + ZZI)) / YSQR

          SR = SR - XR
          SI = SI - XI
       END IF
    END IF

    FOR I = LOW TO IEN
        HR(I, I) = HR(I, I) - SR
        HI(I, I) = HI(I, I) - SI
    NEXT I
     
    TR = TR + SR
    TI = TI + SI
    ITS = ITS + 1
    LP1 = L + 1
   
    FOR I = LP1 TO IEN
        SR = HR(I, I - 1)
        HR(I, I - 1) = 0#
        XNORM = SQR(HR(I - 1, I - 1) * HR(I - 1, I - 1) + HI(I - 1, I - 1) * HI(I - 1, I - 1) + SR * SR)
        XR = HR(I - 1, I - 1) / XNORM
        WR(I - 1) = XR
        XI = HI(I - 1, I - 1) / XNORM
        WI(I - 1) = XI
        HR(I - 1, I - 1) = XNORM
        HI(I - 1, I - 1) = 0#
        HI(I, I - 1) = SR / XNORM
       
        FOR J = I TO N
            YR = HR(I - 1, J)
            YI = HI(I - 1, J)
            ZZR = HR(I, J)
            ZZI = HI(I, J)
            HR(I - 1, J) = XR * YR + XI * YI + HI(I, I - 1) * ZZR
            HI(I - 1, J) = XR * YI - XI * YR + HI(I, I - 1) * ZZI
            HR(I, J) = XR * ZZR - XI * ZZI - HI(I, I - 1) * YR
            HI(I, J) = XR * ZZI + XI * ZZR - HI(I, I - 1) * YI
        NEXT J
    NEXT I
     
    SI = HI(IEN, IEN)
   
    IF (SI <> 0#) THEN
       XNORM = SQR(HR(IEN, IEN) * HR(IEN, IEN) + SI * SI)
       SR = HR(IEN, IEN) / XNORM
       SI = SI / XNORM
       HR(IEN, IEN) = XNORM
       HI(IEN, IEN) = 0#
       IF (IEN <> N) THEN
          IP1 = IEN + 1
          FOR J = IP1 TO N
              YR = HR(IEN, J)
              YI = HI(IEN, J)
              HR(IEN, J) = SR * YR + SI * YI
              HI(IEN, J) = SR * YI - SI * YR
          NEXT J
       END IF
    END IF

    FOR J = LP1 TO IEN
        XR = WR(J - 1)
        XI = WI(J - 1)
        FOR I = 1 TO J
            YR = HR(I, J - 1)
            YI = 0#
            ZZR = HR(I, J)
            ZZI = HI(I, J)
            IF (I <> J) THEN
               YI = HI(I, J - 1)
               HI(I, J - 1) = XR * YI + XI * YR + HI(J, J - 1) * ZZI
            END IF
            HR(I, J - 1) = XR * YR - XI * YI + HI(J, J - 1) * ZZR
            HR(I, J) = XR * ZZR + XI * ZZI - HI(J, J - 1) * YR
            HI(I, J) = XR * ZZI - XI * ZZR - HI(J, J - 1) * YI
        NEXT I
        
        FOR I = LOW TO IGH
            YR = ZR(I, J - 1)
            YI = ZI(I, J - 1)
            ZZR = ZR(I, J)
            ZZI = ZI(I, J)
            ZR(I, J - 1) = XR * YR - XI * YI + HI(J, J - 1) * ZZR
            ZI(I, J - 1) = XR * YI + XI * YR + HI(J, J - 1) * ZZI
            ZR(I, J) = XR * ZZR + XI * ZZI - HI(J, J - 1) * YR
            ZI(I, J) = XR * ZZI - XI * ZZR - HI(J, J - 1) * YI
        NEXT I
    NEXT J
     
    IF (SI = 0#) THEN GOTO CQR4
     
    FOR I = 1 TO IEN
        YR = HR(I, IEN)
        YI = HI(I, IEN)
        HR(I, IEN) = SR * YR - SI * YI
        HI(I, IEN) = SR * YI + SI * YR
    NEXT I
     
    FOR I = LOW TO IGH
        YR = ZR(I, IEN)
        YI = ZI(I, IEN)
        ZR(I, IEN) = SR * YR - SI * YI
        ZI(I, IEN) = SR * YI + SI * YR
    NEXT I
     
    GOTO CQR4
     
END SUB

SUB CORTH (NM, N, LOW, IGH, AR(), AI(), ORTR(), ORTI()) STATIC

    ' Reduce a submatrix subroutine

    LA = IGH - 1
    KP1 = LOW + 1
   
    IF (LA < KP1) THEN EXIT SUB

    FOR M = KP1 TO LA
        M1 = M - 1
        H = 0#
        ORTR(M) = 0#
        ORTI(M) = 0#
        SCALE = 0#
       
        FOR I = M TO IGH
            SCALE = SCALE + ABS(AR(I, M1)) + ABS(AI(I, M1))
        NEXT I
       
        IF (SCALE <> 0#) THEN
           MP = M + IGH
          
           FOR II = M TO IGH
               I = MP - II
               ORTR(I) = AR(I, M1) / SCALE
               ORTI(I) = AI(I, M1) / SCALE
               H = H + ORTR(I) * ORTR(I) + ORTI(I) * ORTI(I)
           NEXT II
       
           G = SQR(H)
           FR = ORTR(M)
           FI = ORTI(M)
           F = SQR(FR * FR + FI * FI)
       
           IF (F = 0#) THEN
              ORTR(M) = G
              AR(M, M1) = SCALE
           ELSE
              H = H + F * G
              G = G / F
              ORTR(M) = (1# + G) * ORTR(M)
              ORTI(M) = (1# + G) * ORTI(M)
           END IF
       
           FOR J = M TO N
               FR = 0#
               FI = 0#
           
               FOR II = M TO IGH
                   I = MP - II
                   FR = FR + ORTR(I) * AR(I, J) + ORTI(I) * AI(I, J)
                   FI = FI + ORTR(I) * AI(I, J) - ORTI(I) * AR(I, J)
               NEXT II
           
               FR = FR / H
               FI = FI / H
       
               FOR I = M TO IGH
                   AR(I, J) = AR(I, J) - FR * ORTR(I) + FI * ORTI(I)
                   AI(I, J) = AI(I, J) - FR * ORTI(I) - FI * ORTR(I)
               NEXT I
           NEXT J
       
           FOR I = 1 TO IGH
               FR = 0#
               FI = 0#
           
               FOR JJ = M TO IGH
                   J = MP - JJ
                   FR = FR + ORTR(J) * AR(I, J) - ORTI(J) * AI(I, J)
                   FI = FI + ORTR(J) * AI(I, J) + ORTI(J) * AR(I, J)
               NEXT JJ
           
               FR = FR / H
               FI = FI / H
           
               FOR J = M TO IGH
                   AR(I, J) = AR(I, J) - FR * ORTR(J) - FI * ORTI(J)
                   AI(I, J) = AI(I, J) + FR * ORTI(J) - FI * ORTR(J)
               NEXT J
           NEXT I
        
           ORTR(M) = SCALE * ORTR(M)
           ORTI(M) = SCALE * ORTI(M)
           AR(M, M - 1) = -G * AR(M, M - 1)
           AI(M, M - 1) = -G * AI(M, M - 1)
        END IF
    NEXT M

END SUB

SUB EIGEN (N, NM, AR(), AI(), WR(), WI(), ZR(), ZI(), IERR) STATIC

    ' Eigenvalues and eigenvectors subroutine

    ' Input

    '  N    = matrix dimension
    '  NM   = row dimension of matrix
    '  AR() = real components of the matrix
    '         (N rows by N columns)
    '  AI() = complex components of the matrix
    '         (N rows by N columns) 

    ' Output

    '  WR() = array of real components of eigenvalues
    '         (N rows by 1 column)
    '  WI() = array of complex components of eigenvalues
    '         (N rows by 1 column)
    '  ZR() = array of real components of eigenvectors
    '         (N rows by N columns)
    '  ZI() = array of real components of eigenvextors
    '         (N rows by N columns)
    '  IERR = error flag (0 = no error, > 0 = algorithm error)

    ' NOTE: requires subroutines CORTH, COMQR2, CBABK2, and CBAL

    REDIM SCALE(N)
   
    ' balance matrix

    CALL CBAL(NM, N, AR(), AI(), LOW, IGH, SCALE())
   
    REDIM ORTR(IGH), ORTI(IGH)

    ' reduce submatrix

    CALL CORTH(NM, N, LOW, IGH, AR(), AI(), ORTR(), ORTI())
   
    ' apply QR method

    CALL COMQR2(NM, N, LOW, IGH, ORTR(), ORTI(), AR(), AI(), WR(), WI(), ZR(), ZI(), IERR)
   
    IF (IERR = 0) THEN
       ' perform back substitution
       CALL CBABK2(NM, N, LOW, IGH, SCALE(), N, ZR(), ZI())
    END IF
  
    ' erase working arrays

    ERASE SCALE, ORTR, ORTI

END SUB

