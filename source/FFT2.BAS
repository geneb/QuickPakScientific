' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB FFT2 (NN(), X(), ISIGN) STATIC

    ' Two-dimensional Fast Fourier transform subroutine
   
    ' Input

    '  NN(1) = number of X data points
    '  NN(2) = number of Y data points
    '  X()   = vector of data points ( 2 * NN(1) * NN(2) rows )
    '  ISIGN = type of Fast Fourier transform flag
    '      1 = foward Fast Fourier transform
    '     -1 = inverse Fast Fourier transform

    ' Output

    '  X() = vector of transformed values ( 2 * NN(1) * NN(2) rows )
        
    PI2 = 6.28318530717959#
   
    NTOT = NN(1) * NN(2)
     
    NPREV = 1
     
    FOR IDIM = 1 TO 2
        N = NN(IDIM)
        NREM = NTOT / (N * NPREV)
        IP1 = 2 * NPREV
        IP2 = IP1 * N
        IP3 = IP2 * NREM
        I2REV = 1
       
        FOR I2 = 1 TO IP2 STEP IP1
            IF (I2 < I2REV) THEN
               FOR I1 = I2 TO (I2 + IP1 - 2) STEP 2
                   FOR I3 = I1 TO IP3 STEP IP2
                       I3REV = I2REV + I3 - I2
                       TEMPR = X(I3)
                       TEMPI = X(I3 + 1)
                       X(I3) = X(I3REV)
                       X(I3 + 1) = X(I3REV + 1)
                       X(I3REV) = TEMPR
                       X(I3REV + 1) = TEMPI
                   NEXT I3
               NEXT I1
            END IF
           
            IBIT = IP2 / 2
           
            WHILE ((IBIT >= IP1) AND (I2REV > IBIT))
               I2REV = I2REV - IBIT
               IBIT = IBIT / 2
            WEND

            I2REV = I2REV + IBIT
       NEXT I2
       
       IFP1 = IP1
      
       WHILE (IFP1 < IP2)
          IFP2 = 2 * IFP1
          THETA = ISIGN * PI2 / (IFP2 / IP1)
          WPR = -2# * SIN(.5# * THETA) ^ 2
          WPI = SIN(THETA)
          WR = 1#
          WI = 0#
          FOR I3 = 1 TO IFP1 STEP IP1
              FOR I1 = I3 TO (I3 + IP1 - 2) STEP 2
                  FOR I2 = I1 TO IP3 STEP IFP2
                      K1 = I2
                      K2 = K1 + IFP1
                      TEMPR = WR * X(K2) - WI * X(K2 + 1)
                      TEMPI = WR * X(K2 + 1) + WI * X(K2)
                      X(K2) = X(K1) - TEMPR
                      X(K2 + 1) = X(K1 + 1) - TEMPI
                      X(K1) = X(K1) + TEMPR
                      X(K1 + 1) = X(K1 + 1) + TEMPI
                  NEXT I2
              NEXT I1
              WTEMP = WR
              WR = WR * WPR - WI * WPI + WR
              WI = WI * WPR + WTEMP * WPI + WI
          NEXT I3

          IFP1 = IFP2
        WEND
       
        NPREV = N * NPREV
   
    NEXT IDIM

END SUB

