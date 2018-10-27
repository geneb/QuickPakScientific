' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB FFT1 (NN, X(), ISIGN) STATIC

    ' One-dimensional Fast Fourier transform subroutine

    ' Input

    '  NN    = number of data points
    '  X()   = vector of data points ( 2 * NN rows )
    '  ISIGN = type of Fast Fourier transform flag
    '      1 = foward Fast Fourier transform
    '     -1 = inverse Fast Fourier transform

    ' Output

    '  X() = vector of transformed values ( 2 * NN rows )
      
    PI2 = 6.28318530717959#
   
    N = 2 * NN
    J = 1
     
    FOR I = 1 TO N STEP 2
        IF (J > I) THEN
          TEMPR = X(J)
          TEMPI = X(J + 1)
          X(J) = X(I)
          X(J + 1) = X(I + 1)
          X(I) = TEMPR
          X(I + 1) = TEMPI
        END IF
       
        M = N / 2

        DO
           IF ((M >= 2) AND (J > M)) THEN
              J = J - M
              M = M / 2
           ELSE
              EXIT DO
           END IF
        LOOP

        J = J + M
    NEXT I

    MMAX = 2
   
    WHILE (N > MMAX)
       ISTEP = 2 * MMAX
       THETA = PI2 / (ISIGN * MMAX)
       WPR = -2# * SIN(.5# * THETA) ^ 2
       WPI = SIN(THETA)
       WR = 1#
       WI = 0#
       
       FOR M = 1 TO MMAX STEP 2
           FOR I = M TO N STEP ISTEP
               J = I + MMAX
               TEMPR = WR * X(J) - WI * X(J + 1)
               TEMPI = WR * X(J + 1) + WI * X(J)
               X(J) = X(I) - TEMPR
               X(J + 1) = X(I + 1) - TEMPI
               X(I) = X(I) + TEMPR
               X(I + 1) = X(I + 1) + TEMPI
           NEXT I
           WTEMP = WR
           WR = WR * WPR - WI * WPI + WR
           WI = WI * WPR + WTEMP * WPI + WI
       NEXT M

       MMAX = ISTEP
    WEND

END SUB

