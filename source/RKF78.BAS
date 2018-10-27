' Copyright 1989, 1990, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, Y(), YDOT())

SUB RKF78 (NEQ, TI, TF, H, TETOL, Y()) STATIC

    ' Runge-Kutta-Fehlberg 7(8) subroutine
  
    ' Seventh-order solution with eighth-order error control

    ' Input

    '  NEQ   = number of equations in system
    '  TI    = initial simulation time
    '  TF    = final simulation time
    '  H     = guess for step size
    '  TETOL = truncation error tolerance
    '  Y()   = initial integration vector ( NEQ rows )

    ' Output

    '  Y()  = final integration vector ( NEQ rows )

    ' NOTE: requires SUB DERIVATIVE

    STATIC ALPH, CH, BETA, ICOEF

    IF (ICOEF = 0) THEN
      
       ' dimension and define integration coefficients
      
       DIM ALPH(13), CH(13), BETA(13, 12)

       CH(6) = 34# / 105#
       CH(7) = 9# / 35#
       CH(8) = CH(7)
       CH(9) = 9# / 280#
       CH(10) = CH(9)
       CH(12) = 41# / 840#
       CH(13) = CH(12)

       ALPH(2) = 2# / 27#
       ALPH(3) = 1# / 9#
       ALPH(4) = 1# / 6#
       ALPH(5) = 5# / 12#
       ALPH(6) = .5#
       ALPH(7) = 5# / 6#
       ALPH(8) = 1# / 6#
       ALPH(9) = 2# / 3#
       ALPH(10) = 1# / 3#
       ALPH(11) = 1#
       ALPH(13) = 1#

       BETA(2, 1) = 2# / 27#
       BETA(3, 1) = 1# / 36#
       BETA(4, 1) = 1# / 24#
       BETA(5, 1) = 5# / 12#
       BETA(6, 1) = .05#
       BETA(7, 1) = -25# / 108#
       BETA(8, 1) = 31# / 300#
       BETA(9, 1) = 2#
       BETA(10, 1) = -91# / 108#
       BETA(11, 1) = 2383# / 4100#
       BETA(12, 1) = 3# / 205#
       BETA(13, 1) = -1777# / 4100#
       BETA(3, 2) = 1# / 12#
       BETA(4, 3) = 1# / 8#
       BETA(5, 3) = -25# / 16#
       BETA(5, 4) = -BETA(5, 3)
       BETA(6, 4) = .25#
       BETA(7, 4) = 125# / 108#
       BETA(9, 4) = -53# / 6#
       BETA(10, 4) = 23# / 108#
       BETA(11, 4) = -341# / 164#
       BETA(13, 4) = BETA(11, 4)
       BETA(6, 5) = .2#
       BETA(7, 5) = -65# / 27#
       BETA(8, 5) = 61# / 225#
       BETA(9, 5) = 704# / 45#
       BETA(10, 5) = -976# / 135#
       BETA(11, 5) = 4496# / 1025#
       BETA(13, 5) = BETA(11, 5)
       BETA(7, 6) = 125# / 54#
       BETA(8, 6) = -2# / 9#
       BETA(9, 6) = -107# / 9#
       BETA(10, 6) = 311# / 54#
       BETA(11, 6) = -301# / 82#
       BETA(12, 6) = -6# / 41#
       BETA(13, 6) = -289# / 82#
       BETA(8, 7) = 13# / 900#
       BETA(9, 7) = 67# / 90#
       BETA(10, 7) = -19# / 60#
       BETA(11, 7) = 2133# / 4100#
       BETA(12, 7) = -3# / 205#
       BETA(13, 7) = 2193# / 4100#
       BETA(9, 8) = 3#
       BETA(10, 8) = 17# / 6#
       BETA(11, 8) = 45# / 82#
       BETA(12, 8) = -3# / 41#
       BETA(13, 8) = 51# / 82#
       BETA(10, 9) = -1# / 12#
       BETA(11, 9) = 45# / 164#
       BETA(12, 9) = 3# / 41#
       BETA(13, 9) = 33# / 164#
       BETA(11, 10) = 18# / 41#
       BETA(12, 10) = 6# / 41#
       BETA(13, 10) = 12# / 41#
       BETA(13, 12) = 1#
      
       ICOEF = 1
    END IF

    DIM YDOT(NEQ), YWRK(NEQ), F(NEQ, 13)

    ' compute integration "direction"

    SDT = SGN(TF - TI)
    DT = ABS(H) * SDT

    DO
       ' load "working" time and integration vector

       TWRK = TI
       
       FOR I = 1 TO NEQ
           YWRK(I) = Y(I)
       NEXT I

       ' check for last DT

       IF (ABS(DT) > ABS(TF - TI)) THEN DT = TF - TI

       ' check for end of integration period

       IF (ABS(TI - TF) < .00000001#) THEN EXIT DO

       ' evaluate system of differential equations

       CALL DERIVATIVE(TI, Y(), YDOT())

       FOR I = 1 TO NEQ
           F(I, 1) = YDOT(I)
       NEXT I

       ' compute solution

       FOR K = 2 TO 13
           KK = K - 1
           FOR I = 1 TO NEQ
               TEMP = 0#
               FOR J = 1 TO KK
                   TEMP = TEMP + BETA(K, J) * F(I, J)
               NEXT J
               Y(I) = YWRK(I) + DT * TEMP
           NEXT I
           TI = TWRK + ALPH(K) * DT
           CALL DERIVATIVE(TI, Y(), YDOT())
           FOR J = 1 TO NEQ
               F(J, K) = YDOT(J)
           NEXT J
       NEXT K
       
       FOR I = 1 TO NEQ
           TEMP = 0#
           FOR L = 1 TO 13
               TEMP = TEMP + CH(L) * F(I, L)
           NEXT L
           Y(I) = YWRK(I) + DT * TEMP
       NEXT I

       ' truncation error calculations

       YERR = TETOL

       FOR I = 1 TO NEQ
           TER = ABS((F(I, 1) + F(I, 11) - F(I, 12) - F(I, 13)) * CH(12) * DT)
           YTOL = ABS(Y(I)) * TETOL + TETOL
           TCONST = TER / YTOL
           IF (TCONST > YERR) THEN YERR = TCONST
       NEXT I

       ' compute new step size

       DT = .7# * DT * (1# / YERR) ^ (1# / 8#)

       IF (YERR > 1#) THEN
          ' reject current step
          TI = TWRK
          FOR I = 1 TO NEQ
              Y(I) = YWRK(I)
          NEXT I
       ELSE
          ' accept current step
       END IF
       
    LOOP

    ' erase working arrays

    ERASE F, YDOT, YWRK

END SUB

