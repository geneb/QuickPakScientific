' Copyright 1988, 1989, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, Y(), YDOT())

SUB RKF45 (NEQ, TI, TF, H, TETOL, Y()) STATIC

    ' Runge-Kutta-Fehlberg 4(5) subroutine

    ' Fourth-order solution with fifth-order error control
 
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

    STATIC ICOEF, ALPH, CH, CT, BETA

    IF (ICOEFF = 0) THEN

       ' dimension and define integration coefficients
      
       DIM ALPH(6), CH(6), CE(6), BETA(6, 5)

       ALPH(2) = 1# / 4#
       ALPH(3) = 3# / 8#
       ALPH(4) = 12# / 13#
       ALPH(5) = 1#
       ALPH(6) = .5#
      
       BETA(2, 1) = 1# / 4#
       BETA(3, 1) = 3# / 32#
       BETA(3, 2) = 9# / 32#
       BETA(4, 1) = 1932# / 2197#
       BETA(4, 2) = -7200# / 2197#
       BETA(4, 3) = 7296# / 2197#
       BETA(5, 1) = 439# / 216#
       BETA(5, 2) = -8#
       BETA(5, 3) = 3680# / 513#
       BETA(5, 4) = -845# / 4104#
       BETA(6, 1) = -8# / 27#
       BETA(6, 2) = 2#
       BETA(6, 3) = -3544# / 2565#
       BETA(6, 4) = 1859# / 4104#
       BETA(6, 5) = -11# / 40#

       CH(1) = 16# / 135#
       CH(2) = 0#
       CH(3) = 6656# / 12825#
       CH(4) = 28561# / 56430#
       CH(5) = -9# / 50#
       CH(6) = 2# / 55#

       CE(1) = -1# / 360#
       CE(2) = 0#
       CE(3) = 128# / 4275#
       CE(4) = 2197# / 75240#
       CE(5) = -1# / 50#
       CE(6) = -2# / 55#

       ICOEF = 1
    END IF

    DIM YDOT(NEQ), YWRK(NEQ), F(NEQ, 6)

    ' compute integration "direction"

    SDT = SGN(TF - TI)
    DT = ABS(H) * SDT
    DT1 = DT
    EDS = TETOL / 20#

    DO
       ' load "working" time and integration vector

       TWRK = TI
      
       FOR I = 1 TO NEQ
           YWRK(I) = Y(I)
       NEXT I

       ' evaluate system of differential equations

       CALL DERIVATIVE(TI, Y(), YDOT())

       FOR I = 1 TO NEQ
           F(I, 1) = YDOT(I)
       NEXT I

       IF (ABS(DT) < .0000000001#) THEN EXIT DO

       ' compute solution

       FOR K = 2 TO 6
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
           FOR L = 1 TO 6
               TEMP = TEMP + CH(L) * F(I, L)
           NEXT L
           Y(I) = YWRK(I) + DT * TEMP
       NEXT I

       ' truncation error calculations

       TER = 0#

       FOR I = 1 TO NEQ
           A = ABS(Y(I))
          
           IF (A < TETOL) THEN A = TETOL

           TEI = ABS(CE(1) * F(I, 1) + CE(3) * F(I, 3) + CE(4) * F(I, 4) + CE(5) * F(I, 5) + CE(6) * F(I, 6)) / A
          
           IF (TEI > TER) THEN TER = TEI
       NEXT I
       
       TER = TER * ABS(DT) + 9.999999999999999D-21

       DT1 = DT

       Q = EDS / TER

       ' compute new step size

       DT = DT1 * Q ^ (1# / 5#)

       IF (TER > TETOL) THEN
          ' reject current step
          TI = TWRK
          FOR I = 1 TO NEQ
              Y(I) = YWRK(I)
          NEXT I
       ELSE
          ' accept current step
          TI = TWRK + DT1
          IF (ABS(DT) > ABS(TF - TI)) THEN DT = TF - TI
       END IF
      
    LOOP

    ' erase working arrays

    ERASE F, YDOT, YWRK

END SUB

