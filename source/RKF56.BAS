' Copyright (c) 1989, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, Y(), YDOT())

SUB RKF56 (NEQ, TI, TF, H, TETOL, Y()) STATIC

    ' Runge-Kutta-Fehlberg 5(6) subroutine
   
    ' Fifth-order solution with sixth-order error control

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

    STATIC A, CH, B, ICOEF, RKF1

    IF (ICOEF = 0) THEN

       ' dimension and define integration coefficients

       DIM A(8), CH(8), B(8, 7)

       A(1) = 0#
       A(2) = 1# / 6#
       A(3) = 4# / 15#
       A(4) = 2# / 3#
       A(5) = 4# / 5#
       A(6) = 1#
       A(7) = 0#
       A(8) = 1#

       CH(1) = 7# / 1408#
       CH(2) = 0#
       CH(3) = 1125# / 2816#
       CH(4) = 9# / 32#
       CH(5) = 125# / 768#
       CH(6) = 0#
       CH(7) = 5# / 66#
       CH(8) = 5# / 66#

       B(2, 1) = 1# / 6#
       B(3, 1) = 4# / 75#
       B(3, 2) = 16# / 75#
       B(4, 1) = 5# / 6#
       B(4, 2) = -8# / 3#
       B(4, 3) = 5# / 2#
       B(5, 1) = -8# / 5#
       B(5, 2) = 144# / 25#
       B(5, 3) = -4#
       B(5, 4) = 16# / 25#
       B(6, 1) = 361# / 320#
       B(6, 2) = -18# / 5#
       B(6, 3) = 407# / 128#
       B(6, 4) = -11# / 80#
       B(6, 5) = 55# / 128#
       B(7, 1) = -11# / 640#
       B(7, 2) = 0#
       B(7, 3) = 11# / 256#
       B(7, 4) = -11# / 160#
       B(7, 5) = 11# / 256#
       B(7, 6) = 0#
       B(8, 1) = 93# / 640#
       B(8, 2) = -18# / 5#
       B(8, 3) = 803# / 256#
       B(8, 4) = -11# / 160#
       B(8, 5) = 99# / 256#
       B(8, 6) = 0#
       B(8, 7) = 1#

       RKF1 = 5# / 66#

       ICOEF = 1
    END IF

    DIM YDOT(NEQ), YT(NEQ), F(NEQ, 8)

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

       FOR K = 2 TO 8
           KK = K - 1
           FOR I = 1 TO NEQ
               TEMP = 0#
               FOR J = 1 TO KK
                   TEMP = TEMP + B(K, J) * F(I, J)
               NEXT J
               Y(I) = YWRK(I) + DT * TEMP
           NEXT I
           TI = TWRK + A(K) * DT
           CALL DERIVATIVE(TI, Y(), YDOT())

           FOR J = 1 TO NEQ
               F(J, K) = YDOT(J)
           NEXT J
       NEXT K

       FOR I = 1 TO NEQ
           TEMP = 0#
           Y(I) = YT(I)
           FOR L = 1 TO 8
               TEMP = TEMP + CH(L) * F(I, L)
           NEXT L
           Y(I) = YWRK(I) + DT * TEMP
       NEXT I

       ' truncation error calculations

       FOR I = 1 TO NEQ
           A = ABS(Y(I))

           IF (A < TETOL) THEN A = TETOL

           TEI = ABS(RKF1 * (F(I, 1) + F(I, 6) - F(I, 7) - F(I, 8))) / A
          
           IF (TEI > TER) THEN TER = TEI
       NEXT I
       
       TER = TER * ABS(DT) + 9.999999999999999D-21

       DT1 = DT

       Q = EDS / TER

       ' compute new step size

       DT = DT1 * Q ^ (1# / 6#)

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
    
    ERASE YDOT, YT, F

END SUB

