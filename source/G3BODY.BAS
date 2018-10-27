' Program "G3BODY"                     November 26, 1993
'
' Copyright (c) 1991, 1992, 1993 Crescent Software
'
' Graphics display of circular-restricted
' three body motion (Earth-Moon-Satellite)
'
' ****************************************************

' variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' subroutine declarations

DECLARE SUB KEYCHECK ()
DECLARE SUB RKF45 (NEQ, TI, TF, H, TETOL, Y())
DECLARE SUB DERIVATIVE (TIME, Y(), F())

' global gravitational constant

COMMON SHARED XMU

' ****************************************************

' Define number of differential equations

NEQ = 4

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(33); "Program G3BODY"
   PRINT
   PRINT TAB(20); "< Graphics display of three-body motion >"
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(35); "Main Menu"
      PRINT
      PRINT
      PRINT TAB(20); "< 1 > periodic orbit about L1"
      PRINT
      PRINT TAB(20); "< 2 > periodic orbit about L2"
      PRINT
      PRINT TAB(20); "< 3 > periodic orbit about L3"
      PRINT
      PRINT TAB(20); "< 4 > user input of initial conditions"
      PRINT
      PRINT
      PRINT "Selection ( 1, 2, 3 or 4 )"
      INPUT ICFLG
   LOOP UNTIL (ICFLG >= 1 AND ICFLG <= 4)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the convergence tolerance"
      PRINT "(a value of 1D-8 is recommended)"
      INPUT TOL
   LOOP UNTIL (TOL > 0#)
      
   SELECT CASE ICFLG
   CASE 1
        ' periodic L1 orbit (TR 32-1168, pp 25,29; #74)
        Y(1) = .300000161#
        Y(3) = 0#
        Y(2) = 0#
        Y(4) = -2.536145497#
        TI = 0#
        TF = 5.349501906#
        DT = .01#
        XMU = .012155092#
   CASE 2
        ' periodic L2 orbit (TR 32-1168, pp 31,34; #126)
        Y(1) = 2.840829343#
        Y(3) = 0#
        Y(2) = 0#
        Y(4) = -2.747640074#
        TI = 0#
        TF = 2# * 5.966659294#
        DT = .01#
        XMU = .012155085#
   CASE 3
        ' periodic L3 orbit (TR 32-1168, pp 37,39; #63)
        Y(1) = -1.600000312#
        Y(3) = 0#
        Y(2) = 0#
        Y(4) = 2.066174572#
        TI = 0#
        TF = 2# * 3.151928156#
        DT = .01#
        XMU = .012155092#
   CASE 4
        ' user input of initial conditions
        CLS
        LOCATE 5, 1
        PRINT "Please input the x component of the radius vector"
        INPUT Y(1)
        PRINT
        PRINT "Please input the y component of the radius vector"
        INPUT Y(3)
        PRINT
        PRINT
        PRINT "Please input the x component of the velocity vector"
        INPUT Y(2)
        PRINT
        PRINT "Please input the y component of the velocity vector"
        INPUT Y(4)
        PRINT
        DO
           PRINT
           PRINT "Please input the value for the Earth-Moon mass ratio"
           INPUT XMU
        LOOP UNTIL (XMU > 0#)
        CLS
        LOCATE 5, 1
        PRINT "Please input the X and Y coordinates of the lower"
        PRINT "left hand corner of the graphics screen"
        INPUT XL, YL
        PRINT
        PRINT
        PRINT "Please input the X and Y coordinates of the upper"
        PRINT "right hand corner of the graphics screen"
        INPUT XU, YU
   END SELECT

   CLS
   LOCATE 5, 1

   IF (ICFLG = 4) THEN
      DO
         PRINT "Please input the initial time"
         INPUT TI
      LOOP UNTIL (ABS(TI) >= 0#)

      DO
         PRINT
         PRINT
         PRINT "Please input the final time"
         INPUT TF
      LOOP UNTIL (ABS(TF) > ABS(TI))
     
      DO
         PRINT
         PRINT
         PRINT "Please input the integration step size"
         PRINT "(a value of .01 is recommended)"
         INPUT DT
      LOOP UNTIL (ABS(DT) > 0#)
   END IF

   IF (IMODE = 0) THEN
      DO
         CLS
         LOCATE 5, 1
         PRINT TAB(27); "Graphics Menu"
         PRINT
         PRINT
         PRINT TAB(20); "< 1 > CGA graphics mode"
         PRINT
         PRINT TAB(20); "< 2 > EGA graphics mode"
         PRINT
         PRINT TAB(20); "< 3 > VGA graphics mode"
         PRINT
         PRINT TAB(20); "< 4 > Hercules graphics mode"
         PRINT
         PRINT
         PRINT "Selection ( 1, 2, 3 or 4)"
         INPUT IMODE
      LOOP UNTIL (IMODE >= 1 AND IMODE <= 4)
   END IF

   VTAB = 25

   CLS
   SELECT CASE IMODE
   CASE 1
        ' CGA
        SCREEN 2
   CASE 2
        ' EGA
        SCREEN 9
   CASE 3
        ' VGA
        SCREEN 12
        VTAB = 30
   CASE 4
        ' Hercules
        SCREEN 3
   END SELECT

   SELECT CASE ICFLG
   CASE 1
        WINDOW (-1.25, -1.25)-(1.25, 1.25)
        PSET (.836892919#, 0#)
        LOCATE VTAB, 1
        PRINT TAB(18); "Periodic orbit about the L1 libration point";
   CASE 2
        WINDOW (-3, -3)-(3, 3)
        PSET (1.115699521#, 0#)
        LOCATE VTAB, 1
        PRINT TAB(18); "Periodic orbit about the L2 libration point";
   CASE 3
        WINDOW (-1.75, -1.75)-(1.75, 1.75)
        PSET (-1.005064527#, 0#)
        LOCATE VTAB, 1
        PRINT TAB(18); "Periodic orbit about the L3 libration point";
   CASE 4
        WINDOW (XL, YL)-(XU, YU)
   END SELECT

   LOCATE 1, 1
   PRINT TAB(5); "X0 = "; Y(1);
   PRINT TAB(55); "XDOT0 = "; Y(3);
   LOCATE 2, 1
   PRINT TAB(5); "Y0 = "; Y(2);
   PRINT TAB(55); "YDOT0 = "; Y(4);

   CIRCLE (XMU, 0), .025
   CIRCLE (1# - XMU, 0), .01

   CALL RKF45(NEQ, TI, TF, DT, TOL, Y())

   A$ = ""
   WHILE A$ = ""
      A$ = INKEY$
   WEND

   SCREEN 0

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END

SUB DERIVATIVE (TIME, Y(), Z()) STATIC

    ' Equations of motion subroutine

    ' Input

    '  Y() = state vector (4 rows by 1 column)

    ' Output

    '  Z() = integration vector (4 rows by 1 column)

    R1 = SQR((Y(1) + XMU) ^ 2 + Y(3) ^ 2)

    R2 = SQR((Y(1) - 1# + XMU) ^ 2 + Y(3) ^ 2)

    Z(1) = Y(2)
    Z(2) = 2# * Y(4) + Y(1) - (1# - XMU) * (Y(1) + XMU) / R1 ^ 3 - XMU * (Y(1) - 1# + XMU) / R2 ^ 3
    Z(3) = Y(4)
    Z(4) = -2 * Y(2) + Y(3) - (1# - XMU) * Y(3) / R1 ^ 3 - XMU * Y(3) / R2 ^ 3

END SUB

SUB KEYCHECK STATIC

    ' Check user response subroutine

    LOCATE 25, 1
    PRINT TAB(25); "< press any key to continue >";

    A$ = ""
    WHILE A$ = ""
       A$ = INKEY$
    WEND

END SUB

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
          LOCATE 1, 1
          PSET (Y(1), Y(3))
          TI = TWRK + DT1
          IF (ABS(DT) > ABS(TF - TI)) THEN DT = TF - TI
       END IF
      
    LOOP

    ' erase working arrays

    ERASE F, YDOT, YWRK

END SUB

