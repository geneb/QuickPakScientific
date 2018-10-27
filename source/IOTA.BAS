' Program "IOTA"                       February 4, 1991
'
' Copyright (c) 1991 Crescent Software
'
' Impulsive Orbital Transfer Analysis
'
' Input
'
'  Altitude of initial circular orbit  ( nautical miles )
'  Altitude of final circular orbit    ( nautical miles )
'  Total plane change angle            ( degrees )
'
' Output
'
'  Delta-V at initial orbit            ( feet/second )
'  Delta-V at final orbit              ( feet/second )
'  Total delta-V required              ( feet/second )
'  Plane change at initial orbit       ( degrees )
'  Plane change at final orbit         ( degrees )
'
' ****************************************************

' variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' subroutine declarations

DECLARE SUB KEYCHECK ()
DECLARE SUB USERFUNC (X, FX)
DECLARE SUB REALROOT (XL, XU, TOL, MAXITER, NITER, XROOT, FROOT)

' define astrodynamic and utility constants

CONST XMU = 1.40764579D+16     ' gravitational constant
CONST REQ = 20925656.2#        ' Earth equatorial radius
CONST XNMFT = 6076.115486#     ' nm to feet conversion
CONST FTNM = 1# / XNMFT        ' feet to nm conversion
CONST PI = 3.141592653589793#
CONST DTR = PI / 180#          ' degrees to radians
CONST RTD = 180# / PI          ' radians to degrees

' global variables

COMMON SHARED DINC, H1, H2, H3

' ****************************************************

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(34); "Program IOTA"
   PRINT
   PRINT TAB(18); "< Impulsive Orbital Transfer Analysis >"

   DO
      LOCATE 12, 1
      PRINT "Please input the altitude of the initial circular orbit (nm)"
      INPUT ALT1
   LOOP UNTIL (ALT1 > 0#)

   DO
      LOCATE 16, 1
      PRINT "Please input the altitude of the final circular orbit (nm)"
      INPUT ALT2
   LOOP UNTIL (ALT2 >= ALT1)

   DO
      LOCATE 20, 1
      PRINT "Please input the total plane change angle (degrees)"
      INPUT DINC
   LOOP UNTIL (DINC >= 0#)

   DINC = DTR * DINC

   ' compute radii

   R1 = REQ + XNMFT * ALT1
   R2 = REQ + XNMFT * ALT2

   ' compute "normalized" radii

   H1 = SQR(2# * R2 / (R2 + R1))
   H2 = SQR(R1 / R2)
   H3 = SQR(2# * R1 / (R2 + R1))

   ' compute "local circular velocity" at initial orbit

   VLC = SQR(XMU / R1)

   ' solve for root of f(x)

   XL = 0#
   XU = DINC
   TOL = .00000001#
   MAXITER = 50

   CALL REALROOT(XL, XU, TOL, MAXITER, NITER, XROOT, FROOT)

   ' compute inclinations

   DINC1 = XROOT
   DINC2 = DINC - DINC1

   ' calculate delta-V's

   DV1 = VLC * SQR(1# + H1 ^ 2 - 2# * H1 * COS(DINC1))

   DV2 = VLC * SQR(H2 ^ 2 * H3 ^ 2 + H2 ^ 2 - 2# * H2 ^ 2 * H3 * COS(DINC2))

   ' print results

   FORMAT$ = "#######.###"

   CLS
   PRINT
   PRINT TAB(34); "Program IOTA"
   PRINT
   PRINT TAB(21); "< Impulsive Orbital Transfer Analysis >"
   PRINT
   PRINT
   PRINT TAB(10); "Initial orbit altitude      ( nm )"; TAB(55);
   PRINT USING FORMAT$; ALT1
   PRINT TAB(10); "Initial orbit radius        ( nm )"; TAB(55);
   PRINT USING FORMAT$; R1 * FTNM
   PRINT
   PRINT TAB(10); "Final orbit altitude        ( nm )"; TAB(55);
   PRINT USING FORMAT$; ALT2
   PRINT TAB(10); "Final orbit radius          ( nm )"; TAB(55);
   PRINT USING FORMAT$; R2 * FTNM
   PRINT
   PRINT
   PRINT TAB(10); "Initial orbit delta-V       ( ft/sec )"; TAB(55);
   PRINT USING FORMAT$; DV1
   PRINT TAB(10); "Final orbit delta-V         ( ft/sec )"; TAB(55);
   PRINT USING FORMAT$; DV2
   PRINT TAB(10); "Total delta-V               ( ft/sec )"; TAB(55);
   PRINT USING FORMAT$; DV1 + DV2
   PRINT
   PRINT
   PRINT TAB(10); "Initial orbit plane change  ( degrees )"; TAB(55);
   PRINT USING FORMAT$; DINC1 * RTD
   PRINT TAB(10); "Final orbit plane change    ( degrees )"; TAB(55);
   PRINT USING FORMAT$; DINC2 * RTD
   PRINT TAB(10); "Total plane change          ( degrees )"; TAB(55);
   PRINT USING FORMAT$; DINC * RTD
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END

SUB KEYCHECK STATIC

    ' check user response subroutine

    PRINT
    PRINT
    PRINT TAB(25); "< press any key to continue >";

    A$ = ""
    WHILE A$ = ""
      A$ = INKEY$
    WEND

END SUB

SUB REALROOT (XL, XU, TOL, MAXITER, NITER, XROOT, FROOT) STATIC

    ' Real root of a non-linear function subroutine

    ' Input

    '  XL      = lower bound of search interval
    '  XU      = upper bound of search interval
    '  TOL     = convergence criteria
    '  MAXITER = maximum number of iterations

    ' Output

    '  XROOT = real root of F(X) = 0
    '  FROOT = function value
    '  NITER = number of iterations

    ' NOTE: requires SUB USERFUNC

    BEPS = .0000000001#

    E = 0#
    A = XL
    B = XU

    CALL USERFUNC(A, FA)
    CALL USERFUNC(B, FB)

    FC = FB

    NITER = 0

    FOR ITER = 1 TO MAXITER
        NITER = NITER + 1
        IF (FB * FC > 0#) THEN
           C = A
           FC = FA
           D = B - A
           E = D
        END IF

        IF (ABS(FC) < ABS(FB)) THEN
           A = B
           B = C
           C = A
           FA = FB
           FB = FC
           FC = FA
        END IF
       
        TOL1 = 2# * BEPS * ABS(B) + .5# * TOL
       
        XM = .5# * (C - B)

        IF (ABS(XM) <= TOL1 OR FB = 0#) THEN EXIT FOR

        IF (ABS(E) >= TOL1 AND ABS(FA) > ABS(FB)) THEN
           S = FB / FA
           IF (A = C) THEN
              P = 2# * XM * S
              Q = 1# - S
           ELSE
              Q = FA / FC
              R = FB / FC
              P = S * (2# * XM * Q * (Q - R) - (B - A) * (R - 1#))
              Q = (Q - 1#) * (R - 1#) * (S - 1#)
           END IF

           IF (P > 0#) THEN Q = -Q

           P = ABS(P)
           XMIN = ABS(E * Q)
           TMP = 3# * XM * Q - ABS(TOL1 * Q)

           IF (XMIN < TMP) THEN XMIN = TMP

           IF (2# * P < XMIN) THEN
              E = D
              D = P / Q
           ELSE
              D = XM
              E = D
           END IF
        ELSE
           D = XM
           E = D
        END IF

        A = B
        FA = FB

        IF (ABS(D) > TOL1) THEN
           B = B + D
        ELSE
           B = B + SGN(XM) * TOL1
        END IF

        CALL USERFUNC(B, FB)
    NEXT ITER

    FROOT = FB
    XROOT = B

END SUB

SUB USERFUNC (X, FX) STATIC

    ' Inclination function subroutine

    ' Input

    '  X = function argument

    ' Output

    '  FX = function value

    H = H2 * H2

    A = H1 * SIN(X) / SQR(1# + H1 ^ 2 - 2# * H1 * COS(X))
    B = H * H3 * (SIN(DINC) * COS(X) - COS(DINC) * SIN(X))
    FX = A - B / SQR(H * H3 ^ 2 + H - 2# * H * H3 * COS(DINC - X))

END SUB

