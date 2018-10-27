' Program "APMOON"                     January 4, 1993
'
' Copyright (c) 1991, 1993 Crescent Software
'
' Computes time of apogee and perigee of the moon
'
' ****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION AMODULO (X)
DECLARE FUNCTION R2R (X)

DECLARE SUB BMINIMA (XI, DX, XMULT, NBRAC, XB1, XB2, IER)
DECLARE SUB MINIMA2 (A, B, EPS, MAXITER, NITER, XMIN, FMIN)
DECLARE SUB JULIAN (XMONTH, DAY, YEAR, XJD, VDATE$)
DECLARE SUB CDATE (XJDATE, XMONTH, DAY, YEAR)
DECLARE SUB USERFUNC (XJD, RM)
DECLARE SUB KEYCHECK ()

' define global constants

CONST REQ = 6378.14#             ' Earth equatorial radius
CONST PI = 3.141592653589793#
CONST PI2 = 2# * PI

' global apogee/perigee flag

COMMON SHARED APFLAG

' dimension and fill calendar month array

DIM MONTH$(12)

MONTH$(1) = "January"
MONTH$(2) = "February"
MONTH$(3) = "March"
MONTH$(4) = "April"
MONTH$(5) = "May"
MONTH$(6) = "June"
MONTH$(7) = "July"
MONTH$(8) = "August"
MONTH$(9) = "September"
MONTH$(10) = "October"
MONTH$(11) = "November"
MONTH$(12) = "December"

' ****************************************************

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(33); "Program APMOON"
   LOCATE 7, 1
   PRINT TAB(23); "< Perigee and Apogee of the Moon >"

   CALL KEYCHECK

   DO
      DO
         CLS
         LOCATE 5, 1
         PRINT "Please input the initial calendar date"
         PRINT "( month [ 1 - 12 ], day [ 1 - 31 ], year [ 1600 - 2200 ] )"
         PRINT "< For example, October 21, 1948 is input as 10,21,1948 >"
         INPUT XMONTH, DAY, YEAR
      LOOP UNTIL ((XMONTH >= 1# AND XMONTH <= 12#) AND (DAY >= 1# AND DAY <= 31#) AND (YEAR >= 1600# AND YEAR <= 2200#))
  
      ' compute initial Julian Date

      CALL JULIAN(XMONTH, DAY, YEAR, XJD0, VDATE$)
  
      IF (VDATE$ = "N") THEN
         LOCATE 5, 1
         PRINT TAB(10); "This date does not exist !!"
         CALL KEYCHECK
      END IF

   LOOP UNTIL (VDATE$ = "Y")

   DO
      PRINT
      PRINT
      PRINT "Simulation period ( days )"
      INPUT NDAYS
   LOOP UNTIL (NDAYS >= 0)

   ' initialization

   TIME = XJD0
   APFLAG = -1#

   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(5); "Working ..."

      ' set up bracketing control parameters

      XI = TIME
      DX = .1#
      XMULT = 1.25#
      NBRAC = 50
     
      ' bracket function minimum or maximum

      CALL BMINIMA(XI, DX, XMULT, NBRAC, XB1, XB2, IER)

      IF (IER = 1) THEN
         CLS
         LOCATE 5, 1
         PRINT TAB(10); "Error in Bracketing Subroutine"
         CALL KEYCHECK
         EXIT DO
      END IF

      ' set up minimization control parameters

      EPS = .000000001#
      MAXITER = 100
     
      ' calculate minimum or maximum
     
      CALL MINIMA2(XB1, XB2, EPS, MAXITER, NITER, XMIN, FMIN)

      ' calculate calendar date of apogee or perigee

      CALL CDATE(XMIN, XMONTH, DAY, YEAR)
     
      ' compute Universal Time

      FDAY = DAY - FIX(DAY)
      UT.HR = INT(24# * FDAY)
      UT.MIN = INT(60# * (24# * FDAY - UT.HR))

      UT$ = STR$(UT.HR) + " hours" + STR$(UT.MIN) + " minutes"
   
      ' print results

      FORMAT$ = "########.###"

      GDATE$ = MONTH$(FIX(XMONTH)) + STR$(FIX(DAY)) + "," + STR$(YEAR)
    
      CLS
      LOCATE 5, 1
      IF (APFLAG = 1#) THEN
         PRINT TAB(32); "Lunar Perigee"
      ELSE
         PRINT TAB(32); "Lunar Apogee"
      END IF
      PRINT
      PRINT
      PRINT
      PRINT TAB(10); "Calendar date"; TAB(65 - LEN(GDATE$)); GDATE$
      PRINT
      PRINT TAB(10); "Universal time"; TAB(65 - LEN(UT$)); UT$
      PRINT
      PRINT
      PRINT TAB(10); "Julian Date";
      PRINT USING FORMAT$; TAB(53); XMIN
      PRINT
      PRINT
      PRINT TAB(10); "Geocentric distance  ( kilometers )";
      PRINT USING FORMAT$; TAB(53); ABS(FMIN * REQ)
      CALL KEYCHECK

      ' increment time by 1 day
     
      TIME = XMIN + 1#

      ' toggle function multiplier flag
     
      APFLAG = -APFLAG
  
   LOOP UNTIL (TIME > (XJD0 + NDAYS))

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END

FUNCTION AMODULO (X) STATIC

   ' Modulo 2 PI function

   ' Input

   '  X = argument (radians)

   ' Output

   '  AMODULO = equivalent X argument (radians)
   '            0 <= X <= PI2

   ' Note: PI2 defined in CONST statement

   A = X - PI2 * FIX(X / PI2)
  
   IF (A < 0#) THEN A = A + PI2
  
   AMODULO = A

END FUNCTION

SUB BMINIMA (XI, DX, XMULT, NBRAC, XB1, XB2, IER) STATIC

    ' Minima bracketing subroutine

    ' Input

    '  XI    = initial X search value
    '  DX    = X step size
    '  NOTE: DX > 0 ==> search forward
    '        DX < 0 ==> search backward
    '  XMULT = X step size multiplier
    '  NBRAC = maximum number of bracketing iterations

    ' Output

    '  XB1 = first X bracket value
    '  XB2 = second X bracket value
    '  IER = error flag
    '    0 = no error
    '    1 = iterations > NBRAC
   
    ' NOTE: requires SUB USERFUNC

    IER = 0
    NITER = 0

    XNM2 = XI
    DXSAVED = DX

    CALL USERFUNC(XNM2, FXNM2)

    XNM1 = XNM2 + DXSAVED

    CALL USERFUNC(XNM1, FXNM1)

    N = 2

    DO
       NITER = NITER + 1

       DXSAVED = XMULT * DXSAVED

       XN = XNM1 + DXSAVED

       CALL USERFUNC(XN, FXN)

       IF (FXNM2 > FXNM1) AND (FXNM1 < FXN) OR (NITER > NBRAC) THEN
          IF (DXSAVED > 0#) THEN
             XB1 = XNM2
             XB2 = XN
          ELSE
             XB1 = XN
             XB2 = XNM2
          END IF
          IF (NITER > NBRAC) THEN IER = 1
          EXIT DO
       END IF

       ' save X(N-2) value

       IF ((N / 2) = FIX(N / 2)) THEN XNM2 = XNM1

       XNM1 = XN

       FXNM2 = FXNM1
       FXNM1 = FXN

       N = N + 1
       
    LOOP

END SUB

SUB CDATE (XJDATE, XMONTH, DAY, YEAR) STATIC

    ' Calendar date subroutine

    ' Input

    '  XJDATE = Julian date

    ' Output

    '  XMONTH = calendar month ( 1 <= XMONTH <= 12 )
    '  DAY    = calendar day ( 1 <= DAY <= 31 )
    '  YEAR   = calendar year

    Z = FIX(XJDATE + .5#)
    F = XJDATE + .5# - Z

    IF (Z < 2299161#) THEN
       A = Z
    ELSE
       ALPHA = FIX((Z - 1867216.25#) / 36524.25#)
       A = Z + 1# + ALPHA - FIX(ALPHA / 4#)
    END IF

    B = A + 1524#
    C = FIX((B - 122.1#) / 365.25#)
    D = FIX(365.25# * C)
    E = FIX((B - D) / 30.6001#)

    DAY = B - D - FIX(30.6001# * E) + F

    IF (E < 13.5#) THEN
       XMONTH = E - 1#
    ELSE
       XMONTH = E - 13#
    END IF

    IF (XMONTH > 2.5#) THEN
       YEAR = C - 4716#
    ELSE
       YEAR = C - 4715#
    END IF

END SUB

SUB JULIAN (XMONTH, DAY, YEAR, XJD, VDATE$) STATIC

    ' Julian Date subroutine

    ' Input

    '  XMONTH = calendar month ( 1 <= XMONTH <= 12 )
    '  DAY    = calendar day ( 1 <= DAY <= 31 )
    '  YEAR   = calendar year

    ' Output

    '  XJD    = Julian Date
    '  VDATE$ = valid date flag ( Y = yes, N = no )

    Y = YEAR
    XM = XMONTH
    B = 0#
    C = 0#

    IF (XMONTH <= 2#) THEN
       Y = YEAR - 1#
       XM = XMONTH + 12#
    END IF

    IF (Y < 0#) THEN C = -.75#

    IF (YEAR < 1582#) THEN
       VDATE$ = "Y"
    ELSEIF (YEAR > 1582#) THEN
       A = FIX(Y / 100#)
       B = 2# - A + FIX(A / 4#)
       VDATE$ = "Y"
    ELSEIF (XMONTH < 10#) THEN
       VDATE$ = "Y"
    ELSEIF (XMONTH > 10#) THEN
       A = FIX(Y / 100#)
       B = 2# - A + FIX(A / 4#)
       VDATE$ = "Y"
    ELSEIF (DAY <= 4#) THEN
       VDATE$ = "Y"
    ELSEIF (DAY > 14#) THEN
       A = FIX(Y / 100#)
       B = 2# - A + FIX(A / 4#)
       VDATE$ = "Y"
    ELSE
       VDATE$ = "N"
    END IF

    XJD = FIX(365.25# * Y + C) + FIX(30.6001# * (XM + 1#)) + DAY + B + 1720994.5#

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

SUB MINIMA2 (A, B, EPS, MAXITER, NITER, XMIN, FMIN) STATIC

    ' Brent's method minimization subroutine

    ' Input

    '  A       = initial X search value
    '  B       = final X search value
    '  EPS     = convergence criteria
    '  MAXITER = maximum number of iterations

    ' Output

    '  NITER = number of iterations performed
    '  XMIN  = minimum X value
    '  FMIN  = minimum function value

    ' NOTE: requires SUB USERFUNC

    C = (3# - SQR(5#)) / 2#

    X = A + C * (B - A)
   
    W = X
    V = W
   
    E = 0#
    P = 0#
    Q = 0#
    R = 0#

    CALL USERFUNC(X, FX)

    FW = FX
    FV = FW

    NITER = 0

    FOR I = 1 TO MAXITER

        NITER = NITER + 1

        XM = .5# * (A + B)
        TOL = EPS * ABS(X) + .0000000001#
        T2 = 2# * TOL

        IF (ABS(X - XM) <= (T2 - .5# * (B - A))) THEN
           XMIN = X
           FMIN = FX
           EXIT FOR
        ELSE
           IF (ABS(E) > TOL) THEN
              R = (X - W) * (FX - FV)
              Q = (X - V) * (FX - FW)
              P = (X - V) * Q - (X - W) * R
              Q = 2# * (Q - R)
              IF (Q > 0#) THEN P = -P
              Q = ABS(Q)
              R = E
              E = D
           END IF

           IF ((ABS(P) >= ABS(.5# * Q * R)) OR (P <= Q * (A - X)) OR (P >= Q * (B - X))) THEN
              IF (X >= XM) THEN
                 E = A - X
              ELSE
                 E = B - X
              END IF
              D = C * E
           ELSE
              D = P / Q
              U = X + D
              IF ((U - A) < T2) OR ((B - U) < T2) THEN D = SGN(XM - X) * TOL
           END IF

           IF (ABS(D) >= TOL) THEN
              U = X + D
           ELSE
              U = X + SGN(D) * TOL
           END IF

           CALL USERFUNC(U, FU)

           IF (FU <= FX) THEN
              IF (U >= X) THEN
                 A = X
              ELSE
                 B = X
              END IF
              V = W
              FV = FW
              W = X
              FW = FX
              X = U
              FX = FU
           ELSE
              IF (U < X) THEN
                 A = U
              ELSE
                 B = U
              END IF
              IF ((FU <= FW) OR (W = X)) THEN
                 V = W
                 FV = FW
                 W = U
                 FW = FU
              ELSEIF ((FU <= FV) OR (V = X) OR (V = W)) THEN
                 V = U
                 FV = FU
              END IF
           END IF
        END IF
    NEXT I

END SUB

FUNCTION R2R (X) STATIC

   ' Revolution to radians function

   ' Input

   '  X = argument (revolutions)

   ' Output

   '  R2R = equivalent X argument (radians)

   ' Note: PI2 defined in CONST statement

   R2R = PI2 * (X - FIX(X))

END FUNCTION

SUB USERFUNC (XJD, RM) STATIC

    ' Lunar position subroutine

    ' Input

    '  XJD    = Julian Date
    '  APFLAG = apogee/perigee multiplier
    '           (passed through COMMON SHARED)
    '           +1 => search for lunar perigee
    '           -1 => search for lunar apogee

    ' Output

    '  RM = geocentric distance of the moon (Earth radii)

    DJD = XJD - 2451545#
    GM = R2R(.374897# + .03629164709# * DJD)
    GM2 = AMODULO(2# * GM)
    GM3 = AMODULO(3# * GM)
    FM = R2R(.259091# + .0367481952# * DJD)
    FM2 = AMODULO(2# * FM)
    EM = R2R(.827362# + .03386319198# * DJD)
    EM2 = AMODULO(2# * EM)
    EM4 = AMODULO(4# * EM)
    GS = R2R(.993126# + .0027377785# * DJD)

    RP = 60.36298# - 3.27746# * COS(GM) - .57994# * COS(GM - EM2)
    RP = RP - .46357# * COS(EM2) - 8.903999999999999D-02 * COS(GM2) + .03865# * COS(GM2 - EM2)
    RP = RP - .03237# * COS(EM2 - GS) - .02688# * COS(GM + EM2) - .02358# * COS(GM - EM2 + GS)
    RP = RP - .0203# * COS(GM - GS) + .01719# * COS(EM) + .01671# * COS(GM + GS)
    RP = RP + .01247# * COS(GM - FM2) + .00704# * COS(GS) + .00529# * COS(EM2 + GS)
    RP = RP - .00524# * COS(GM - EM4) + .00398# * COS(GM - EM2 - GS) - .00366# * COS(GM3)
    RP = RP - .00295# * COS(GM2 - EM4) - .00263# * COS(EM + GS) + .00249# * COS(GM3 - EM2)
    RP = RP - .00221# * COS(GM + EM2 - GS) + .00185# * COS(FM2 - EM2) - .00161# * COS(2# * (EM - GS))
    RP = RP + .00147# * COS(GM + FM2 - EM2) - .00142# * COS(EM4) + .00139# * COS(GM2 - EM2 + GS)
    RM = RP - .00118# * COS(GM - EM4 + GS) - .00116# * COS(GM2 + EM2) - .0011# * COS(GM2 - GS)

    RM = APFLAG * RM

END SUB

