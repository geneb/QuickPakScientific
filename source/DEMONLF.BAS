' Program "DEMONLF"                    December 26, 1992
'
' Copyright (c) 1992 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine NLFIT which performs a non-linear curve
' fit of X and Y data input by the user using a
' Levenberg-Marquardt algorithm.
'
' NLFIT requires a subroutine called USERFUNC which
' returns the scalar function value of a user-defined
' function of several fitting parameters.
'
' This program demonstrates the use of NLFIT to fit
' experimental data to the non-linear equation defined by;
'
'   y = f(b1,b2,b3,x) = b1 / (1+b2*exp(x*b3))
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB NLFIT (N, M, X(), Y(), PI(), PU(), PL(), POPT(), SDEV(), NITER)
DECLARE SUB KEYCHECK ()

' Define number of fitting parameters

NVAR = 3

' Define number of X and Y data points

NPTS = 12

' Dimension solution vector and data arrays

DIM PI(NVAR), XDATA(NPTS), YDATA(NPTS), PL(NVAR), PU(NVAR)
DIM POPT(NVAR), SDEV(NVAR)

' Define array of X data points

FOR I = 1 TO NPTS
    XDATA(I) = I
NEXT I

' Define array of Y data points

YDATA(1) = 5.308#
YDATA(2) = 7.24#
YDATA(3) = 9.638#
YDATA(4) = 12.866#
YDATA(5) = 17.069#
YDATA(6) = 23.192#
YDATA(7) = 31.443#
YDATA(8) = 38.558#
YDATA(9) = 50.156#
YDATA(10) = 62.948#
YDATA(11) = 75.995#
YDATA(12) = 91.97199999999999#

' Initial guesses for fitting parameters

PI(1) = 200#
PI(2) = 30#
PI(3) = -.4#

' Upper and lower bounds for each fit parameter

PU(1) = 1000#
PL(1) = .1#
PU(2) = 100#
PL(2) = 1#
PU(3) = 10#
PL(3) = -10#

' ****************************************************

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMONLF"
   LOCATE 8, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine NLFIT which performs a non-linear curve"
   PRINT TAB(12); "fit of X and Y data input by the user using the"
   PRINT TAB(12); "Levenberg-Marquardt algorithm."
   PRINT
   PRINT TAB(12); "NLFIT requires a subroutine called USERFUNC which"
   PRINT TAB(12); "returns the scalar function value of a user-defined"
   PRINT TAB(12); "function of several fitting parameters."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of NLFIT to fit"
   PRINT TAB(12); "experimental data to the non-linear equation defined by;"
   PRINT
   PRINT TAB(12); "  y = f(b1,b2,b3,x) = b1 / (1 + b2*exp(x*b3))"

   CALL KEYCHECK

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NVAR
       PRINT
       PRINT "Please input the initial value of fitting parameter #"; I
       PRINT "( Try a value of "; PI(I); " )"
       INPUT PI(I)
   NEXT I

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL NLFIT(NVAR, NPTS, XDATA(), YDATA(), PI(), PU(), PL(), POPT(), SDEV(), NITER)

   ' print results

   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMONLF"
   PRINT
   PRINT TAB(26); "< Non-linear Curve Fitting >"
   PRINT

   FOR I = 1 TO NVAR
       PRINT
       PRINT TAB(15); "Value of fitting parameter #"; I; TAB(50); POPT(I)
   NEXT I
   PRINT
   PRINT
   PRINT TAB(29); "< Standard Deviations >"
   FOR I = 1 TO NVAR
       PRINT
       PRINT TAB(15); "Fitting parameter #"; I; TAB(50); SDEV(I)
   NEXT I
   PRINT
   PRINT
   PRINT TAB(15); "Number of iterations"; TAB(50); NITER

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

