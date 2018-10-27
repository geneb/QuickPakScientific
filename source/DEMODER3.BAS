' Program "DEMODER3"                   February 12, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine DERIV3 which numerically estimates the
' first derivative of user-supplied tabulated data of
' the form y = f(x) using the method of cubic splines.
'
' NOTE: X(1) < X(2) < X(3) < .... < X(N)
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIV3 (N, X(), Y(), XVAL, DVAL)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 31
PRINT "Program DEMODER3"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling the"
PRINT TAB(12); "subroutine DERIV3 which numerically estimates the first"
PRINT TAB(12); "derivative of user-supplied tabulated data of the form"
PRINT TAB(12); "y = f(x) using the method of cubic splines."
PRINT
PRINT TAB(12); "NOTE: X(1) < X(2) < X(3) < .... < X(N)"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X and Y data points"
      PRINT "( a minimum of two data points must be input )"
      INPUT NPTS
   LOOP UNTIL (NPTS >= 2)

   ' Dimension arrays

   DIM X(NPTS), Y(NPTS)

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NPTS
       PRINT
       PRINT
       PRINT "Please input the X data for point #"; I
       INPUT X(I)
       PRINT
       PRINT "Please input the Y data for point #"; I
       INPUT Y(I)
   NEXT I

   CLS
   LOCATE 5, 1

   DO
      PRINT
      PRINT
      PRINT "Please input X value for evaluation"
      INPUT XVAL
   LOOP UNTIL (XVAL >= X(1) AND XVAL <= X(NPTS))

   ' perform cubic spline curve fit

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL DERIV3(NPTS, X(), Y(), XVAL, DVAL)

   CLS
   LOCATE 5, 31
   PRINT "Program DEMODER3"
   PRINT
   PRINT TAB(26); "< Cubic Spline Derivative >"
   PRINT
   PRINT
   PRINT TAB(25); "Number of data points = "; NPTS
   PRINT
   PRINT TAB(25); "X value               = "; XVAL
   PRINT
   PRINT TAB(25); "Derivative value      = "; DVAL

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, Y

LOOP UNTIL (SLCT$ = "N")

END

