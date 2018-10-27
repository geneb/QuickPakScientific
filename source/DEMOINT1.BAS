' Program "DEMOINT1"                   February 12, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutines SIMPSON and SPLINE which integrate tabulated
' data of the form y = f(x) input by the user.
'
' For the Simpson method, the number of X and Y data points
' must be odd and may be tabulated at unequal intervals.
'
' For the spline method, a minimum of two X and Y data
' points must be input by the user.  The X data must be
' input in ascending order.
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB SIMPSON (N, X(), Y(), SUM)
DECLARE SUB SPLINE (N, X(), Y(), SUM)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMOINT1"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutines SIMPSON and SPLINE which integrate tabulated"
PRINT TAB(12); "data of the form y = f(x) input by the user."
PRINT
PRINT TAB(12); "For the Simpson method, the number of X and Y data points"
PRINT TAB(12); "must be odd and may be tabulated at unequal intervals."
PRINT
PRINT TAB(12); "For the spline method, a minimum of two X and Y data"
PRINT TAB(12); "points must be input by the user. The X data must be"
PRINT TAB(12); "input in ascending order."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(10); "Type of Integration Method"
      PRINT
      PRINT
      PRINT TAB(10); "< 1 > Simpson's Method"
      PRINT
      PRINT TAB(10); "< 2 > Cublic Spline Method"
      PRINT
      PRINT
      PRINT "Selection ( 1 or 2 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT = 1 OR ISELECT = 2)

   SELECT CASE ISELECT
   CASE 1
      DO
        CLS
        LOCATE 5, 1
        PRINT "Please input the number of X and Y data points"
        PRINT "( this must be an odd number )"
        INPUT NPTS
      LOOP UNTIL (INT(NPTS / 2) * 2 <> NPTS)
   CASE 2
      DO
        CLS
        LOCATE 5, 1
        PRINT "Please input the number of X and Y data points"
        PRINT "( this must be an integer >= 2 )"
        INPUT NPTS
      LOOP UNTIL (NPTS >= 2)
   END SELECT

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

   ' Perform integration

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   SELECT CASE ISELECT
   CASE 1
      ' Simpson
      CALL SIMPSON(NPTS, X(), Y(), SUM)
   CASE 2
      ' Spline
      CALL SPLINE(NPTS, X(), Y(), SUM)
   END SELECT

   CLS
   LOCATE 5, 32
   PRINT "Program DEMOINT1"
   PRINT
   PRINT TAB(18); "< Numerical Integration of Tabulated Data >"
   PRINT
   PRINT
   PRINT TAB(22); "Number of data points   = "; NPTS
   PRINT
   PRINT
   PRINT TAB(22); "Lower integration limit = "; X(1)
   PRINT
   PRINT TAB(22); "Upper integration limit = "; X(NPTS)
   PRINT
   PRINT
   PRINT TAB(22); "Integral value          = "; SUM

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

