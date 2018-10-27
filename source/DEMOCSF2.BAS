' Program "DEMOCSF2"                   December 23, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine CSFIT2 which performs a "clamped" cubic
' spline interpolation of X and Y data input by the user.
'
' NOTE: X(1) < X(2) < X(3) < .... < X(N)
'
'*******************************************************

DEFDBL A-H, O-Z
DEFINT I-N

DECLARE SUB CSFIT2 (N, X(), Y(), YP1, YPN, X, Y)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 31
PRINT "Program DEMOCSF2"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling the"
PRINT TAB(12); "subroutine CSFIT2 which performs a clamped cubic spline"
PRINT TAB(12); "interpolation of X and Y data input by the user."
PRINT
PRINT TAB(12); "      NOTE: X(1) < X(2) < X(3) < .... < X(N)"
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
   
   PRINT TAB(21); "NOTE: X(1) < X(2) < X(3) < .... < X(N)"
   PRINT

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
   PRINT "Please input the corresponding derivative at point 1"
   INPUT YP1
   PRINT
   PRINT "Please input the corresponding derivative at point"; NPTS
   INPUT YPN

   DO
      PRINT
      PRINT
      PRINT "Please input X value to interpolate"
      INPUT XVAL
   LOOP UNTIL (XVAL >= X(1) AND XVAL <= X(NPTS))

   ' perform clamped cubic spline interpolation

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL CSFIT2(NPTS, X(), Y(), YP1, YPN, XVAL, FVAL)

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOCSF2"
   PRINT
   PRINT TAB(21); "< Clamped Cubic Spline Interpolation >"
   PRINT
   PRINT
   PRINT TAB(25); "Number of data points = "; NPTS
   PRINT
   PRINT
   PRINT TAB(25); "X value = "; XVAL
   PRINT
   PRINT TAB(25); "Y value = "; FVAL

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

