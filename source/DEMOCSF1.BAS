' Program "DEMOCSF1"                   January 9, 1993
'
' Copyright (c) 1988-1991, 1993 Crescent Software
'
' This program demonstrates the procedure for calling the
' subroutines CSCOEF and CSFIT1 which perform a natural
' cubic spline interpolation of user-defined X and Y data.
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB CSCOEF (N, XN(), Y(), S(), INDEX())
DECLARE SUB CSFIT1 (N, XN(), Y(), S(), INDEX(), X, SX)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 31
PRINT "Program DEMOCSF1"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling the"
PRINT TAB(12); "subroutines CSCOEF and CSFIT1 which perform a natural"
PRINT TAB(12); "cubic spline interpolation of user-defined X and Y data."

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

   DIM X(NPTS), Y(NPTS), S(NPTS), INDEX(NPTS)

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
   LOCATE 5, 5
   PRINT "Working ..."

   ' generate cubic spline coefficients

   CALL CSCOEF(NPTS, X(), Y(), S(), INDEX())

   DO
      CLS
      LOCATE 5, 1

      DO
         PRINT
         PRINT
         PRINT "Please input X value to interpolate"
         INPUT XVAL
      LOOP UNTIL (XVAL >= X(1) AND XVAL <= X(NPTS))

      ' interpolate user-defined data point

      CALL CSFIT1(NPTS, X(), Y(), S(), INDEX(), XVAL, YVAL)

      CLS
      LOCATE 5, 31
      PRINT "Program DEMOCSF1"
      PRINT
      PRINT TAB(25); "< Cubic Spline Interpolation >"
      PRINT
      PRINT
      PRINT TAB(25); "Number of data points = "; NPTS
      PRINT
      PRINT
      PRINT TAB(25); "X value        = "; XVAL
      PRINT
      PRINT TAB(25); "Function value = "; YVAL

      CALL KEYCHECK

      DO
         CLS
         LOCATE 5, 1
         PRINT "Another interpolation ( y = yes, n = no )"
         INPUT CSFIT$
         CSFIT$ = UCASE$(CSFIT$)
      LOOP UNTIL (CSFIT$ = "Y" OR CSFIT$ = "N")
   LOOP UNTIL (CSFIT$ = "N")

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, Y, S, INDEX

LOOP UNTIL (SLCT$ = "N")

END


