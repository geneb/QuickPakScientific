' Program "DEMOLNT1"                   January 27, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' the subroutine INTERP1 which performs a linear
' interpolation of tabular data input by the user.
'
' Subroutine INTERP1 performs linear interpolation of
' data of the form y = f(x).
'
' NOTE: the data must be ordered as follows:
'
'  X(1) < X(2) < X(3) < .... < X(N)
'
'*******************************************************

' Variable type definition

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB INTERP1 (N, X(), Y(), XVAL, FVAL)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 31
PRINT "Program DEMOLNT1"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "the subroutine INTERP1 which performs a linear"
PRINT TAB(12); "interpolation of tabular data input by the user."
PRINT
PRINT TAB(12); "Subroutine INTERP1 performs linear interpolation of"
PRINT TAB(12); "data of the form y = f(x)."
PRINT
PRINT TAB(12); "NOTE: the data must be ordered as follows:"
PRINT
PRINT TAB(15); "X(1) < X(2) < X(3) < .... < X(N)"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X and Y data points"
      PRINT "( a minimum of two X and Y data points must be input )"
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
      PRINT "Please input the X value to interpolate"
      INPUT XVAL
   LOOP UNTIL (XVAL >= X(1) AND XVAL <= X(NPTS))
  
   ' perform linear interpolation

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL INTERP1(NPTS, X(), Y(), XVAL, FVAL)

   CLS
   LOCATE 5, 32
   PRINT "Program DEMOLNT1"
   PRINT
   PRINT TAB(22); "< Linear Interpolation of y = f(x) >"
   PRINT
   PRINT
   PRINT TAB(25); "Number of data points = "; NPTS
   PRINT
   PRINT
   PRINT TAB(25); "X value        = "; XVAL
   PRINT
   PRINT TAB(25); "Function value = ";
   PRINT USING "##.######^^^^"; FVAL

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

