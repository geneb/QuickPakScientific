' Program "DEMOLNT2"                   January 27, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' the subroutines INTERP2 which performs a linear
' interpolation of tabular data of the form z = f(x,y)
' input by the user.
'
' NOTE: the data must be ordered as follows:
'
'  X(1) < X(2) < X(3) < .... < X(N)
'  Y(1) < Y(2) < Y(3) < .... < Y(N)
'
' This program demonstrates the use of INTERP2 for the
' interpolation of the function defined by
'
'      z = f(x,y) = SIN(x + y)
'
'*******************************************************

' Variable type definition

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB INTERP2 (NX, NY, X(), Y(), Z(), XVAL, YVAL, ZVAL)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 31
PRINT "Program DEMOLNT2"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling the"
PRINT TAB(12); "the subroutine INTERP2 which performs a linear"
PRINT TAB(12); "interpolation of tabular data of the form z = f(x,y)"
PRINT TAB(12); "input by the user."
PRINT
PRINT TAB(12); "NOTE: the data must be ordered as follows:"
PRINT
PRINT TAB(15); "X(1) < X(2) < X(3) < .... < X(N)"
PRINT TAB(15); "Y(1) < Y(2) < Y(3) < .... < Y(N)"
PRINT
PRINT TAB(12); "This program demonstrates the use of INTERP2 for the"
PRINT TAB(12); "interpolation of the function defined by"
PRINT
PRINT TAB(12); "      z = f(x,y) = SIN(x + y)"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X data points"
      PRINT "( a minimum of two X data points must be input )"
      INPUT NX
   LOOP UNTIL (NX >= 2)

   DO
      PRINT
      PRINT "Please input the number of Y data points"
      PRINT "( a minimum of two Y data points must be input )"
      INPUT NY
   LOOP UNTIL (NY >= 2)
  
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input a value for delta-X"
      INPUT DX
   LOOP UNTIL (DX > 0#)

   DO
      PRINT
      PRINT "Please input a value for delta-Y"
      INPUT DY
   LOOP UNTIL (DY > 0#)

   ' Dimension arrays

   DIM X(NX), Y(NY), Z(NX, NY)

   CLS
   LOCATE 5, 1
   PRINT TAB(5); "Please wait, computing data points ..."

   ' compute data points for the function defined by

   ' Z = f(X,Y) = SIN(X + Y)

   ' where X = DX * I  and  Y = DY * J

   ' for I = 1, 2, ... NX  and  J = 1, 2, ... NY

   FOR I = 1 TO NX
       X(I) = I * DX
       FOR J = 1 TO NY
           Y(J) = J * DY
           Z(I, J) = SIN(X(I) + Y(J))
       NEXT J
   NEXT I

   CLS
   LOCATE 5, 1

   DO
      PRINT
      PRINT
      PRINT "Please input the X value to interpolate"
      PRINT "(this must be a value between"; X(1); "and"; X(NX)
      INPUT XVAL
   LOOP UNTIL (XVAL >= X(1) AND XVAL <= X(NX))
  
   DO
      PRINT
      PRINT
      PRINT "Please input the Y value to interpolate"
      PRINT "(this must be a value between"; Y(1); "and"; Y(NY)
      INPUT YVAL
   LOOP UNTIL (YVAL >= Y(1) AND YVAL <= Y(NY))

   ' perform linear interpolation

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL INTERP2(NX, NY, X(), Y(), Z(), XVAL, YVAL, FVAL)

   CLS
   LOCATE 5, 32
   PRINT "Program DEMOLNT2"
   PRINT
   PRINT TAB(21); "< Linear Interpolation of z = f(x,y) >"
   PRINT
   PRINT
   PRINT TAB(24); "Number of X data points = "; NX
   PRINT
   PRINT TAB(24); "Number of Y data points = "; NY
   PRINT
   PRINT
   PRINT TAB(24); "X value             = "; XVAL
   PRINT
   PRINT TAB(24); "Y value             = "; YVAL
   PRINT
   PRINT
   PRINT TAB(21); "Function value      = ";
   PRINT USING "##.######^^^^"; FVAL
   PRINT
   PRINT TAB(21); "Interpolation error = ";
   PRINT USING "##.######^^^^"; ABS(FVAL - SIN(XVAL + YVAL))

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, Y, Z

LOOP UNTIL (SLCT$ = "N")

END

