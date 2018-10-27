' Program "DEMOPLOT"                   January 15, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine XYPLOT which displays a graphics plot
' of X and Y data. This program displays the graphics
' for the simple function, y = sin(x * x).
'
'*******************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine and function declarations

DECLARE SUB XYPLOT (MODE, NPTS, X(), Y(), XAXIS$, YAXIS$, TITLE$)
DECLARE SUB SCALE (XMIN, XMAX, N, XMINP, XMAXP)
DECLARE SUB KEYCHECK ()

DECLARE FUNCTION ROUND (X, N)

' degrees to radians conversion factor

DTR = 3.14159265# / 180#

CLS
LOCATE 5, 32
PRINT "Program DEMOPLOT"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine XYPLOT which displays a graphics plot"
PRINT TAB(12); "of X and Y data. This program displays the graphics"
PRINT TAB(12); "for the simple function, y = sin( x * x)."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(12); "Graphics Mode"
      PRINT
      PRINT
      PRINT TAB(10); "< 1 > CGA Mode"
      PRINT
      PRINT TAB(10); "< 2 > EGA Mode"
      PRINT
      PRINT TAB(10); "< 3 > VGA Mode"
      PRINT
      PRINT TAB(10); "< 4 > Hercules Mode"
      PRINT
      PRINT
      PRINT "Selection ( 1, 2, 3, or 4 )"
      INPUT MODE
   LOOP UNTIL (MODE >= 1 AND MODE <= 4)

   ' request axes and plot titles

   CLS
   LOCATE 5, 1
   PRINT "Please input the x-axis title"
   INPUT XAXIS$
   PRINT
   PRINT "Please input the y-axis title"
   INPUT YAXIS$
   PRINT
   PRINT "Please input the plot title"
   INPUT TITLE$

   CLS
   LOCATE 5, 1
   PRINT "Please input the initial angle (degrees)"
   INPUT XI
   PRINT
   PRINT "Please input the final angle (degrees)"
   INPUT XF
   PRINT
   PRINT
   PRINT "Please input the number of data points"
   INPUT NPTS

   DX = (XF - XI) / NPTS

   ' dimension data arrays

   DIM X(NPTS + 1), Y(NPTS + 1)

   ' generate x and y data points

   CLS
   LOCATE 5, 1
   PRINT TAB(5); "Please wait, computing data points ..."

   FOR I = 1 TO (NPTS + 1)
       X(I) = XI + (I - 1) * DX
       Y(I) = SIN(DTR * X(I) * X(I))
   NEXT I

   ' display graphics

   CALL XYPLOT(MODE, NPTS, X(), Y(), XAXIS$, YAXIS$, TITLE$)

   ' request another selection

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

