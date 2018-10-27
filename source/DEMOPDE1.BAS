' Program "DEMOPDE1"                   August 13, 1993
'
' Copyright (c) 1991, 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine POISSON which solves the elliptic partial
' differential equation of the form
'
'    d2u(x,y)/dx2 + d2u(x,y)/dy2 = f(x,y)
'
' subject to the boundary conditions
'
'    u(x,y) = g(x,y)   if x = a  or  x = b  and  c <= y <= d
'
'    u(x,y) = g(x,y)   if y = c  or  y = d  and  a <= x <= b     
'
' DEMOPDE1 requires two subroutines which define the "forcing"
' function f(x,y) and the boundary condition function g(x,y).
' The forcing function must be in a subroutine called FFUN.BAS
' and the boundary function in the subroutine GFUN.BAS.
'
' For this example the forcing function is as follows:
'
'    f(x) = x*exp(y)
'
' and the boundary condition function is
'
'    g(0,y) = 0,  g(2,y) = 2*exp(y),    0 <= y <= 1
'    g(x,0) = x,  g(x,1) = x*e,         0 <= x <= 2
'
' ***********************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB GFUN (X, Y, GVAL)
DECLARE SUB POISSON (A, B, C, D, NX, NY, TOL, NITER, X(), Y(), W())
DECLARE SUB FFUN (X, Y, FVAL)
DECLARE SUB KEYCHECK ()

' Begin demo program

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPDE1"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine POISSON which solves the elliptic partial"
   PRINT TAB(12); "differential equation of the form"
   PRINT
   PRINT TAB(12); "    d2u(x, y) / dx2 + d2u(x, y) / dy2 = f(x, y)"
   PRINT
   PRINT TAB(12); "subject to the boundary conditions"
   PRINT
   PRINT TAB(12); "    u(x,y) = g(x,y)   if x = a or x = b and c <= y <= d"
   PRINT
   PRINT TAB(12); "    u(x,y) = g(x,y)   if y = c or y = d and a <= x <= b"
   PRINT
   PRINT TAB(12); "DEMOPDE1 requires two subroutines which define the forcing"
   PRINT TAB(12); "function f(x,y) and the boundary condition function g(x,y)."
   PRINT TAB(12); "The forcing function must be in the subroutine FFUN.BAS and"
   PRINT TAB(12); "the boundary function in the subroutine GFUN.BAS."

   CALL KEYCHECK

   CLS
   LOCATE 5, 1
   DO
     PRINT "Please input the number of x grid lines"
     INPUT NX
   LOOP UNTIL (NX > 0)
  
   DO
     PRINT
     PRINT "Please input the number of y grid lines"
     INPUT NY
   LOOP UNTIL (NY > 0)

   ' dimension mesh and solutions arrays

   DIM X(NX - 1), Y(NY - 1), W(NX - 1, NY - 1)

   CLS
   LOCATE 5, 1
   PRINT "Please input the initial x endpoint"
   INPUT A
   PRINT
   PRINT "Please input the final x endpoint"
   INPUT B
   PRINT
   PRINT
   PRINT "Please input the initial y endpoint"
   INPUT C
   PRINT
   PRINT "Please input the final y endpoint"
   INPUT D
   
   CLS
   LOCATE 5, 1
   DO
      PRINT "Please input the convergence tolerance"
      PRINT "( a value of 1D-10 is recommended )"
      INPUT TOL
   LOOP UNTIL (TOL > 0#)

   DO
      PRINT
      PRINT "Please input the maximum number of iterations"
      PRINT "( a value of 200 is recommended )"
      INPUT NITER
   LOOP UNTIL (NITER > 0)
  
   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' solve Poisson's Equation

   CALL POISSON(A, B, C, D, NX, NY, TOL, NITER, X(), Y(), W())
  
   ' print results

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPDE1"
   PRINT
   PRINT TAB(24); "< Solution of Poisson's Equation >"
   PRINT
   PRINT
   PRINT TAB(25); "Initial x endpoint     = "; A
   PRINT TAB(25); "Final x endpoint       = "; B
   PRINT
   PRINT TAB(25); "Initial y endpoint     = "; C
   PRINT TAB(25); "Final y endpoint       = "; D
   PRINT
   PRINT TAB(25); "Number of x grid lines = "; NX
   PRINT TAB(25); "Number of y grid lines = "; NY
   PRINT
   PRINT TAB(25); "Convergence tolerance  = "; TOL
   PRINT
   PRINT TAB(25); "Number of iterations   = "; NITER

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Display solution array ( y = yes, n = no )"
      INPUT SA$
      SA$ = UCASE$(SA$)
   LOOP UNTIL (SA$ = "Y" OR SA$ = "N")

   DO
      PRINT
      PRINT
      PRINT "Display solution error array ( y = yes, n = no )"
      INPUT EA$
      EA$ = UCASE$(EA$)
   LOOP UNTIL (EA$ = "Y" OR EA$ = "N")

   DX = (B - A) / NX
   DY = (D - C) / NY

   IF (SA$ = "Y") THEN
      CLS
      LOCATE 3, 1
      PRINT TAB(31); "< Solution Array >"
      PRINT
      PRINT
      PRINT TAB(14); "J = 1 ";

      FOR J = 2 TO (NY - 1)
          PRINT SPACE$(12); J;
      NEXT J

      PRINT
      PRINT

      FOR I = 1 TO NX - 1
          PRINT TAB(5); "I ="; I; "  ";
          FOR J = 1 TO NY - 1
              PRINT USING "##.######^^^^"; W(I, J);
              PRINT "  ";
          NEXT J
          PRINT
          PRINT
      NEXT I
  
      CALL KEYCHECK
   END IF

   IF (EA$ = "Y") THEN
      CLS
      LOCATE 3, 1
      PRINT TAB(28); "< Solution Error Array >"
      PRINT
      PRINT
      PRINT TAB(14); "J = 1 ";

      FOR J = 2 TO (NY - 1)
          PRINT SPACE$(12); J;
      NEXT J

      PRINT
      PRINT

      FOR I = 1 TO NX - 1
          PRINT TAB(5); "I ="; I; "  ";
          X = A + I * DX
          FOR J = 1 TO NY - 1
              Y = C + J * DY
              PRINT USING "##.######^^^^"; ABS(W(I, J) - X * EXP(Y));
              PRINT "  ";
          NEXT J
          PRINT
          PRINT
      NEXT I
 
      CALL KEYCHECK
   END IF

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, Y, W

LOOP UNTIL (SLCT$ = "N")

END

