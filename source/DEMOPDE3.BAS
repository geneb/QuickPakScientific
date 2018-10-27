' Program "DEMOPDE3"                   August 13, 1993
'
' Copyright (c) 1991, 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine WAVE which solves the hyperbolic partial
' differential equation of the form
'
'    d2u(x,t)/dt2 - alpha^2 * d2u(x,t)/dx2 = 0
'
'    0 < x < l,  0 < t < T
'
' subject to the boundary conditions
'
'    u(0,t) = u(l,t) = 0   for  0 < t < T
'
' and the initial conditions
'
'    u(x,0) = f(x)   for  0 <= x <= l
'
'    du/dt(x,0) = g(x)  for  0 <= x <= l
'
' DEMOPDE3 requires a subroutine which defines the initial
' conditions functions f(x) and g(x). The first function must
' be in a subroutine called FFUN.BAS and g(x) is defined in
' the subroutine GFUN.BAS.
'
' For this example the initial conditions functions are:
'
'    f(x) = sin(pi*x)   and   g(x) = 0
'
' ***********************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB WAVE (ALPHA, NX, NT, XL, T, S())
DECLARE SUB FFUN (X, FVAL)
DECLARE SUB GFUN (X, FVAL)
DECLARE SUB KEYCHECK ()

' utility constants

PI = 4# * ATN(1#)
PI2 = 2# * PI

' Begin demo program

DO
   CLS
   PRINT TAB(32); "Program DEMOPDE3"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine WAVE which solves the hyperbolic partial"
   PRINT TAB(12); "differential equation of the form"
   PRINT
   PRINT TAB(12); "   d2u(x,t)/dt2 - alpha^2 * d2u(x,t)/dt2 = 0"
   PRINT
   PRINT TAB(12); "subject to the boundary conditions"
   PRINT
   PRINT TAB(12); "   u(0,t) = u(l,t) = 0   for  0 < t < T"
   PRINT
   PRINT TAB(12); "and the initial conditions"
   PRINT
   PRINT TAB(12); "   u(x,0) = f(x)   for 0 <= x <= l      "
   PRINT
   PRINT TAB(12); "   du/dt(x,0) = g(x)  for 0 <= x <= l"
   PRINT
   PRINT TAB(12); "DEMOPDE3 requires a subroutine which defines the initial"
   PRINT TAB(12); "conditions functions f(x) and g(x). The first function must"
   PRINT TAB(12); "be in a subroutine called FFUN.BAS and g(x) is defined in"
   PRINT TAB(12); "the subroutine GFUN.BAS."

   CALL KEYCHECK

   CLS
   LOCATE 5, 1
   PRINT "Please input the equation constant ALPHA"
   INPUT ALPHA
   
   DO
     PRINT
     PRINT
     PRINT "Please input the number of spatial increments"
     INPUT NX
   LOOP UNTIL (NX > 0)
  
   DO
     PRINT
     PRINT "Please input the number of time increments"
     INPUT NT
   LOOP UNTIL (NT > 0)

   ' dimension solution array

   DIM S(NX, NT)

   CLS
   LOCATE 5, 1
   PRINT "Please input the final spatial value"
   INPUT XF
   PRINT
   PRINT
   PRINT "Please input the final time"
   INPUT TF
   
   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' solve the wave equation
  
   CALL WAVE(ALPHA, NX, NT, XF, TF, S())
  
   ' print results

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPDE3"
   PRINT
   PRINT TAB(23); "< Solution of the wave equation >"
   PRINT
   PRINT
   PRINT TAB(20); "Final spatial value          = "; XF
   PRINT
   PRINT TAB(20); "Final time value             = "; TF
   PRINT
   PRINT TAB(20); "Number of spatial increments = "; NX
   PRINT
   PRINT TAB(20); "Number of time increments    = "; NT

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
  
   DX = XF / NX
   DT = TF / NT

   IF (SA$ = "Y") THEN
      FOR J = 0 TO NT
          CLS
          LOCATE 3, 1
          PRINT TAB(31); "< Solution Array >"
          PRINT
          PRINT
          T = J * DT
          PRINT TAB(5); "T =";
          PRINT USING "####.####"; T;
          PRINT "    "; "X = 0 to"; XF; "    "; "DX =";
          PRINT USING "####.####"; DX
          PRINT
          FOR I = 0 TO NX
              PRINT USING "##.######^^^^"; S(I, J);
              PRINT "  ";
          NEXT I
          CALL KEYCHECK
      NEXT J
   END IF

   IF (EA$ = "Y") THEN
      FOR J = 0 TO NT
          CLS
          LOCATE 3, 1
          PRINT TAB(28); "< Solution Error Array >"
          PRINT
          PRINT
          T = J * DT
          PRINT TAB(5); "T =";
          PRINT USING "####.####"; TF;
          PRINT "    "; "X = 0 to"; XF; "    "; "DX =";
          PRINT USING "####.####"; DX
          PRINT
          FOR I = 0 TO NX
              X = I * DX
              PRINT USING "##.######^^^^"; ABS(S(I, J) - SIN(PI * X) * COS(PI2 * T));
              PRINT "  ";
          NEXT I
          CALL KEYCHECK
      NEXT J
   END IF

   ' request another selection

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END

