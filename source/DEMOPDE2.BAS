' Program "DEMOPDE2"                   August 13, 1993
'
' Copyright (c) 1991, 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine HEAT which solves the parabolic partial
' differential equation of the form
'
'    du(x,t)/dt - alpha^2 * d2u(x,t)/dt2 = 0
'
'    0 < x < l,  0 < t < T
'
' subject to the boundary conditions
'
'    u(0,t) = u(l,t) = 0   for  0 < t < T
'
' and the initial conditions
'
'    u(x,0) = f(x)   for 0 <= x <= l
'
' DEMOPDE2 requires a subroutine which defines the "forcing"
' function y = f(x). This forcing function must be coded in
' a subroutine called FFUN.BAS.
'
' For this example the forcing function is as follows:
'
'    f(x) = sin(pi*x) + 3 sin(2*pi*x)
'
' ***********************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB HEAT (ALPHA, NX, NT, XL, T, S())
DECLARE SUB FFUN (X, FVAL)
DECLARE SUB KEYCHECK ()

' utility constants

PI = 4# * ATN(1#)
PI2 = 2# * PI

' Begin demo program

DO
   CLS
   PRINT
   PRINT TAB(32); "Program DEMOPDE2"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine HEAT which solves the parabolic partial"
   PRINT TAB(12); "differential equation of the form"
   PRINT
   PRINT TAB(12); "   du(x,t)/dt - alpha^2 * d2u(x,t)/dt2 = 0"
   PRINT
   PRINT TAB(12); "   0 < x < l,  0 < t < T"
   PRINT
   PRINT TAB(12); "subject to the boundary conditions"
   PRINT
   PRINT TAB(12); "   u(0,t) = u(l,t) = 0   for  0 < t < T"
   PRINT
   PRINT TAB(12); "and the initial conditions"
   PRINT
   PRINT TAB(12); "   u(x,0) = f(x)   for 0 <= x <= l      "
   PRINT
   PRINT TAB(12); "DEMOPDE2 requires a subroutine which defines the forcing"
   PRINT TAB(12); "function y = f(x). This forcing function must be coded in"
   PRINT TAB(12); "a subroutine called FFUN.BAS. For this example the forcing"
   PRINT TAB(12); "function is  f(x) = sin(pi*x) + 3 sin(2*pi*x)"

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

   ' dimension solution arrays

   REDIM S(NX, NT), USOL(NX, NT)

   CLS
   LOCATE 5, 1
   PRINT "Please input the spatial endpoint"
   INPUT XF
   PRINT
   PRINT
   PRINT "Please input the maximum time"
   INPUT TF
   
   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' solve the heat equation
  
   CALL HEAT(ALPHA, NX, NT, XF, TF, S())
  
   ' print results

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPDE2"
   PRINT
   PRINT TAB(23); "< Solution of the heat equation >"
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
      ' display solution array
      FOR I = 1 TO (NX - 1)
          CLS
          PRINT TAB(31); "< Solution Array >"
          X = I * DX
          PRINT
          PRINT TAB(5); "I ="; I; "   "; "J = 1 to"; NT - 1
          PRINT
          FOR J = 1 TO (NT - 1)
              PRINT USING "##.######^^^^"; S(I, J);
              PRINT "  ";
          NEXT J
          CALL KEYCHECK
          CLS
      NEXT I
   END IF
  
   IF (EA$ = "Y") THEN
      ' display solution error array
      FOR I = 1 TO (NX - 1)
          CLS
          PRINT TAB(28); "< Solution Error Array >"
          X = I * DX
          PRINT
          PRINT TAB(5); "I ="; I; "   "; "J = 1 to"; NT - 1
          PRINT
          FOR J = 1 TO (NT - 1)
              T = J * DT
              ' exact solution
              USOL(I, J) = SIN(PI * X) * EXP(-PI * PI * T) + 3# * SIN(PI2 * X) * EXP(-4# * PI * PI * T)
              PRINT USING "##.######^^^^"; ABS(S(I, J) - USOL(I, J));
              PRINT "  ";
          NEXT J
          CALL KEYCHECK
          CLS
      NEXT I
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

