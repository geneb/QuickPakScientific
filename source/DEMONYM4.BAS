' Program "DEMONYM4"                   January 1, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine NYM4 which solves a system of second order
' vector differential equations of the form
'
'    x'' = f(x, x', t).
'
' DEMONYM4 requires a subroutine called DERIVATIVE which
' evaluates a system of second order differential equations
' defined by the user. The user must also specify the
' initial conditions for each equation and input an
' initial time, integration step size and a final time.
'
' This program demonstrates the use of NYM4 to integrate
' the equations of motion of an earth-orbiting spacecraft.
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB NYM4 (N, TP, DT, X1(), XDOT1())
DECLARE SUB DERIVATIVE (T, X(), XDOT(), XDDOT())
DECLARE SUB KEYCHECK ()

' Dimension the solution vector for three equations

NEQ = 3

DIM R(NEQ), V(NEQ)

DO
   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMONYM4"
   LOCATE 6, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine NYM4 which solves a system of second order"
   PRINT TAB(12); "differential equations of the form x'' = f(x, x', t)."
   PRINT
   PRINT TAB(12); "DEMONYM4 requires a subroutine called DERIVATIVE which"
   PRINT TAB(12); "evaluates a system of second order differential equations"
   PRINT TAB(12); "defined by the user. The user must also specify the"
   PRINT TAB(12); "initial conditions for each equation and input an"
   PRINT TAB(12); "initial time, integration step size and a final time."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of NYM4 to integrate"
   PRINT TAB(12); "the equations of motion of an earth-orbiting spacecraft."

   CALL KEYCHECK

   CLS
   LOCATE 5, 1

   PRINT "Please input the x component of the radius vector ( feet )"
   INPUT R(1)
   PRINT
   PRINT "Please input the y component of the radius vector ( feet )"
   INPUT R(2)
   PRINT
   PRINT "Please input the z component of the radius vector ( feet )"
   INPUT R(3)
   PRINT
   PRINT
   PRINT "Please input the x component of the velocity vector ( feet per second )"
   INPUT V(1)
   PRINT
   PRINT "Please input the y component of the velocity vector ( feet per second )"
   INPUT V(2)
   PRINT
   PRINT "Please input the z component of the velocity vector ( feet per second )"
   INPUT V(3)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the initial time ( seconds )"
      INPUT TI
   LOOP UNTIL (ABS(TI) >= 0#)

   TISAVED = TI
  
   DO
      PRINT
      PRINT "Please input the final time ( seconds )"
      INPUT TF
   LOOP UNTIL (ABS(TF) > ABS(TI))

   DO
      PRINT
      PRINT "Please input the integration step size ( seconds )"
      INPUT DT
   LOOP UNTIL (ABS(DT) > 0#)

   DTSAVED = DT
  
   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   WHILE (ABS(TF - TI) > .00000001#)
      A = TF - TI
      IF (ABS(A) < DT) THEN DT = A * SGN(A)
      CALL NYM4(NEQ, TI, DT, R(), V())
      TI = TI + DT
   WEND

   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMONYM4"
   PRINT
   PRINT TAB(15); "< Solution of Second Order Differential Equations >"
   PRINT
   PRINT TAB(15); "Initial time"; TAB(45); TISAVED
   PRINT
   PRINT TAB(15); "Final time"; TAB(45); TF
   PRINT
   PRINT TAB(15); "Step size"; TAB(45); DTSAVED
   PRINT
   PRINT
   PRINT TAB(32); "Final Conditions"
   PRINT

   FOR I = 1 TO NEQ
       PRINT TAB(15); "R("; I; ")"; TAB(45); R(I)
   NEXT I

   PRINT
   PRINT

   FOR I = 1 TO NEQ
       PRINT TAB(15); "V("; I; ")"; TAB(45); V(I)
   NEXT I

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END

