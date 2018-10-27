' Program "DEMOAPC"                    May 18, 1993
'
' Copyright (c) 1989, 1990, 1993 Crescent Software
'
' This program demonstrates the procedure for calling the
' the subroutine ADAMSPC which solves a system of first
' order vector differential equations of the form
' y' = f(y, t) using an Adams/Bashforth/Moulton method.
'
' DEMOAPC requires a subroutine called DERIVATIVE which
' evaluates a system of first order differential equations
' defined by the user. The user must also specify the
' initial conditions, a step size, and the order of the
' numerical integration.
'
' This program demonstrates the use of DEMOAPC to integrate
' the system of differential equations defined by
'                    
'   Y1' = -4 * Y(1) - 2 * Y(2) + COS(T) + 4 SIN(T)
'
'   Y2' =  3 * Y(1) + Y(2) - 3 SIN(T)
'
' ********************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, Y(), YDOT())
DECLARE SUB RKF56 (NEQ, TI, TF, H, TOL, Y())
DECLARE SUB ADAMSPC (INIZ, NEQ, NORD, TIME, DT, Y())
DECLARE SUB KEYCHECK ()

' Dimension the solution vector for two
' first-order vector differential equations

NEQ = 2

DIM Y(NEQ), YE(NEQ)

DO
   CLS
   PRINT
   PRINT TAB(32); "Program DEMOAPC"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling the"
   PRINT TAB(12); "subroutine ADAMSPC which solves a system of first order"
   PRINT TAB(12); "vector differential equations of the form y' = f(y, t)."
   PRINT
   PRINT TAB(12); "DEMOAPC requires a subroutine called DERIVATIVE which"
   PRINT TAB(12); "evaluates a system of first order differential equations"
   PRINT TAB(12); "defined by the user. The user must also specify the"
   PRINT TAB(12); "initial conditions, a step size, and the order of the"
   PRINT TAB(12); "numerical integration."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of ADAMSPC to integrate"
   PRINT TAB(12); "the system of differential equations defined by"
   PRINT
   PRINT TAB(15); "Y1' = -4 * Y(1) - 2 * Y(2) + COS(T) + 4 SIN(T)"
   PRINT
   PRINT TAB(15); "Y2' =  3 * Y(1) + Y(2) - 3 SIN(T)  "

   CALL KEYCHECK

   ' request initial conditions

   CLS
   LOCATE 5, 1
   FOR I = 1 TO NEQ
       PRINT
       PRINT "Please input the initial value of Y("; I; ")"
       INPUT Y(I)
   NEXT I
 
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the initial time"
      INPUT TI
   LOOP UNTIL (ABS(TI) >= 0#)

   TISAVED = TI

   DO
      PRINT
      PRINT "Please input the final time"
      INPUT TF
   LOOP UNTIL (ABS(TF) > ABS(TI))

   DO
      PRINT
      PRINT "Please input the integration step size"
      PRINT "< a value of .01 is recommended >"
      INPUT DT
   LOOP UNTIL (ABS(DT) > 0#)

   DTSAVED = DT

   DO
      PRINT
      PRINT "Please input the order of integration"
      PRINT "< an order of 4 or 5 is recommended >"
      INPUT NORD
   LOOP UNTIL (NORD > 0)

   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' algorithm initialization
  
   CALL ADAMSPC(1, NEQ, NORD, TI, DT, Y())
  
   ' solve system of equations
  
   WHILE (ABS(TF - TI) > .0000000001#)
      A = TF - TI
      IF (ABS(A) < DT) THEN DT = A * SGN(A)
      CALL ADAMSPC(0, NEQ, NORD, TI, DT, Y())
   WEND

   CLS
   PRINT
   PRINT TAB(32); "Program DEMOAPC"
   PRINT
   PRINT TAB(15); "< Solution of First Order Differential Equations >"
   PRINT
   PRINT
   PRINT TAB(15); "Initial time"; TAB(45); TISAVED
   PRINT TAB(15); "Final time"; TAB(45); TF
   PRINT
   PRINT TAB(15); "Integration step size"; TAB(45); DTSAVED
   PRINT
   PRINT TAB(15); "Order of integration"; TAB(45); NORD
   PRINT
   PRINT
   PRINT TAB(32); "Final Conditions"
   PRINT

   FOR I = 1 TO NEQ
       PRINT TAB(15); "Y("; I; ")"; TAB(45); Y(I)
   NEXT I
 
   ' compute exact solutions

   YE(1) = 2# * EXP(-TF) - 2# * EXP(-2# * TF) + SIN(TF)

   YE(2) = -3# * EXP(-TF) + 2# * EXP(-2# * TF)

   ' print solution errors

   PRINT
   PRINT TAB(32); "Solution Errors"
   PRINT

   FOR I = 1 TO NEQ
       PRINT TAB(15); "ERROR("; I; ")";
       PRINT TAB(45); ABS(Y(I) - YE(I));
   NEXT I
  
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "N" OR SLCT$ = "Y")

LOOP UNTIL (SLCT$ = "N")

END


