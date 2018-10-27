' Program "DEMORK4"                    December 31, 1990
'
' Copyright (c) 1990 Crescent Software
'
' This program demonstrates the procedure for calling the
' the subroutines RK4 which solves a system of first order
' vector differential equations of the form y' = f(y, t)
' using the fourth-order Runge-Kutta method.
'
' DEMORK4 requires a subroutine called DERIVATIVE which
' evaluates a system of first order differential equations
' defined by the user. The user must also specify the
' initial conditions and an integration step size.
'
' This program demonstrates the use of RK4 to integrate the
' system of differential equations defined by
'                      
'     Y1' = Y2 - Y3 + t
'     Y2' = 3 t^2  
'     Y3' = Y2 + e^(-t)
'
'********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB RK4 (NEQ, TIME, DT, Y())
DECLARE SUB KEYCHECK ()
DECLARE SUB DERIVATIVE (T, Y(), Z())

' Dimension the integration vector for three equations

NEQ = 3

DIM Y(NEQ), YE(NEQ)

DO
   CLS
   PRINT
   PRINT TAB(32); "Program DEMORK4"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling the"
   PRINT TAB(12); "subroutine RKF4 which solves a system of first order"
   PRINT TAB(12); "vector differential equations of the form y' = f(y, t)"
   PRINT TAB(12); "using the fourth-order Runge-Kutta method."
   PRINT
   PRINT TAB(12); "DEMORK4 requires a subroutine called DERIVATIVE which"
   PRINT TAB(12); "evaluates a system of first order differential equations"
   PRINT TAB(12); "defined by the user. The user must also specify the"
   PRINT TAB(12); "initial conditions and an integration step size."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of RK4 to integrate"
   PRINT TAB(12); "the system of differential equations defined by"
   PRINT
   PRINT TAB(27); "  Y1' = Y2 - Y3 + t"
   PRINT
   PRINT TAB(27); "  Y2' = 3 t^2"
   PRINT
   PRINT TAB(27); "  Y3' = Y2 + e^(-t)"

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

   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' solve system of equations

   WHILE (ABS(TF - TI) > .000001#)
      A = TF - TI
      IF (ABS(A) < DT) THEN DT = A * SGN(A)
      CALL RK4(NEQ, TI, DT, Y())
   WEND

   CLS
   PRINT
   PRINT TAB(32); "Program DEMORK4"
   PRINT
   PRINT TAB(9); "< Runge-Kutta Solution of First Order Differential Equations >"
   PRINT
   PRINT
   PRINT TAB(15); "Step size"; TAB(45); DTSAVED
   PRINT
   PRINT
   PRINT TAB(32); "Final Conditions"
   PRINT

   FOR I = 1 TO NEQ
       PRINT TAB(15); "Y("; I; ")"; TAB(45); Y(I)
   NEXT I
  
   ' compute exact solutions

   YE(1) = -.05# * TF ^ 5 + .25# * TF ^ 4 + TF + 2# - EXP(-TF)

   YE(2) = TF ^ 3 + 1#

   YE(3) = .25# * TF ^ 4 + TF - EXP(-TF)

   ' print solution errors

   PRINT
   PRINT TAB(32); "Solution Errors"

   FOR I = 1 TO NEQ
       PRINT
       PRINT TAB(15); "Error("; I; ")";
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


