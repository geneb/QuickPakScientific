' Program "DEMORKF"                    May 18, 1993
'
' Copyright (c) 1988, 1989, 1990, 1993 Crescent Software
'
' This program demonstrates the procedure for calling the
' the subroutines RKF45, RKF56 and RKF78 which solve a system
' of first order vector differential equations of the form
' y' = f(y, t) using the Runge-Kutta-Fehlberg method.
'
' DEMORKF requires a subroutine called DERIVATIVE which
' evaluates a system of first order differential equations
' defined by the user. The user must also specify the
' initial conditions, an initial step size guess, and a
' truncation error tolerance.
'
' This program demonstrates the use of RKF45, RKF56 and RKF78
' to integrate the system of differential equations defined by
'                     
'     Y1' = Y2 - Y3 + t
'     Y2' = 3 t^2 
'     Y3' = Y2 + e^(-t)
'
'********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB RKF45 (NEQ, TI, TF, H, TETOL, Y())
DECLARE SUB RKF56 (NEQ, TI, TF, H, TETOL, Y())
DECLARE SUB RKF78 (NEQ, TI, TF, H, TETOL, Y())
DECLARE SUB KEYCHECK ()
DECLARE SUB DERIVATIVE (T, Y(), Z())

' Dimension the solution vector for three equations

NEQ = 3

DIM Y(NEQ), YE(NEQ)

DO
   CLS
   PRINT
   PRINT TAB(32); "Program DEMORKF"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling the"
   PRINT TAB(12); "subroutines RKF45, RKF56 and RKF78 which solve a system of first"
   PRINT TAB(12); "order vector differential equations of the form y' = f(y, t)."
   PRINT
   PRINT TAB(12); "DEMORKF requires a subroutine called DERIVATIVE which"
   PRINT TAB(12); "evaluates a system of first order differential equations"
   PRINT TAB(12); "defined by the user. The user must also specify the"
   PRINT TAB(12); "initial conditions, an initial step size guess, and a"
   PRINT TAB(12); "truncation error tolerance."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of RKF45, RKF56 and RKF78"
   PRINT TAB(12); "to integrate the system of differential equations defined by"
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
      INPUT H
   LOOP UNTIL (ABS(H) > 0#)

   DTSAVED = H

   DO
      PRINT
      PRINT "Please input the truncation error tolerance"
      PRINT "< a value of 1D-10 is recommended >"
      INPUT TETOL
   LOOP UNTIL (TETOL > 0#)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please indicate the solution method"
      PRINT "( 1 = RKF45,  2 = RKF56, 3 = RKF78 )"
      INPUT METHOD
   LOOP UNTIL (METHOD = 1 OR METHOD = 2 OR METHOD = 3)

   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' solve system of equations

   SELECT CASE METHOD
   CASE 1
      CALL RKF45(NEQ, TI, TF, H, TETOL, Y())
   CASE 2
      CALL RKF56(NEQ, TI, TF, H, TETOL, Y())
   CASE 3
      CALL RKF78(NEQ, TI, TF, H, TETOL, Y())
   END SELECT

   CLS
   PRINT
   PRINT TAB(32); "Program DEMORKF"
   PRINT
   PRINT TAB(15); "< Solution of First Order Differential Equations >"
   PRINT
   PRINT
   PRINT TAB(15); "Initial step size"; TAB(45); DTSAVED
   PRINT
   PRINT TAB(15); "Truncation error tolerance"; TAB(45); TETOL
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


