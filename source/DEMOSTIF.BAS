' Program "DEMOSTIF"                   September 3, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine STIFF which solves systems of stiff
' ordinary differential equations.
'
' Subroutine STIFF requires a subroutine called DIFFEQS
' which evaluates the system of user-defined differential
' equations and a subroutine called JACOBIAN which computes
' the partial derivatives of this system.
'
' This program demonstrates the use of STIFF to solve
' the following system of stiff differential equations;
'
'  y1' = -0.013 * y1 - 1000 * y1 * y2
'
'  y2' = -2500 * y2 * y3
'
'  y3' = -0.013 * y1 - 1000 * y1 * y3 - 2500 * y2 * y3
'
' ****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB STIFF (N, Y(), X1, X2, EPS, H1, HMIN)
DECLARE SUB KEYCHECK ()

' Define number of differential equations

NEQS = 3

' dimension solution vector

DIM Y(NEQS)

DO
   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMOSTIF"
   LOCATE 4, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine STIFF which solves systems of stiff"
   PRINT TAB(12); "ordinary differential equations."
   PRINT
   PRINT TAB(12); "Subroutine STIFF requires a subroutine called DIFFEQS"
   PRINT TAB(12); "which evaluates the system of user-defined differential"
   PRINT TAB(12); "equations and a subroutine called JACOBIAN which computes"
   PRINT TAB(12); "the partial derivatives of this system."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of STIFF to solve"
   PRINT TAB(12); "the following system of stiff differential equations;"
   PRINT
   PRINT TAB(12); "  y1' = -0.013 * y1 - 1000 * y1 * y2"
   PRINT
   PRINT TAB(12); "  y2' = -2500 * y2 * y3"
   PRINT
   PRINT TAB(12); "  y3' = -0.013 * y1 - 1000 * y1 * y3 - 2500 * y2 * y3"

   CALL KEYCHECK
 
   DO
      CLS
      LOCATE 5, 1
      PRINT
      PRINT "Please input the initial value of the independent variable X"
      INPUT X1
   LOOP UNTIL (X1 >= 0#)

   DO
      PRINT
      PRINT "Please input the final value of the independent variable X"
      INPUT X2
   LOOP UNTIL (X2 > X1)

   PRINT

   FOR I = 1 TO NEQS
       PRINT
       PRINT "Please input the initial value of the dependent variable vector Y("; I; ")"
       INPUT Y(I)
   NEXT I

   CLS
   LOCATE 5, 1
   PRINT "Please input the initial step size"
   PRINT "( a value of .0002 is recommended )"
   INPUT H1

   DO
      PRINT
      PRINT "Please input the minimum step size"
      PRINT "( this number must be >= 0 )"
      INPUT HMIN
   LOOP UNTIL (HMIN >= 0#)

   DO
      PRINT
      PRINT "Please input the convergence criteria"
      PRINT "( a value of 1D-6 is recommended )"
      INPUT EPS
   LOOP UNTIL (EPS > 0#)

   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7
 
   CALL STIFF(NEQS, Y(), X1, X2, EPS, H1, HMIN)

   CLS
   PRINT
   PRINT TAB(32); "Program DEMOSTIF"
   PRINT
   PRINT TAB(19); "< Solution of Stiff Differential Equations >"
   PRINT
   PRINT
   PRINT TAB(32); "Final Conditions"
   PRINT

   FOR I = 1 TO NEQS
       PRINT TAB(15); "Y("; I; ")"; TAB(45); Y(I)
   NEXT I

   PRINT
   PRINT
   PRINT TAB(15); "Initial step size"; TAB(45); H1
   PRINT
   PRINT TAB(15); "Minimum step size"; TAB(45); HMIN
   PRINT
   PRINT TAB(15); "Convergence criteria"; TAB(45); EPS

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


