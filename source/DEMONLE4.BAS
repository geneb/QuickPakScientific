' Program "DEMONLE4"                   December 6, 1992
'
' Copyright (c) 1992 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine LMNLE which solves a system of non-linear
' equations using a Levenberg-Marquardt algorithm.
'
' LMNLE requires a subroutine called USERFUNC which
' returns the values of each of the non-linear equations.
'
' This program demonstrates the use of LMNLE to solve
' the system of non-linear equations given by;
'
'   3*X1 - COS(X2*X3) - .5 = 0
'
'   X1^2 - 81*(X2 + .1)^2 + SIN(X3) + 1.06 = 0
'
'   EXP(-X1*X2) + 20*X3 + (10*PI - 3 ) / 3 = 0
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB LMNLE (N, MAXITER, EPS, PI(), PU(), PL(), POPT(), NITER)
DECLARE SUB KEYCHECK ()

PI = 4# * ATN(1#)

' Define number of variables

NVAR = 3

' Dimension solution vector and working arrays

DIM PI(NVAR), PL(NVAR), PU(NVAR), POPT(NVAR)

' Initial guesses for solution

PI(1) = .4#
PI(2) = .1#
PI(3) = -.45#

' Upper and lower bounds for each variable

PU(1) = 10#
PL(1) = -10#

PU(2) = 10#
PL(2) = -10#

PU(3) = 10#
PL(3) = -10#

' ****************************************************

DO
   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMONLE4"
   PRINT
   PRINT TAB(16); "< Solution of a System of Non-linear Equations >"
   PRINT
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine LMNLE which solves a system of non-linear"
   PRINT TAB(12); "equations using a Levenberg-Marquardt algorithm."
   PRINT
   PRINT TAB(12); "LMNLE requires a subroutine called USERFUNC which"
   PRINT TAB(12); "returns the values of each of the non-linear equations."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of LMNLE to solve"
   PRINT TAB(12); "the system of non-linear equations given by;"
   PRINT
   PRINT TAB(12); "  3*X1 - COS(X2*X3) - .5 = 0"
   PRINT
   PRINT TAB(12); "  X1^2 - 81*(X2 + .1)^2 + SIN(X3) + 1.06 = 0"
   PRINT
   PRINT TAB(12); "  EXP(-X1*X2) + 20*X3 + (10*PI - 3 ) / 3 = 0"

   CALL KEYCHECK

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NVAR
       PRINT
       PRINT "Please input the initial value of variable #"; I
       PRINT "( Try a value of "; PI(I); " )"
       INPUT PI(I)
   NEXT I

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum number of iterations"
      PRINT "( a value of between 10 and 50 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   DO
      PRINT
      PRINT
      PRINT "Please input the convergence criteria"
      PRINT "( a value of 1D-5 is recommended )"
      INPUT EPS
   LOOP UNTIL (EPS > 0#)

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   ' solve system of non-linear equations

   CALL LMNLE(NVAR, MAXITER, EPS, PI(), PU(), PL(), POPT(), NITER)

   ' print results

   CLS
   PRINT
   PRINT TAB(32); "Program DEMONLE4"
   PRINT
   PRINT TAB(16); "< Solution of a System of Non-linear Equations >"
   PRINT
   FOR I = 1 TO NVAR
       PRINT
       PRINT TAB(15); "Value of variable #"; I; TAB(50); POPT(I)
   NEXT I
   PRINT
   PRINT
   PRINT TAB(15); "Convergence criteria"; TAB(50); EPS
   PRINT
   PRINT TAB(15); "Number of iterations"; TAB(50); NITER

   X1 = POPT(1)
   X2 = POPT(2)
   X3 = POPT(3)

   ' compute value of each non-linear equation

   Y1 = 3# * X1 - COS(X2 * X3) - .5#
   Y2 = X1 * X1 - 81# * (X2 + .1#) ^ 2 + SIN(X3) + 1.06#
   Y3 = EXP(-X1 * X2) + 20# * X3 + (10# * PI - 3#) / 3#

   PRINT
   PRINT
   PRINT TAB(15); "Function Values"
   PRINT
   PRINT TAB(25); "Y1 = "; Y1
   PRINT TAB(25); "Y2 = "; Y2
   PRINT TAB(25); "Y3 = "; Y3

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

