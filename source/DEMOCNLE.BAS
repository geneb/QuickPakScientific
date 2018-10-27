' Program "DEMOCNLE"                   December 27, 1992
'
' Copyright (c) 1992 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine CNLE which solves a constrained system
' of non-linear equations.
'
' CNLE requires a subroutine called USERFUNC which
' returns the function values of a user-defined system
' of non-linear equations. The Jacobian of the non-linear
' system is also required. It can either be provided by
' the user in a subroutine called JACOBIAN or calculated
' numerically by the CNLE software.
'
' This program demonstrates the use of CNLE to solve
' the system of non-linear equations defined by;
'
'   X1/X2 - 5 * LN(0.4*X2/X3) + 4.45977 = 0
'
'   X2 + X1 - 1 = 0
'
'   X3 + 0.5 * X1 - 0.4 = 0
'
' subject to the constraints
'
'   X1 >= 0, X2 > 0, and X3 > 0.
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB CNLE (N, X(), ICON(), TOL, MAXIT, IDER, Y(), INFO, NITER)
DECLARE SUB USERFUNC (X(), Y())
DECLARE SUB JACOBIAN (X(), DY())
DECLARE SUB LINEAR1 (N, A(), B(), X(), IER)
DECLARE SUB KEYCHECK ()

' define number of unknowns for this example
' NOTE: N >= 2

N = 3

DIM X(N), Y(N)
   
' define type of constraints for each unknown

ICON(1) = 2
ICON(2) = 2
ICON(3) = 2
   
DO
   CLS
   PRINT TAB(32); "Program DEMOCNLE"
   PRINT
   PRINT TAB(12); " This program demonstrates the procedure for calling"
   PRINT TAB(12); " subroutine CNLE which solves a constrained system"
   PRINT TAB(12); " of non-linear equations."
   PRINT
   PRINT TAB(12); " CNLE requires a subroutine called USERFUNC which"
   PRINT TAB(12); " returns the function values of a user-defined system"
   PRINT TAB(12); " of non-linear equations. The Jacobian of the non-linear"
   PRINT TAB(12); " system is also required. It can either be provided by"
   PRINT TAB(12); " the user in a subroutine called JACOBIAN or calculated"
   PRINT TAB(12); " numerically by the CNLE software."
   PRINT
   PRINT TAB(12); " This program demonstrates the use of CNLE to solve"
   PRINT TAB(12); " the system of non-linear equations defined by;"
   PRINT
   PRINT TAB(12); "    X1/X2 - 5 * LN(0.4*X2/X3) + 4.45977 = 0"
   PRINT TAB(12); "    X2 + X1 - 1 = 0"
   PRINT TAB(12); "    X3 + 0.5 * X1 - 0.4 = 0"
   PRINT
   PRINT TAB(12); " subject to the constraints"
   PRINT
   PRINT TAB(12); "    X1 >= 0, X2 > 0, and X3 > 0."

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum number of iterations"
      PRINT "( a value of between 50 and 100 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   DO
      PRINT
      PRINT
      PRINT "Please input the convergence criteria"
      PRINT "( a value of 1D-5 is recommended )"
      INPUT TOL
   LOOP UNTIL (TOL > 0#)
 
   ' define type of derivative calculation
   
   DO
      PRINT
      PRINT
      PRINT "Please input the type of Jacobian calculation"
      PRINT "( 1 = analytic, 2 = numerical )"
      INPUT IDER
   LOOP UNTIL (IDER = 1 OR IDER = 2)

   ' request initial guesses

   CLS
   LOCATE 5, 1

   FOR I = 1 TO N
       PRINT "Please input the initial guess for X("; I; ")"
       INPUT X(I)
       PRINT
   NEXT I

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL CNLE(N, X(), ICON(), TOL, MAXITER, IDER, Y(), INFO, NITER)

   CLS
   PRINT TAB(32); "Program DEMOCNLE"
   PRINT
   PRINT TAB(16); "< Solution of a Constrained Non-linear System >"
   PRINT

   SELECT CASE INFO
   CASE 0
      ' NULL
   CASE 1
      PRINT TAB(27); "Improper input parameters!"
   CASE 2
      PRINT TAB(11); "Initial estimate of solution does not satisfy constraints!"
   CASE 3
      PRINT TAB(25); "Number of iterations >= MAXIT!"
   CASE 4
      PRINT TAB(23); "DTHETA has reached a minimal value!"
   CASE 5
      PRINT TAB(17); "Iteration is not making progress or is diverging!"
   END SELECT
   
   PRINT
   PRINT TAB(32); "Solution Vector"

   FOR I = 1 TO N
       PRINT
       PRINT TAB(24); "X("; I; ") = "; X(I)
   NEXT I

   PRINT
   PRINT TAB(35); "Residuals"
   
   FOR I = 1 TO N
       PRINT
       PRINT TAB(24); "Y("; I; ") = "; Y(I)
   NEXT I

   PRINT
   PRINT TAB(24); "Number of iterations = "; NITER

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

