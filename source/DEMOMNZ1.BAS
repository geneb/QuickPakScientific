' Program "DEMOMNZ1"                   December 29, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine MINIMIZ1 which finds a minimum (or maximum)
' of a scalar function of several variables.
'
' MINIMIZ1 requires a subroutine called USERFUNC which
' returns the function and analytic gradient values of a
' user-defined scalar function of several variables.
'
' This program demonstrates the use of MINIMIZ1 to find
' a minimum of Wood's function which is defined by;
'
' F(x) = 100(x2 - x1^2)^2 + (1 - x1)^2 + 90(x4 - x3^2)^2
'        + (1 - x3^2)^2 + 10.1((x2 - 1)^2 + (x4 - 1)^2)
'        + 19.9(x2 - 1)(x4 - 1)
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB KEYCHECK ()
DECLARE SUB MINIMIZ1 (METHOD, N, EPS, MAXITER, IFLAG, NITER, F, X())
DECLARE SUB USERFUNC (X(), F, GRADIENT())

' Define number of variables

NVAR = 4

' Dimension solution vector

DIM X(NVAR)

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOMNZ1"
   LOCATE 8, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine MINIMIZ1 which finds a minimum (or maximum)"
   PRINT TAB(12); "of a scalar function of several variables."
   PRINT
   PRINT TAB(12); "MINIMIZ1 requires a subroutine called USERFUNC which"
   PRINT TAB(12); "returns the function and analytic gradient values of a"
   PRINT TAB(12); "user-defined scalar function of several variables."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of MINIMIZ1 to find"
   PRINT TAB(12); "a minimum of Wood's function which is defined by;"
   PRINT
   PRINT TAB(12); "F(x) = 100(x2 - x1^2)^2 + (1 - x1)^2 + 90(x4 - x3^2)^2"
   PRINT TAB(12); "       + (1 - x3^2)^2 + 10.1((x2 - 1)^2 + (x4 - 1)^2)"
   PRINT TAB(12); "       + 19.9(x2 - 1)(x4 - 1)"

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(24); "Please Select the Solution Method"
      PRINT
      PRINT
      PRINT TAB(26); "< 1 > Conjugate Gradient"
      PRINT
      PRINT TAB(26); "< 2 > Quasi-Newton"
      PRINT
      PRINT
      PRINT "Selection ( 1 or 2 )"
      INPUT METHOD
   LOOP UNTIL (METHOD = 1 OR METHOD = 2)

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NVAR
       PRINT
       PRINT "Please input the initial value of variable #"; I
       INPUT X(I)
   NEXT I

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum number of iterations"
      PRINT "( a value of between 100 and 300 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   DO
      PRINT
      PRINT
      PRINT "Please input the convergence criteria"
      PRINT "( a value of 1D-8 is recommended )"
      INPUT EPS
   LOOP UNTIL (EPS > 0#)

   CLS
   LOCATE 5, 1
   PRINT TAB(5); "Working ..."

   CALL MINIMIZ1(METHOD, NVAR, EPS, MAXITER, IFLAG, NITER, FMIN, X())

   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMOMNZ1"
   PRINT

   SELECT CASE METHOD
   CASE 1
        PRINT TAB(25); "< Conjugate Gradient Solution >"
   CASE 2
        PRINT TAB(28); "< Quasi-Newton Solution >"
   END SELECT

   PRINT

   SELECT CASE IFLAG
   CASE 0
        FOR I = 1 TO NVAR
            PRINT
            PRINT TAB(15); "Value of variable #"; I; "  = "; X(I)
        NEXT I
        PRINT
        PRINT
        PRINT TAB(15); "Minimum function value  = "; FMIN
        PRINT
        PRINT TAB(15); "Number of iterations    = "; NITER
        PRINT
        PRINT TAB(15); "Convergence criteria    = "; EPS
   CASE 1
        PRINT
        PRINT TAB(21); "Maximum number of iterations exceeded!"
   CASE 2
        PRINT
        PRINT
        PRINT TAB(19); "Linear search failed - check USERFUNC code!"
   CASE 3
        PRINT
        PRINT
        PRINT TAB(15); "Search vector failed - reduce convergence criteria!"
   END SELECT

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

