' Program "DEMOMNZ2"                   June 15, 1993
'
' Copyright (c) 1988, 1989, 1990, 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine MINIMIZ2 which finds a minimum (or maximum)
' of a scalar function of several variables.
'
' MINIMIZ2 requires a subroutine called USERFUNC which
' returns the function value of a user-defined scalar
' function of several variables. A numerical gradient
' is calculated by the software.
'
' This program demonstrates the use of MINIMIZ2 to find
' a minimum of Wood's function which is defined by;
'
' F(x) = 100(x2 - x1^2)^2 + (1 - x1)^2 + 90(x4 - x3^2)^2
'        + (1 - x3^2)^2 + 10.1((x2 - 1)^2 + (x4 - 1)^2)
'        + 19.9(x2 - 1)(x4 - 1)
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB MINIMIZ2 (METHOD, N, EPS, MAXITER, IFLAG, NITER, F, X())
DECLARE SUB NUMGRAD1 (N, X(), FX, GRADIENT())
DECLARE SUB USERFUNC (X(), F)
DECLARE SUB KEYCHECK ()

' Define number of variables

NVAR = 4

' Dimension solution vector

DIM X(NVAR)

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOMNZ2"
   LOCATE 8, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine MINIMIZ2 which finds a minimum (or maximum)"
   PRINT TAB(12); "of a scalar function of several variables."
   PRINT
   PRINT TAB(12); "MINIMIZ2 requires a subroutine called USERFUNC which"
   PRINT TAB(12); "returns the function value of a user-defined scalar"
   PRINT TAB(12); "of several variables. A numerical gradient is"
   PRINT TAB(12); "calculated by the software."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of MINIMIZ2 to find"
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
      LOCATE 8, 1
      PRINT TAB(26); "< 1 > Conjugate Gradient"
      LOCATE 10, 1
      PRINT TAB(26); "< 2 > Quasi-Newton"
      LOCATE 13, 1
      PRINT "Please enter your selection ( 1 or 2 )"
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
   LOCATE 5, 5
   PRINT "Working ..."

   CALL MINIMIZ2(METHOD, NVAR, EPS, MAXITER, IFLAG, NITER, FMIN, X())

   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMOMNZ2"
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
            PRINT TAB(15); "Value of variable #"; I; " = "; X(I)
        NEXT I
        PRINT
        PRINT
        PRINT TAB(15); "Minimum function value = "; FMIN
        PRINT
        PRINT TAB(15); "Number of iterations   = "; NITER
        PRINT
        PRINT TAB(15); "Convergence criteria   = "; EPS
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

