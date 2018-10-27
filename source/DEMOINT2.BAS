' Program "DEMOINT2"                   February 12, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' ROMBERG which integrates a user-defined function of
' the form y = f(x) using Romberg's method.
'
' ROMBERG requires a user-defined function described in a
' subroutine called USERFUNC(X,FVAL) which evaluates the
' function for any value of X. The user must also specify
' the lower and upper limits of integration, a convergence
' criteria, and the maximum number of iterations.
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB ROMBERG (A, B, MAXITER, EPS, NITER, S)
DECLARE SUB TRAPEZOID (A, B, N, S)
DECLARE SUB USERFUNC (X, FVAL)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMOINT2"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "ROMBERG which integrates a user-defined function of"
PRINT TAB(12); "the form y = f(x) using Romberg's method."
PRINT
PRINT TAB(12); "ROMBERG requires a user-defined function described in a"
PRINT TAB(12); "subroutine called USERFUNC(X,FVAL) which evaluates the"
PRINT TAB(12); "function for any value of X. The user must also specify"
PRINT TAB(12); "the lower and upper limits of integration, a convergence"
PRINT TAB(12); "criteria, and the maximum number of iterations."
PRINT
PRINT TAB(12); "This program demonstrates the use of ROMBERG to compute"
PRINT TAB(12); "the integral of EXP( - X^2 ) for user-specified lower"
PRINT TAB(12); "and upper integration values."

CALL KEYCHECK

DO
   CLS
   LOCATE 5, 1
   PRINT "Please input the lower integration limit"
   INPUT XL
   PRINT
   PRINT "Please input the upper integration limit"
   INPUT XU
  
   DO
      PRINT
      PRINT
      PRINT
      PRINT "Please input the maximum number of iterations"
      PRINT "( a value between 5 and 10 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)
  
   DO
      PRINT
      PRINT "Please input the convergence criteria"
      PRINT "( a value of 1D-8 is recommended )"
      INPUT EPS
   LOOP UNTIL (EPS > 0#)

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL ROMBERG(XL, XU, MAXITER, EPS, NITER, S)

   CLS
   LOCATE 5, 32
   PRINT "Program DEMOINT2"
   PRINT
   PRINT TAB(15); "< Numerical Integration of User-defined Functions >"
   PRINT
   PRINT
   PRINT TAB(15); "Lower integration limit"; TAB(45); XL
   PRINT
   PRINT TAB(15); "Upper integration limit"; TAB(45); XU
   PRINT
   PRINT TAB(15); "Convergence criteria"; TAB(45); EPS
   PRINT
   PRINT
   PRINT TAB(15); "Integral value"; TAB(45); S
   PRINT
   PRINT TAB(15); "Number of iterations"; TAB(45); NITER

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

