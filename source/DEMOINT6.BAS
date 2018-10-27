' Program "DEMOINT6"                   January 22, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' GAUSLEG which integrates a user-defined function of
' the form y = f(x) using a Gauss-Legendre method.
'
' Subroutine GAUSLEG requires a user-defined function
' coded in a second subroutine called USERFUNC which
' evaluates the function for any value of X. The user
' must specify the lower and upper integration limits, the
' number of quadrature points, and a convergence criteria.
'
' This program demonstrates the use of GAUSLEG to compute
' the integral of EXP( - X^2 ) for user-specified lower
' and upper integration values for X.
'
'**********************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB GAUSLEG (XMIN, XMAX, N, EPS, S)
DECLARE SUB KEYCHECK ()

' Begin demo

CLS
LOCATE 3, 32
PRINT "Program DEMOINT6"

LOCATE 6, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "GAUSLEG which integrates a user-defined function of"
PRINT TAB(12); "the form y = f(x) using a Gauss-Legendre method."
PRINT
PRINT TAB(12); "Subroutine GAUSLEG requires a user-defined function"
PRINT TAB(12); "coded in a second subroutine called USERFUNC which"
PRINT TAB(12); "evaluates the function for any value of X. The user"
PRINT TAB(12); "must specify the lower and upper integration limits, the"
PRINT TAB(12); "number of quadrature points, and a convergence criteria."
PRINT
PRINT TAB(12); "This program demonstrates the use of GAUSLEG to compute"
PRINT TAB(12); "the integral of EXP( - X^2 ) for user-specified lower"
PRINT TAB(12); "and upper integration values for X."

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
      PRINT "Please enter the number of quadrature points"
      INPUT N
   LOOP UNTIL (N > 0)

   DO
      PRINT
      PRINT
      PRINT "Please enter the convergence criteria"
      PRINT "( a value of 1D-8 is recommended )"
      INPUT EPS
   LOOP UNTIL (EPS > 0)

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL GAUSLEG(XL, XU, N, EPS, RESULT)

   ' Print results

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOINT6"
   PRINT
   PRINT TAB(12); "< Gauss-Legendre Integration of User-defined Functions >"
   PRINT
   PRINT
   PRINT TAB(20); "Lower integration limit     = "; XL
   PRINT
   PRINT TAB(20); "Upper integration limit     = "; XU
   PRINT
   PRINT
   PRINT TAB(20); "Number of quadrature points = "; N
   PRINT
   PRINT TAB(20); "Convergence criteria        = "; EPS
   PRINT
   PRINT
   PRINT TAB(20); "Integral value              = "; RESULT

   CALL KEYCHECK

   ' Request another selection

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END
 

