' Program "DEMOMIN2"                   December 29, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine MINIMA2 which finds a minimum (or maximum)
' of a scalar function of one variable.
'
' MINIMA2 requires a subroutine called USERFUNC which
' returns the function value of a user-defined scalar
' function of one variable. Derivatives are not required.
'
' This program demonstrates the use of MINIMA2 to find
' a minimum of the function defined by;
'
'      F(X) = X^3 + 5 / SQR(X + 2)
'
'********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB KEYCHECK ()
DECLARE SUB MINIMA2 (A, B, EPS, MAXITER, NITER, XMIN, FMIN)
DECLARE SUB BMINIMA (XI, DX, XMULT, NBRAC, XB1, XB2, IER)
DECLARE SUB USERFUNC (X, FX)

CLS
LOCATE 5, 31
PRINT "Program DEMOMIN2"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine MINIMA2 which finds a minimum (or maximum)"
PRINT TAB(12); "of a scalar function of one variable."
PRINT
PRINT TAB(12); "MINIMA2 requires a subroutine called USERFUNC which"
PRINT TAB(12); "returns the function value of a user-defined scalar"
PRINT TAB(12); "function of one variable. Derivatives are not required."
PRINT
PRINT TAB(12); "This program demonstrates the use of MINIMA2 to find"
PRINT TAB(12); "a minimum of the function defined by;"
PRINT
PRINT TAB(25); "F(X) = X^3 + 5 / SQR(X + 2)"

CALL KEYCHECK

DO
   CLS
   LOCATE 5, 1
   PRINT "Please input the initial X search value"
   INPUT XI

   DO
      PRINT
      PRINT "Please input the step size for X"
      PRINT "( a value of +/- 0.1 is recommended )"
      PRINT
      PRINT "NOTE: a positive step size will cause the"
      PRINT "bracketing subroutine to search forward and"
      PRINT "a negative value will search backward."
      INPUT DX
   LOOP UNTIL (ABS(DX) > 0#)

   DO
      PRINT
      PRINT "Please input the step size multiplier"
      PRINT "( a value of 1.25 is recommended )"
      INPUT XMULT
   LOOP UNTIL (XMULT > 0#)

   DO
      PRINT
      PRINT "Please input the convergence tolerance"
      PRINT "( a value of 1D-8 is recommended )"
      INPUT TOL
   LOOP UNTIL (TOL > 0#)
  
   DO
      PRINT
      PRINT "Please input the maximum number of bracketing iterations"
      PRINT "( a value of 100 is recommended )"
      INPUT NBRAC
   LOOP UNTIL (NBRAC > 0)

   DO
      PRINT
      PRINT "Please input the maximum number of minimizer iterations"
      PRINT "( a value of 100 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   ' bracket the function minima

   CALL BMINIMA(XI, DX, XMULT, NBRAC, XB1, XB2, IER)

   IF (IER = 1) THEN
      CLS
      LOCATE 5, 1
      PRINT TAB(22); "More than"; NBRAC; "bracketing iterations!!"
   ELSE
      ' find the minima
      CALL MINIMA2(XB1, XB2, TOL, MAXITER, N, XM, FM)

      CLS
      LOCATE 5, 31
      PRINT "Program DEMOMIN2"
      PRINT
      PRINT TAB(19); "< Minima of a One-dimensional Function >"
      PRINT
      PRINT
      PRINT TAB(15); "Number of iterations   = "; N
      PRINT
      PRINT TAB(15); "Convergence tolerance  = "; TOL
      PRINT
      PRINT
      PRINT TAB(15); "Minimum X value        = "; XM
      PRINT
      PRINT TAB(15); "Minimum function value = "; FM
   END IF

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

