' Program "DEMONLE1"                   January 6, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' REALROOT which finds a single real root of a function.
'
' REALROOT requires a user-defined function coded in a
' subroutine called USERFUNC(X,FVAL) which evaluates the
' function for any value of X. The software calculates
' a search interval which contains the real root.
'
' This program demonstrates the use of REALROOT to find the
' real root of F(X) = X * EXP(X) - 10 in a user specified
' search interval. No derivative evaluations are required.
'
'****************************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB REALROOT (XL, XU, TOL, MAXITER, NITER, XROOT, FROOT)
DECLARE SUB BROOT (X1, DX, XMULT, DXMAX, NROOT, XB1, XB2, IER)
DECLARE SUB USERFUNC (X, FVAL)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 1
PRINT TAB(32); "Program DEMONLE1"
PRINT
PRINT
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "REALROOT which finds a single real root of a function."
PRINT
PRINT TAB(12); "REALROOT requires a user-defined function coded in a"
PRINT TAB(12); "subroutine called USERFUNC(X,FVAL) which evaluates the"
PRINT TAB(12); "function for any value of X. The software calculates"
PRINT TAB(12); "a search interval which contains the real root."
PRINT
PRINT TAB(12); "This program demonstrates the use of REALROOT to find the"
PRINT TAB(12); "real root of F(X) = X * EXP(X) - 10 in a user specified"
PRINT TAB(12); "search interval. No derivative evaluations are required."

CALL KEYCHECK

DO
   CLS
   LOCATE 5, 1
   PRINT "Please input the initial search X value"
   INPUT XL
  
   DO
      PRINT
      PRINT
      PRINT "Please input the initial X step size"
      PRINT "( a value of +/- 0.1 is recommended )"
      INPUT DX
   LOOP UNTIL (ABS(DX) > 0#)
    
   DO
      PRINT
      PRINT
      PRINT "Please input the X step size multiplier"
      PRINT "( a value of .25 is recommended )"
      INPUT XMULT
   LOOP UNTIL (XMULT > 0#)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum number of bracketing iterations"
      PRINT "( a value of 50 is recommended )"
      INPUT NROOT
   LOOP UNTIL (NROOT > 0)
  
   DO
      PRINT
      PRINT
      PRINT "Please input the bracketing rectification interval"
      PRINT "( a value of 1 is recommended )"
      INPUT DXMAX
   LOOP UNTIL (DXMAX > 0)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum number of root-finding iterations"
      PRINT "( a value between 10 to 50 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)
  
   DO
      PRINT
      PRINT
      PRINT "Please input the convergence criteria"
      PRINT "( a value of 1D-10 is recommended )"
      INPUT TOL
   LOOP UNTIL (TOL > 0#)

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   ' bracket root

   CALL BROOT(XL, DX, XMULT, DXMAX, NROOT, XB1, XB2, IER)

   IF (IER = 1) THEN
      CLS
      LOCATE 5, 1
      PRINT TAB(22); "More than"; NROOT; "bracketing iterations!!"
   ELSE
      ' calculate root

      CALL REALROOT(XB1, XB2, TOL, MAXITER, NITER, ROOT, FROOT)

      CLS
      LOCATE 5, 1
      PRINT TAB(32); "Program DEMONLE1"
      PRINT
      PRINT TAB(26); "< Real Root of a Function >"
      LOCATE 12, 1
      PRINT TAB(15); "Value of real root"; TAB(50); ROOT
      PRINT
      PRINT TAB(15); "Function value"; TAB(50); FROOT
      PRINT
      PRINT
      PRINT TAB(15); "Number of iterations"; TAB(50); NITER
      PRINT
      PRINT TAB(15); "Convergence criteria"; TAB(50); TOL
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

