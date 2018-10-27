' Program "DEMOMIN1"                   December 29, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine MINIMA1 which finds a minimum (or maximum)
' of a scalar function of one variable.
'
' MINIMA1 requires a subroutine called USERFUNC which
' returns the function value of a user-defined scalar
' function of one variable.
'
' This program demonstrates the use of MINIMA1 to find
' a minimum of the function defined by;
'
'      F(X) = X^4 - 12*X^3 + 15*X^2 + 56*X - 60
'
'********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB KEYCHECK ()
DECLARE SUB MINIMA1 (X1, DX, XMULT, XTOL, NITER, XMIN, FMIN)
DECLARE SUB USERFUNC (X, FX)

CLS
LOCATE 5, 31
PRINT "Program DEMOMIN1"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine MINIMA1 which finds a minimum (or maximum)"
PRINT TAB(12); "of a scalar function of one variable."
PRINT
PRINT TAB(12); "MINIMA1 requires a subroutine called USERFUNC which"
PRINT TAB(12); "returns the function value of a user-defined scalar"
PRINT TAB(12); "function of one variable."
PRINT
PRINT TAB(12); "This program demonstrates the use of MINIMA1 to find"
PRINT TAB(12); "a minimum of the function defined by;"
PRINT
PRINT TAB(12); "    F(X) = X^4 - 12*X^3 + 15*X^2 + 56*X - 60"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the convergence tolerance"
      PRINT "( a value of 1D-6 is recommended )"
      INPUT TOLX
   LOOP UNTIL (TOLX > 0#)

   DO
      PRINT
      PRINT
      PRINT "Please input the search step size for X"
      PRINT "( a value of .01 is recommended )"
      INPUT DX
      DXSAVED = DX
   LOOP UNTIL (DX > 0#)

   DO
      PRINT
      PRINT
      PRINT "Please input the search multiplier for X"
      PRINT "( a value of 1.25 is recommended )"
      INPUT XMULT
   LOOP UNTIL (XMULT > 0#)

   PRINT
   PRINT
   PRINT "Please input the initial search value for X"
   INPUT XI

   ' search for function minima

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL MINIMA1(XI, DX, XMULT, TOLX, NITER, XMIN, FMIN)

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOMIN1"
   PRINT
   PRINT TAB(19); "< Minima of a One-dimensional Function >"
   PRINT
   PRINT
   PRINT TAB(15); "Step size"; TAB(50); DXSAVED
   PRINT
   PRINT TAB(15); "Search multiplier"; TAB(50); XMULT
   PRINT
   PRINT TAB(15); "Initial X value"; TAB(50); XI
   PRINT
   PRINT TAB(15); "Number of iterations"; TAB(50); NITER
   PRINT
   PRINT TAB(15); "Minimum X value"; TAB(50); XMIN
   PRINT
   PRINT TAB(15); "Minimium function value"; TAB(50); FMIN

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

