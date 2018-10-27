' Program "DEMODER1"                   February 12, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine DERIV1 which numerically estimates the
' first, second, third and fourth derivatives of an
' analytic function of the form y = f(x) defined by
' the user.
'
' This program demonstrates the use of DERIV1 to
' estimate the derivatives of:
'
'     F(X) = SQUARE ROOT( SIN(X) + 2.5 * EXP(X) )
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIV1 (X, H, FP1, FP2, FP3, FP4)
DECLARE SUB USERFUNC (X, FX)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMODER1"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine DERIV1 which numerically estimates the"
PRINT TAB(12); "first, second, third and fourth derivatives of an"
PRINT TAB(12); "analytic function of the form y = f(x) defined by"
PRINT TAB(12); "the user."
PRINT
PRINT TAB(12); "This program demonstrates the use of DERIV1 to"
PRINT TAB(12); "estimate the derivatives of:"
PRINT
PRINT TAB(17); "F(X) = SQUARE ROOT( SIN(X) + 2.5 * EXP(X) )"


CALL KEYCHECK

DO
   CLS
   LOCATE 5, 1

   PRINT "Please input an X value for evaluation"
   INPUT XVAL

   DO
      PRINT
      PRINT
      PRINT "Please input a step size for differentiation"
      PRINT "( a value between .01 and .0001 is recommended )"
      INPUT SS
   LOOP UNTIL (SS > 0#)

   ' evaluate the derivatives

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL DERIV1(XVAL, SS, DYDX1, DYDX2, DYDX3, DYDX4)

   CLS
   LOCATE 3, 31
   PRINT "Program DEMODER1"
   PRINT
   PRINT TAB(25); "< Numerical Differentiation >"
   PRINT
   PRINT
   PRINT TAB(15); "X value"; TAB(45); XVAL
   PRINT
   PRINT TAB(15); "Step size"; TAB(45); SS
   PRINT
   PRINT
   PRINT TAB(15); "First derivative"; TAB(45); DYDX1
   PRINT
   PRINT TAB(15); "Second derivative"; TAB(45); DYDX2
   PRINT
   PRINT TAB(15); "Third derivative"; TAB(45); DYDX3
   PRINT
   PRINT TAB(15); "Fourth derivative"; TAB(45); DYDX4

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

