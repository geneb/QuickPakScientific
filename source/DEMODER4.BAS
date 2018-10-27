' Program "DEMODER4"                   December 24, 1992
'
' Copyright (c) 1992 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine DERIV4 which numerically estimates the
' first derivatives of an analytic function of the
' form y = f(x) defined by the user.
'
' This program demonstrates the use of DERIV4 to
' estimate the first derivative of:
'
'     F(X) = X^2 * (X^2 - 2) * SIN(X)
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIV4 (X, H, DFDX, XERR)
DECLARE SUB USERFUNC (X, FX)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMODER4"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine DERIV4 which numerically estimates the"
PRINT TAB(12); "first derivative of an analytic function of the"
PRINT TAB(12); "form y = f(x) defined by the user."
PRINT
PRINT TAB(12); "This program demonstrates the use of DERIV4 to"
PRINT TAB(12); "estimate the first derivative of:"
PRINT
PRINT TAB(17); "F(X) = X^2 * (X^2 - 2) * SIN(X)"

CALL KEYCHECK

' test function analytic derivative

DEF FNDFDX (X) = 4# * X * (X * X - 1#) * SIN(X) + X * X * (X * X - 2#) * COS(X)

DO
   CLS
   LOCATE 5, 1

   PRINT "Please input an X value for evaluation"
   INPUT XVAL

   DO
      PRINT
      PRINT
      PRINT "Please input a step size for differentiation"
      PRINT "( a value between .1 and .01 is recommended )"
      INPUT H
   LOOP UNTIL (H > 0#)

   ' evaluate the derivative

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL DERIV4(XVAL, H, DFDX, XERR)

   CLS
   LOCATE 3, 31
   PRINT "Program DEMODER4"
   PRINT
   PRINT TAB(25); "< Numerical Differentiation >"
   PRINT
   PRINT
   PRINT TAB(15); "X value"; TAB(45); XVAL
   PRINT
   PRINT TAB(15); "Step size"; TAB(45); H
   PRINT
   PRINT TAB(15); "First derivative"; TAB(45); DFDX
   PRINT
   PRINT TAB(15); "Estimated error"; TAB(45); XERR
   PRINT
   PRINT
   PRINT TAB(15); "Analytic derivative"; TAB(45); FNDFDX(XVAL)
   PRINT
   PRINT TAB(15); "Error"; TAB(45); ABS(DFDX - FNDFDX(XVAL))
  
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


