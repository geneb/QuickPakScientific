' Program "DEMOINT5"                   January 31, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' ASIMPSON which integrates a user-defined function of
' the form y = f(x) using an adaptive Simpson method.
'
' Subroutine ASIMPSON requires a user-defined function
' coded in a second subroutine called USERFUNC which
' evaluates the function for any value of X. The user
' must specify the lower and upper limits of integration,
' and a desired solution accuracy.
'
' This program demonstrates the use of ASIMPSON to compute
' the integral of EXP( - X^2 ) for user-specified lower
' and upper integration values for X.
'
'**********************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB ASIMPSON (A, B, ACC, ANS, XERROR, IFLAG)
DECLARE SUB USERFUNC (X, FVAL)
DECLARE SUB KEYCHECK ()

' Begin demo

CLS
LOCATE 3, 32
PRINT "Program DEMOINT5"

LOCATE 6, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "ASIMPSON which integrates a user-defined function of"
PRINT TAB(12); "the form y = f(x) using an adaptive Simpson method."
PRINT
PRINT TAB(12); "Subroutine ASIMPSON requires a user-defined function"
PRINT TAB(12); "coded in a second subroutine called USERFUNC which"
PRINT TAB(12); "evaluates the function for any value of X. The user"
PRINT TAB(12); "must specify the lower and upper limits of integration,"
PRINT TAB(12); "and a desired solution accuracy."
PRINT
PRINT TAB(12); "This program demonstrates the use of ASIMPSON to compute"
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
      PRINT "Please input the solution accuracy"
      PRINT "(A value of 1D-8 is recommended)"
      INPUT ACC
   LOOP UNTIL (ACC > 0#)

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL ASIMPSON(XL, XU, ACC, S, XERROR, IFLAG)

   ' Print results

   CLS
   LOCATE 3, 32
   PRINT "Program DEMOINT5"
   PRINT
   PRINT TAB(15); "< Adaptive Integration of User-defined Functions >"
   PRINT
   PRINT
   PRINT TAB(20); "Lower integration limit = "; XL
   PRINT
   PRINT TAB(20); "Upper integration limit = "; XU
   PRINT
   PRINT
   PRINT TAB(20); "Solution accuracy       = "; ACC
   PRINT
   IF (IFLAG > 1) THEN
      PRINT
      SELECT CASE IFLAG
      CASE 2
           PRINT TAB(24); "<< Error: more than 30 levels >>"
      CASE 3
           PRINT TAB(23); "<< Error: subinterval too small >>"
      CASE 4
           PRINT TAB(16); "<< Error: more than 2000 function evaluations >>"
      END SELECT
      PRINT
   END IF
   PRINT
   PRINT TAB(20); "Integral value          = "; S
   PRINT
   PRINT TAB(20); "Estimated error         = "; XERROR

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


