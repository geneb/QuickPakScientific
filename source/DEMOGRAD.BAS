' Program "DEMOGRAD.BAS"               Janaury 7, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine NUMGRAD which numerically estimates the
' gradient of a user-defined analytic equation.
'
' NUMGRAD requires a subroutine called USERFUNC which
' returns the scalar function value of a user-defined
' analytic equation.
'
' This program demonstrates the use of NUMGRAD to estimate
' the gradient of the non-linear equation given by;
'
' F(x1,x2,x3,x4) = 100(x2-x1^2)^2 + (1-x1)^2 + 90(x4-x3^2)^2
'                  + (1-x3^2)^2 + 10.1((x2-1)^2 + (x4-1)^2)
'                  + 19.9(x2-1)(x4-1)
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB NUMGRAD (N, X(), G())
DECLARE SUB USERFUNC (X(), FX)
DECLARE SUB GRADIENT (X(), G())
DECLARE SUB KEYCHECK ()

' define the number of variables

NCV = 4

' dimension working arrays

DIM XI(NCV), GRAD(NCV), AGRAD(NCV), XERR(NCV)

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOGRAD"
   LOCATE 8, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine NUMGRAD which numerically estimates the"
   PRINT TAB(12); "gradient of a user-defined analytic equation."
   PRINT
   PRINT TAB(12); "NUMGRAD requires a subroutine called USERFUNC which"
   PRINT TAB(12); "returns the scalar function value of a user-defined"
   PRINT TAB(12); "analytic equation."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of NUMGRAD to estimate"
   PRINT TAB(12); "the gradient of the non-linear equation given by;"
   PRINT
   PRINT TAB(12); "F(x1,x2,x3,x4) = 100(x2-x1^2)^2 + (1-x1)^2 + 90(x4-x3^2)^2"
   PRINT TAB(12); "                 + (1-x3^2)^2 + 10.1((x2-1)^2 + (x4-1)^2)"
   PRINT TAB(12); "                 + 19.9(x2-1)(x4-1)"

   CALL KEYCHECK

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NCV
       PRINT
       PRINT "Please input the initial value of variable #"; I
       INPUT XI(I)
   NEXT I

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL NUMGRAD(NCV, XI(), GRAD())

   CALL GRADIENT(XI(), AGRAD())

   FOR I = 1 TO 4
       XERR(I) = ABS(GRAD(I) - AGRAD(I))
   NEXT I

   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMOGRAD"
   PRINT
   PRINT TAB(29); "< Numerical Gradient >"
   PRINT
   PRINT

   PRINT TAB(5); "Variable"; TAB(25); "Numerical"; TAB(45); "Analytic"; TAB(66); "Error"
   PRINT TAB(5); " Value"; TAB(25); "Gradient"; TAB(45); "Gradient"; TAB(65); "Estimate"

   FORMAT$ = "##.##########^^^^"

   FOR I = 1 TO NCV
       PRINT
       PRINT USING FORMAT$; XI(I);
       PRINT TAB(21);
       PRINT USING FORMAT$; GRAD(I);
       PRINT TAB(41);
       PRINT USING FORMAT$; AGRAD(I);
       PRINT TAB(61);
       PRINT USING FORMAT$; XERR(I)
   NEXT I

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

