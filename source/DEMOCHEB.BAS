' Program "DEMOCHEB"                   January 5, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedures for calling
' the QuickPak Scientific Chebyshev subroutines. These
' subroutines can be used to approximate the integral,
' derivative, and value of a user-defined analytic
' function. This same information can be determined
' for tabular data of the form y = f(x).
'
' This program demonstrates the use of the Chebyshev
' subroutines for evaluating information about
'
'      y = f(x) = sin(x)
'
' ****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB CHEBDER (A, B, C(), N, CDER())
DECLARE SUB CHEBCOEF (A, B, C(), N)
DECLARE SUB CHEBINT (A, B, C(), N, CINQ())
DECLARE SUB CHEBEVAL (A, B, C(), N, X, VCHEB)
DECLARE SUB USERFUNC (X, FX)
DECLARE SUB KEYCHECK ()

' test function integral

DEF FNXINTX (X) = -COS(X)

' test function derivative

DEF FNDFDX (X) = COS(X)

' begin main program

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOCHEB"
   PRINT
   PRINT
   PRINT TAB(12); "This program demonstrates the procedures for calling"
   PRINT TAB(12); "the QuickPak Scientific Chebyshev subroutines. These"
   PRINT TAB(12); "subroutines can be used to approximate the integral,"
   PRINT TAB(12); "derivative, and value of a user-defined analytic"
   PRINT TAB(12); "function. This same information can be determined"
   PRINT TAB(12); "for tabular data of the form y = f(x)."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of the Chebyshev"
   PRINT TAB(12); "subroutines for evaluating information about"
   PRINT
   PRINT TAB(12); "            y = f(x) = sin(x)"

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum degree of the Chebyshev approximation"
      INPUT NVAL
   LOOP UNTIL (NVAL > 0)

   DIM C(NVAL), CDER(NVAL), CINQ(NVAL)

   PRINT
   PRINT
   PRINT "Please input the lower limit of the evaluation interval"
   INPUT XL

   PRINT
   PRINT
   PRINT "Please input the upper limit of the evaluation interval"
   INPUT XU

   ' compute Chebyshev coefficients

   CLS
   LOCATE 5, 1
   PRINT TAB(5); "Please wait, computing Chebyshev coefficients ...."

   ' compute function coefficients

   CALL CHEBCOEF(XL, XU, C(), NVAL)

   ' compute derivative coefficients

   CALL CHEBDER(XL, XU, C(), NVAL, CDER())
  
   ' compute integral coefficients

   CALL CHEBINT(XL, XU, C(), NVAL, CINQ())
  
   DO
      DO
         CLS
         LOCATE 5, 1
         PRINT "Please input the number of terms in the Chebyshev approximation"
         INPUT N
      LOOP UNTIL (N > 0)
      
      DO
         PRINT
         PRINT
         PRINT "Please input the X argument for evaluation"
         INPUT X
      LOOP UNTIL (X >= XL AND X <= XU)

      ' evaluate function

      CALL CHEBEVAL(XL, XU, C(), N, X, FVAL1)

      ' evaluate Chebyshev derivative

      CALL CHEBEVAL(XL, XU, CDER(), N, X, DFDX1)

      ' evaluate Chebyshev integral

      CALL CHEBEVAL(XL, XU, CINQ(), N, X, FINT1)

      ' compute analytic value of function

      CALL USERFUNC(X, FVAL2)
  
      ' compute analytic value of derivative
  
      DFDX2 = FNDFDX(X)

      ' compute analytic value of integral
  
      FINT2 = FNXINTX(X) - FNXINTX(XL)
   
      ' print results

      CLS
      PRINT TAB(32); "Program DEMOCHEB"
      PRINT
      PRINT TAB(26); "< Chebyshev Approximations >"
      PRINT
      PRINT TAB(12); "Lower limit of interval         = "; XL
      PRINT TAB(12); "Upper limit of interval         = "; XU
      PRINT
      PRINT TAB(12); "Function argument               = "; X
      PRINT
      PRINT TAB(12); "Chebyshev function value        = "; FVAL1
      PRINT TAB(12); "Analytic function value         = "; FVAL2
      PRINT TAB(12); "Error                           = "; ABS(FVAL1 - FVAL2)
      PRINT
      PRINT TAB(12); "Chebyshev derivative            = "; DFDX1
      PRINT TAB(12); "Analytic derivative             = "; DFDX2
      PRINT TAB(12); "Error                           = "; ABS(DFDX1 - DFDX2)
      PRINT
      PRINT TAB(12); "Chebyshev integral              = "; FINT1
      PRINT TAB(12); "Analytic integral               = "; FINT2
      PRINT TAB(12); "Error                           = "; ABS(FINT1 - FINT2)
      PRINT
      PRINT TAB(12); "Maximum degree of approximation = "; NVAL
      PRINT TAB(12); "Degree of Chebyshev fit         = "; N

      CALL KEYCHECK
  
      DO
         CLS
         LOCATE 5, 1
         PRINT "Another evaluation ( y = yes, n = no )"
         INPUT EVAL$
         EVAL$ = UCASE$(EVAL$)
      LOOP UNTIL (EVAL$ = "N" OR EVAL$ = "Y")
  
   LOOP UNTIL (EVAL$ = "N")

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "N" OR SLCT$ = "Y")

   ERASE C, CDER, CINQ

LOOP UNTIL (SLCT$ = "N")

END


