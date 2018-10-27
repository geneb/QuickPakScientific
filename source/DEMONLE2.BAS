' Program "DEMONLE2"                   December 29, 1990
'
' Copyright (c) 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine NEWTON which iteratively solves for a
' real root of an unconstrained non-linear function
' using Newton's method.
'
' DEMONLE2 requires a subroutine called USERFUNC which
' evaluates the non-linear function and its derivative.
'
' This program demonstrates the use of NEWTON to solve
'
'     F(X) = X - e^(1 / X)
'
' *****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB NEWTON (X1, X2, TOL, MAXITER, ROOT, NITER)
DECLARE SUB USERFUNC (X, FX, FPX)
DECLARE SUB KEYCHECK ()

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMONLE2"
   LOCATE 8, 1
   PRINT TAB(12); " This program demonstrates the procedure for calling"
   PRINT TAB(12); " subroutine NEWTON which iteratively solves for a"
   PRINT TAB(12); " real root of an unconstrained non-linear function"
   PRINT TAB(12); " using Newton's method."
   PRINT
   PRINT TAB(12); " DEMONLE2 requires a subroutine called USERFUNC which"
   PRINT TAB(12); " evaluates the non-linear function and its derivative."
   PRINT
   PRINT TAB(12); " This program demonstrates the use of NEWTON to solve"
   PRINT
   PRINT TAB(12); "          F(X) = X - e^(1 / X)"
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the initial X search value"
      INPUT XI
      PRINT
      PRINT "Please input the final X search value"
      INPUT XF
     
      CALL USERFUNC(XI, FI, DFI)
      CALL USERFUNC(XF, FF, DFF)

      IF ((FI * FF) > 0#) THEN
         CLS
         LOCATE 5, 1
         PRINT TAB(28); "Root must be bracketed!!"
         CALL KEYCHECK
      ELSE
         EXIT DO
      END IF
   LOOP

   DO
      PRINT
      PRINT "Please input the convergence tolerance"
      PRINT "( A value of 1D-10 is recommended )"
      INPUT TOL
   LOOP UNTIL (TOL > 0#)

   DO
      PRINT
      PRINT "Please input the maximum number of iterations"
      PRINT "( A value between 50 and 100 is recomended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   CLS
   LOCATE 5, 1
   PRINT "Working ..."

   CALL NEWTON(XI, XF, TOL, MAXITER, ROOT, NITER)

   FX = ROOT - EXP(1# / ROOT)

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMONLE2"
   PRINT
   PRINT TAB(22); "< Solution of a Non-linear Equation >"
   PRINT
   PRINT
   PRINT TAB(15); "Lower bound of search interval"; TAB(50); XI
   PRINT
   PRINT TAB(15); "Upper bound of search interval"; TAB(50); XF
   PRINT
   PRINT
   PRINT TAB(15); "Value of real root"; TAB(40); TAB(50); ROOT
   PRINT
   PRINT TAB(15); "Function value"; TAB(50); FX
   PRINT
   PRINT TAB(15); "Number of iterations"; TAB(50); NITER
   PRINT
   PRINT TAB(15); "Convergence criteria"; TAB(50); TOL
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



