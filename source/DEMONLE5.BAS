' Program "DEMONLE5"                   September 15, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine IMMROOT which iteratively solves for a
' real root of an unconstrained non-linear function
' using a variable order improved memory method.
'
' DEMONLE5 requires a subroutine called USERFUNC which
' evaluates the non-linear function and its derivative.
'
' This program demonstrates the use of IMMROOT to solve
'
'     F(X) = X - e^(1 / X)
'
' *****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB IMMROOT (XMIN, XMAX, TOL, IER, MAXIT, NORDER)
DECLARE SUB USERFUNC (X, FX)
DECLARE SUB KEYCHECK ()

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMONLE5"
   LOCATE 8, 1
   PRINT TAB(12); " This program demonstrates the procedure for calling"
   PRINT TAB(12); " subroutine IMMROOT which iteratively solves for a"
   PRINT TAB(12); " real root of an unconstrained non-linear function"
   PRINT TAB(12); " using a variable order improved memory method."
   PRINT
   PRINT TAB(12); " DEMONLE5 requires a subroutine called USERFUNC which"
   PRINT TAB(12); " evaluates the non-linear function and its derivative."
   PRINT
   PRINT TAB(12); " This program demonstrates the use of IMMROOT to solve"
   PRINT
   PRINT TAB(12); "          F(X) = X - e^(1 / X)"
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the initial X search value"
      INPUT XMIN
      XMINSAVED = XMIN
      PRINT
      PRINT "Please input the final X search value"
      INPUT XMAX
      XMAXSAVED = XMAX

      CALL USERFUNC(XMIN, FI)
      CALL USERFUNC(XMAX, FF)

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
      PRINT "( A value between 25 and 50 is recomended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   DO
      PRINT
      PRINT "Please input the algorithm order"
      PRINT "( An order >= 2 is recomended )"
      INPUT NORDER
   LOOP UNTIL (NORDER >= 2)

   CLS
   LOCATE 5, 1
   PRINT "Working ..."

   CALL IMMROOT(XMIN, XMAX, TOL, IER, MAXITER, NORDER)

   CALL USERFUNC(XMIN, FX)

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMONLE5"
   PRINT
   PRINT TAB(22); "< Solution of a Non-linear Equation >"
   PRINT
   PRINT
   PRINT TAB(15); "Lower bound of search interval"; TAB(50); XMINSAVED
   PRINT
   PRINT TAB(15); "Upper bound of search interval"; TAB(50); XMAXSAVED
   PRINT
   PRINT
   PRINT TAB(15); "Value of real root"; TAB(40); TAB(50); XMIN
   PRINT
   PRINT TAB(15); "Function value"; TAB(50); FX
   PRINT
   PRINT TAB(15); "Number of iterations"; TAB(50); MAXITER
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



