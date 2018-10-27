' Program "DEMONLE3"                   December 29, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' NLINEAR which solves a system of non-linear equations.
'
' NLINEAR requires subroutine LINEAR and a subroutine
' called USERFUNC which returns the negative function
' values and the gradient array of a user-defined
' system of non-linear equations.
'
' This program solves the non-linear system of equations
'
'        - X1^2 - X2^2 - X3^2 + X4 = 0
'
'    X1^2 + X2^2 + X3^2 + X4^2 - 1 = 0
'
'                          X1 - X2 = 0
'
'                          X2 - X3 = 0
'
'***********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB NLINEAR (N, MAXITER, XTOL, FTOL, NITER, X(), IER)
DECLARE SUB USERFUNC (X(), FX(), DFDX())
DECLARE SUB LINEAR1 (N, A(), B(), X(), IER)
DECLARE SUB KEYCHECK ()

' Dimension solution vector for 4 equations

NEQ = 4

DIM X(NEQ)

CLS
LOCATE 5, 33
PRINT "Program DEMONLE3"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "NLINEAR which solves a system of non-linear equations."
PRINT
PRINT TAB(12); "NLINEAR requires subroutine LINEAR1 and a subroutine"
PRINT TAB(12); "called USERFUNC which returns the negative function"
PRINT TAB(12); "values and the gradient array of a user-defined"
PRINT TAB(12); "system of non-linear equations."

CALL KEYCHECK

CLS
LOCATE 8, 1
PRINT TAB(12); "This program solves the non-linear system of equations"
LOCATE 11, 1
PRINT TAB(18); "      - X1^2 - X2^2 - X3^2 + X4 = 0"
PRINT
PRINT TAB(18); "  X1^2 + X2^2 + X3^2 + X4^2 - 1 = 0"
PRINT
PRINT TAB(18); "                        X1 - X2 = 0"
PRINT
PRINT TAB(18); "                        X2 - X3 = 0"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the maximum number of iterations"
      PRINT "( a value of 5 to 10 is recommended )"
      INPUT MAXITER
   LOOP UNTIL (MAXITER > 0)

   DO
      PRINT
      PRINT
      PRINT "Please input a convergence criteria for X values"
      PRINT "( a value of 1D-6 is recommended )"
      INPUT EPSX
   LOOP UNTIL (EPSX > 0#)
  
   DO
      PRINT
      PRINT
      PRINT "Please input a convergence criteria for function values"
      PRINT "( a value of 1D-6 is recommended )"
      INPUT EPSF
   LOOP UNTIL (EPSF > 0#)

   CLS
   PRINT
   PRINT
   FOR I = 1 TO NEQ
       PRINT
       PRINT "Please input the initial value for X("; I; ")"
       INPUT X(I)
   NEXT I

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL NLINEAR(NEQ, MAXITER, EPSX, EPSF, NITER, X(), IER)

   CLS
   LOCATE 5, 33
   PRINT "Program DEMONLE3"
   PRINT
   PRINT TAB(17); "< Solution of Non-linear System of Equations >"
   PRINT
   PRINT
   IF (IER = 1) THEN
      PRINT TAB(10); "Singular matrix encountered!"
   ELSE
      PRINT TAB(32); "Solution vector"
      PRINT
      FOR I = 1 TO NEQ
          PRINT
          PRINT TAB(26); "X("; I; ") = "; X(I)
      NEXT I
      PRINT
      PRINT
      PRINT TAB(26); "Number of iterations = "; NITER
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

