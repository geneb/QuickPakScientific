' Program "DEMOIMPR"                   January 5, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine IMPROVE which solves the system of linear
' equations defined by [ A ] * { X } = { B } using
' iterative improvement.
'
' [ A ] is a square matrix of coefficents, { B } is
' the right side column vector and { X } is the
' solution vector of the linear system.
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB IMPROVE (NITER, N, A(), B(), X(), IER)
DECLARE SUB LUD (N, A(), INDEX(), IER)
DECLARE SUB SOLVE (N, A(), X(), B(), INDEX())
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMOIMPR"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine IMPROVE which solves the system of linear"
PRINT TAB(12); "equations defined by [ A ] * { X } = { B } using"
PRINT TAB(12); "iterative improvement."
PRINT
PRINT TAB(12); "[ A ] is a square matrix of coefficents, { B } is the"
PRINT TAB(12); "right side column vector and { X } is the solution"
PRINT TAB(12); "column vector of the linear system of equations."
PRINT
PRINT TAB(12); "The elements of the [ A ] matrix are input by rows and"
PRINT TAB(12); "the elements of the { B } vector are input by columns."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the dimension of the linear system"
      INPUT NDIM
   LOOP UNTIL (NDIM > 0)

   ' Dimension arrays

   DIM AMATRIX(NDIM, NDIM), BVECTOR(NDIM), XVECTOR(NDIM)

   FOR I = 1 TO NDIM
       CLS
       LOCATE 2, 1
       FOR J = 1 TO NDIM
           PRINT
           PRINT "Please input matrix element A("; I; ","; J; ")"
           INPUT AMATRIX(I, J)
       NEXT J
   NEXT I

   CLS
   LOCATE 2, 1

   FOR I = 1 TO NDIM
       PRINT
       PRINT "Please input vector element B("; I; ")"
       INPUT BVECTOR(I)
   NEXT I

   CLS
   LOCATE 2, 1

   FOR I = 1 TO NDIM
       PRINT
       PRINT "Please input initial vector element X("; I; ")"
       INPUT XVECTOR(I)
   NEXT I

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of iterations"
      INPUT NITER
   LOOP UNTIL (NITER > 0)

   CLS
   LOCATE 5, 1
   PRINT "Working ..."

   ' Solve linear system using iterative improvement

   CALL IMPROVE(NITER, NDIM, AMATRIX(), BVECTOR(), XVECTOR(), IER)

   CLS
   LOCATE 3, 32
   PRINT "Program DEMOIMPR"
   PRINT
   PRINT TAB(19); "< Solution of Linear System of Equations >"
   PRINT
   PRINT
   IF (IER = 1) THEN
      PRINT TAB(17); "'Singular matrix or Factorization not possible"
   ELSE
      PRINT TAB(32); "Solution Vector"
      PRINT
      FOR I = 1 TO NDIM
          PRINT
          PRINT TAB(25); "X("; I; ") =  "; XVECTOR(I)
      NEXT I

      PRINT
      PRINT
      PRINT TAB(25); "Number of iterations = "; NITER
   END IF

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")
  
   ERASE AMATRIX, BVECTOR, XVECTOR
  
   LOOP UNTIL (SLCT$ = "N")

END

