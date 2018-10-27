' Program "DEMOLIN2"                   January 2, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine LINEAR2 which solves the system of linear
' equations defined by [ A ] * { X } = { B }. This
' program uses the Gauss-Jordan solution method.
'
' [ A ] is a square matrix of coefficents, { B } is
' the right side column vector and { X } is the
' solution vector of the linear system.
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB LINEAR2 (N, A(), B(), IER)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMOLIN2"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine LINEAR2 which solves the system of linear"
PRINT TAB(12); "equations defined by [ A ] * { X } = { B }. This"
PRINT TAB(12); "program uses the Gauss-Jordan solution method."
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

   DIM AMATRIX(NDIM, NDIM), BVECTOR(NDIM)

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

   ' Solve linear system

   CALL LINEAR2(NDIM, AMATRIX(), BVECTOR(), IER)

   CLS
   LOCATE 5, 32
   PRINT "Program DEMOLIN2"
   PRINT
   PRINT TAB(19); "< Solution of System of Linear Equations >"
   PRINT
   PRINT
   IF (IER = 0) THEN
      PRINT TAB(32); "Solution Vector"
      PRINT
      FOR I = 1 TO NDIM
          PRINT
          PRINT TAB(25); "X("; I; ") =  "; BVECTOR(I)
      NEXT I
      CALL KEYCHECK
      CLS
      LOCATE 5, 1
      PRINT TAB(32); "Inverted Matrix"
      PRINT
      FOR I = 1 TO NDIM
          PRINT
          FOR J = 1 TO NDIM
              PRINT " A("; I; ","; J; ") = ";
              PRINT USING "##.#####^^^^"; AMATRIX(I, J);
          NEXT J
          PRINT
      NEXT I
   ELSE
      PRINT TAB(17); "Singular Matrix!"
   END IF

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE AMATRIX, BVECTOR

LOOP UNTIL (SLCT$ = "N")

END

