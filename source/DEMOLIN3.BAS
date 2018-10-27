' Program "DEMOLIN3"                   January 30, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine LINEAR3 which solves a tridiagonal system
' of linear equations defined by [ A ] * { X } = { B }.
' This program uses a Gaussian elimination method with
' partial pivoting.
'
' [ A ] is a square matrix of coefficents, { B } is
' the right side column vector and { X } is the
' solution vector of the linear system.
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB LINEAR3 (N, A(), B(), C(), F(), X(), IER)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMOLIN3"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine LINEAR3 which solves a tridiagonal system"
PRINT TAB(12); "of linear equations defined by [ A ] * { X } = { B }."
PRINT TAB(12); "This program uses a Gaussian elimination method with"
PRINT TAB(12); "partial pivoting."
PRINT
PRINT TAB(12); "[ A ] is a square matrix of coefficents, { B } is the"
PRINT TAB(12); "right side column vector and { X } is the solution"
PRINT TAB(12); "column vector of the linear system of equations."
PRINT
PRINT TAB(12); "The elements of the [ A ] matrix and the { B } vector"
PRINT TAB(12); "should be input by rows."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the dimension of the linear system"
      INPUT NDIM
   LOOP UNTIL (NDIM > 0)

   ' Dimension arrays

   DIM A(NDIM), B(NDIM), C(NDIM), F(NDIM), X(NDIM)

   CLS
   LOCATE 5, 1

   ' request elements of superdiagonal

   PRINT

   FOR I = 1 TO (NDIM - 1)
       PRINT
       PRINT "Please input superdiagonal matrix element A("; I; ","; I + 1; ")"
       INPUT C(I)
   NEXT I
  
   CLS
   LOCATE 5, 1

   ' request elements of diagonal

   FOR I = 1 TO NDIM
       PRINT
       PRINT "Please input diagonal matrix element A("; I; ","; I; ")"
       INPUT B(I)
   NEXT I
  
   CLS
   LOCATE 5, 1

   ' request elements of subdiagonal

   FOR I = 1 TO (NDIM - 1)
       PRINT
       PRINT "Please input subdiagonal matrix element A("; I + 1; ","; I; ")"
       INPUT A(I + 1)
   NEXT I

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NDIM
       PRINT
       PRINT "Please input vector element B("; I; ")"
       INPUT F(I)
   NEXT I

   ' Solve tridiagonal linear system

   CALL LINEAR3(NDIM, A(), B(), C(), F(), X(), IER)

   CLS
   LOCATE 5, 32
   PRINT "Program DEMOLIN3"
   PRINT
   PRINT TAB(13); "< Solution of Tridiagonal Linear System of Equations >"
   PRINT
   PRINT
   IF (IER = 0) THEN
      PRINT TAB(32); "Solution Vector"
      PRINT
      FOR I = 1 TO NDIM
          PRINT
          PRINT TAB(25); "X("; I; ") =  "; X(I)
      NEXT I
   ELSE
      PRINT TAB(31); "Singular Matrix !!"
   END IF

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE A, B, C, F, X

LOOP UNTIL (SLCT$ = "N")

END

