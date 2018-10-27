' Program "DEMOINVR"                   August 9, 1993
'
' Copyright (c) 1988, 1989, 1990, 1991, 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine INVERSE which solves for the inverse of
' an N by N square matrix [ A ].
'
' The elements of the [ A ] matrix are input by rows.
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

' subroutine declarations

DECLARE SUB INVERSE (N, A(), AINV(), IER)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 33
PRINT "Program DEMOINVR"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine INVERSE which solves for the inverse of"
PRINT TAB(12); "an N by N square matrix [ A ]."
PRINT
PRINT TAB(12); "The elements of the [ A ] matrix are input by rows."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the dimension of the matrix"
      INPUT NDIM
   LOOP UNTIL (NDIM > 0)

   ' Dimension arrays

   DIM AMATRIX(NDIM, NDIM), AINVERSE(NDIM, NDIM)

   FOR I = 1 TO NDIM
       CLS
       LOCATE 2, 1
       FOR J = 1 TO NDIM
           PRINT
           PRINT "Please input matrix element A("; I; ","; J; ")"
           INPUT AMATRIX(I, J)
       NEXT J
   NEXT I

   ' Compute matrix inverse

   CALL INVERSE(NDIM, AMATRIX(), AINVERSE(), IER)

   CLS
   LOCATE 5, 33
   PRINT TAB(33); "Program DEMOINVR"
   PRINT
   PRINT TAB(30); "< Matrix Inversion >"
   PRINT
   PRINT
   IF (IER = 1) THEN
      PRINT TAB(17); "Singular Matrix or Factorization not possible"
   ELSE
      PRINT TAB(32); "Inverted Matrix"
      PRINT
      FOR I = 1 TO NDIM
          PRINT
          FOR J = 1 TO NDIM
              PRINT " A("; I; ","; J; ") = ";
              PRINT USING "##.#####^^^^"; AINVERSE(I, J);
          NEXT J
          PRINT
       NEXT I
   END IF

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE AMATRIX, AINVERSE

LOOP UNTIL (SLCT$ = "N")

END

