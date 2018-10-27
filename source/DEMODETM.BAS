' Program "DEMODETM"                   February 12, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine DETERMIN which solves for the determinant
' of an N by N matrix [ A ].
'
'******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DETERMIN (N, A(), DM, IER)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMODETM"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine DETERMIN which solves for the determinant"
PRINT TAB(12); "of an N by N matrix [ A ]."
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

   ' Dimension matrix

   DIM AMATRIX(NDIM, NDIM)

   FOR I = 1 TO NDIM
       CLS
       LOCATE 2, 1
       FOR J = 1 TO NDIM
           PRINT
           PRINT "Please input matrix element A("; I; ","; J; ")"
           INPUT AMATRIX(I, J)
       NEXT J
   NEXT I

   ' calculate determinant

   CALL DETERMIN(NDIM, AMATRIX(), DETERM, IER)

   CLS
   LOCATE 5, 32
   PRINT "Program DEMODETM"
   PRINT
   PRINT TAB(22); "< Determinant of a Square Matrix >"
   PRINT
   PRINT
   IF (IER = 1) THEN
      PRINT TAB(17); "Singular Matrix or Factorization not possible"
   ELSE
      PRINT TAB(24); "Determinant ="; TAB(40); DETERM
   END IF

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE AMATRIX

LOOP UNTIL (SLCT$ = "N")

END

