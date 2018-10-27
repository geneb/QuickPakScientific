' Program "DEMOINT4"                   February 12, 1991
'
' Copyright (c) 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling the
' subroutine INTEGRA3 which numerically approximates a
' triple integral using the Composite Simpson method.
'
' This program demostrates the use of INTEGRA3 to evaluate
' the triple integral of the function defined by
'
'    f(x,y,z) = z * SQR( x^2 + y^2 )
'
' ********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FCX (X, F)
DECLARE SUB FDX (X, F)
DECLARE SUB FAXY (X, Y, F)
DECLARE SUB FBXY (X, Y, F)
DECLARE SUB FXYZ (X, Y, Z, F)
DECLARE SUB INTEGRA3 (XA, XB, L, M, N, XJ)
DECLARE SUB KEYCHECK ()

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOINT4"
   PRINT
   PRINT
   PRINT TAB(12); " This program demonstrates the procedure for calling the"
   PRINT TAB(12); " subroutine INTEGRA3 which numerically approximates a"
   PRINT TAB(12); " triple integral using the Composite Simpson method."
   PRINT
   PRINT TAB(12); " This program demostrates the use of INTEGRA3 to evaluate"
   PRINT TAB(12); " the triple integral of the function defined by"
   PRINT
   PRINT TAB(12); "         f(x,y,z) = z * SQR( x^2 + y^2 )"

   CALL KEYCHECK

   CLS
   LOCATE 5, 1
   PRINT "Please input the lower X integration limit"
   INPUT A
   PRINT
   PRINT "Please input the upper X integration limit"
   INPUT B

   DO
      PRINT
      PRINT
      PRINT "Please input the number of X subdivisions"
      INPUT NX
   LOOP UNTIL (NX > 0)

   DO
      PRINT
      PRINT "Please input the number of Y subdivisions"
      INPUT NY
   LOOP UNTIL (NY > 0)

   DO
      PRINT
      PRINT "Please input the number of Z subdivisions"
      INPUT NZ
   LOOP UNTIL (NZ > 0)

   CLS
   LOCATE 5, 1
   PRINT "Working ..."

   CALL INTEGRA3(A, B, NZ, NY, NX, XJ)

   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMOINT4"
   PRINT
   PRINT TAB(22); "< Approximation of Triple Integrals >"
   PRINT
   PRINT
   PRINT TAB(15); "Lower X integration limit ="; TAB(45); A
   PRINT
   PRINT TAB(15); "Upper X integration limit ="; TAB(45); B
   PRINT
   PRINT
   PRINT TAB(15); "Number of X subdivisions  ="; TAB(45); NX
   PRINT
   PRINT TAB(15); "Number of Y subdivisions  ="; TAB(45); NY
   PRINT
   PRINT TAB(15); "Number of Z subdivisions  ="; TAB(45); NZ
   PRINT
   PRINT
   PRINT TAB(15); "Triple integral ="; TAB(45); XJ
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

