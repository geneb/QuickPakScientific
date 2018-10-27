' Program "DEMOBVP2"                   August 13, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine DIVDIFF which solves two-point boundary
' value problems using the method of divided differences.
'
' Subroutine DIVDIFF solves second-order boundary value
' problems of the following form;
'
'    y'' = f(t, y, y')
'
'    subject to the boundary conditions
'
'    y(a) = alpha and y(b) = beta
'
' This program demonstrates the use of DIVDIFF to solve
'
'    y'' = y'(1/t + 2y'/y)
'
' subject to user-defined initial and final times and
' initial and final boundary conditions.
'
' ****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DIVDIFF (N, AIV, ALPHA, BIV, BETA, TR(), YR())
DECLARE SUB KEYCHECK ()

' begin main program

DO
   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMOBVP2"
   LOCATE 4, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine DIVDIFF which solves two-point boundary"
   PRINT TAB(12); "value problems using the method of divided differences."
   PRINT
   PRINT TAB(12); "Subroutine DIVDIFF solves second-order boundary value"
   PRINT TAB(12); "problems of the following form;"
   PRINT
   PRINT TAB(12); "   y'' = f(t, y, y')"
   PRINT
   PRINT TAB(12); "   subject to the boundary conditions"
   PRINT
   PRINT TAB(12); "   y(a) = alpha and y(b) = beta"
   PRINT
   PRINT TAB(12); "This program demonstrates the use of DIVDIFF to solve"
   PRINT
   PRINT TAB(12); "   y'' = y'(1/t + 2y'/y)"
   PRINT
   PRINT TAB(12); "subject to user-defined initial and final times and"
   PRINT TAB(12); "initial and final boundary conditions."

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the initial time"
      INPUT AIV
   LOOP UNTIL (AIV >= 0#)

   DO
      PRINT
      PRINT "Please input the final time"
      INPUT BIV
   LOOP UNTIL (BIV > AIV)
  
   PRINT
   PRINT
   PRINT "Please input the value of the initial boundary condition"
   INPUT ALPHA

   PRINT
   PRINT "Please input the value of the final boundary condition"
   INPUT BETA

   DO
      PRINT
      PRINT
      PRINT "Please input the number of divided difference intervals"
      PRINT "( a value between 10 and 20 is recommended )"
      INPUT N
   LOOP UNTIL (N > 0)

   DIM TR(N), YR(N)

   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7

   ' solve the TPBVP

   CALL DIVDIFF(N, AIV, ALPHA, BIV, BETA, TR(), YR())

   ' print results

   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMOBVP2"
   PRINT
   PRINT TAB(18); "< Divided Difference Solution of the TPBVP >"
   PRINT
   PRINT
   PRINT TAB(5); "Time"; TAB(18); "Approximation"; TAB(42); "Exact"; TAB(62); "Error"
   PRINT

   FOR I = 0 TO N
       T = TR(I)
       PRINT TAB(5); TR(I); TAB(15); YR(I); TAB(35); 24# / (7# - T * T); TAB(55); ABS(YR(I) - 24# / (7# - T * T))
   NEXT I

   CALL KEYCHECK

   ' request another selection

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "N" OR SLCT$ = "Y")

   ERASE TR, YR

LOOP UNTIL (SLCT$ = "N")

END


