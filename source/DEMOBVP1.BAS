' Program "DEMOBVP1"                   August 13, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine SHOOT which solves two-point boundary
' value problems using the shooting method.
'
' Subroutine SHOOT solves second-order boundary value
' problems of the following form
'
'    y'' = f(t, y, y')
'
'    subject to the boundary conditions
'
'    y(a) = alpha   and   y(b) = beta
'
' This program demonstrates the use of DIVDIFF to solve
' the suspended cable problem defined as follows:
'  
'    y'' = -w(x)/T
'  
' subject to a user-defined cable length and initial
' and final boundary or "attachment" conditions.
'
' ****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB SHOOT (AIV, ALPHA, BIV, BETA)
DECLARE SUB KEYCHECK ()

DO
   CLS
   LOCATE 2, 1
   PRINT TAB(32); "Program DEMOBVP1"
   LOCATE 4, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine SHOOT which solves two-point boundary"
   PRINT TAB(12); "value problems using the shooting method."
   PRINT
   PRINT TAB(12); "Subroutine SHOOT solves second-order boundary value"
   PRINT TAB(12); "problems of the following form;"
   PRINT
   PRINT TAB(12); "   y'' = f(t, y, y')"
   PRINT
   PRINT TAB(12); "   subject to the boundary conditions"
   PRINT
   PRINT TAB(12); "   y(a) = alpha   and   y(b) = beta"
   PRINT
   PRINT TAB(12); "This program demonstrates the use of DIVDIFF to solve"
   PRINT TAB(12); "the suspended cable problem defined as follows:"
   PRINT
   PRINT TAB(12); "   y'' = -w(x)/T"
   PRINT
   PRINT TAB(12); "subject to a user-defined cable length and initial"
   PRINT TAB(12); "and final boundary or 'attachment' conditions."

   CALL KEYCHECK
  
   AIV = 0#

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the total cable length (feet)"
      INPUT BIV
   LOOP UNTIL (BIV > AIV)

   PRINT
   PRINT
   PRINT "Please input the attachment height at the beginning of the cable (feet)"
   INPUT ALPHA

   PRINT
   PRINT "Please input the attachment height at the far end of the cable (feet)"
   INPUT BETA

   CLS
   LOCATE 5, 34
   PRINT "Working";
   COLOR 23
   LOCATE 5, 41
   PRINT " ...."
   COLOR 7
  
   CALL SHOOT(AIV, ALPHA, BIV, BETA)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "N" OR SLCT$ = "Y")

LOOP UNTIL (SLCT$ = "N")

END


