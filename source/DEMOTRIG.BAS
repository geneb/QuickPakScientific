' Program "DEMOTRIG"                   December 26, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' the QuickPak Scientific trig functions and subroutines.
'
' The available functions and subroutines are:
'
'     Inverse sine function
'     Inverse cosine function
'     Four quadrant inverse tangent function
'     Hyperbolic sine function
'     Hyperbolic cosine function
'     Hyperbolic tangent function
'     Inverse hyperbolic sine function
'     Inverse hyperbolic cosine function
'     Inverse hyperbolic tangent function
'
'*******************************************************

' Variable type definitions

DEFDBL A-H, O-Z
DEFINT I-N

' Function and subroutine declarations

DECLARE FUNCTION ASIN (X)
DECLARE FUNCTION ACOS (X)
DECLARE FUNCTION ATAN3 (X1, X2)
DECLARE FUNCTION SINH (X)
DECLARE FUNCTION COSH (X)
DECLARE FUNCTION TANH (X)
DECLARE FUNCTION ASINH (X)
DECLARE FUNCTION ACOSH (X)
DECLARE FUNCTION ATANH (X)
DECLARE SUB KEYCHECK ()

' Allocate working arrays

DIM T$(10)

' Define radians to degrees conversion factor

CONST RTD = 180# / 3.141592653589793#

' Fill title strings

T$(1) = "Inverse sine function"
T$(2) = "Inverse cosine function"
T$(3) = "Four quadrant inverse tangent function"
T$(4) = "Hyperbolic sine function"
T$(5) = "Hyperbolic cosine function"
T$(6) = "Hyperbolic tangent function"
T$(7) = "Inverse hyperbolic sine function"
T$(8) = "Inverse hyperbolic cosine function"
T$(9) = "Inverse hyperbolic tangent function"
T$(10) = "End this demonstration"

' Begin demo program

CLS
LOCATE 5, 32
PRINT "Program DEMOTRIG"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "the following QuickPak Scientific trigonometry"
PRINT TAB(12); "functions and subroutines."
PRINT
FOR I = 1 TO 9
    PRINT TAB(18); T$(I)
NEXT I

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(32); "Program DEMOTRIG"
      LOCATE 8, 1
      FOR I = 1 TO 10
          PRINT TAB(15); "<"; I; "> "; T$(I)
      NEXT I
      PRINT
      PRINT
      PRINT "Selection ( 1 - 10 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 10)

   IF (ISELECT = 10) THEN END

   SELECT CASE ISELECT
   CASE 1
        DO
           CLS
           LOCATE 5, 1
           PRINT "Please input the sine of an angle"
           PRINT "( this is a number between -1 and +1 )"
           INPUT X
        LOOP UNTIL (ABS(X) <= 1#)
   CASE 2
        DO
           CLS
           LOCATE 5, 1
           PRINT "Please input the cosine of an angle"
           PRINT "( this is a number between -1 and +1 )"
           INPUT X
        LOOP UNTIL (ABS(X) <= 1#)
   CASE 3
        CLS
        LOCATE 5, 1
        PRINT "Please input the sine of an angle"
        PRINT "( this is a number between -1 and +1 )"
        INPUT X1
        PRINT
        PRINT "Please input the cosine of an angle"
        PRINT "( this is a number between -1 and +1 )"
        INPUT X2
   CASE 4 TO 9
        CLS
        LOCATE 5, 1
        PRINT "Please input the value for the function argument"
        INPUT X
   END SELECT

   A$ = "< " + T$(ISELECT) + " >"

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOTRIG"
   LOCATE 7, 1
   PRINT TAB(40 - .5 * LEN(A$)); A$
   PRINT
   PRINT

   SELECT CASE ISELECT
   CASE 1
        PRINT TAB(25); "Function argument = "; X
        FX = ASIN(X)
        PRINT
        PRINT TAB(25); "Angle ( radians ) = "; FX
        PRINT
        PRINT TAB(25); "Angle ( degrees ) = "; FX * RTD
   CASE 2
        PRINT TAB(25); "Function argument = "; X
        FX = ACOS(X)
        PRINT
        PRINT TAB(25); "Angle ( radians ) = "; FX
        PRINT
        PRINT TAB(25); "Angle ( degrees ) = "; FX * RTD
   CASE 3
        PRINT TAB(25); "Sine of an angle   = "; X1
        PRINT
        PRINT TAB(25); "Cosine of an angle = "; X2
        FX = ATAN3(X1, X2)
        PRINT
        PRINT TAB(25); "Angle ( radians )  = "; FX
        PRINT
        PRINT TAB(25); "Angle ( degrees )  = "; FX * RTD
   CASE 4
        FX = SINH(X)
   CASE 5
        FX = COSH(X)
   CASE 6
        FX = TANH(X)
   CASE 7
        FX = ASINH(X)
   CASE 8
        FX = ACOSH(X)
   CASE 9
        FX = ATANH(X)
   END SELECT
       
   IF (ISELECT >= 4 AND ISELECT <= 9) THEN
      PRINT TAB(18); "Function argument = "; X
      PRINT
      PRINT TAB(18); "Function value    = "; FX
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

