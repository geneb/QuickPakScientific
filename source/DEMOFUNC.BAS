' Program "DEMOFUNC"                   May 17, 1993
'
' Copyright (c) 1989, 1990, 1993 Crescent Software
'
' This program demonstrates the procedures for calling
' the QuickPak Scientific utility functions.
'
' The function BESSEL recursively calculates the
' Bessel functions of integer order, Jn(X).
' Note that X must be >= -3
'
' The function GAMMA recursively calculates the
' Gamma function for both positive and negative
' values of x.
'
' The function ERF calculates the Error function
' for any value of X.
'
' The function BETA calculates the Beta function
' for any values of Z and W.
'
' The function DLOG10 calculates the base 10 logarithm
' (common log) for any value of X.
'
' *******************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Function declarations

DECLARE FUNCTION BESSEL (N, X)
DECLARE FUNCTION GAMMA (X)
DECLARE FUNCTION ERF (X)
DECLARE FUNCTION BETA (Z, W)
DECLARE FUNCTION GBESSEL (D)
DECLARE FUNCTION UBESSEL (D)
DECLARE FUNCTION TBESSEL (D)
DECLARE FUNCTION BBESSEL (D)
DECLARE FUNCTION ABESSEL (D)
DECLARE FUNCTION FBESSEL (D)
DECLARE FUNCTION DLOG10 (X)

DECLARE SUB KEYCHECK ()

' dimension and define options

DIM T$(6)

T$(1) = "Bessel function"
T$(2) = "Gamma function"
T$(3) = "Error function"
T$(4) = "Beta function"
T$(5) = "Common log function"
T$(6) = "End this demonstration"

DO
   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMOFUNC"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling the"
   PRINT TAB(12); "QuickPak Scientific utility functions."
   PRINT
   PRINT TAB(12); "The function BESSEL recursively calculates the Bessel"
   PRINT TAB(12); "functions Jn(X) of integer order."
   PRINT TAB(12); "Please note that X must be >= -3"
   PRINT
   PRINT TAB(12); "The function GAMMA recursively calculates the Gamma"
   PRINT TAB(12); "function for both positive and negative values of X."
   PRINT
   PRINT TAB(12); "The function ERF calculates the Error function for"
   PRINT TAB(12); "any value of X."
   PRINT
   PRINT TAB(12); "The function BETA calculates the Beta function for"
   PRINT TAB(12); "any values of Z and W."
   PRINT
   PRINT TAB(12); "The function DLOG10 calculates the base 10 or common"
   PRINT TAB(12); "logarithm for any value of X."

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(31); "Program DEMOFUNC"
      PRINT
      FOR I = 1 TO 6
          PRINT
          PRINT TAB(25); "<"; I; "> "; T$(I)
      NEXT I
      PRINT
      PRINT
      PRINT "Selection ( 1, 2, 3, 4, 5 or 6 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 6)
 
   IF (ISELECT = 6) THEN END

   CLS
   LOCATE 5, 1

   SELECT CASE ISELECT
   CASE 1
      ' Bessel function
      DO
         PRINT "Please input the integer order"
         INPUT NORD
      LOOP UNTIL (NORD >= 0)
      DO
         PRINT
         PRINT "Please input the X value"
         PRINT "( X must be >= -3 )"
         INPUT X
      LOOP UNTIL (X >= -3#)
      BVALUE = BESSEL(NORD, X)
   CASE 2
      ' Gamma function
      PRINT "Please input the X value"
      INPUT X
      BVALUE = GAMMA(X)
   CASE 3
      ' Error function
      PRINT "Please input the X value"
      INPUT X
      BVALUE = ERF(X)
   CASE 4
      ' Beta function
      PRINT "Please input the Z value"
      INPUT Z
      PRINT
      PRINT "Please input the W value"
      INPUT W
      BVALUE = BETA(Z, W)
   CASE 5
      ' Common log function
      PRINT "Please input the X value"
      INPUT X
      BVALUE = DLOG10(X)
   END SELECT

   ' print results

   A$ = "< " + T$(ISELECT) + " >"

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOFUNC"
   PRINT
   PRINT TAB(40 - .5 * LEN(A$)); A$
   PRINT
   PRINT
   SELECT CASE ISELECT
   CASE 1
      PRINT TAB(20); "Order of Bessel function = "; NORD
      PRINT
      PRINT TAB(20); "X value                  = "; X
      PRINT
      PRINT TAB(20); "Function value           = "; BVALUE
   CASE 2, 3, 5
      PRINT TAB(20); "X value        = "; X
      PRINT
      PRINT TAB(20); "Function value = "; BVALUE
   CASE 4
      PRINT TAB(20); "Z value        = "; Z
      PRINT
      PRINT TAB(20); "W value        = "; W
      PRINT
      PRINT TAB(20); "Function value    = "; BVALUE
   END SELECT

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "N" OR SLCT$ = "Y")

LOOP UNTIL (SLCT$ = "N")

END

