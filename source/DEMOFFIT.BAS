' Program "DEMOFFIT"                   February 12, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine FFIT which performs a curve fit of X and
' Y data input by the user to simple functions.
'
' The types of curve fits available are:
'
'  Linear         Y = A + B * X
'  Logarithmic    Y = A + B * LOG(X)
'  Exponential    Y = A * EXP(B * X)
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FFIT (N, X(), Y(), ITYPE, A, B)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 31
PRINT "Program DEMOFFIT"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine FFIT which performs a curve fit of X"
PRINT TAB(12); "and Y data input by the user to simple functions."
PRINT
PRINT TAB(12); "The types of curve fits available are:"
PRINT
PRINT TAB(15); "Linear         Y = A + B * X"
PRINT
PRINT TAB(15); "Logarithmic    Y = A + B * LOG(X)"
PRINT
PRINT TAB(15); "Exponential    Y = A * EXP(B * X)"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(25); "Curve Fit Menu"
      PRINT
      PRINT
      PRINT TAB(12); "< 1 > Linear         Y = A + B * X"
      PRINT
      PRINT TAB(12); "< 2 > Logarithmic    Y = A + B * LOG(X)"
      PRINT
      PRINT TAB(12); "< 3 > Exponential    Y = A * EXP(B * X)"
      PRINT
      PRINT
      PRINT "Selection ( 1, 2 or 3 )"
      INPUT ITYPE
   LOOP UNTIL (ITYPE >= 1 AND ITYPE <= 3)

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X and Y data points"
      PRINT "( a minimum of 3 data points must be input )"
      INPUT NPTS
   LOOP UNTIL (NPTS >= 3)

   ' Dimension arrays

   DIM X(NPTS), Y(NPTS)

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NPTS
       PRINT
       PRINT
       PRINT "Please input the X data for point #"; I
       INPUT X(I)
       PRINT
       PRINT "Please input the Y data for point #"; I
       INPUT Y(I)
   NEXT I

   ' perform curve fit

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL FFIT(NPTS, X(), Y(), ITYPE, A, B)

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOFFIT"
   PRINT
   SELECT CASE ITYPE
   CASE 1
        PRINT TAB(23); "< Linear         Y = A + B * X >"
   CASE 2
        PRINT TAB(20); "< Logarithmic    Y = A + B * LOG(X) >"
   CASE 3
        PRINT TAB(20); "< Exponential    Y = A * EXP(B * X) >"
   END SELECT
   PRINT
   PRINT
   PRINT TAB(25); "A coefficient = "; A
   PRINT
   PRINT TAB(25); "B coefficient = "; B
  
   CALL KEYCHECK

   DO
      DO
         CLS
         LOCATE 5, 1
         PRINT "Would you like to fit an X data point ( y = yes, n = no )"
         INPUT FIT$
         FIT$ = UCASE$(FIT$)
      LOOP UNTIL (FIT$ = "Y" OR FIT$ = "N")

      IF (FIT$ = "N") THEN EXIT DO

      PRINT
      PRINT
      PRINT "Please input an X value to fit"
      INPUT X

      SELECT CASE ITYPE
      CASE 1
           ' Linear    
           Y = A + B * X
      CASE 2
           ' Logarithmic
           Y = A + B * LOG(X)
      CASE 3
           ' Exponential
           Y = A * EXP(B * X)
      END SELECT

      CLS
      LOCATE 5, 1
      PRINT TAB(31); "Program DEMOFFIT"
      PRINT
      PRINT
      PRINT TAB(29); "< Fit an X data point >"
      PRINT
      PRINT
      PRINT TAB(25); "X value = "; X
      PRINT
      PRINT TAB(25); "Y value = "; Y

      CALL KEYCHECK

   LOOP

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, Y
  
LOOP UNTIL (SLCT$ = "N")

END

