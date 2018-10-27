' Program "DEMODER2"                   January 6, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine DERIV2 which numerically estimates the
' first derivative of user-supplied tabulated data
' of the form y = f(x).
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIV2 (NPTS, NDEG, X(), Y(), XVAL, YVAL)
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMODER2"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine DERIV2 which numerically estimates the"
PRINT TAB(12); "first derivative of user-supplied tabulated data"
PRINT TAB(12); "of the form y = f(x)."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X and Y data points"
      PRINT "( a minimum of four data points must be input )"
      INPUT NPTS
   LOOP UNTIL (NPTS >= 4)

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

   N = NPTS - 2

   IF (N < 9) THEN
      NDMAX = N
   ELSE
      NDMAX = 9
   END IF

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the degree of interpolation"
      PRINT "( a number between 1 and"; NDMAX; "is recommended )"
      INPUT NDEG
   LOOP UNTIL (NDEG >= 1 AND NDEG <= NDMAX)

   DO
      PRINT
      PRINT "Please input an X value for evaluation"
      PRINT "( X must be between"; X(1); "and"; X(NPTS - NDEG); ")"
      INPUT XVAL
   LOOP UNTIL (XVAL >= X(1) AND XVAL <= X(NPTS - NDEG))

   ' evaluate the derivative

   CLS
   LOCATE 5, 1
   PRINT "Working ..."

   CALL DERIV2(NPTS, NDEG, X(), Y(), XVAL, DYDX)

   CLS
   LOCATE 3, 31
   PRINT "Program DEMODER2"
   PRINT
   PRINT TAB(25); "< Numerical Differentiation >"
   PRINT
   PRINT
   PRINT TAB(15); "X value"; TAB(45); XVAL
   PRINT
   PRINT
   PRINT TAB(15); "Degree of interpolation"; TAB(45); NDEG
   PRINT
   PRINT
   PRINT TAB(15); "First derivative"; TAB(45); DYDX

   CALL KEYCHECK

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

