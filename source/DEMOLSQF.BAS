' Program "DEMOLSQF"                   February 12, 1991
'
' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine LSQFIT which performs a least squares fit
' of X and Y data input by the user.
'
' The least squares curve fit is given by:
'
'   Y = C(1) + C(2)*X + C(3)*X^2 + .... + C(N)*X^(N-1)
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB LSQFIT (NPTS, X(), Y(), MDEG, SDEV, COEF())
DECLARE SUB KEYCHECK ()

CLS
LOCATE 5, 32
PRINT "Program DEMOLSQF"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine LSQFIT which performs a least squares fit"
PRINT TAB(12); "of X and Y data input by the user."
PRINT
PRINT TAB(12); "The least squares curve fit is given by:"
PRINT
PRINT TAB(12); "   Y = C(1) + C(2)*X + C(3)*X^2 + .... + C(N)*X^(N-1)"

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X and Y data points"
      PRINT "( a minimum of three data points must be input )"
      INPUT NPTS
   LOOP UNTIL (NPTS >= 3)

   ' Dimension arrays

   DIM X(NPTS), Y(NPTS), C(NPTS)

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

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the degree of fit"
      PRINT "( this must be a number less than or equal to"; NPTS - 2; ")"
      INPUT NFIT
   LOOP UNTIL (NFIT <= NPTS - 2)

   ' perform least squares curve fit

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL LSQFIT(NPTS, X(), Y(), NFIT, S, C())

   CLS
   LOCATE 1, 31
   PRINT "Program DEMOLSQF"
   PRINT
   PRINT TAB(26); "< Least Squares Curve Fit >"
   PRINT
   PRINT TAB(29); "Table of Coefficients"
   PRINT
   FOR I = NFIT TO 1 STEP -1
       PRINT TAB(30); C(I);
   NEXT I

   CALL KEYCHECK

   CLS
   LOCATE 5, 1
   PRINT TAB(15); "Number of data points"; TAB(40); NPTS
   PRINT
   PRINT TAB(15); "Degree of fit"; TAB(40); NFIT - 1
   PRINT
   PRINT TAB(15); "Standard deviation"; TAB(40); S

   CALL KEYCHECK

   DO
      DO
         CLS
         LOCATE 5, 1
         PRINT "Fit an X data point ( y = yes, n = no )"
         INPUT FIT$
         FIT$ = UCASE$(FIT$)
      LOOP UNTIL (FIT$ = "Y" OR FIT$ = "N")

      IF (FIT$ = "N") THEN EXIT DO

      DO
         PRINT
         PRINT
         PRINT "Please input X value"
         INPUT X
      LOOP UNTIL (X >= X(1) AND X <= X(NPTS))

      Y = 0#

      FOR I = 1 TO NFIT
          Y = Y + C(I) * X ^ (I - 1)
      NEXT I

      CLS
      LOCATE 5, 1
      PRINT TAB(15); "X value"; TAB(40); X
      PRINT
      PRINT
      PRINT TAB(15); "Y value"; TAB(40); Y

      CALL KEYCHECK

   LOOP

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, Y, C
  
LOOP UNTIL (SLCT$ = "N")

END

