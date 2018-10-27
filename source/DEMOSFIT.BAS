' Program "DEMOSFIT"                   December 20, 1993
'
' Copyright (c) 1989, 1990, 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' the subroutine SURFIT which calculates fitting
' coefficients for 3-dimensional surfaces of the form
' z = f(x,y) from X, Y and Z data points input by the
' user.
'
' This program demonstrates the use of SURFIT to find
' fitting coefficients for the surface defined by
'
'    Z = -COS(3*PI*X/10)*COS(PI*Y/10)
'
'    for  -5 <= X <= +5  and  -5 <= Y <= +5
'
' ****************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB SFUNCTION (NDEG, COEF(), X, Y, Z)
DECLARE SUB SURFIT (NDEG, NDATA, X(), Y(), Z(), COEF())
DECLARE SUB KEYCHECK ()

PI = 3.141592653589793#

' dimension data and coefficient arrays

DIM XDATA(121), YDATA(121), ZDATA(121), COEF(11, 11)

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOSFIT"
   PRINT
   PRINT
   PRINT TAB(12); " This program demonstrates the procedure for calling"
   PRINT TAB(12); " the subroutine SURFIT which calculates fitting"
   PRINT TAB(12); " coefficients for 3-dimensional surfaces of the form"
   PRINT TAB(12); " z = f(x,y) from X, Y and Z data points input by the"
   PRINT TAB(12); " user."
   PRINT
   PRINT TAB(12); " This program demonstrates the use of SURFIT to find"
   PRINT TAB(12); " fitting coefficients for the surface defined by"
   PRINT
   PRINT TAB(12); "      Z = -COS(3*PI*X/10)*COS(PI*Y/10)"
   PRINT
   PRINT TAB(12); "      for  -5 <= X <= +5  and  -5 <= Y <= +5"

   CALL KEYCHECK

   ' generate data points for the surface defined by
   '   Z = -COS(3*PI*X/10)*COS(PI*Y/10)
   '   for -5 <= X <= +5  and  -5 <= Y <= +5

   CLS
   LOCATE 5, 1
   PRINT "Please wait, computing data points ..."

   NDATA = 0

   FOR I# = -5# TO 5# STEP 1#
       FOR J# = -5# TO 5# STEP 1#
           NDATA = NDATA + 1
           X = I#
           XDATA(NDATA) = I#
           Y = J#
           YDATA(NDATA) = J#
           Z = -COS(3# * PI * X / 10#) * COS(PI * Y / 10#)
           ZDATA(NDATA) = Z
       NEXT J#
   NEXT I#

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the degree of the surface fit (degree <= 10)"
      INPUT NDEG
   LOOP UNTIL (NDEG >= 0 AND NDEG <= 10)

   CLS
   LOCATE 5, 1
   PRINT "Please wait, computing coefficients";
   COLOR 23
   LOCATE 5, 36
   PRINT " ....."
   COLOR 7

   CALL SURFIT(NDEG, NDATA, XDATA(), YDATA(), ZDATA(), COEF())

   ' print fitting coefficients

   FOR I = 1 TO NDEG + 1
       CLS
       LOCATE 5, 1
       PRINT TAB(30); "Fitting Coefficients"
       PRINT
       PRINT

       I1 = I + 1
       FOR J = 1 TO I
           PRINT TAB(10); "COEF("; I1 - J - 1; ","; J - 1; ") =";
           PRINT TAB(40); COEF(J, I1 - J)
       NEXT J
       CALL KEYCHECK
   NEXT I

   ' compute percentage of fit

   CLS
   LOCATE 5, 1
   PRINT "Please wait, computing percentage of fit ..."

   ZRMS = 0#
   RRMS = 0#

   FOR I = 1 TO NDATA
       ZRMS = ZRMS + ZDATA(I) ^ 2
       CALL SFUNCTION(NDEG, COEF(), XDATA(I), YDATA(I), A)
       RRMS = RRMS + (A - ZDATA(I)) ^ 2
   NEXT I

   IF (ZRMS = 0# AND RRMS = 0#) THEN
      FIT = 100#
   ELSE
      FIT = 100# * (ZRMS / (ZRMS + RRMS))
   END IF

   CLS
   LOCATE 5, 1
   PRINT TAB(10); "Percentage of fit = "; TAB(40); FIT
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Would you like to fit an X,Y data point"
      PRINT "( y = yes, n = no )"
      INPUT FIT$
      FIT$ = UCASE$(FIT$)
   LOOP UNTIL (FIT$ = "Y" OR FIT$ = "N")

   IF (FIT$ = "Y") THEN
      DO
         PRINT
         PRINT "Please input the X data point (-5 <= X <= +5)"
         INPUT X
      LOOP UNTIL (ABS(X) <= 5#)
      DO
         PRINT
         PRINT "Please input the Y data point (-5 <= Y <= +5)"
         INPUT Y
      LOOP UNTIL (ABS(Y) <= 5#)

      CALL SFUNCTION(NDEG, COEF(), X, Y, Z)

      CLS
      LOCATE 5, 1
      PRINT
      PRINT TAB(10); "X data point ="; TAB(40); X
      PRINT
      PRINT TAB(10); "Y data point ="; TAB(40); Y
      PRINT
      PRINT TAB(10); "Z data point ="; TAB(40); Z
      CALL KEYCHECK
   END IF

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END



