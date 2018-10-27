' Program "DEMOFFT1"                   January 26, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine FFT1 which performs a one-dimensional
' Fast Fourier and inverse Fast Fourier transform of
' real and complex data.
'
' This program illustrates the use of FFT1 to calculate
' the forward FFT and the inverse FFT of Runge's
' function which is defined by the equation:
'
'     f(x) = 1 / (1 + 25 * x^2)
'
' The number of data points must be an integer power of 2.
'
'*******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FFT1 (NN, X(), ISIGN)
DECLARE SUB PRTFFT1 (NN, X(), XSAVED(), ISIGN)
DECLARE SUB KEYCHECK ()

PI = 3.14159265#

CLS
LOCATE 5, 32
PRINT "Program DEMOFFT1"
PRINT
PRINT
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine FFT1 which performs a one-dimensional"
PRINT TAB(12); "Fast Fourier and inverse Fast Fourier transform of"
PRINT TAB(12); "real and complex data."
PRINT
PRINT TAB(12); "This program illustrates the use of FFT1 to calculate"
PRINT TAB(12); "the forward FFT and the inverse FFT of Runge's"
PRINT TAB(12); "function which is defined by the equation:"
PRINT
PRINT TAB(12); "     f(x) = 1 / (1 + 25 * x^2)"
PRINT
PRINT TAB(12); "The number of data points must be an integer power of 2."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of function data points"
      PRINT "(NOTE: this number must be an integer power of two)"
      INPUT NN
      TST = LOG(NN) / LOG(2#)
   LOOP UNTIL (TST = FIX(TST))
  
   PRINT
   PRINT
   PRINT "Please input the initial time"
   INPUT TI
      
   PRINT
   PRINT
   PRINT "Please input the final time"
   INPUT TF
   
   ' dimension arrays

   DIM X(2 * NN), XSAVED(2 * NN)

   ' calculate delta-t

   DEL = (TF - TI) / NN

   ' generate data points for

   ' Runge's function f(x) = 1 / (1 + 25 * x^2)

   II = 1

   FOR I = 0 TO NN - 1
       XJ = TI + I * DEL
       X(II) = 1# / (1# + 25# * XJ * XJ)
       X(II + 1) = 0#
       ' save function values
       XSAVED(II) = X(II)
       XSAVED(II + 1) = X(II + 1)
       II = II + 2
   NEXT I

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   ' perform forward FFT

   ISIGN = 1

   CALL FFT1(NN, X(), ISIGN)

   CALL PRTFFT1(NN, X(), XSAVED(), ISIGN)
  
   DO
      CLS
      LOCATE 5, 1
      PRINT "Calculate inverse FFT of this data (y = yes, n = no)"
      INPUT FFT$
      FFT$ = UCASE$(FFT$)
   LOOP UNTIL (FFT$ = "N" OR FFT$ = "Y")

   IF (FFT$ = "Y") THEN
      ' perform inverse FFT

      II = 1

      FOR I = 0 TO NN - 1
          XSAVED(II) = X(II)
          XSAVED(II + 1) = X(II + 1)
          II = II + 2
      NEXT I

      ISIGN = -1
  
      CALL FFT1(NN, X(), ISIGN)

      CALL PRTFFT1(NN, X(), XSAVED(), ISIGN)
   END IF

   ' request another selection

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, XSAVED
  
LOOP UNTIL (SLCT$ = "N")

END

