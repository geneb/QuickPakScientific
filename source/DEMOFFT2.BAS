' Program "DEMOFFT2"                   January 26, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine FFT2 which performs a two-dimensional
' Fast Fourier and inverse Fast Fourier transform of
' real or complex data.
'
' This program illustrates the use of FFT2 to calculate
' the forward FFT and the inverse FFT of a two-dimensional
' function defined by the equation:
'
' f(X(j),Y(k)) = 4 - .02 ((X(j) - 7.5)^2 + (Y(k) - 7.5)^2)
'
' for  j = 1, ...M    k = 1, ...N
'
' where M and N are integer powers of 2.
'
'*******************************************************

' variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' subroutine declarations

DECLARE SUB FFT2 (NN(), X(), ISIGN)
DECLARE SUB PRTFFT2 (NN(), X(), XSAVED(), ISIGN)
DECLARE SUB KEYCHECK ()

' begin demo program

CLS
LOCATE 3, 32
PRINT "Program DEMOFFT2"
PRINT
PRINT
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "subroutine FFT2 which performs a two-dimensional"
PRINT TAB(12); "Fast Fourier and inverse Fast Fourier transform of"
PRINT TAB(12); "real or complex data."
PRINT
PRINT TAB(12); "This program illustrates the use of FFT2 to calculate"
PRINT TAB(12); "the forward FFT and the inverse FFT of the two-dimensional"
PRINT TAB(12); "function defined by the equation:"
PRINT
PRINT TAB(12); "f(X(j),Y(k)) = 4 - .02 ((X(j) - 7.5)^2 + (Y(k) - 7.5)^2)"
PRINT
PRINT TAB(12); "for  j = 1, ...M    k = 1, ...N"
PRINT
PRINT TAB(12); "where M and N are integer powers of 2."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the number of X data points"
      PRINT "(NOTE: this number must be an integer power of two)"
      INPUT NN(2)
      TST = LOG(NN(2)) / LOG(2#)
   LOOP UNTIL (TST = FIX(TST))
  
   DO
      PRINT
      PRINT
      PRINT "Please input the number of Y data points"
      PRINT "(NOTE: this number must be an integer power of two)"
      INPUT NN(1)
      TST = LOG(NN(1)) / LOG(2#)
   LOOP UNTIL (TST = FIX(TST))

   ' dimension arrays

   DIM X(2 * NN(1) * NN(2)), XSAVED(2 * NN(1) * NN(2))
  
   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   ' generate data points for the function

   ' f(X(j),Y(k)) = 4 - .02 ((X(j) - 7.5)^2 + (Y(k) - 7.5)^2)

   FOR J = 1 TO NN(2)
       FOR K = 1 TO NN(1)
           L = K + (J - 1) * NN(1)
           LL = 2 * L - 1
           X(LL) = 4# - .02# * ((J - 7.5#) ^ 2 + (K - 7.5#) ^ 2)
           X(LL + 1) = 0#
           ' save function values
           XSAVED(LL) = X(LL)
           XSAVED(LL + 1) = X(LL + 1)
       NEXT K
   NEXT J
   
   ' perform forward FFT

   ISIGN = 1

   CALL FFT2(NN(), X(), ISIGN)
   
   CALL PRTFFT2(NN(), X(), XSAVED(), ISIGN)
   
   DO
      CLS
      LOCATE 5, 1
      PRINT "Calculate inverse FFT of this data (y = yes, n = no)"
      INPUT FFT$
      FFT$ = UCASE$(FFT$)
   LOOP UNTIL (FFT$ = "N" OR FFT$ = "Y")

   IF (FFT$ = "Y") THEN
      ' perform inverse FFT
      FOR J = 1 TO NN(2)
          FOR K = 1 TO NN(1)
              L = K + (J - 1) * NN(1)
              LL = 2 * L - 1
              XSAVED(LL) = X(LL)
              XSAVED(LL + 1) = X(LL + 1)
          NEXT K
      NEXT J

      ISIGN = -1
  
      CALL FFT2(NN(), X(), ISIGN)

      CALL PRTFFT2(NN(), X(), XSAVED(), ISIGN)
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

