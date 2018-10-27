' Program "DEMOJAC"                    January 7, 1993
'
' Copyright (c) 1993 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutine NUMJAC which numerically estimates the
' Jacobian of a user-defined system of equations.
'
' NUMJAC requires a subroutine called USERFUNC which
' returns the function values of a user-defined system
' of analytic equations.
'
' This program demonstrates the use of NUMJAC to estimate
' the Jacobian of the non-linear equations given by;
'
'   FX(1) = X(1)/X(2) - 5 * LN(0.4*X(2)/X(3)) + 4.45977
'   FX(2) = X(2) + X(1) - 1 
'   FX(3) = X(3) + 0.5 * X(1) - 0.4
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X(), FX())
DECLARE SUB NUMJAC (N, X(), XJAC())
DECLARE SUB JACOBIAN (X(), XJAC())
DECLARE SUB KEYCHECK ()

' define the number of variables 

NVAR = 3

' dimension working arrays

DIM X(NVAR), XNJAC(NVAR, NVAR), XAJAC(NVAR, NVAR), XERR(NVAR, NVAR)

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOJAC"
   LOCATE 8, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutine NUMJAC which numerically estimates the"
   PRINT TAB(12); "Jacobian of a user-defined system of equations."
   PRINT
   PRINT TAB(12); "NUMJAC requires a subroutine called USERFUNC which"
   PRINT TAB(12); "returns the function values of a user-defined system"
   PRINT TAB(12); "of analytic equations."
   PRINT
   PRINT TAB(12); "This program demonstrates the use of NUMJAC to estimate"
   PRINT TAB(12); "the Jacobian of the non-linear equations given by;"
   PRINT
   PRINT TAB(12); "  FX(1) = X(1)/X(2) - 5 * LN(0.4*X(2)/X(3)) + 4.45977"
   PRINT TAB(12); "  FX(2) = X(2) + X(1) - 1"
   PRINT TAB(12); "  FX(3) = X(3) + 0.5 * X(1) - 0.4"

   CALL KEYCHECK

   CLS
   LOCATE 5, 1

   FOR I = 1 TO NVAR
       PRINT
       PRINT "Please input the initial value of variable #"; I
       INPUT X(I)
   NEXT I

   CLS
   LOCATE 5, 5
   PRINT "Working ..."

   CALL NUMJAC(NVAR, X(), XNJAC())

   CALL JACOBIAN(X(), XAJAC())

   FORMAT$ = "##.##########^^^^"

   CLS
   LOCATE 3, 1
   PRINT TAB(32); "Program DEMOJAC"
   PRINT
   PRINT TAB(29); "< Numerical Jacobian >"
   PRINT
   PRINT

   PRINT TAB(5); "Numerical Jacobian"
   PRINT

   FOR I = 1 TO NVAR
       PRINT TAB(5);
       PRINT USING FORMAT$; XNJAC(I, 1);
       PRINT TAB(25);
       PRINT USING FORMAT$; XNJAC(I, 2);
       PRINT TAB(45);
       PRINT USING FORMAT$; XNJAC(I, 3)
   NEXT I

   PRINT
   PRINT TAB(5); "Analytic Jacobian"
   PRINT
  
   FOR I = 1 TO NVAR
       PRINT TAB(5);
       PRINT USING FORMAT$; XAJAC(I, 1);
       PRINT TAB(25);
       PRINT USING FORMAT$; XAJAC(I, 2);
       PRINT TAB(45);
       PRINT USING FORMAT$; XAJAC(I, 3)
   NEXT I

   PRINT
   PRINT TAB(5); "Errors"
   PRINT

   FOR I = 1 TO NVAR
       FOR J = 1 TO NVAR
           XERR(I, J) = ABS(XNJAC(I, J) - XAJAC(I, J))
       NEXT J
   NEXT I

   FOR I = 1 TO NVAR
       PRINT TAB(5);
       PRINT USING FORMAT$; XERR(I, 1);
       PRINT TAB(25);
       PRINT USING FORMAT$; XERR(I, 2);
       PRINT TAB(45);
       PRINT USING FORMAT$; XERR(I, 3)
   NEXT I

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

