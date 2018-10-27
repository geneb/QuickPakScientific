' Program "DEMOPNRT"                   December 29, 1990
'
' Copyright (c) 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' the subroutine POLYROOT which solves for the real
' and complex roots of a polynomial.
'
' The polynomial is expressed in the form
'
'   F(X) = A(N+1) * X ^ N + A(N) * X ^ (N-1)
'          + A(N-1) * X ^ (N-2) + ... + A(1)
'
'   1 <= ( N = polynomial order ) <= 36
'
' ******************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB POLYROOT (COEF(), NPOLY, ROOTR(), ROOTI(), NFLG)
DECLARE SUB KEYCHECK ()

DO
   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPNRT"
   LOCATE 8, 1
   PRINT TAB(12); " This program demonstrates the procedure for calling"
   PRINT TAB(12); " the subroutine POLYROOT which solves for the real"
   PRINT TAB(12); " and complex roots of a polynomial."
   PRINT
   PRINT TAB(12); " The polynomial is expressed in the form"
   PRINT
   PRINT TAB(12); "   F(X) = A(N+1) * X ^ N + A(N) * X ^ (N-1)"
   PRINT TAB(12); "          + A(N-1) * X ^ (N-2) + ... + A(1)"
   PRINT
   PRINT TAB(12); "       1 <= ( N = polynomial order ) <= 36"
  
   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Please input the order of the polynomial"
      PRINT "( 1 <= order <= 36 )"
      INPUT NPOLY
   LOOP UNTIL (NPOLY >= 1 AND NPOLY <= 36)

   DIM COEF(NPOLY + 1), ROOTR(NPOLY + 1), ROOTI(NPOLY + 1)

   CLS
   LOCATE 5, 1
   PRINT TAB(12); "   F(X) = A(N+1) * X ^ N + A(N) * X ^ (N-1)"
   PRINT TAB(12); "          + A(N-1) * X ^ (N-2) + ... + A(1)"
   PRINT

   ' request coefficients

   FOR I = NPOLY + 1 TO 1 STEP -1
       PRINT
       PRINT "Please input coefficient A("; I; ")"
       INPUT COEF(I)
      
       IF (COEF(NPOLY + 1) = 0#) THEN
          DO
             CLS
             LOCATE 5, 1
             PRINT TAB(24); "This coefficient cannot be zero!"
             CALL KEYCHECK
             CLS
             LOCATE 5, 1
             PRINT "Please input coefficient A("; NPOLY + 1; ")"
             INPUT COEF(NPOLY + 1)
          LOOP UNTIL (COEF(NPOLY + 1) > 0#)
       END IF
   NEXT I

   CLS
   LOCATE 5, 1
   PRINT "Working ..."

   CALL POLYROOT(COEF(), NPOLY, ROOTR(), ROOTI(), NFLG)

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPNRT"
   PRINT
   PRINT TAB(20); "< Real and Complex Roots of a Polynomial >"
   PRINT
 
   FOR I = 1 TO NPOLY
       PRINT
       PRINT TAB(15); "ROOT("; I; ") =";
       PRINT TAB(28); ROOTR(I);
       IF (ROOTI(I) > 0#) THEN
          PRINT TAB(52); "+"; ABS(ROOTI(I)); "i"
       ELSEIF (ROOTI(I) < 0#) THEN
          PRINT TAB(52); "-"; ABS(ROOTI(I)); "i"
       ELSE
          PRINT
       END IF
   NEXT I

   CALL KEYCHECK
  
   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE COEF, ROOTR, ROOTI

LOOP UNTIL (SLCT$ = "N")

END

