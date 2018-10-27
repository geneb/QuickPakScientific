' Program "DEMOPOLY"                   December 29, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' subroutines QUADRATIC, CUBIC and QUARTIC which solve for
' the real roots of quadratic, cubic and quartic equations.
'
'**********************************************************

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB KEYCHECK ()
DECLARE SUB QUARTIC (A1, A2, A3, A4, A5, Q1, Q2, Q3, Q4, NROOT)
DECLARE SUB CUBIC (B1, B2, B3, B4, X1, X2, X3, NROOT)
DECLARE SUB QUADRATIC (C1, C2, C3, X1, X2, NROOT)

DO
   NROOTS = 0
   Q1 = 0#
   Q2 = 0#
   Q3 = 0#
   Q4 = 0#

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPOLY"
   LOCATE 8, 1
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "subroutines QUADRATIC, CUBIC and QUARTIC which solve for"
   PRINT TAB(12); "the real roots of quadratic, cubic and quartic equations."

   CALL KEYCHECK

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPOLY"

   DO
      PRINT
      PRINT
      PRINT TAB(15); "< 1 > Solve a quartic equation"
      PRINT
      PRINT TAB(15); "< 2 > Solve a cubic equation"
      PRINT
      PRINT TAB(15); "< 3 > Solve a quadratic equation"
      PRINT
      PRINT
      PRINT "Selection ( 1, 2 or 3 )"
      INPUT ITYPE
   LOOP UNTIL (ITYPE >= 1 AND ITYPE <= 3)

   CLS
   LOCATE 5, 1

   SELECT CASE ITYPE
   CASE 1
        PRINT TAB(32); "Quartic Equation"
        PRINT
        PRINT TAB(16); "A1 * X^4 + A2 * X^3 + A3 * X^2 + A4 * X + A5 = 0"
        PRINT
        DO
           PRINT
           PRINT "Please input coefficient A1"
           INPUT A1
        LOOP UNTIL (ABS(A1) > 0#)
        PRINT
        PRINT "Please input coefficient A2"
        INPUT A2
        PRINT
        PRINT "Please input coefficient A3"
        INPUT A3
        PRINT
        PRINT "Please input coefficient A4"
        INPUT A4
        PRINT
        PRINT "Please input coefficient A5"
        INPUT A5
        CALL QUARTIC(A1, A2, A3, A4, A5, Q1, Q2, Q3, Q4, NROOTS)
   CASE 2
        PRINT TAB(33); "Cubic Equation"
        PRINT
        PRINT TAB(21); "B1 * X^3 + B2 * X^2 + B3 * X + B4 = 0"
        PRINT
        DO
           PRINT
           PRINT "Please input coefficient B1"
           INPUT B1
        LOOP UNTIL (ABS(B1) > 0#)
        PRINT
        PRINT "Please input coefficient B2"
        INPUT B2
        PRINT
        PRINT "Please input coefficient B3"
        INPUT B3
        PRINT
        PRINT "Please input coefficient B4"
        INPUT B4
        CALL CUBIC(B1, B2, B3, B4, Q1, Q2, Q3, NROOTS)
   CASE 3
        PRINT TAB(31); "Quadratic Equation"
        PRINT
        PRINT TAB(27); "C1 * X^2 + C2 * X + C3 = 0"
        PRINT
        PRINT
        PRINT "Please input coefficient C1"
        INPUT C1
        PRINT
        PRINT "Please input coefficient C2"
        INPUT C2
        PRINT
        PRINT "Please input coefficient C3"
        INPUT C3
        CALL QUADRATIC(C1, C2, C3, Q1, Q2, NROOTS)
   END SELECT

   CLS
   LOCATE 5, 1
   PRINT TAB(32); "Program DEMOPOLY"
   PRINT
   PRINT TAB(13); "< Solution of Quartic, Cubic and Quadratic Equations >"
   PRINT
   PRINT
   PRINT TAB(25); "Number of real roots = "; NROOTS

   IF (NROOTS > 0) THEN
      PRINT
      PRINT

      SELECT CASE ITYPE
      CASE 1
           PRINT TAB(15); "Root 1 ="; TAB(40); Q1
           PRINT
           PRINT TAB(15); "Root 2 ="; TAB(40); Q2
           PRINT
           PRINT TAB(15); "Root 3 ="; TAB(40); Q3
           PRINT
           PRINT TAB(15); "Root 4 ="; TAB(40); Q4
      CASE 2
           PRINT TAB(15); "Root 1 ="; TAB(40); Q1
           PRINT
           PRINT TAB(15); "Root 2 ="; TAB(40); Q2
           PRINT
           PRINT TAB(15); "Root 3 ="; TAB(40); Q3
      CASE 3
           PRINT TAB(15); "Root 1 ="; TAB(40); Q1
           PRINT
           PRINT TAB(15); "Root 2 ="; TAB(40); Q2
      END SELECT
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

