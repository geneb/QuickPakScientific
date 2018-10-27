' Program "DEMOSTAT"                   December 26, 1990
'
' Copyright (c) 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' the QuickPak Scientific statistics subroutines.
'
' The following types of statistics are available:
'
'     Normal distribution
'     Chi-squared distribution
'     F-distribution
'     T-distribution
'
'*******************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB NORMAL (IFLAG, X, PROB)
DECLARE SUB CHI (IFLAG, N, X, PROB)
DECLARE SUB FDIST (IFLAG, N1, N2, X, PROB)
DECLARE SUB TDIST (IFLAG, N, X, PROB)
DECLARE SUB KEYCHECK ()

DIM T$(5)

' Fill title strings

T$(1) = "Normal distribution"
T$(2) = "Chi-squared distribution"
T$(3) = "F-distribution"
T$(4) = "T-distribution"
T$(5) = "End This Demonstration"

' Begin demo program

CLS
LOCATE 5, 31
PRINT "Program DEMOSTAT"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "the QuickPak Scientific statistics subroutines."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(31); "Program DEMOSTAT"
      PRINT
      FOR I = 1 TO 5
          PRINT
          PRINT TAB(25); "<"; I; "> "; T$(I)
      NEXT I
      PRINT
      PRINT
      PRINT "Selection ( 1 - 5 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 5)
  
   IF (ISELECT = 5) THEN END

   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(15); "Please input the type of calculation"
      PRINT
      PRINT
      PRINT TAB(15); "< 1 > given x, compute probability"
      PRINT
      PRINT TAB(15); "< 2 > given probability, compute x"
      PRINT
      PRINT
      PRINT "Selection ( 1 or 2 )"
      INPUT ITYPE
   LOOP UNTIL (ITYPE = 1 OR ITYPE = 2)

   PRINT
   PRINT

   SELECT CASE ISELECT
   CASE 1
        ' Normal distribution
        IF (ITYPE = 1) THEN
           PRINT "Please input the value for X"
           INPUT X
        ELSE
           DO
              PRINT "Please input the value for probability"
              PRINT "(NOTE: 0 < probability < 1 )"
              INPUT PROB
           LOOP UNTIL (PROB > 0# AND PROB < 1#)
        END IF
        CALL NORMAL(ITYPE, X, PROB)
   CASE 2
        ' Chi-squared distribution
        IF (ITYPE = 1) THEN
           DO
              PRINT "Please input the value for X"
              PRINT "(NOTE: X >= 0)"
              INPUT X
           LOOP UNTIL (X >= 0#)
        ELSE
           DO
              PRINT "Please input the value for probability"
              PRINT "(NOTE: 0 < probability <= 1 )"
              INPUT PROB
           LOOP UNTIL (PROB > 0# AND PROB <= 1#)
        END IF
        DO
           PRINT
           PRINT "Please input the number of degrees of freedom"
           PRINT "(NOTE: this must be an integer >= 1)"
           INPUT N
        LOOP UNTIL (N >= 1)
        CALL CHI(ITYPE, N, X, PROB)
   CASE 3
        ' F-distribution
        IF (ITYPE = 1) THEN
           DO
              PRINT "Please input the value for X"
              PRINT "(NOTE: X > 0)"
              INPUT X
           LOOP UNTIL (X > 0#)
        ELSE
           DO
              PRINT "Please input the value for probability"
              PRINT "(NOTE: 0 < probability <= 1 )"
              INPUT PROB
           LOOP UNTIL (PROB > 0# AND PROB <= 1#)
        END IF
        DO
           PRINT
           PRINT "Please input the number of degrees of freedom #1"
           PRINT "(NOTE: this must be an integer >= 1)"
           INPUT N1
        LOOP UNTIL (N1 >= 1)
        DO
           PRINT
           PRINT "Please input the number of degrees of freedom #2"
           PRINT "(NOTE: this must be an integer >= 1)"
           INPUT N2
        LOOP UNTIL (N2 >= 1)
        CALL FDIST(ITYPE, N1, N2, X, PROB)
   CASE 4
        ' T-distribution
        IF (ITYPE = 1) THEN
           PRINT "Please input the value for X"
           INPUT X
        ELSE
           DO
              PRINT "Please input the value for probability"
              PRINT "(NOTE: 0 < probability < 1 )"
              INPUT PROB
           LOOP UNTIL (PROB > 0# AND PROB < 1#)
        END IF
        DO
           PRINT
           PRINT "Please input the number of degrees of freedom"
           PRINT "(NOTE: this must be an integer >= 1)"
           INPUT N
        LOOP UNTIL (N >= 1)
        CALL TDIST(ITYPE, N, X, PROB)
   END SELECT

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOSTAT"
   A$ = "< " + T$(ISELECT) + " >"
   LOCATE 7, 1
   PRINT TAB(39 - .5 * LEN(A$)); A$
   PRINT
   PRINT

   ' print results

   PRINT TAB(28); "X value     = ";
   PRINT USING "##.####"; X
   PRINT
   PRINT TAB(28); "Probability = ";
   PRINT USING "##.####"; PROB
   IF (ISELECT = 2 OR ISELECT = 4) THEN
      PRINT
      PRINT TAB(28); "Degrees of freedom = "; N
   END IF
   IF (ISELECT = 3) THEN
      PRINT
      PRINT TAB(26); "Degrees of freedom #1 = "; N1
      PRINT
      PRINT TAB(26); "Degrees of freedom #2 = "; N2
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


