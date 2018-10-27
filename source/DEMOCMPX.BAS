' Program "DEMOCMPX"                   December 29, 1990
'
' Copyright (c) 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' the QuickPak Scientific complex number subroutines.
'
' The available complex number subroutines are:
'
'     Complex number addition subroutine
'     Complex number subtraction subroutine
'     Complex number division subroutine
'     Complex number multiplication subroutine
'     Reciprocal of a complex number subroutine
'     Square root of a complex number subroutine
'     Root of a complex number subroutine
'     Complex number raised to a power subroutine
'
'*******************************************************

' Variable type definitions

DEFDBL A-H, O-Z
DEFINT I-N

' Subroutine declarations

DECLARE SUB CMPXADD (A(), B(), C())
DECLARE SUB CMPXSUB (A(), B(), C())
DECLARE SUB CMPXDIV (A(), B(), C())
DECLARE SUB CMPXMULT (A(), B(), C())
DECLARE SUB CMPXRECP (A(), B())
DECLARE SUB CMPXSQRT (A(), B())
DECLARE SUB CMPXROOT (N, A(), B())
DECLARE SUB CMPXPOWR (N, A(), B())
DECLARE SUB KEYCHECK ()

' Allocate working arrays

DIM T$(9), A(2), B(2), C(2)

' Fill title strings

T$(1) = "Complex number addition subroutine"
T$(2) = "Complex number subtraction subroutine"
T$(3) = "Complex number division subroutine"
T$(4) = "Complex number multiplication subroutine"
T$(5) = "Reciprocal of a complex number subroutine"
T$(6) = "Square root of a complex number subroutine"
T$(7) = "Root of a complex number subroutine"
T$(8) = "Complex number raised to a power subroutine"
T$(9) = "End this demonstration"

' Print format string

F1$ = "####.######"

' Begin demo program

CLS
LOCATE 5, 32
PRINT "Program DEMOCMPX"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "the following QuickPak Scientific complex number"
PRINT TAB(12); "subroutines."
PRINT
FOR I = 1 TO 8
    PRINT TAB(18); T$(I)
NEXT I

CALL KEYCHECK

DO
   DO
      ' print menu
      CLS
      LOCATE 5, 1
      PRINT TAB(32); "Program DEMOCMPX"
      LOCATE 8, 1
      FOR I = 1 TO 9
          PRINT TAB(15); "<"; I; "> "; T$(I)
      NEXT I
      PRINT
      PRINT
      PRINT "Selection ( 1 - 9 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 9)

   IF (ISELECT = 9) THEN END

   ' request complex numbers

   CLS
   LOCATE 5, 1
   PRINT "Please input the real part of complex number #1"
   INPUT A(1)
   PRINT
   PRINT "Please input the imaginary part of complex number #1"
   INPUT A(2)

   IF (ISELECT >= 1 AND ISELECT <= 4) THEN
      PRINT
      PRINT
      PRINT "Please input the real part of complex number #2"
      INPUT B(1)
      PRINT
      PRINT "Please input the imaginary part of complex number #2"
      INPUT B(2)
   END IF

   ' call subroutine

   SELECT CASE ISELECT
   CASE 1
        ' Complex number addition
        CALL CMPXADD(A(), B(), C())
   CASE 2
        ' Complex number subtraction
        CALL CMPXSUB(A(), B(), C())
   CASE 3
        ' Complex number division
        CALL CMPXDIV(A(), B(), C())
   CASE 4
        ' Complex number multiplication
        CALL CMPXMULT(A(), B(), C())
   CASE 5
        ' Reciprocal of a complex number
        CALL CMPXRECP(A(), B())
   CASE 6
        ' Square root of a complex number
        CALL CMPXSQRT(A(), B())
   CASE 7
        ' Root of a complex number
        DO
           PRINT
           PRINT
           PRINT "Please input an integer root"
           PRINT "(NOTE: root >= 1)"
           INPUT N
        LOOP UNTIL (N >= 1)
        DIM D(2, N)
        CALL CMPXROOT(N, A(), D())
   CASE 8
        ' Complex number raised to a power
        DO
           PRINT
           PRINT
           PRINT "Please input an integer power"
           PRINT "(NOTE: ABS(power) >= 1)"
           INPUT N
        LOOP UNTIL (ABS(N) >= 1)
        CALL CMPXPOWR(N, A(), B())
   END SELECT

   A$ = "< " + T$(ISELECT) + " >"

   ' print results

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOCMPX"
   LOCATE 7, 1
   PRINT TAB(40 - .5 * LEN(A$)); A$
   PRINT
   PRINT
   PRINT TAB(15); "Complex number #1 =  ";
   PRINT USING F1$; A(1);
   IF (A(2) > 0#) THEN
      PRINT "  + ";
      PRINT USING F1$; ABS(A(2));
      PRINT " i"
   ELSE
      PRINT "  - ";
      PRINT USING F1$; ABS(A(2));
      PRINT " i"
   END IF
  
   SELECT CASE ISELECT
   CASE 1 TO 4
        PRINT
        PRINT TAB(15); "Complex number #2 =  ";
        PRINT USING F1$; B(1);
        IF (B(2) > 0#) THEN
           PRINT "  + ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        ELSE
           PRINT "  - ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        END IF
        PRINT
        PRINT
        PRINT TAB(13); "Complex number result =  ";
        PRINT USING F1$; C(1);
        IF (C(2) > 0#) THEN
           PRINT "  + ";
           PRINT USING F1$; ABS(C(2));
           PRINT " i"
        ELSE
           PRINT "  - ";
           PRINT USING F1$; ABS(C(2));
           PRINT " i"
        END IF
   CASE 5
        PRINT
        PRINT
        PRINT TAB(15); "Complex reciprocal =  ";
        PRINT USING F1$; B(1);
        IF (B(2) > 0#) THEN
           PRINT "  + ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        ELSE
           PRINT "  - ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        END IF
   CASE 6
        PRINT
        PRINT
        PRINT TAB(15); "Complex square root =  ";
        PRINT USING F1$; B(1);
        IF (B(2) > 0#) THEN
           PRINT "  + ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        ELSE
           PRINT "  - ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        END IF
   CASE 7
        PRINT
        PRINT
        PRINT TAB(27); "Requested root =  "; N
        FOR I = 1 TO N
            PRINT
            PRINT TAB(15); "Complex root ("; I; ") = ";
            PRINT USING F1$; D(1, I);
            IF (D(2, I) > 0#) THEN
               PRINT "  + ";
               PRINT USING F1$; ABS(D(2, I));
               PRINT " i"
            ELSE
               PRINT "  - ";
               PRINT USING F1$; ABS(D(2, I));
               PRINT " i"
            END IF
        NEXT I
   CASE 8
        PRINT
        PRINT
        PRINT TAB(27); "Requested power =  "; N
        PRINT
        PRINT TAB(8); "Complex number raised to a power =  ";
        PRINT USING F1$; B(1);
        IF (B(2) > 0#) THEN
           PRINT "  + ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        ELSE
           PRINT "  - ";
           PRINT USING F1$; ABS(B(2));
           PRINT " i"
        END IF
   END SELECT

   CALL KEYCHECK

   ' request another selection

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")
 
   ERASE D

LOOP UNTIL (SLCT$ = "N")

END

