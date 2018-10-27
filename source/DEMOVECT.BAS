' Program "DEMOVECT"                   December 26, 1990
'
' Copyright (c) 1988, 1989, 1990 Crescent Software
'
' This program demonstrates the procedure for calling
' the QuickPak Scientific vector utility subroutines.
'
' The available vector subroutines are:
'
'     Unit vector
'     Vector cross product
'     Vector dot product
'     Vector magnitude
'     Vector addition
'     Vector subtraction
'     Vector triple scalar product
'     Vector triple product
'
'*******************************************************

' Variable type definitions

DEFDBL A-H, O-Z
DEFINT I-N

' Subroutine declarations

DECLARE SUB UVECTOR (A(), B())
DECLARE SUB VCROSS (A(), B(), C())
DECLARE SUB VDOT (N, A(), B(), C)
DECLARE SUB VECADD (A(), B(), C(), N)
DECLARE SUB VECMAG (A(), A)
DECLARE SUB VECSTP (A(), B(), C(), VTSP)
DECLARE SUB VECSUB (A(), B(), C(), N)
DECLARE SUB VECVTP (A(), B(), C(), D())
DECLARE SUB KEYCHECK ()

' Allocate working arrays

DIM T$(9), A(3), B(3), C(3), D(3)

' Fill title strings

T$(1) = "Unit vector subroutine"
T$(2) = "Vector cross product subroutine"
T$(3) = "Vector dot product subroutine"
T$(4) = "Vector magnitude subroutine"
T$(5) = "Vector addition subroutine"
T$(6) = "Vector subtraction subroutine"
T$(7) = "Vector triple scalar product subroutine"
T$(8) = "Vector triple product subroutine"
T$(9) = "End This Demonstration"

' Begin demo program

CLS
LOCATE 5, 31
PRINT "Program DEMOVECT"

LOCATE 8, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "the QuickPak Scientific vector utility subroutines."
PRINT
FOR I = 1 TO 8
    PRINT TAB(18); T$(I)
NEXT I

CALL KEYCHECK

DO
   DO
      CLS
      PRINT
      PRINT TAB(31); "Program DEMOVECT"
      FOR I = 1 TO 9
          PRINT
          PRINT TAB(15); "<"; I; "> "; T$(I)
      NEXT I
      PRINT
      PRINT
      PRINT "Selection ( 1 - 9 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 9)

   SELECT CASE ISELECT
   CASE 1
        ' Unit vector
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A(I)
        NEXT I
   CASE 2
        ' Vector cross product
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B(I)
        NEXT I
   CASE 3
        ' Vector dot product
        CLS
        LOCATE 5, 1
        DO
           PRINT "Please input the number of rows in each column vector"
           INPUT NROWS
        LOOP UNTIL (NROWS > 0)
        DIM A1(NROWS), B1(NROWS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A1(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B1(I)
        NEXT I
   CASE 4
        ' Vector magnitude
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A(I)
        NEXT I
   CASE 5
        ' Vector addition
        CLS
        LOCATE 5, 1
        DO
           PRINT "Please input the number of rows in each column vector"
           INPUT NROWS
        LOOP UNTIL (NROWS > 0)
        DIM A2(NROWS), B2(NROWS), C2(NROWS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A2(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B2(I)
        NEXT I
   CASE 6
        ' Vector subtraction
        CLS
        LOCATE 5, 1
        DO
           PRINT "Please input the number of rows in each column vector"
           INPUT NROWS
        LOOP UNTIL (NROWS > 0)
        DIM A3(NROWS), B3(NROWS), C3(NROWS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A3(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B3(I)
        NEXT I
   CASE 7
        ' Vector triple scalar product
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element C("; I; ")"
            INPUT C(I)
        NEXT I
   CASE 8
        ' Vector triple product
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element A("; I; ")"
            INPUT A(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B(I)
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO 3
            PRINT
            PRINT "Please input vector element C("; I; ")"
            INPUT C(I)
        NEXT I
   CASE 9
        END
   END SELECT

   A$ = "< " + T$(ISELECT) + " >"

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOVECT"
   LOCATE 7, 1
   PRINT TAB(40 - .5 * LEN(A$)); A$
   PRINT
   PRINT

   SELECT CASE ISELECT
   CASE 1
        ' Unit vector
        CALL UVECTOR(A(), B())
        FOR I = 1 TO 3
            PRINT
            PRINT TAB(25); "B("; I; ") = "; B(I)
        NEXT I
   CASE 2
        ' Vector cross product
        CALL VCROSS(A(), B(), C())
        FOR I = 1 TO 3
            PRINT
            PRINT TAB(25); "C("; I; ") = "; C(I)
        NEXT I
   CASE 3
        ' Vector dot product
        CALL VDOT(NROWS, A1(), B1(), ADOTB)
        PRINT
        PRINT TAB(25); "Vector dot product = "; ADOTB
        ERASE A1, B1
   CASE 4
        ' Vector magnitude
        CALL VECMAG(A(), AMAG)
        PRINT
        PRINT TAB(20); "Vector magnitude = "; AMAG
   CASE 5
        ' Vector addition
        CALL VECADD(A2(), B2(), C2(), NROWS)
        FOR I = 1 TO NROWS
            PRINT
            PRINT TAB(25); "C("; I; ") = "; C2(I)
        NEXT I
        ERASE A2, B2, C2
   CASE 6
        ' Vector subtraction
        CALL VECSUB(A3(), B3(), C3(), NROWS)
        FOR I = 1 TO NROWS
            PRINT
            PRINT TAB(25); "C("; I; ") = "; C3(I)
        NEXT I
        ERASE A3, B3, C3
   CASE 7
        ' Vector triple scalar product
        CALL VECSTP(A(), B(), C(), VTSP)
        PRINT
        PRINT TAB(24); "Triple scalar product = "; VTSP
   CASE 8
        ' Vector triple product
        CALL VECVTP(A(), B(), C(), D())
        FOR I = 1 TO 3
            PRINT
            PRINT TAB(25); "D("; I; ") = "; D(I)
        NEXT I
   END SELECT

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")
  
   ERASE A, B, C, D

LOOP UNTIL (SLCT$ = "N")

END

