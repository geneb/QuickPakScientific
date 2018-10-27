' Program "DEMOMATX"                   December 19, 1993
'
' Copyright (c) 1988-1993 Crescent Software
'
' This program demonstrates the procedure for calling
' the QuickPak Scientific matrix subroutines.
'
' The available matrix routines are as follows:
'
'   Matrix addition
'   Matrix subtraction
'   Matrix multiplication subroutine
'   Matrix/vector multiplication subroutine
'   Identity matrix subroutine  
'   Trace of a square matrix subroutine
'   Matrix transpose subroutine
'   Matrix rank subroutine
'   Eigenvalues and Eigenvectors subroutine
'
' NOTE: All matrices and vectors are input by rows.
'
'*******************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB IMATRIX (N, A())
DECLARE SUB MATADD (A(), B(), C(), N, M)
DECLARE SUB MATSUB (A(), B(), C(), N, M)
DECLARE SUB MATXMAT (A(), B(), C(), L, M, N)
DECLARE SUB MATXVEC (A(), B(), C(), L, M)
DECLARE SUB TRACE (N, A(), T)
DECLARE SUB TRANSPOSE (A(), B(), M, N)
DECLARE SUB RANK (A(), N, M, K)
DECLARE SUB CORTH (NM, N, LOW, IGH, AR(), AI(), ORTR(), ORTI())
DECLARE SUB COMQR2 (NM, N, LOW, IGH, ORTR(), ORTI(), HR(), HI(), WR(), WI(), ZR(), ZI(), IERR)
DECLARE SUB CBABK2 (NM, N, LOW, IGH, SCALE(), M, ZR(), ZI())
DECLARE SUB CBAL (NM, N, AR(), AI(), LOW, IGH, SCALE())
DECLARE SUB EIGEN (N, NM, AR(), AI(), WR(), WI(), ZR(), ZI(), IERR)
DECLARE SUB KEYCHECK ()

' Dimension menu array

DIM T$(10)

' Fill title strings

T$(1) = "Matrix addition"
T$(2) = "Matrix subtraction"
T$(3) = "Matrix multiplication subroutine"
T$(4) = "Matrix/vector multiplication subroutine"
T$(5) = "Identity matrix subroutine"
T$(6) = "Trace of a square matrix subroutine"
T$(7) = "Matrix transpose subroutine"
T$(8) = "Matrix rank subroutine"
T$(9) = "Eigenvalues and eigenvectors subroutine"
T$(10) = "End this demonstration"

' Begin demo program

CLS
LOCATE 3, 32
PRINT "Program DEMOMATX"

LOCATE 6, 1
PRINT TAB(12); "This program demonstrates the procedure for calling"
PRINT TAB(12); "the QuickPak Scientific matrix subroutines."
PRINT
PRINT TAB(12); "The available matrix routines are as follows:"
PRINT
FOR I = 1 TO 9
    PRINT TAB(15); T$(I)
NEXT I
PRINT
PRINT TAB(12); "NOTE: All matrices and vectors are input by rows."

CALL KEYCHECK

DO
   DO
      CLS
      LOCATE 3, 1
      PRINT TAB(31); "Program DEMOMATX"
      LOCATE 6, 1
      FOR I = 1 TO 10
          PRINT TAB(15); "<"; I; "> "; T$(I)
      NEXT I
      PRINT
      PRINT
      PRINT "Selection ( 1 - 11 )"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 11)

   SELECT CASE ISELECT
   CASE 1
        ' Matrix addition
        CLS
        LOCATE 5, 1
        PRINT TAB(29); "[ C ] = [ A ] + [ B ]"
        LOCATE 8, 1
        DO
           PRINT "Please input the number of rows in each matrix"
           INPUT NROWS
        LOOP UNTIL (NROWS > 0)
        PRINT
        DO
           PRINT "Please input the number of columns in each matrix"
           INPUT MCOLUMNS
        LOOP UNTIL (MCOLUMNS > 0)
        DIM A1(NROWS, MCOLUMNS), B1(NROWS, MCOLUMNS), C1(NROWS, MCOLUMNS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A1(I, J)
            NEXT J
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element B("; I; ","; J; ")"
                INPUT B1(I, J)
            NEXT J
        NEXT I
   CASE 2
        ' Matrix subtraction
        CLS
        LOCATE 5, 1
        PRINT TAB(29); "[ C ] = [ A ] - [ B ]"
        LOCATE 8, 1
        DO
           PRINT "Please input the number of rows in each matrix"
           INPUT NROWS
        LOOP UNTIL (NROWS > 0)
        PRINT
        DO
           PRINT "Please input the number of columns in each matrix"
           INPUT MCOLUMNS
        LOOP UNTIL (MCOLUMNS > 0)
        DIM A2(NROWS, MCOLUMNS), B2(NROWS, MCOLUMNS), C2(NROWS, MCOLUMNS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A2(I, J)
            NEXT J
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element B("; I; ","; J; ")"
                INPUT B2(I, J)
            NEXT J
        NEXT I
   CASE 3
        ' Matrix multiplication
        CLS
        LOCATE 5, 1
        PRINT TAB(29); "[ C ] = [ A ] * [ B ]"
        LOCATE 8, 1
        DO
           PRINT "Please input the number of rows of matrix [ A ]"
           INPUT LROWS
        LOOP UNTIL (LROWS > 0)
        PRINT
        DO
           PRINT "Please input the number of columns of matrix [ A ]"
           INPUT MCOLUMNS
        LOOP UNTIL (MCOLUMNS > 0)
        PRINT
        PRINT
        DO
           PRINT "Please input the number of columns of matrix [ B ]"
           INPUT NCOLUMNS
        LOOP UNTIL (NCOLUMNS > 0)
        MROWS = MCOLUMNS
        DIM A3(LROWS, MCOLUMNS), B3(MROWS, NCOLUMNS), C3(LROWS, NCOLUMNS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO LROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A3(I, J)
            NEXT J
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO MROWS
            PRINT
            FOR J = 1 TO NCOLUMNS
                PRINT
                PRINT "Please input matrix element B("; I; ","; J; ")"
                INPUT B3(I, J)
            NEXT J
        NEXT I
   CASE 4
        ' Matrix * vector
        CLS
        LOCATE 5, 1
        PRINT TAB(29); "{ C } = [ A ] * { B }"
        LOCATE 8, 1
        DO
           PRINT "Please input the number of rows of matrix [ A ]"
           INPUT LROWS
        LOOP UNTIL LROWS > 0
        PRINT
        DO
           PRINT "Please input the number of columns of matrix [ A ]"
           INPUT MCOLUMNS
        LOOP UNTIL (MCOLUMNS > 0)
        DIM A4(LROWS, MCOLUMNS), B4(MCOLUMNS), C4(LROWS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO LROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A4(I, J)
            NEXT J
        NEXT I
        CLS
        LOCATE 5, 1
        FOR I = 1 TO MCOLUMNS
            PRINT
            PRINT "Please input vector element B("; I; ")"
            INPUT B4(I)
        NEXT I
   CASE 5
        ' Identity matrix
        DO
           CLS
           LOCATE 5, 1
           PRINT TAB(32); "Identity Matrix"
           PRINT
           PRINT
           PRINT "Please input the dimension of the matrix"
           INPUT NDIM
        LOOP UNTIL (NDIM > 0)
        DIM A5(NDIM, NDIM)
   CASE 6
        ' Trace of a square matrix
        DO
           CLS
           LOCATE 5, 1
           PRINT TAB(28); "Trace of a square Matrix"
           PRINT
           PRINT
           PRINT "Please input the dimension of the matrix"
           INPUT NDIM
        LOOP UNTIL (NDIM > 0)
        DIM A6(NDIM, NDIM)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO NDIM
            PRINT
            FOR J = 1 TO NDIM
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A6(I, J)
            NEXT J
        NEXT I
   CASE 7
        ' Matrix transpose
        CLS
        LOCATE 5, 1
        PRINT TAB(28); "[ B ] = Transpose [ A ]"
        LOCATE 8, 1
        DO
           PRINT "Please input the number of rows of matrix [ A ]"
           INPUT LROWS
        LOOP UNTIL (LROWS > 0)
        PRINT
        DO
           PRINT "Please input the number of columns of matrix [ A ]"
           INPUT MCOLUMNS
        LOOP UNTIL (MCOLUMNS > 0)
        DIM A7(LROWS, MCOLUMNS), B7(MCOLUMNS, LROWS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO LROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A7(I, J)
            NEXT J
        NEXT I
   CASE 8
        ' Matrix rank
        CLS
        LOCATE 5, 1
        PRINT TAB(28); "Rank of a Matrix"
        LOCATE 8, 1
        DO
           PRINT "Please input the number of rows of matrix [ A ]"
           INPUT LROWS
        LOOP UNTIL (LROWS > 0)
        PRINT
        DO
           PRINT "Please input the number of columns of matrix [ A ]"
           INPUT MCOLUMNS
        LOOP UNTIL (MCOLUMNS > 0)
        DIM A8(LROWS, MCOLUMNS), B8(MCOLUMNS, LROWS)
        CLS
        LOCATE 5, 1
        FOR I = 1 TO LROWS
            PRINT
            FOR J = 1 TO MCOLUMNS
                PRINT
                PRINT "Please input matrix element A("; I; ","; J; ")"
                INPUT A8(I, J)
            NEXT J
        NEXT I
   CASE 9
        ' Eigenvalues and Eigenvectors
        DO
           CLS
           LOCATE 5, 1
           PRINT TAB(26); "Eigenvalues and Eigenvectors"
           PRINT
           PRINT
           PRINT "Please input the dimension of the matrix"
           INPUT NDIM
        LOOP UNTIL (NDIM > 0)
      
        DIM AR(NDIM, NDIM), AI(NDIM, NDIM), WR(NDIM), WI(NDIM)
        DIM ZR(NDIM, NDIM), ZI(NDIM, NDIM)
      
        CLS
        LOCATE 5, 1

        FOR I = 1 TO NDIM
            FOR J = 1 TO NDIM
                CLS
                LOCATE 5, 1
                PRINT "Please input the real component of matrix element A("; I; ","; J; ")"
                INPUT AR(I, J)
                PRINT
                PRINT "Please input the complex component of matrix element A("; I; ","; J; ")"
                INPUT AI(I, J)
            NEXT J
        NEXT I
   CASE 10
        END
   END SELECT

   CLS
   LOCATE 5, 31
   PRINT "Program DEMOMATX"
   LOCATE 7, 1
   PRINT TAB(40 - .5 * LEN(T$(ISELECT))); T$(ISELECT)
   PRINT
   PRINT

   SELECT CASE ISELECT
   CASE 1
        ' Matrix addition
        CALL MATADD(A1(), B1(), C1(), NROWS, MCOLUMNS)
        FOR I = 1 TO NROWS
            PRINT
            PRINT
            PRINT TAB(20);
            FOR J = 1 TO MCOLUMNS
                PRINT USING "##.######^^^^"; C1(I, J);
                PRINT "  ";
            NEXT J
        NEXT I
        ERASE A1, B1, C1
   CASE 2
        ' Matrix subtraction
        CALL MATSUB(A2(), B2(), C2(), NROWS, MCOLUMNS)
        FOR I = 1 TO NROWS
            PRINT
            PRINT
            PRINT TAB(20);
            FOR J = 1 TO MCOLUMNS
                PRINT USING "##.######^^^^"; C2(I, J);
                PRINT "  ";
            NEXT J
        NEXT I
        ERASE A2, B2, C2
   CASE 3
        ' Matrix multiplication
        CALL MATXMAT(A3(), B3(), C3(), LROWS, MCOLUMNS, NCOLUMNS)
        FOR I = 1 TO LROWS
            PRINT
            PRINT
            PRINT TAB(20);
            FOR J = 1 TO NCOLUMNS
                PRINT USING "##.######^^^^"; C3(I, J);
                PRINT "  ";
            NEXT J
        NEXT I
        ERASE A3, B3, C3
   CASE 4
        ' Matrix/vector multiplication
        CALL MATXVEC(A4(), B4(), C4(), LROWS, MCOLUMNS)
        PRINT
        FOR I = 1 TO LROWS
            PRINT
            PRINT TAB(25); "C("; I; ") =  ";
            PRINT USING "##.######^^^^"; C4(I)
        NEXT I
        ERASE A4, B4, C4
   CASE 5
        ' Identity matrix
        CALL IMATRIX(NDIM, A5())
        FOR I = 1 TO NDIM
            PRINT TAB(25);
            FOR J = 1 TO NDIM
                PRINT A5(I, J);
            NEXT J
        NEXT I
        ERASE A5
   CASE 6
        ' Trace of a matrix
        CALL TRACE(NDIM, A6(), T)
        PRINT
        PRINT TAB(27); "Trace = ";
        PRINT USING "##.######^^^^"; T
        ERASE A6
   CASE 7
        ' Matrix transpose
        CALL TRANSPOSE(A7(), B7(), LROWS, MCOLUMNS)
        FOR I = 1 TO MCOLUMNS
            PRINT
            PRINT
            PRINT TAB(15);
            FOR J = 1 TO LROWS
                PRINT USING "##.######^^^^"; B7(I, J);
                PRINT "  ";
            NEXT J
        NEXT I
        ERASE A7, B7
   CASE 8
        ' Matrix rank
        CALL RANK(A8(), LROWS, MCOLUMNS, IRANK)
        PRINT TAB(25); "Matrix rank =";
        PRINT USING "##.######^^^^"; IRANK
        ERASE A8
   CASE 9
        ' Real and complex eigenvalues
       
        CALL EIGEN(NDIM, NDIM, AR(), AI(), WR(), WI(), ZR(), ZI(), IERR)

        IF (IERR = 0) THEN
           FOR I = 1 TO NDIM
               CLS
               LOCATE 3, 24
               PRINT "Eigenvalues and Eigenvectors"
               LOCATE 6, 1
               PRINT TAB(37); "real"; TAB(51); "imaginary"
               PRINT
               PRINT TAB(15); "Eigenvalue  #"; I;
               PRINT USING "  ##.######^^^^  "; WR(I); WI(I)
               PRINT
               PRINT
               PRINT TAB(15); "Eigenvector #"; I
               PRINT
               PRINT TAB(26); "real"; TAB(40); "imaginary"
               PRINT
               FOR J = 1 TO NDIM
                   PRINT TAB(20);
                   PRINT USING "  ##.######^^^^  "; ZR(J, I); ZI(J, I)
               NEXT J
               IF (I < NDIM) THEN CALL KEYCHECK
           NEXT I
        ELSE
           CLS
           LOCATE 5, 1
           PRINT TAB(30); "Algorithm error !!"
        END IF
        ERASE AR, AI, WR, WI, ZR, ZI
   END SELECT

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

