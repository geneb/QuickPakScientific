' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB MATSUB (A(), B(), C(), N, M) STATIC

    ' Matrix subtraction subroutine

    ' [ C ] = [ A ] - [ B ]

    ' Input

    '  N   = number of rows in each matrix
    '  M   = number of columns in each matrix
    '  A() = matrix A ( N rows by M columns )
    '  B() = matrix B ( N rows by M columns )

    ' Output
        
    '  C() = matrix C ( N rows by M columns )

    FOR I = 1 TO N
        FOR J = 1 TO M
            C(I, J) = A(I, J) - B(I, J)
        NEXT J
    NEXT I

END SUB

