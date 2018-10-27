' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB TRANSPOSE (A(), B(), M, N) STATIC

    ' Matrix traspose subroutine

    ' Input

    '  M   = number of rows in matrix [ A ]
    '  N   = number of columns in matrix [ A ]
    '  A() = matrix A ( M rows by N columns )

    ' Output

    '  B() = matrix transpose ( N rows by M columns )

    FOR I = 1 TO N
        FOR J = 1 TO M
            B(I, J) = A(J, I)
        NEXT J
    NEXT I

END SUB

