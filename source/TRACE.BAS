' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB TRACE (N, A(), T) STATIC

    ' Trace of a square matrix subroutine

    ' Input

    '  N   = dimension of matrix
    '  A() = matrix A ( N rows by N columns )

    ' Output

    '  T = trace of matrix [ A ]

    T = 0#

    FOR I = 1 TO N
        T = T + A(I, I)
    NEXT I

END SUB

