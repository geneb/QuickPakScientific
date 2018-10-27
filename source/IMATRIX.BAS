' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB IMATRIX (N, A()) STATIC

    ' Identity matrix subroutine

    ' Input

    '  N = dimension of matrix

    ' Output

    '  A() = identity matrix ( N rows by N columns )

    FOR I = 1 TO N
        FOR J = 1 TO N
            IF I <> J THEN
               A(I, J) = 0#
            ELSE
               A(I, J) = 1#
            END IF
        NEXT J
    NEXT I

END SUB

