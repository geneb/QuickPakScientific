' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB VDOT (N, A(), B(), C) STATIC

    ' Vector dot product subroutine

    ' C = { A } dot { B }

    ' Input

    '  N   = number of rows
    '  A() = column vector with N rows
    '  B() = column vector with N rows

    ' Output

    '  C = dot product of { A } dot { B }

    C = 0#

    FOR I = 1 TO N
        C = C + A(I) * B(I)
    NEXT I

END SUB

