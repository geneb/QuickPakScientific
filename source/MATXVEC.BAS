' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB MATXVEC (A(), B(), C(), L, M) STATIC

    ' Matrix/vector multiplication subroutine

    ' { C } = [ A ] * { B }

    ' Input

    '  L   = number of rows of matrix [ A ]
    '  M   = number of columns of matrix [ A ]
    '      = number of rows of vector { B }
    '  A() = matrix A ( L rows by M columns )
    '  B() = vector B ( M rows )

    ' Output

    '  C() = vector C ( L rows )

    FOR I = 1 TO L
        S = 0#
        FOR J = 1 TO M
            S = S + A(I, J) * B(J)
        NEXT J
        C(I) = S
    NEXT I

END SUB

