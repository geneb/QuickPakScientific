' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB MATXMAT (A(), B(), C(), L, M, N) STATIC

    ' Matrix multiplication subroutine

    ' [ C ] = [ A ] * [ B ]

    ' Input
   
    '  L   = number of rows of matrix [ A ]
    '  M   = number of columns of matrix [ A ]
    '      = number of rows of matrix [ B ]
    '  N   = number of columns of matrix [ B ]
    '  A() = matrix A ( L rows by M columns )
    '  B() = matrix B ( M rows by N columns )

    ' Output
        
    '  C() = matrix C ( L rows by N columns )

    FOR I = 1 TO L
        FOR J = 1 TO N
            S = 0#
            FOR K = 1 TO M
                S = S + A(I, K) * B(K, J)
            NEXT K
            C(I, J) = S
        NEXT J
    NEXT I

END SUB

