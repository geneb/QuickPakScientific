' Copyright (c) 1989, 1990, 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB LINEAR2 (N, A(), B(), IER) STATIC

    ' Solution of a system of linear equations subroutine

    ' Solves [ A ] * { X } = { B } using the Gauss-Jordan method

    ' Input

    '  N   = number of equations
    '  A() = matrix of coefficients   ( N rows by N columns )
    '  B() = right hand column vector ( N rows )

    ' Output

    '  A() = inverse of incoming matrix [ A ] ( N rows by N columns )
    '  B() = solution vector of linear system ( N rows )
    '  IER = error flag
    '    0 = no error
    '    1 = singular matrix

    DIM IPIVOT(N), INDEXR(N), INDEXC(N)

    IER = 0

    FOR J = 1 TO N
        IPIVOT(J) = 0
    NEXT J

    FOR I = 1 TO N
        PMAX = 0#

        FOR J = 1 TO N
            IF (IPIVOT(J) <> 1) THEN
               FOR K = 1 TO N
                   IF (IPIVOT(K) = 0) THEN
                      IF (ABS(A(J, K)) >= PMAX) THEN
                         PMAX = ABS(A(J, K))
                         IR = J
                         IC = K
                      END IF
                   ELSEIF (IPIVOT(K) > 1) THEN
                      IER = 1
                      GOTO EXITSUB
                   END IF
               NEXT K
            END IF
        NEXT J

        IPIVOT(IC) = IPIVOT(IC) + 1
       
        IF (IR <> IC) THEN
           FOR L = 1 TO N
               TMP = A(IR, L)
               A(IR, L) = A(IC, L)
               A(IC, L) = TMP
           NEXT L

           TMP = B(IR)
           B(IR) = B(IC)
           B(IC) = TMP
        END IF

        INDEXR(I) = IR
        INDEXC(I) = IC
       
        IF (A(IC, IC) = 0#) THEN
           IER = 1
           GOTO EXITSUB
        END IF
       
        PIVINV = 1# / A(IC, IC)
        A(IC, IC) = 1#

        FOR L = 1 TO N
            A(IC, L) = A(IC, L) * PIVINV
        NEXT L

        B(IC) = B(IC) * PIVINV

        FOR LL = 1 TO N
            IF (LL <> IC) THEN
               TMP = A(LL, IC)
               A(LL, IC) = 0#
               FOR L = 1 TO N
                   A(LL, L) = A(LL, L) - A(IC, L) * TMP
               NEXT L
               B(LL) = B(LL) - B(IC) * TMP
            END IF
        NEXT LL
    NEXT I

    FOR L = N TO 1 STEP -1
        IF (INDEXR(L) <> INDEXC(L)) THEN
           FOR K = 1 TO N
               TMP = A(K, INDEXR(L))
               A(K, INDEXR(L)) = A(K, INDEXC(L))
               A(K, INDEXC(L)) = TMP
           NEXT K
        END IF
    NEXT L

EXITSUB:
    ERASE IPIVOT, INDEXC, INDEXR

END SUB

