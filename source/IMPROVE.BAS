' Copyright (c) 1988, 1989, 1990, 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB LUD (N, A(), INDEX(), IER)

DECLARE SUB SOLVE (N, A(), X(), B(), INDEX())

SUB IMPROVE (NITER, N, A(), B(), X(), IER) STATIC

    ' Solution of a system of linear equations subroutine

    ' solves [ A ] * { X } = { B } using iterative improvement

    ' Input

    '  NITER = number of iterations
    '  N     = number of equations
    '  A()   = matrix of coefficients    ( N rows by N columns )
    '  B()   = right hand column vector  ( N rows )
    '  X()   = initial solution vector   ( N rows )

    ' Output

    '  X() = improved solution vector ( N rows )
    '  IER = error flag
    '    0 = no error
    '    1 = singular matrix or factorization not possible

    ' NOTE: requires subroutines LUD.BAS and SOLVE.BAS

    DIM INDEX(N), DX(N), R(N), ASAVED(N, N), BSAVED(N)

    FOR I = 1 TO N
        FOR J = 1 TO N
            ASAVED(I, J) = A(I, J)
        NEXT J
        BSAVED(I) = B(I)
    NEXT I

    CALL LUD(N, A(), INDEX(), IER)

    IF (IER = 1) THEN GOTO EXITSUB1

    FOR ITER = 1 TO NITER

        FOR I = 1 TO N
            FOR J = 1 TO N
                A(I, J) = ASAVED(I, J)
            NEXT J
            B(I) = BSAVED(I)
        NEXT I

        FOR I = 1 TO N
            TMP = -B(I)
            FOR J = 1 TO N
                TMP = TMP + ASAVED(I, J) * X(J)
            NEXT J
            R(I) = TMP
        NEXT I

        CALL SOLVE(N, A(), DX(), R(), INDEX())

        FOR I = 1 TO N
            X(I) = X(I) - DX(I)
        NEXT I

    NEXT ITER

EXITSUB1:
    ERASE INDEX, DX, R, ASAVED, BSAVED

END SUB

SUB LUD (N, A(), INDEX(), IER) STATIC

    ' LU decomposition subroutine

    ' Input

    '  N       = number of equations
    '  A()     = matrix of coefficients ( N rows by N columns )
    '  INDEX() = permutation vector ( N rows )

    ' Output

    '  A() = LU matrix of coefficients ( N rows by N columns )
    '  IER = error flag
    '    0 = no error
    '    1 = singular matrix or factorization not possible

    DIM SCALE(N)

    IER = 0

    FOR I = 1 TO N
        ROWMAX = 0#
        FOR J = 1 TO N
            IF (ABS(A(I, J)) > ROWMAX) THEN ROWMAX = ABS(A(I, J))
        NEXT J
        ' check for singular matrix
        IF (ROWMAX = 0#) THEN
           IER = 1
           GOTO EXITSUB
        ELSE
           SCALE(I) = 1# / ROWMAX
        END IF
    NEXT I

    FOR J = 1 TO N
        IF (J > 1) THEN
           FOR I = 1 TO J - 1
               S = A(I, J)
               IF (I > 1) THEN
                  FOR K = 1 TO I - 1
                      S = S - A(I, K) * A(K, J)
                  NEXT K
                  A(I, J) = S
               END IF
           NEXT I
        END IF

        PIVOTMAX = 0#

        FOR I = J TO N
            S = A(I, J)
            IF (J > 1) THEN
               FOR K = 1 TO J - 1
                   S = S - A(I, K) * A(K, J)
               NEXT K
               A(I, J) = S
            END IF
            PIVOT = SCALE(I) * ABS(S)
            IF (PIVOT >= PIVOTMAX) THEN
               IMAX = I
               PIVOTMAX = PIVOT
            END IF
        NEXT I

        IF (J <> IMAX) THEN
           FOR K = 1 TO N
               TMP = A(IMAX, K)
               A(IMAX, K) = A(J, K)
               A(J, K) = TMP
           NEXT K
           SCALE(IMAX) = SCALE(J)
        END IF

        INDEX(J) = IMAX

        IF (J <> N) THEN
           ' check for singular matrix
           IF (A(J, J) = 0#) THEN
              IER = 1
              GOTO EXITSUB
           END IF
           TMP = 1# / A(J, J)
           FOR I = J + 1 TO N
               A(I, J) = A(I, J) * TMP
           NEXT I
        END IF
    NEXT J

    IF (A(N, N) = 0#) THEN IER = 1

EXITSUB:
    ERASE SCALE

END SUB

SUB SOLVE (N, A(), X(), B(), INDEX()) STATIC

    ' Solve linear system subroutine

    ' Input

    '  N       = number of equations
    '  A       = LU matrix of coefficients ( N rows by N columns )
    '  B       = right hand column vector  ( N rows )
    '  INDEX() = permutation vector        ( N rows )

    ' Output

    '  X() = solution vector ( N rows )

    FOR I = 1 TO N
        X(I) = B(I)
    NEXT I

    FOR I = 1 TO N
        L = INDEX(I)
        S = X(L)
        X(L) = X(I)
        IF (I1 <> 0) THEN
           FOR J = I1 TO I - 1
               S = S - A(I, J) * X(J)
           NEXT J
        ELSEIF (S <> 0#) THEN
           I1 = I
        END IF
        X(I) = S
    NEXT I

    FOR I = N TO 1 STEP -1
        S = X(I)
        IF (I < N) THEN
           FOR J = I + 1 TO N
               S = S - A(I, J) * X(J)
           NEXT J
        END IF
        X(I) = S / A(I, I)
    NEXT I

END SUB

