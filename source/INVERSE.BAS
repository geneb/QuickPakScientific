' Copyright (c) 1988, 1989, 1990, 1991, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB INVERSE (N, A(), AINV(), IER) STATIC

    ' Inverse of a matrix subroutine

    ' [ AINV ] = inverse [ A ]

    ' Input

    '  N   = dimension of array
    '  A() = matrix of coefficients ( N rows by N columns )

    ' Output

    '  AINV() = inverse of [ A ] ( N rows by N column )
    '  IER    = error flag ( 0 = no error, 1 = singular matrix
    '           or factorization not possible )

    DIM INDEX(N), SCALE(N), X(N)

    FOR I = 1 TO N
        ROWMAX = 0#
        FOR J = 1 TO N
            IF (ABS(A(I, J)) > ROWMAX) THEN ROWMAX = ABS(A(I, J))
        NEXT J
        ' first check for singular matrix
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
           ' second check for singular matrix
           IF (ABS(A(J, J)) <= .000000000001#) THEN
              IER = 1
              GOTO EXITSUB
           END IF
           TMP = 1# / A(J, J)
           FOR I = J + 1 TO N
               A(I, J) = A(I, J) * TMP
           NEXT I
        END IF
    NEXT J

    ' third check for singular matrix

    IF (ABS(A(N, N)) <= .000000000001#) THEN
       IER = 1
       GOTO EXITSUB
    END IF

    FOR II = 1 TO N
        FOR JJ = 1 TO N
            X(JJ) = 0#
            IF (II = JJ) THEN X(JJ) = 1#
        NEXT JJ

        I1 = 0

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

        FOR JJ = 1 TO N
            AINV(JJ, II) = X(JJ)
        NEXT JJ
    NEXT II

EXITSUB:
    ERASE INDEX, SCALE, X

END SUB

