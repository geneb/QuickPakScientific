' Copyright (c) 1988, 1989, 1990, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB DETERMIN (N, A(), DM, IER) STATIC

    ' Matrix determinant subroutine

    ' Input

    '  N   = dimension of matrix
    '  A() = matrix of coefficients ( N rows by N columns )

    ' Output

    '  DM  = determinant of [ A ]
    '  IER = error flag ( 0 = no error, 1 = singular matrix
    '        or factorization not possible )

    REDIM INDEX(N), SCALE(N)

    IER = 0

    DM = 1#

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
           DM = -DM
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

    FOR I = 1 TO N
        DM = DM * A(I, I)
    NEXT I

EXITSUB:
    ERASE INDEX, SCALE

END SUB

