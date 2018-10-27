' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB RANK (A(), N, M, K) STATIC

    ' Matrix rank subroutine

    ' Input
    
    '  N   = number of rows of matrix [ A ]
    '  M   = number of columns of matrix [ A ]
    '  A() = matrix ( N rows by M columns )

    ' Output

    '  K = rank of matrix [ A ]

    IF (N > M) THEN
        NDIM = N
    ELSE
        NDIM = M
    END IF

    DIM ATMP(NDIM + 1, NDIM + 1)

    FOR I = 1 TO N
        FOR J = 1 TO M
            ATMP(I, J) = A(I, J)
        NEXT J
    NEXT I

    K = 1

    DO
       KK = K + 1
       ISS = K
       ITT = K
       B = ABS(ATMP(K, K))

       FOR I = K TO N
           FOR J = K TO M
               IF (ABS(ATMP(I, J)) - B) > 0# THEN
                  ISS = I
                  ITT = J
                  B = ABS(ATMP(I, J))
               END IF
           NEXT J
       NEXT I

       IF (ISS - K) > 0 THEN
          FOR J = K TO M
              C = ATMP(ISS, J)
              ATMP(ISS, J) = ATMP(K, J)
              ATMP(K, J) = C
          NEXT J
       END IF

       IF (ITT - K) > 0 THEN
          FOR I = K TO N
              C = ATMP(I, ITT)
              ATMP(I, ITT) = ATMP(I, K)
              ATMP(I, K) = C
          NEXT I
       END IF

       IF (ATMP(K, K)) = 0# THEN EXIT DO

       IF ((K - N) = 0 OR (K - M)) = 0 THEN
          ERASE ATMP
          EXIT SUB
       END IF

       FOR J = KK TO M
           ATMP(K, J) = ATMP(K, J) / ATMP(K, K)
           FOR I = KK TO N
               W = ATMP(I, K) * ATMP(K, J)
               ATMP(I, J) = ATMP(I, J) - W
               IF (ABS(ATMP(I, J)) - .0001# * ABS(W)) < 0# THEN
                  ATMP(I, J) = 0#
               END IF
           NEXT I
       NEXT J

       K = KK
    LOOP

    K = K - 1

    ERASE ATMP

END SUB
