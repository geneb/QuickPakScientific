' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB LINEAR3 (N, A(), B(), C(), F(), X(), IER) STATIC

    ' Solve a tridiagonal system of linear equations subroutine

    ' Input

    '  N   = number of equations
    '  A() = vector of subdiagonal elements ( N rows )
    '  B() = vector of diagonal elements ( N rows )
    '  C() = vector of superdiagonal elements ( N rows )
    '  F() = right hand side vector ( N rows )

    ' Output

    '  X() = solution vector ( N rows )
    '  IER = error flag
    '    0 = no error
    '    1 = singular matrix

    DIM D(N)

    ' initialization

    IER = 0

    FOR I = 1 TO N
        X(I) = F(I)
        D(I) = 0#
    NEXT I

    ' begin forward elimination

    FOR K = 1 TO (N - 1)
        IF (ABS(A(K + 1)) > ABS(B(K))) THEN
           TEMP = B(K)
           B(K) = A(K + 1)
           A(K + 1) = TEMP
           TEMP = C(K)
           C(K) = B(K + 1)
           B(K + 1) = TEMP
           TEMP = X(K)
           X(K) = X(K + 1)
           X(K + 1) = TEMP
          
           IF (K < (N - 1)) THEN
              TEMP = D(K)
              D(K) = C(K + 1)
              C(K + 1) = TEMP
           END IF
        END IF

        ' check for singular matrix

        IF (B(K) = 0#) THEN
           IER = 1
           GOTO EXITSUB
        END IF

        AMUL = -A(K + 1) / B(K)
        B(K + 1) = B(K + 1) + AMUL * C(K)
        X(K + 1) = X(K + 1) + AMUL * X(K)

        IF (K < (N - 1)) THEN
           C(K + 1) = C(K + 1) + AMUL * D(K)
        END IF

    NEXT K
       
    ' check for singular matrix

    IF (B(N) = 0#) THEN
       IER = 1
       GOTO EXITSUB
    END IF

    ' back substitution

    X(N) = X(N) / B(N)
    X(N - 1) = (X(N - 1) - C(N - 1) * X(N)) / B(N - 1)

    FOR K = (N - 2) TO 1 STEP -1
        X(K) = (X(K) - C(K) * X(K + 1) - D(K) * X(K + 2)) / B(K)
    NEXT K

EXITSUB:

    ERASE D

END SUB

