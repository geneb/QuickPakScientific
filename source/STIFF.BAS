' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

' subroutine declarations

DECLARE SUB DIFFEQS (X, Y(), DYDX())
DECLARE SUB JACOBIAN (X, Y(), DFDX(), DFDY())
DECLARE SUB LUB (N, A(), INDEX(), X())
DECLARE SUB LUD (N, A(), INDEX(), IER)

SUB DIFFEQS (X, Y(), DYDX()) STATIC

    ' Differential equations subroutine

    ' Input

    '  X   = current X value
    '  Y() = current integration vector
    '        ( 3 rows by 1 column )

    ' Output

    '  DYDX() = system of differential equations evaluated
    '           at X, Y() ( 3 rows by 1 column )

    DYDX(1) = -.013# * Y(1) - 1000# * Y(1) * Y(3)
    DYDX(2) = -2500# * Y(2) * Y(3)
    DYDX(3) = -.013# * Y(1) - 1000# * Y(1) * Y(3) - 2500# * Y(2) * Y(3)

END SUB

SUB JACOBIAN (X, Y(), DFDX(), DFDY()) STATIC

    ' Jacobian matrix subroutine

    ' Input

    '  X   = current X value
    '  Y() = current integration vector
    '        ( 3 rows by 1 column )

    ' Output

    '  DFDX() = vector of partial derivatives of system of
    '           differential equations with respect to X
    '           ( 3 rows by 1 column )
    '  DFDY() = matrix of partial derivatives of system of
    '           differential equations with respect to Y()
    '           ( 3 rows by 3 columns )

    FOR I = 1 TO 3
        DFDX(I) = 0#
    NEXT I

    DFDY(1, 1) = -.013# - 1000# * Y(3)
    DFDY(1, 2) = 0#
    DFDY(1, 3) = -1000# * Y(1)

    DFDY(2, 1) = 0#
    DFDY(2, 2) = -2500# * Y(3)
    DFDY(2, 3) = -2500# * Y(2)

    DFDY(3, 1) = -.013# - 1000# * Y(3)
    DFDY(3, 2) = -2500# * Y(3)
    DFDY(3, 3) = -1000# * Y(1) - 2500# * Y(2)

END SUB

SUB LUB (N, A(), INDEX(), X()) STATIC
    
    ' LU back substitution subroutine

    ' Input

    '  N       = number of equations
    '  A()     = matrix of coefficients ( N rows by N columns )
    '  INDEX() = permutation vector ( N rows )

    ' Output

    '  X() = solution vector ( N rows by 1 column )

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

SUB STIFF (N, YI(), X1, X2, EPS, H1, HMIN) STATIC
     
    ' Solution of stiff differential equations subroutine

    ' Input
    '  N    = number of equations
    '  YI() = initial values of integration vector
    '         ( N rows by 1 column )
    '  X1   = initial X value
    '  X2   = final X value
    '  EPS  = convergence criteria
    '  H1   = initial guess for step size
    '  HMIN = minimum step size

    ' Output
    '  YI() = final values of solution vector
    '         ( N rows by 1 column )

    ' NOTE: requires subroutines LUD, LUB, DIFFEQS and JACOBIAN

    REDIM YSCAL(N), Y(N), DYDX(N)
    REDIM A(N, N), DFDX(N), DFDY(N, N), DYSAV(N), YERR(N)
    REDIM G1(N), G2(N), G3(N), G4(N), YSAV(N), INDEX(N)

    IF (ICOEFF = 0) THEN
       GAMMA = .231#
       A21 = 2#
       A31 = 4.52470820736#
       A32 = 4.1635287886#
       C21 = -5.07167533877#
       C31 = 6.02015272865#
       C32 = .159750684673#
       C41 = -1.856343618677#
       C42 = -8.50538085819#
       C43 = -2.08407513602#
       B1 = 3.95750374663#
       B2 = 4.62489238836#
       B3 = .6174772638729999#
       B4 = 1.282612945268#
       E1 = -2.30215540292#
       E2 = -3.07363448539#
       E3 = .873280801802#
       E4 = 1.282612945268#
       C1X = -.039629667752#
       C2X = -.039629667752#
       C3X = .550778939579#
       C4X = -.05535098457#
       A2X = .462#
       A3X = .880208333333#

       ICOEFF = 1
    END IF

    ' compute step size "direction"

    H = SGN(X2 - X1) * H1
   
    ' save initial conditions

    X = X1

    FOR I = 1 TO N
        Y(I) = YI(I)
    NEXT I

    DO
        CALL DIFFEQS(X, Y(), DYDX())
      
        ' compute scaling vector

        FOR I = 1 TO N
            YSCAL(I) = ABS(Y(I)) + ABS(H * DYDX(I)) + 1D-30
        NEXT I
       
        ' compute last step size

        IF ((X + H - X2) * (X + H - X1) > 0#) THEN H = X2 - X
        
        ' perform integration

        XSAV = X

        FOR I = 1 TO N
            YSAV(I) = Y(I)
            DYSAV(I) = DYDX(I)
        NEXT I

        CALL JACOBIAN(XSAV, YSAV(), DFDX(), DFDY())

        DO
           FOR I = 1 TO N
               FOR J = 1 TO N
                   A(I, J) = -DFDY(I, J)
               NEXT J
               A(I, I) = 1# / (GAMMA * H) + A(I, I)
           NEXT I

           CALL LUD(N, A(), INDEX(), IER)

           FOR I = 1 TO N
               G1(I) = DYSAV(I) + H * C1X * DFDX(I)
           NEXT I

           CALL LUB(N, A(), INDEX(), G1())

           FOR I = 1 TO N
               Y(I) = YSAV(I) + A21 * G1(I)
           NEXT I

           X = XSAV + A2X * H

           CALL DIFFEQS(X, Y(), DYDX())

           FOR I = 1 TO N
               G2(I) = DYDX(I) + H * C2X * DFDX(I) + C21 * G1(I) / H
           NEXT I

           CALL LUB(N, A(), INDEX(), G2())

           FOR I = 1 TO N
               Y(I) = YSAV(I) + A31 * G1(I) + A32 * G2(I)
           NEXT I
       
           X = XSAV + A3X * H

           CALL DIFFEQS(X, Y(), DYDX())

           FOR I = 1 TO N
               G3(I) = DYDX(I) + H * C3X * DFDX(I) + (C31 * G1(I) + C32 * G2(I)) / H
           NEXT I

           CALL LUB(N, A(), INDEX(), G3())

           FOR I = 1 TO N
               G4(I) = DYDX(I) + H * C4X * DFDX(I) + (C41 * G1(I) + C42 * G2(I) + C43 * G3(I)) / H
           NEXT I

           CALL LUB(N, A(), INDEX(), G4())

           FOR I = 1 TO N
               Y(I) = YSAV(I) + B1 * G1(I) + B2 * G2(I) + B3 * G3(I) + B4 * G4(I)
               YERR(I) = E1 * G1(I) + E2 * G2(I) + E3 * G3(I) + E4 * G4(I)
           NEXT I

           X = XSAV + H

           IF (X = XSAV) THEN
              CLS
              LOCATE 5, 1
              PRINT "Insignificant step size"
              STOP
           END IF

           ERRMAX = 0#

           FOR I = 1 TO N
               ERRTMP = ABS(YERR(I) / YSCAL(I))
               IF (ERRTMP > ERRMAX) THEN ERRMAX = ERRTMP
           NEXT I
   
           ERRMAX = ERRMAX / EPS
   
           IF (ERRMAX <= 1#) THEN
              IF (ERRMAX > .1296#) THEN
                 HNEXT = .9# * H * ERRMAX ^ (-.25#)
              ELSE
                 HNEXT = 1.5# * H
              END IF
              EXIT DO
           ELSE
              HNEXT = .9# * H * ERRMAX ^ (-1# / 3#)
              IF (HNEXT < .5# * H) THEN HNEXT = .5# * H
              H = HNEXT
           END IF
        LOOP

        ' check for end of integration

        IF ((X - X2) * (X2 - X1) >= 0#) THEN
           FOR I = 1 TO N
               YI(I) = Y(I)
           NEXT I
           EXIT DO
        END IF
     
        ' check for step size too small

        IF (ABS(HNEXT) < HMIN) THEN
           CLS
           LOCATE 5, 1
           PRINT "Step size too small"
           STOP
        END IF

        ' set next step size

        H = HNEXT
    LOOP

    ' erase working arrays

    ERASE YSCAL, DYDX, A, DFDX, DFDY, DYSAV, YERR, G1, G2, G3, G4, YSAV, INDEX

END SUB

