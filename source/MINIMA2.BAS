' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB MINIMA2 (A, B, EPS, MAXITER, NITER, XMIN, FMIN) STATIC

    ' Brent's method minimization subroutine

    ' Input

    '  A       = initial X search value
    '  B       = final X search value
    '  EPS     = convergence criteria
    '  MAXITER = maximum number of iterations

    ' Output

    '  NITER = number of iterations performed
    '  XMIN  = minimum X value
    '  FMIN  = minimum function value

    ' NOTE: requires SUB USERFUNC

    C = (3# - SQR(5#)) / 2#

    X = A + C * (B - A)
   
    W = X
    V = W
   
    E = 0#
    P = 0#
    Q = 0#
    R = 0#

    CALL USERFUNC(X, FX)

    FW = FX
    FV = FW

    NITER = 0

    FOR I = 1 TO MAXITER

        NITER = NITER + 1

        XM = .5# * (A + B)
        TOL = EPS * ABS(X) + .0000000001#
        T2 = 2# * TOL

        IF (ABS(X - XM) <= (T2 - .5# * (B - A))) THEN
           XMIN = X
           FMIN = FX
           EXIT FOR
        ELSE
           IF (ABS(E) > TOL) THEN
              R = (X - W) * (FX - FV)
              Q = (X - V) * (FX - FW)
              P = (X - V) * Q - (X - W) * R
              Q = 2# * (Q - R)
              IF (Q > 0#) THEN P = -P
              Q = ABS(Q)
              R = E
              E = D
           END IF

           IF ((ABS(P) >= ABS(.5# * Q * R)) OR (P <= Q * (A - X)) OR (P >= Q * (B - X))) THEN
              IF (X >= XM) THEN
                 E = A - X
              ELSE
                 E = B - X
              END IF
              D = C * E
           ELSE
              D = P / Q
              U = X + D
              IF ((U - A) < T2) OR ((B - U) < T2) THEN D = SGN(XM - X) * TOL
           END IF

           IF (ABS(D) >= TOL) THEN
              U = X + D
           ELSE
              U = X + SGN(D) * TOL
           END IF

           CALL USERFUNC(U, FU)

           IF (FU <= FX) THEN
              IF (U >= X) THEN
                 A = X
              ELSE
                 B = X
              END IF
              V = W
              FV = FW
              W = X
              FW = FX
              X = U
              FX = FU
           ELSE
              IF (U < X) THEN
                 A = U
              ELSE
                 B = U
              END IF
              IF ((FU <= FW) OR (W = X)) THEN
                 V = W
                 FV = FW
                 W = U
                 FW = FU
              ELSEIF ((FU <= FV) OR (V = X) OR (V = W)) THEN
                 V = U
                 FV = FU
              END IF
           END IF
        END IF
    NEXT I

END SUB

