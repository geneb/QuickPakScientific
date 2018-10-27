' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FVAL)

SUB REALROOT (XL, XU, TOL, MAXITER, NITER, XROOT, FROOT) STATIC

    ' Real root of a non-linear function subroutine

    ' Input

    '  XL      = lower bound of search interval
    '  XU      = upper bound of search interval
    '  TOL     = convergence criteria
    '  MAXITER = maximum number of iterations

    ' Output

    '  XROOT = real root of F(X) = 0
    '  FROOT = function value
    '  NITER = number of iterations

    ' NOTE: requires SUB USERFUNC

    BEPS = .0000000001#

    E = 0#
    A = XL
    B = XU

    CALL USERFUNC(A, FA)
    CALL USERFUNC(B, FB)

    FC = FB

    NITER = 0

    FOR ITER = 1 TO MAXITER
        NITER = NITER + 1
        IF (FB * FC > 0#) THEN
           C = A
           FC = FA
           D = B - A
           E = D
        END IF

        IF (ABS(FC) < ABS(FB)) THEN
           A = B
           B = C
           C = A
           FA = FB
           FB = FC
           FC = FA
        END IF
       
        TOL1 = 2# * BEPS * ABS(B) + .5# * TOL
       
        XM = .5# * (C - B)

        IF (ABS(XM) <= TOL1 OR FB = 0#) THEN EXIT FOR

        IF (ABS(E) >= TOL1 AND ABS(FA) > ABS(FB)) THEN
           S = FB / FA
           IF (A = C) THEN
              P = 2# * XM * S
              Q = 1# - S
           ELSE
              Q = FA / FC
              R = FB / FC
              P = S * (2# * XM * Q * (Q - R) - (B - A) * (R - 1#))
              Q = (Q - 1#) * (R - 1#) * (S - 1#)
           END IF

           IF (P > 0#) THEN Q = -Q

           P = ABS(P)
           XMIN = ABS(E * Q)
           TMP = 3# * XM * Q - ABS(TOL1 * Q)

           IF (XMIN < TMP) THEN XMIN = TMP

           IF (2# * P < XMIN) THEN
              E = D
              D = P / Q
           ELSE
              D = XM
              E = D
           END IF
        ELSE
           D = XM
           E = D
        END IF

        A = B
        FA = FB

        IF (ABS(D) > TOL1) THEN
           B = B + D
        ELSE
           B = B + SGN(XM) * TOL1
        END IF

        CALL USERFUNC(B, FB)
    NEXT ITER

    FROOT = FB
    XROOT = B

END SUB

