' Copyright (c) 1988, 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FVAL)
DECLARE SUB TRAPEZOID (A, B, N, S)

SUB ROMBERG (A, B, MAXITER, EPS, NITER, S) STATIC

    ' Romberg integration subroutine

    ' Input

    '  A       = lower integration limit
    '  B       = upper integration limit
    '  MAXITER = maximum number of iterations
    '  EPS     = convergence criteria

    ' Output

    '  NITER = number of iterations
    '  S     = integral of f(x)

    ' NOTE: requires SUB TRAPEZOID

    DIM S(MAXITER + 1, MAXITER + 1)

    N = 1
    EPSA = 1.1# * EPS

    CALL TRAPEZOID(A, B, N, S(1, 1))

    I = 0

    WHILE (EPSA > EPS AND I < MAXITER)
      I = I + 1
      NITER = I
      N = 2 ^ I

      CALL TRAPEZOID(A, B, N, S(I + 1, 1))

      FOR K = 2 TO I + 1
          J = 2 + I - K
          TMP = 4# ^ (K - 1)
          S(J, K) = (TMP * S(J + 1, K - 1) - S(J, K - 1)) / (TMP - 1#)
          S = S(J, K)
      NEXT K

      EPSA = ABS((S(1, I + 1) - S(1, I)) / S(1, I + 1))

   WEND

   ERASE S

END SUB

SUB TRAPEZOID (A, B, N, S) STATIC

    ' Multiple-application trapezoidal integration subroutine

    ' Input

    '  A = lower integration limit
    '  B = upper integration limit
    '  N = number of intervals

    ' Output

    '  S = integral

    ' NOTE: requires SUB USERFUNC

    H = (B - A) / N

    CALL USERFUNC(A, FA)
    CALL USERFUNC(B, FB)

    SUM = 0#
    X = A

    FOR I = 1 TO N - 1
        X = X + H
        CALL USERFUNC(X, FX)
        SUM = SUM + FX
    NEXT I

    S = (B - A) * (FA + 2# * SUM + FB) / (2# * N)

END SUB

