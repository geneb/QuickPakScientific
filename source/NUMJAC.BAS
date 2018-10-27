' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H,O-Z

DECLARE SUB USERFUNC(X(),F())

SUB NUMJAC (N, X(), XJAC()) STATIC

    ' Numerical Jacobian subroutine

    ' Forward difference method

    ' Input

    '  N   = number of variables
    '  X() = current evaluation vector

    ' Output

    '  XJAC() = numerical Jacobian at X()

    ' Note: requires SUB USERFUNC

    DIM FVEC1(N), FVEC2(N)

    ' machine epsilon

    EPSM = 2.22D-16

    ' working epsilon

    EPS = SQR(EPSM)

    ' evaluate function values at X()

    CALL USERFUNC(X(), FVEC1())

    ' compute components of Jacobian

    FOR J = 1 TO N
        TEMP = X(J)
        H = EPS * ABS(TEMP)
        IF (H = 0#) THEN H = EPS
        X(J) = TEMP + H
        CALL USERFUNC(X(), FVEC2())
        X(J) = TEMP
        FOR I = 1 TO N
            XJAC(I, J) = (FVEC2(I) - FVEC1(I)) / H
        NEXT I
    NEXT J

    ' erase working arrays

    ERASE FVEC1, FVEC2

END SUB


