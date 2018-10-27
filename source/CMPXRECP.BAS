' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CMPXRECP (A(), B()) STATIC

    ' Complex number reciprocal subroutine

    ' Input

    '  A() = complex number, A(1) + A(2) i

    ' Output

    '  B() = complex number, B(1) + B(2) i
    '      = 1 / A()

    ' NOTE: the first element of each vector is the real part and the
    ' second element is the imaginary part of each complex number.

    ASQR = A(1) * A(1) + A(2) * A(2)

    B(1) = A(1) / ASQR
    B(2) = -A(2) / ASQR

END SUB

