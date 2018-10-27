' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CMPXSQRT (A(), B()) STATIC

    ' Complex number square root subroutine

    ' Input

    '  A() = complex number, A(1) + A(2) i

    ' Output

    '  B() = complex number, B(1) + B(2) i
    '      = SQRT(A())

    ' NOTE: the first element of each vector is the real part and the
    ' second element is the imaginary part of each complex number.

    ASQRT = SQR(A(1) * A(1) + A(2) * A(2))

    B(1) = SQR((ASQRT + A(1)) / 2#)
    B(2) = SQR((ASQRT - A(1)) / 2#)

    IF (A(2) < 0#) THEN B(2) = -B(2)

END SUB

