' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CMPXADD (A(), B(), C()) STATIC

    ' Complex number addition subroutine

    ' Input

    '  A() = complex number, A(1) + A(2) i
    '  B() = complex number, B(1) + B(2) i

    ' Output

    '  C() = complex number, C(1) + C(2) i
    '      = A() + B()

    ' NOTE: the first element of each vector is the real part and the
    ' second element is the imaginary part of each complex number.

    C(1) = A(1) + B(1)
    C(2) = A(2) + B(2)

END SUB

