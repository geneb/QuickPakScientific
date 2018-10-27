' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB VECMAG (A(), A) STATIC

    ' Vector magnitude subroutine

    ' Input

    '  A() = column vector ( 3 rows by 1 column )

    ' Output

    '  A = magnitude of vector { A }

    A = SQR(A(1) * A(1) + A(2) * A(2) + A(3) * A(3))

END SUB

