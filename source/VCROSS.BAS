' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB VCROSS (A(), B(), C()) STATIC

    ' Vector cross product subroutine

    ' { C } = { A } X { B }

    ' Input

    '  A() = vector A ( 3 rows by 1 column )
    '  B() = vector B ( 3 rows by 1 column )

    ' Output

    '  C() = { A } * { B } ( 3 rows by 1 column )

    C(1) = A(2) * B(3) - A(3) * B(2)
    C(2) = A(3) * B(1) - A(1) * B(3)
    C(3) = A(1) * B(2) - A(2) * B(1)

END SUB

