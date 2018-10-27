' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB VECVTP (A(), B(), C(), D()) STATIC

    ' Vector triple product subroutine

    ' { D } = { A } cross ( { B } cross {C} )

    ' Input

    '  A() = vector A ( 3 rows by 1 column )
    '  B() = vector B ( 3 rows by 1 column )
    '  C() = vector B ( 3 rows by 1 column )

    ' Output

    '  D() = vector triple product ( 3 rows by 1 column )

    DIM E(3)

    ' compute { B } cross { C }

    E(1) = B(2) * C(3) - B(3) * C(2)
    E(2) = B(3) * C(1) - B(1) * C(3)
    E(3) = B(1) * C(2) - B(2) * C(1)

    ' compute { A } cross ( { B } cross { C } )

    D(1) = A(2) * E(3) - A(3) * E(2)
    D(2) = A(3) * E(1) - A(1) * E(3)
    D(3) = A(1) * E(2) - A(2) * E(1)

    ERASE E

END SUB

