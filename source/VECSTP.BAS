' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB VECSTP (A(), B(), C(), VTSP) STATIC

    ' Vector triple scalar product subroutine

    ' { A } dot ( { B } cross {C} )

    ' Input

    '  A() = vector A ( 3 rows by 1 column )
    '  B() = vector B ( 3 rows by 1 column )
    '  C() = vector B ( 3 rows by 1 column )

    ' Output

    '  VTSP = triple scalar product

    DIM D(3)

    ' compute {B} cross {C}

    D(1) = B(2) * C(3) - B(3) * C(2)
    D(2) = B(3) * C(1) - B(1) * C(3)
    D(3) = B(1) * C(2) - B(2) * C(1)

    ' compute {A} dot [ {B} cross {C} ]

    VTSP = 0#

    FOR I = 1 TO 3
        VTSP = VTSP + A(I) * D(I)
    NEXT I

    ERASE D

END SUB

