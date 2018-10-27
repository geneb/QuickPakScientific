' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB VECSUB (A(), B(), C(), N) STATIC

    ' Vector subtraction subroutine

    ' { C } = { A } - { B }

    ' Input

    '  A() = vector A ( N rows )
    '  B() = vector B ( N rows )

    ' Output
        
    '  C() = vector C ( N rows )

    FOR I = 1 TO N
        C(I) = A(I) - B(I)
    NEXT I

END SUB

