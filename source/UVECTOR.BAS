' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB UVECTOR (A(), B()) STATIC

    ' Unit vector subroutine

    ' Input

    '  A() = column vector ( 3 rows by 1 column )

    ' Output

    '  B() = unit vector ( 3 rows by 1 column )

    AMAG = SQR(A(1) * A(1) + A(2) * A(2) + A(3) * A(3))

    FOR I = 1 TO 3
        IF (AMAG <> 0#) THEN
           B(I) = A(I) / AMAG
        ELSE
           B(I) = 0#
        END IF
    NEXT I

END SUB

