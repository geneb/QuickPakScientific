' Copyright 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB SFUNCTION (NDEG, COEF(), X, Y, Z) STATIC

    ' Surface function subroutine

    ' z = f(x,y)

    ' Input

    '  NDEG   = degree of surface fit
    '  COEF() = array of surface fit coefficents
    '           ( up to 11 rows by 11 columns )
    '  X      = x data value
    '  Y      = y data value

    ' Output

    '  Z = z data value of surface at x, y

    NDEG1 = NDEG + 1
    NDEG2 = NDEG + 2

    DIM WORK(NDEG1 * NDEG2 / 2)

    IF (NDEG = 0) THEN
       Z = COEF(1, 1)
       ERASE WORK
       EXIT SUB
    END IF

    A = 1#
    Y1 = 0#
    WORK(1) = 1#

    FOR I = 2 TO NDEG1
        A = A * X
        WORK(I) = A
        Y1 = Y1 + A * COEF(1, I)
    NEXT I

    FOR I = 2 TO NDEG1
        FOR J = 1 TO NDEG2 - I
            WORK(J) = WORK(J) * Y
            Y1 = Y1 + WORK(J) * COEF(I, J)
        NEXT J
    NEXT I

    Z = Y1 + COEF(1, 1)

    ERASE WORK

END SUB

