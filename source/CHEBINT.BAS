' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CHEBINT (A, B, C(), N, CINQ()) STATIC

    ' Chebyshev integral coefficients subroutine
   
    ' Input

    '  A   = lower limit of evaluation interval
    '  B   = upper limit of evaluation interval
    '  C() = array of Chebyshev function coefficients
    '        ( N rows by 1 column )
    '  N   = size of C() array

    ' Output

    '  CINQ() = array of Chebyshev integral coefficients
    '           ( N rows by 1 column )

    CON = .25# * (B - A)
    SUM = 0#
    FAC = 1#

    FOR J = 2 TO N - 1
        CINQ(J) = CON * (C(J - 1) - C(J + 1)) / (J - 1)
        SUM = SUM + FAC * CINQ(J)
        FAC = -FAC
    NEXT J

    CINQ(N) = CON * C(N - 1) / (N - 1)
   
    SUM = SUM + FAC * CINQ(N)
   
    CINQ(1) = 2# * SUM

END SUB

