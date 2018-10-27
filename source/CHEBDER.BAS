' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CHEBDER (A, B, C(), N, CDER()) STATIC

    ' Chebyshev derivative coefficients subroutine
   
    ' Input

    '  A   = lower limit of evaluation interval
    '  B   = upper limit of evaluation interval
    '  C() = array of Chebyshev function coefficients
    '        ( N rows by 1 column )
    '  N   = size of C() array

    ' Output

    '  CDER() = array of derivative coefficients
    '           ( N rows by 1 column )

    CDER(N) = 0#
    CDER(N - 1) = 2 * (N - 1) * C(N)

    IF (N >= 3) THEN
       FOR J = N - 2 TO 1 STEP -1
           CDER(J) = CDER(J + 2) + 2 * J * C(J + 1)
       NEXT J
    END IF
   
    CON = 2# / (B - A)
   
    FOR J = 1 TO N
        CDER(J) = CDER(J) * CON
    NEXT J

END SUB

