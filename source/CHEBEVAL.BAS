' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CHEBEVAL (A, B, C(), M, X, VCHEB) STATIC

    ' Evaluate Chebyshev coefficients subroutine

    ' Input

    '  A   = lower limit of evaluation interval
    '  B   = upper limit of evaluation interval
    '  C() = array of Chebshev coefficients
    '        ( M rows by 1 column )
    '  M   = size of C() array
    '  X   = argument

    ' Output

    '  VCHEB = function value at X

    D = 0#
    DD = 0#
   
    Y = (2# * X - A - B) / (B - A)
    Y2 = 2# * Y
   
    FOR J = M TO 2 STEP -1
        SV = D
        D = Y2 * D - DD + C(J)
        DD = SV
    NEXT J

    VCHEB = Y * D - DD + .5# * C(1)

END SUB

