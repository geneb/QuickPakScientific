' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB CHEBCOEF (A, B, C(), N) STATIC

    ' Chebyshev function coefficients subroutine
   
    ' Input

    '  A  = lower limit of evaluation interval
    '  B  = upper limit of evaluation interval
    '  N  = degree of Chebyshev approximation

    ' Output

    '  C() = array of Chebyshev coefficients
    '        ( N rows by 1 column )

    ' Note: requires subroutine USERFUNC

    DIM F(N)
   
    PI = 3.141592653589793#
   
    BMA = .5# * (B - A)
    BPA = .5# * (B + A)

    FOR K = 1 TO N
        X = COS(PI * (K - .5#) / N) * BMA + BPA
        CALL USERFUNC(X, FX)
        F(K) = FX
    NEXT K
    
    FAC = 2# / N

    FOR J = 1 TO N
        SUM = 0#
        FOR K = 1 TO N
            SUM = SUM + F(K) * COS((PI * (J - 1)) * ((K - .5#) / N))
        NEXT K
        C(J) = FAC * SUM
    NEXT J

    ERASE F

END SUB

