' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CMPXROOT (N, A(), B()) STATIC

    ' Nth root of a complex number subroutine

    ' Input

    '  A() = complex number, A(1) + A(2) i
    '  N   = root ( N >= 1 )

    ' Output

    '  B() = complex number ( 2 columns by N rows )
    '      = A() ^ (1/N)
   
    ' NOTE1: B(1, M) = Mth element of real part
    '        B(2, M) = Mth element of imaginary part
    '                  where M = 1, 2, ... N

    ' NOTE2: the first element of each vector is the real part and the
    ' second element is the imaginary part of each complex number.

    PI = 3.141592653589793#
    PI2 = 2# * PI
    PIDIV2 = .5# * PI

    ' rectangular to polar conversion

    X = A(1)
    Y = A(2)

    U = SQR(X * X + Y * Y)
 
    ' calculate polar angle

    DO
       IF (ABS(Y) < .00000001#) THEN
          V = (1# - SGN(X)) * PIDIV2
          EXIT DO
       ELSE
          C = (2# - SGN(Y)) * PIDIV2
       END IF

       IF (ABS(X) < .00000001#) THEN
          V = C
          EXIT DO
       ELSE
          V = C + SGN(Y) * SGN(X) * (ABS(ATN(Y / X)) - PIDIV2)
          EXIT DO
       END IF
    LOOP
       
    ' polar power calculation

    U1 = U ^ (1 / N)
    V1 = V / N

    FOR M = 1 TO N
        ' Mth element of polar angle
        V = V1 + PI2 * (M - 1) / N
        ' polar to rectangular conversion
        B(1, M) = U1 * COS(V)
        B(2, M) = U1 * SIN(V)
    NEXT M

END SUB

