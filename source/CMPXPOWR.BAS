' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CMPXPOWR (N, A(), B()) STATIC

    ' Complex number raised to a power subroutine

    ' Input

    '  N   = power ( ABS(N) >= 1 )
    '  A() = complex number, A(1) + A(2) i
    
    ' Output

    '  B() = complex number, B(1) + B(2) i
    '      = A() ^ N

    ' NOTE: the first element of each vector is the real part and the
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

    U1 = U ^ N
    V1 = N * V
    V1 = V1 - PI2 * INT(V1 / PI2)

    ' polar to rectangular conversion

    B(1) = U1 * COS(V1)
    B(2) = U1 * SIN(V1)

END SUB

