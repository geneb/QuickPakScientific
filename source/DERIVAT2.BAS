' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB DERIVATIVE (T, X(), XDOT(), XDDOT()) STATIC

    ' Equations of motion subroutine

    ' x"(x) = - mu * x / x^3

    ' Input

    '  T      = simulation time
    '  X()    = position vector ( 3 rows )
    '  XDOT() = velocity vector ( 3 rows )

    ' Output

    '  XDDOT() = gravity vector ( 3 rows )
    
    ' Earth gravitational constant ( ft^3/sec^2 )

    XMU = 1.407645794D+16

    R = SQR(X(1) ^ 2 + X(2) ^ 2 + X(3) ^ 2)
    R3 = R ^ 3

    XDDOT(1) = -XMU * X(1) / R3
    XDDOT(2) = -XMU * X(2) / R3
    XDDOT(3) = -XMU * X(3) / R3

END SUB

