' Copyright (c) 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION GBESSEL (D)
DECLARE FUNCTION UBESSEL (D)
DECLARE FUNCTION TBESSEL (D)
DECLARE FUNCTION BBESSEL (D)
DECLARE FUNCTION ABESSEL (D)
DECLARE FUNCTION FBESSEL (D)
DECLARE FUNCTION BESSEL (N, X)

FUNCTION ABESSEL (D) STATIC

    ' a(d) function

    A1 = -2.2499997#
    A2 = 1.2656208#
    A3 = -.3163866#
    A4 = .0444479#
    A5 = -.0039444#
    A6 = .00021#

    ABESSEL = (((((A6 * D + A5) * D + A4) * D + A3) * D + A2) * D + A1) * D

END FUNCTION

FUNCTION BBESSEL (D) STATIC

    ' b(d) function

    B1 = -.56249985#
    B2 = .21093573#
    B3 = -.03954289#
    B4 = .00443319#
    B5 = -.00031761#
    B6 = .00001109#

    BBESSEL = (((((B6 * D + B5) * D + B4) * D + B3) * D + B2) * D + B1) * D

END FUNCTION

FUNCTION BESSEL (N, X)

    ' Recursive Bessel function

    ' Bessel functions of integer order, Jn(X), X >= -3

    ' Input

    '  N = integer order
    '  X = function argument ( X >= -3 )

    ' Output

    '  BESSEL = Bessel function of integer order, Jn(x)

    ' NOTE: requires FUNCTIONS ABESSEL, BBESSL,
    '       FBESSEL, GBESSEL, TBESSEL and UBESSEL

    IF ((N = 0) AND (X > -3# AND X <= 3#)) THEN
       D = X * X / 9#
       BESSEL = 1# + ABESSEL(D)
    ELSEIF ((N = 0) AND (X > 3#)) THEN
       D = 3# / X
       F0 = FBESSEL(D)
       T0 = X + TBESSEL(D)
       BESSEL = F0 * COS(T0) / SQR(X)
    ELSEIF ((N = 1) AND (X >= -3# AND X <= 3#)) THEN
       D = X * X / 9#
       BESSEL = (.5# + BBESSEL(D)) * X
    ELSEIF ((N = 1) AND (X > 3)) THEN
       D = 3# / X
       F1 = GBESSEL(D)
       T1 = X + UBESSEL(D)
       BESSEL = F1 * COS(T1) / SQR(X)
    ELSEIF ((N >= 2) AND (X > -3# AND X <> 0#)) THEN
       BESSEL = 2# * (N - 1#) * BESSEL(N - 1, X) / X - BESSEL(N - 2, X)
    END IF

END FUNCTION

FUNCTION FBESSEL (D) STATIC

    ' f(d) function

    F0 = .79788456#
    F1 = -.00000077#
    F2 = -.0055274#
    F3 = -.009512#
    F4 = .00137237#
    F5 = -7.280500000000001D-04
    F6 = .00014476#

    FBESSEL = ((((((F6 * D + F5) * D + F4) * D + F3) * D + F2) * D + F1) * D + F0)

END FUNCTION

FUNCTION GBESSEL (D) STATIC

    ' g(d) function

    G0 = .79788456#
    G1 = .00000156#
    G2 = .01659667#
    G3 = .00017105#
    G4 = -.00249511#
    G5 = .00113653#
    G6 = -.0002033#

    GBESSEL = ((((((G6 * D + G5) * D + G4) * D + G3) * D + G2) * D + G1) * D + G0)

END FUNCTION

FUNCTION TBESSEL (D) STATIC

    ' t(d) function

    T0 = -.78539816#
    T1 = -.04166397#
    T2 = -.00003954#
    T3 = .00262573#
    T4 = -.00054125#
    T5 = -.00029333#
    T6 = .00013558#

    TBESSEL = ((((((T6 * D + T5) * D + T4) * D + T3) * D + T2) * D + T1) * D + T0)

END FUNCTION

FUNCTION UBESSEL (D) STATIC

    ' u(d) function

    U0 = -2.35619449#
    U1 = .12499612#
    U2 = .0000565#
    U3 = -.00637879#
    U4 = .00074348#
    U5 = .00079824#
    U6 = -.00029166#

    UBESSEL = ((((((U6 * D + U5) * D + U4) * D + U3) * D + U2) * D + U1) * D + U0)

END FUNCTION

