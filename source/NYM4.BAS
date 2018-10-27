' Copyright (c) 1988, 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, X(), XDOT(), XDDOT())

SUB NYM4 (N, TP, DT, X1(), XDOT1()) STATIC

    ' Fourth order Nystrom integration subroutine

    ' Input

    '  N       = number of equations
    '  TP      = simulation time
    '  DT      = integration step size
    '  X1()    = X-vector at initial time ( N rows )
    '  XDOT1() = XDOT-vector at initial time ( N rows )

    ' Output

    '  X1()    = X-vector at time = TP + DT ( N rows )
    '  XDOT1() = XDOT-vector at time = TP + DT ( N rows )
   
    ' NOTE: requires SUB DERIVATIVE

    STATIC A5, A6, A7, A8, A9, B5, B6, B7, B8, B9
    STATIC C5, C6, C7, C8, C9, D5, D6, ICOEFF

    IF (ICOEFF = 0) THEN
       ' Define Nystrom integration coefficients
      
       A5 = .045#
       A6 = .3#
       A7 = 13# / 126#
       A8 = 5# / 18#
       A9 = 5# / 42#
       B5 = 7# / 600#
       B6 = 7# / 30#
       B7 = 7# / 15#
       B8 = 7# / 6#
       B9 = 25# / 63#
       C5 = .7#
       C6 = 19# / 78#
       C7 = 35# / 312#
       C8 = 15# / 104#
       C9 = 64# / 39#
       D5 = 70# / 39#
       D6 = 15# / 13#
      
       ICOEFF = 1
    END IF

    DIM A(N), B(N), C(N), D(N), X(N), XDOT(N), XDDOT(N)

    TIME = TP

    FOR I = 1 TO N
        X(I) = X1(I)
        XDOT(I) = XDOT1(I)
    NEXT I

    CALL DERIVATIVE(TIME, X(), XDOT(), XDDOT())

    TIME = TP + A6 * DT

    FOR I = 1 TO N
        A(I) = DT * XDDOT(I)
        X(I) = X1(I) + DT * (A6 * XDOT1(I) + A5 * A(I))
        XDOT(I) = XDOT1(I) + A6 * A(I)
    NEXT I

    CALL DERIVATIVE(TIME, X(), XDOT(), XDDOT())

    TIME = TP + C5 * DT

    FOR I = 1 TO N
        B(I) = DT * XDDOT(I)
        X(I) = X1(I) + DT * (C5 * XDOT1(I) + B5 * A(I) + B6 * B(I))
        XDOT(I) = XDOT1(I) - B7 * A(I) + B8 * B(I)
    NEXT I

    CALL DERIVATIVE(TIME, X(), XDOT(), XDDOT())

    TIME = TP + DT

    FOR I = 1 TO N
        C(I) = DT * XDDOT(I)
        X(I) = X1(I) + DT * (XDOT1(I) + C6 * A(I) + C7 * B(I) + C8 * C(I))
        XDOT(I) = XDOT1(I) + C9 * A(I) - D5 * B(I) + D6 * C(I)
    NEXT I

    CALL DERIVATIVE(TIME, X(), XDOT(), XDDOT())

    FOR I = 1 TO N
        D(I) = DT * XDDOT(I)
        X1(I) = X1(I) + DT * (XDOT1(I) + A7 * A(I) + A8 * B(I) + A9 * C(I))
        XDOT1(I) = XDOT1(I) + A7 * (A(I) + D(I)) + B9 * (B(I) + C(I))
    NEXT I

    ERASE A, B, C, D, X, XDOT, XDDOT

END SUB

