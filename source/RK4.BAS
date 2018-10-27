' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, Y(), YDOT())

SUB RK4 (NEQ, TIME, DT, Y()) STATIC

    ' Fourth order Runge-Kutta integration subroutine

    ' Input

    '  NEQ  = number of equations
    '  TIME = simulation time
    '  DT   = integration step size
    '  Y()  = integration vector at initial time ( NEQ rows )

    ' Output

    '  TIME = simulation time = TIME + DT
    '  Y()  = integration vector at time = TIME + DT ( NEQ rows )
   
    ' NOTE: requires SUB DERIVATIVE

    DIM YT(NEQ), YDOT1(NEQ), YDOT2(NEQ), YDOT3(NEQ), YDOT4(NEQ)

    TSAVED = TIME

    CALL DERIVATIVE(TIME, Y(), YDOT1())

    FOR I = 1 TO NEQ
        YT(I) = Y(I) + .5# * DT * YDOT1(I)
    NEXT I

    TIME = TSAVED + .5# * DT

    CALL DERIVATIVE(TIME, YT(), YDOT2())

    FOR I = 1 TO NEQ
        YT(I) = Y(I) + .5# * DT * YDOT2(I)
    NEXT I

    TIME = TSAVED + .5# * DT

    CALL DERIVATIVE(TIME, YT(), YDOT3())

    FOR I = 1 TO NEQ
        YT(I) = Y(I) + DT * YDOT3(I)
    NEXT I

    TIME = TSAVED + DT

    CALL DERIVATIVE(TIME, YT(), YDOT4())

    FOR I = 1 TO NEQ
        Y(I) = Y(I) + DT * (YDOT1(I) + 2# * YDOT2(I) + 2# * YDOT3(I) + YDOT4(I)) / 6#
    NEXT I

    ERASE YT, YDOT1, YDOT2, YDOT3, YDOT4

END SUB

