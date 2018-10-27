' Copyright (c) 1988, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION ATAN3 (A, B)

FUNCTION ATAN3 (A, B) STATIC

    ' Four quadrant inverse tangent function

    ' Input

    '  A = sine of angle
    '  B = cosine of angle

    ' Output

    '  C = angle ( 0 =< C <= 2 * pi : radians )

    PIDIV2 = 1.570796326794897#

    IF (ABS(A) < .00000001#) THEN
       ATAN3 = (1# - SGN(B)) * PIDIV2
       EXIT FUNCTION
    ELSE
       C = (2# - SGN(A)) * PIDIV2
    END IF

    IF (ABS(B) < .00000001#) THEN
       ATAN3 = C
       EXIT FUNCTION
    ELSE
       ATAN3 = C + SGN(A) * SGN(B) * (ABS(ATN(A / B)) - PIDIV2)
    END IF

END FUNCTION

