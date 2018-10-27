' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB SCALE (XMIN, XMAX, N, XMINP, XMAXP) STATIC

    ' Scale graphics axis subroutine

    ' Input

    '  XMIN = minimum unscaled axis value
    '  XMAX = maximum unscaled axis value
    '  N    = number of axis subdivisions (N >= 1)

    ' Output

    '  XMINP = minimum scaled axis value
    '  XMAXP = maximum scaled axis value

    DIM VINT(5)

    VINT(1) = 1#
    VINT(2) = 2#
    VINT(3) = 5#
    VINT(4) = 10#
    VINT(5) = 20#

    DEL = .00002#
    VN = N
    A = (XMAX - XMIN) / VN
    AL = LOG(A) / LOG(10#)

    NAL = FIX(AL)

    IF (A < 1#) THEN NAL = NAL - 1

    B = A / (10# ^ NAL)
    
    FOR I = 1 TO 3
        IF (B < (VINT(I) + DEL)) THEN EXIT FOR
    NEXT I

    DO
       DIST = VINT(I) * (10# ^ NAL)
      
       FM1 = XMIN / DIST
       M1 = FIX(FM1)
       IF (FM1 < 0#) THEN M1 = M1 - 1
       IF (ABS(M1 + 1# - FM1) < DEL) THEN M1 = M1 + 1
       XMINP = DIST * M1
      
       FM2 = XMAX / DIST
       M2 = FIX(FM2 + 1#)
       IF (FM2 < -1#) THEN M2 = M2 - 1
       IF (ABS(FM2 + 1# - M2)) < DEL THEN M2 = M2 - 1
       XMAXP = DIST * M2
      
       NP = M2 - M1
       IF (NP <= N) THEN EXIT DO
       I = I + 1
    LOOP

    NX = (N - NP) / 2

    XMINP = XMINP - NX * DIST
    XMAXP = XMINP + N * DIST
   
    IF (XMINP > XMIN) THEN XMINP = XMIN
    IF (XMAXP < XMAX) THEN XMAXP = XMAX

    ERASE VINT

END SUB

