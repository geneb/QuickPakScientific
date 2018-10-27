' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB JULIAN (XMONTH, DAY, YEAR, XJD, VDATE$) STATIC

    ' Julian Date subroutine

    ' Input

    '  XMONTH = calendar month ( 1 <= XMONTH <= 12 )
    '  DAY    = calendar day ( 1 <= DAY <= 31 )
    '  YEAR   = calendar year

    ' Output

    '  XJD    = Julian Date
    '  VDATE$ = valid date flag ( Y = yes, N = no )

    Y = YEAR
    XM = XMONTH
    B = 0#
    C = 0#

    IF (XMONTH <= 2#) THEN
       Y = YEAR - 1#
       XM = XMONTH + 12#
    END IF

    IF (Y < 0#) THEN C = -.75#

    IF (YEAR < 1582#) THEN
       VDATE$ = "Y"
    ELSEIF (YEAR > 1582#) THEN
       A = FIX(Y / 100#)
       B = 2# - A + FIX(A / 4#)
       VDATE$ = "Y"
    ELSEIF (XMONTH < 10#) THEN
       VDATE$ = "Y"
    ELSEIF (XMONTH > 10#) THEN
       A = FIX(Y / 100#)
       B = 2# - A + FIX(A / 4#)
       VDATE$ = "Y"
    ELSEIF (DAY <= 4#) THEN
       VDATE$ = "Y"
    ELSEIF (DAY > 14#) THEN
       A = FIX(Y / 100#)
       B = 2# - A + FIX(A / 4#)
       VDATE$ = "Y"
    ELSE
       VDATE$ = "N"
    END IF

    XJD = FIX(365.25# * Y + C) + FIX(30.6001# * (XM + 1#)) + DAY + B + 1720994.5#

END SUB

