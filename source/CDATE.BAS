' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CDATE (XJDATE, XMONTH, DAY, YEAR) STATIC

    ' Calendar date subroutine

    ' Input

    '  XJDATE = Julian date

    ' Output

    '  XMONTH = calendar month ( 1 <= XMONTH <= 12 )
    '  DAY    = calendar day ( 1 <= DAY <= 31 )
    '  YEAR   = calendar year

    Z = FIX(XJDATE + .5#)
    F = XJDATE + .5# - Z

    IF (Z < 2299161#) THEN
       A = Z
    ELSE
       ALPHA = FIX((Z - 1867216.25#) / 36524.25#)
       A = Z + 1# + ALPHA - FIX(ALPHA / 4#)
    END IF

    B = A + 1524#
    C = FIX((B - 122.1#) / 365.25#)
    D = FIX(365.25# * C)
    E = FIX((B - D) / 30.6001#)

    DAY = B - D - FIX(30.6001# * E) + F

    IF (E < 13.5#) THEN
       XMONTH = E - 1#
    ELSE
       XMONTH = E - 13#
    END IF

    IF (XMONTH > 2.5#) THEN
       YEAR = C - 4716#
    ELSE
       YEAR = C - 4715#
    END IF

END SUB

