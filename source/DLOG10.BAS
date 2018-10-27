' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE FUNCTION DLOG10(X)

FUNCTION DLOG10 (X) STATIC

    ' Common logarithm function

    ' Input

    '  X = argument

    ' Output

    '  DLOG10 = common log of X

    IF (X = 0#) THEN
       DLOG10 = -99#
    ELSE
       DLOG10 = .4342944819032518# * LOG(X)
    END IF

END FUNCTION
