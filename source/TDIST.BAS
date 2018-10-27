' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB TDIST (IFLAG, N, X, PROB) STATIC

    ' T-distribution subroutine

    ' Input

    '  IFLAG = type of computation
    '      1 = given probability, compute x
    '      2 = given x, compute probability
    '  N     = degree of freedom; N >= 1

    ' Output (or input)

    '  X     = x value
    '  PROB  = probability; 0 < PROB < 1

    ' NOTE: requires subroutines NORMAL.BAS and FDIST.BAS

    SELECT CASE IFLAG
    CASE 1
       ' T(x)

       QW4 = X
       X = X * X
       CALL FDIST(1, 1, N, X, PROB)
       PROB = PROB / 2#
       X = QW4
       IF (X < 0#) THEN PROB = 1# - PROB
    CASE 2
       ' T(p)

       CALL NORMAL(2, X, PROB)
   
       QW2 = X * X
       QW1 = X + (1# + QW2) * X / (4# * N) + ((5# * QW2 + 16#) * QW2 + 3#) * X / (96# * N * N)
       QW1 = QW1 + (((3# * QW2 + 19#) * QW2 + 17#) * QW2 - 15#) * X / (384# * N ^ 3#)
       QW1 = QW1 + ((((79# * QW2 + 776#) * QW2 + 1482#) * QW2 - 1920#) * QW2 - 945#) * X / (92160# * N ^ 4#)
       X = QW1 + (((((27# * QW2 + 339#) * QW2 + 930#) * QW2 - 1782#) * QW2 - 765#) * QW2 + 17955#) * X / (368640# * N ^ 5#)
    END SELECT

END SUB

