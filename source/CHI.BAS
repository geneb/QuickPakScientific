' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB CHI (IFLAG, N, X, PROB) STATIC
    
    ' Chi-squared distribution subroutine
   
    ' Input

    '  IFLAG = type of computation
    '      1 = given x, compute probability
    '      2 = given probability, compute x
    '  N     = degrees of freedom; N >= 1
   
    ' Output (or input)

    '  X     = x value; X >=0
    '  PROB  = probability; 0 < PROB <= 1

    ' NOTE: requires subroutine NORMAL.BAS

    SELECT CASE IFLAG
    CASE 1
       ' Chi(x)

       X2 = X

       IF (X = 0#) THEN
          PROB = 1#
          EXIT SUB
       END IF

       IF (N > 40) THEN
          X3 = 2# / (9# * N)
          X = ((X / N) ^ (1# / 3#) - 1# + X3) / SQR(X3)
          CALL NORMAL(1, X, PROB)
          X = X2
          EXIT SUB
       END IF

       INDEX = 2 - INT(N) + 2 * INT((INT(N / 2)))

       X3 = SQR(X)
    
       IF (INDEX <> 1) THEN
          PROB = EXP(-X / 2#)
          QF = PROB / 2#
       ELSE
          X = X3
          CALL NORMAL(1, X, PROB)
          PROB = 2# * PROB
          QF = .3989422804# * EXP(-X2 / 2#) / X3
       END IF

       IF (INDEX > (N - 1)) THEN
          ' NULL
       ELSE
          FOR I = INDEX TO (N - 1) STEP 2
              QF = QF * X2 / I
              PROB = PROB + 2# * QF
          NEXT I
       END IF
       X = X2
    CASE 2
       ' Chi(p)
   
       IF (N = 1) THEN
          X2 = PROB
          PROB = PROB / 2#
          CALL NORMAL(2, X, PROB)
          X = X * X
          PROB = X2
          EXIT SUB
       ELSEIF (N = 2) THEN
          X = -2# * LOG(PROB)
          EXIT SUB
       END IF

       QX1 = 0#
       QX2 = 1#
       QX3 = .5#
       QP = PROB

       DO
          X = 1# / QX3 - 1#
    
          CALL CHI(1, N, X, PROB)

          IF (PROB <= QP) THEN
             QX1 = QX3
          ELSE
             QX2 = QX3
          END IF

          QX0 = QX3
          QX3 = (QX1 + QX2) / 2#
          IF ABS(QX3 - QX0) <= (.00001# * QX3) THEN EXIT DO
       LOOP

       X = 1# / QX3 - 1#
       PROB = QP
    END SELECT

END SUB

