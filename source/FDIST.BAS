' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB FDIST (IFLAG, N1, N2, X, PROB) STATIC

    ' F-distribution subroutine
   
    ' NOTE: requires subroutine NORMAL.BAS
   
    ' Input

    '  IFLAG = type of computation
    '      1 = given x, compute probability
    '      2 = given probability, compute x
    '  N1    = degrees of freedom; N1 >= 1
    '  N2    = degrees of freedom; N2 >= 1

    ' Output (or input)

    '  X     = x value; X > 0
    '  PROB  = probability; 0 < PROB <= 1
   
    ' NOTE: requires subroutine NORMAL.BAS

    PI = 3.14159265#

    SELECT CASE IFLAG
    CASE 1
       ' F(x)
    
       QW1 = N2 / (N2 + N1 * X)
       QW2 = SQR(1# - QW1)
       QMD = 2# * INT(N1 / 2) - N1 + 2
       QND = 2# * INT(N2 / 2) - N2 + 2
    
       IQMD = QMD * 2 - QND

       IF (IQMD = 1) THEN
          PROB = 1# - (2 / PI) * ATN(QW2 / SQR(QW1))
          QW3 = QW2 * SQR(QW1) / PI
       ELSEIF (IQMD = 2) THEN
          PROB = QW1
          QW3 = (1# - QW1) * PROB
       ELSEIF (IQMD = 3) THEN
          PROB = SQR(QW1)
          QW3 = (1# - QW1) * PROB / 2#
       ELSE
          PROB = 1# - QW2
          QW3 = QW1 * QW2 / 2#
       END IF

       FOR QIF = QND TO N2 STEP 2
           QJFU = QIF
           IF (N2 <= QIF) OR (ABS(2# / QIF * QW3) < (.00001# * PROB)) THEN EXIT FOR
           PROB = PROB - 2# / QIF * QW3
           QW3 = QW3 * QW1 * (QMD / QIF + 1#)
       NEXT QIF

       FOR QIF = QMD TO N1 STEP 2
           IF (N1 <= QIF) OR (ABS(2# / QIF * QW3) < (.00001# * PROB)) THEN EXIT FOR
           PROB = PROB + 2# / QIF * QW3
           QW3 = QW3 * (1# - QW1) * (QJFU / QIF + 1#)
       NEXT QIF
    CASE 2
       ' F(p)
    
       IF (N1 > 30 AND N2 > 30) THEN
          CALL NORMAL(2, X, PROB)
          QW1 = 2# / 9# / N1
          QW2 = 2# / 9# / N2
          QW3 = (1# - QW1) * (1# - QW2) + X * SQR((1# - QW1) ^ 2# * QW2 + (1# - QW2) ^ 2# * QW1 - QW1 * QW2 * X * X)
          X = (QW3 / ((1# - QW2) ^ 2# - QW2 * X * X)) ^ 3#
       ELSE
          QX1 = 0#
          QX2 = 1#
          QQ = .5#
          QP = PROB

          DO
             X = (1# / QQ - 1#) * N2 / N1
             CALL FDIST(1, N1, N2, X, PROB)
             IF (PROB <= QP) THEN
                QX1 = QQ
             ELSE
                QX2 = QQ
             END IF

             QX0 = QQ
             QQ = (QX1 + QX2) / 2#
             IF (ABS(QQ - QX0) <= (.00001# * QQ)) THEN EXIT DO
          LOOP
       
          X = (1# / QQ - 1#) * N2 / N1
          PROB = QP
       END IF
    END SELECT

END SUB
