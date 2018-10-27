' Copyright (c) 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB NORMAL (IFLAG, X, PROB) STATIC

    ' Normal distribution subroutine
    
    ' Input

    '  IFLAG = type of computation
    '      1 = given x, compute probability
    '      2 = given probability, compute x
   
    ' Output (or input)

    '  X     = x value
    '  PROB  = probability; 0 < PROB < 1

    SELECT CASE IFLAG
    CASE 1
       ' N(x)
       X1 = ABS(X)
   
       IF (X1 > 7#) THEN
          PROB = 0#
       ELSE
          PROB = X1 * (.0000380036# + X1 * (.0000488906# + X1 * .000005383#))
          PROB = 1# + X1 * (.049867347# + X1 * (.0211410061# + X1 * (.0032776263# + PROB)))
          PROB = .5# * PROB ^ (-16#)
       END IF
       
       IF (X < 0#) THEN PROB = 1# - PROB
    CASE 2
       ' N(p)
       IF (PROB = .5#) THEN
          X = 0#
          EXIT SUB
       END IF

       IF (PROB > .5#) THEN
          X1 = 1# - PROB
       ELSE
          X1 = PROB
       END IF
      
       IF (X1 = .05#) THEN
          X = 1.64485#
       ELSEIF (X1 = .025#) THEN
          X = 1.95996#
       ELSEIF (X1 = .01#) THEN
          X = 2.32635#
       ELSEIF (X1 = .005#) THEN
          X = 2.57583#
       ELSE
          X1 = -LOG(4# * X1 * (1# - X1))
          X = (-3.231081277D-09 * X1 + 8.360937017D-08) * X1 - .00000104527497#
          X = (X * X1 + .000005824238515#) * X1 + .000006841218299#
          X = ((X * X1 - .0002250947176#) * X1 - .000836435359#) * X1 + .03706987906#
          X = X * X1 + 1.570796288#
          X = SQR(X1 * X)
       END IF

       IF (PROB > .5#) THEN X = -X
    END SELECT

END SUB

