' Copyright (c) 1988, 1989, 1994 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB QUADRATIC (C1, C2, C3, X1, X2, NROOT)

SUB CUBIC (B1, B2, B3, B4, X1, X2, X3, NROOT) STATIC

    ' Solution of the cubic equation subroutine

    ' B1 * X^3 + B2 * X^2 + B3 * X + B4 = 0

    ' Input

    '  B1, B2, B3, B4 = coefficients of the cubic equation

    ' Output

    '  NROOT      = number of real roots
    '  X1, X2, X3 = real roots of the cubic equation

    ' NOTE: requires SUB QUADRATIC

    PI = 3.141592653589793#
    PI2 = 2# * PI
    PIDIV2 = .5# * PI
    ONETHIRD = 1# / 3#

    NROOT = 0

    X1 = 0#
    X2 = 0#
    X3 = 0#

    IF (ABS(B1) <= 1D-25) THEN
       CALL QUADRATIC(B2, B3, B4, X1, X2, NROOT)
       EXIT SUB
    END IF

    P = B2 / B1
    Q = B3 / B1
    R = B4 / B1

    SA = (3# * Q - P * P) / 3#
    SB = (2# * P * P * P - 9# * P * Q + 27# * R) / 27#
    DEL = (4# * Q * Q * Q - Q * Q * P * P - 18# * Q * P * R + 27# * R * R + 4# * P * P * P * R) / 108#

    IF (DEL < 0#) THEN
       NROOT = 3
      
       CPHI = -SB / 2# / SQR(SA * SA * SA / (-27#))
      
       IF (ABS(CPHI) > 1#) THEN
          ' small angle approximation
          SPHI = SQR((27# * DEL) / (SA * SA * SA))
          BETA = SPHI
          IF (-SB > 0#) THEN PHI = BETA
          IF (-SB < 0#) THEN PHI = PI - BETA
       ELSE
          ' ATAN3 calculation
          SPHI = SQR(1# - CPHI * CPHI)
          IF (ABS(SPHI) < .00000001#) THEN
             PHI = (1# - SGN(CPHI)) * PIDIV2
          ELSE
             PHI = (2# - SGN(SPHI)) * PIDIV2
             IF ABS(CPHI) > .00000001# THEN
                PHI = PHI + SGN(SPHI) * SGN(CPHI) * (ABS(ATN(SPHI / CPHI)) - PIDIV2)
             END IF
          END IF
       END IF
      
       E0 = 2# * SQR(-SA / 3#)
      
       X1 = E0 * COS(PHI / 3#) - P / 3#
       X2 = E0 * COS(PHI / 3# + PI2 / 3#) - P / 3#
       X3 = E0 * COS(PHI / 3# + 2# * PI2 / 3#) - P / 3#
       EXIT SUB
    ELSEIF (DEL = 0#) THEN
       NROOT = 3
       XTMP1 = -SB / 2#
       X1 = 2# * SGN(XTMP1) * ABS(XTMP1) ^ ONETHIRD - P / 3#
       XTMP2 = SB / 2#
       X2 = SGN(XTMP2) * ABS(XTMP2) ^ ONETHIRD - P / 3#
       X3 = X2
       EXIT SUB
    ELSEIF (DEL > 0#) THEN
       NROOT = 1
       XTMP1 = -SB / 2# + SQR(DEL)
       XTMP2 = -SB / 2# - SQR(DEL)
       X1 = SGN(XTMP1) * ABS(XTMP1) ^ ONETHIRD + SGN(XTMP2) * ABS(XTMP2) ^ ONETHIRD - P / 3#
    END IF

END SUB

