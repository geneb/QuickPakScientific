' Copyright (c) 1988 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB DERIV1 (X, H, FP1, FP2, FP3, FP4) STATIC

    ' Derivatives of an analytic function subroutine

    ' Input

    '  X = point of interest
    '  H = step size

    ' Output

    '  FP1 = first derivative
    '  FP2 = second derivative
    '  FP3 = third derivative
    '  FP4 = fourth derivative

    ' NOTE: requires SUB USERFUNC

    CALL USERFUNC(X, FX)

    CALL USERFUNC(X + 2# * H, FXP2)

    CALL USERFUNC(X + H, FXP1)

    CALL USERFUNC(X - H, FXM1)

    CALL USERFUNC(X - 2# * H, FXM2)

    CALL USERFUNC(X + 3# * H, FXP3)

    CALL USERFUNC(X - 3# * H, FXM3)

    FP1 = (-FXP2 + 8# * FXP1 - 8# * FXM1 + FXM2) / (12# * H)

    FP2 = (-FXP2 + 16# * FXP1 - 30# * FX + 16# * FXM1 - FXM2) / (12# * H ^ 2)

    FP3 = (-FXP3 + 8# * FXP2 - 13# * FXP1 + 13# * FXM1 - 8# * FXM2 + FXM3) / (8# * H ^ 3)

    FP4 = (-FXP3 + 12# * FXP2 - 39# * FXP1 + 56# * FX - 39# * FXM1 + 12# * FXM2 - FXM3) / (6# * H ^ 4)

END SUB

