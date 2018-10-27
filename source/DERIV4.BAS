' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB DERIV4 (X, H, DFDX, XERR) STATIC

    ' Numerical derivative subroutine

    ' Input
   
    '  X = X value of interest
    '  H = step size

    ' Output

    '  DFDX = estimate of first derivative at X
    '  XERR = error estimate

    ' Note: requires SUB USERFUNC

    DIM A(10, 10)

    CON = 1.4#
    CON2 = CON * CON

    HH = H
   
    CALL USERFUNC(X + HH, FUNC1)
    CALL USERFUNC(X - HH, FUNC2)

    A(1, 1) = (FUNC1 - FUNC2) / (2# * HH)

    XERR = 1D+30

    ' compute derivative

    FOR I = 2 TO 10
        HH = HH / CON
        CALL USERFUNC(X + HH, FUNC1)
        CALL USERFUNC(X - HH, FUNC2)
        A(1, I) = (FUNC1 - FUNC2) / (2# * HH)
        FAC = CON2
        FOR J = 2 TO I
            A(J, I) = (A(J - 1, I) * FAC - A(J - 1, I - 1)) / (FAC - 1#)
            FAC = CON2 * FAC
            TMP1 = ABS(A(J, I) - A(J - 1, I))
            TMP2 = ABS(A(J, I) - A(J - 1, I - 1))
            ERRT = TMP1
            IF (TMP2 > ERRT) THEN ERRT = TMP2
            IF (ERRT <= XERR) THEN
               XERR = ERRT
               DFDX = A(J, I)
            END IF
        NEXT J
        IF (ABS(A(I, I) - A(I - 1, I - 1)) >= 2# * XERR) THEN EXIT FOR
    NEXT I

    ERASE A

END SUB
