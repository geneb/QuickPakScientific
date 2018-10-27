' Copyright (c) 1988, 1989, 1990, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X(), F)

SUB NUMGRAD1 (N, X(), FX, GX()) STATIC

    ' Numerical gradient subroutine

    ' Input

    '  N   = number of variables
    '  X() = vector of independent variables

    ' Output

    '  FX   = scalar function value at X()
    '  GX() = gradient values at X()

    ' Note: requires SUB USERFUNC
   
    REDIM A(10, 10), XS(N)

    CON = 1.4#
    CON2 = CON * CON

    ' save initial X vector

    FOR I = 1 TO N
        XS(I) = X(I)
    NEXT I

    ' evalute current function value

    CALL USERFUNC(XS(), FX)

    ' compute partial derivatives

    FOR ICV = 1 TO N
        ' restore initial values
        FOR I = 1 TO N
            X(I) = XS(I)
        NEXT I

        ' zero working array

        FOR I = 1 TO 10
            FOR J = 1 TO 10
                A(I, J) = 0#
            NEXT J
        NEXT I

        ' set initial DX

        DX = 5#
  
        X(ICV) = XS(ICV) + DX

        CALL USERFUNC(X(), FUNC1)
 
        X(ICV) = XS(ICV) - DX

        CALL USERFUNC(X(), FUNC2)

        A(1, 1) = (FUNC1 - FUNC2) / (2# * DX)

        XERR = 1D+30

        ' compute partial derivative

        FOR I = 2 TO 10
            DX = DX / CON
            X(ICV) = XS(ICV) + DX
            CALL USERFUNC(X(), FUNC1)
            X(ICV) = XS(ICV) - DX
            CALL USERFUNC(X(), FUNC2)
            A(1, I) = (FUNC1 - FUNC2) / (2# * DX)
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
                   GX(ICV) = A(J, I)
                END IF
            NEXT J
            IF (ABS(A(I, I) - A(I - 1, I - 1)) >= 2# * XERR) THEN EXIT FOR
        NEXT I
    NEXT ICV

    ' restore initial values

    FOR I = 1 TO N
        X(I) = XS(I)
    NEXT I

    ERASE A, XS

END SUB

